#' CalcFeatures
#'
#' \code{CalcFeatures} Calculates additional features describing movements to be used with machine-learning models:
#' acceleration, proximity index, jerk, bearing rate, speed change, straigthness, sinuosity, turning angle, speed and direction change.
#' if an harbour object is provided another covariate "PointInharbour" is added.
#' A moving window may be provided
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param col.speed DEFAULT = "SPEED.kn", name of the speed column to be used, see ?CalcSpeed
#' @param speed.units DEFAULT = "knots", if different, the function will consider speed unit as being m/s
#' @param radius DEFAULT = NULL, radius used for calculating the proximity index in meters. If NULL it is calculated as the distance crossed by a boat sailing at 4.5 knots during the timelapse between two positions
#' @param n.nghbrs DEFAULT = 4, number of neighbours taken into account when calculating the proximity index
#' @param Harbours DEFAULT = NULL, a spatial object either points or polygons of class sf to describe the harbours
#' @param buf2Harb DEFAULT = 1, distance to the harbour for a position to be considered as "InHarbour".
#' @param movingWindow DEFAULT = 1, number of neighbours to consider when calculating the movingWindow
#'
#' @return trip.path, a spatial points object of class sf with new columns for calculated features
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' data(positions)
#'
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#' head(pos.sf)
#'
#' # Process the positions
#' posPro <- Process_TripPositions(pos.sf, resampling = 900, keep.var = "FishingOperation")
#' head(posPro)
#'
#' # Calculates the features
#' posForMl <- CalcFeatures(posPro$trip.path)
#' head(posForMl)
#'
#' plot_Trip(boat.path = posForMl,
#'           col.features = c("SPEED.kn", "BearingRate", "Sinuosity"),
#'           col.FishingOp = "FishingOperation")
#'
#'
#' @export
#'

CalcFeatures <- function(trip.path,
                         col.time =  "DATE_TIME",
                         col.speed = "SPEED.kn",
                         speed.units = "knots",
                         radius = NULL,
                         n.nghbrs = 4,
                         Harbours = NULL,
                         buf2Harb = 1,
                         movingWindow = 1){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(!col.speed %in% colnames(trip.path)) {stop("Speed column not found")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  if(nrow(trip.path) < 3) {stop("Less than 3 positions available: can't be processed")}

  if(!all(c("VESSEL_FK", "FISHING_TRIP_FK") %in% colnames(trip.path))) {stop("Vessel and fishingtrip columns not recognized: must be identified as VESSEL_FK and FISHING_TRIP_FK")}
  vessel.id <- unique(trip.path$VESSEL_FK)
  FT.id <- unique(trip.path$FISHING_TRIP_FK)
  if(length(vessel.id) > 1| length(FT.id) > 1) {stop("CalcFeatures function must be processed by vessel and fishing trip")}

  colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"

  timestamp <-   Char2Time(trip.path$DATE_TIME)
  vector.length <- length(timestamp)
  diff.Time <- c(NA, as.numeric(difftime(timestamp[2:vector.length], timestamp[1:(vector.length -1)], units = "sec")))
  if( any(!duplicated(diff.Time)[-(1:2)]) ) {stop("Timelapse between points is not regular, apply Resample_Traj function")}
  time.lapse <- unique(diff.Time)[!is.na(unique(diff.Time))]
  trip.path <- trip.path[order(timestamp), ]

  ### Acceleration
  if( !"Acceleration" %in% colnames(trip.path) ){
    trip.path <- CalcAcceleration(trip.path, col.time = "DATE_TIME", col.speed = col.speed, speed.units = speed.units)
  }

  ### Proximity index
  radius <- ifelse(is.null(radius), 4.5/0.539957*time.lapse/3.6, radius)

  planar.proj <- CustomizedProjectedCRS(trip.path)
  trip.planar <- sf::st_transform(trip.path, planar.proj)
  coords <- sf::st_coordinates(trip.planar)
  nnn <- nrow(coords)

  Proximity.index <- do.call(what = function(...){ c(...)},
                             args = lapply(1:nnn, function(k){

                                index.coords <- (k-n.nghbrs):(k+n.nghbrs)
                                MyCenter <- index.coords[ 1 + floor(length(index.coords)/2) ]
                                index.coords <- index.coords[ index.coords > 0 & index.coords < (nnn)]
                                MyCenter.pos <- which(index.coords == MyCenter)

                                Rann.output <- RANN::nn2(coords[index.coords, ], k = length(index.coords))
                                Rann.dist <- Rann.output$nn.dists[ MyCenter.pos,]
                                prox <- sum(Rann.dist < radius)-1
                                if(prox < 0){ prox <- 0}

                                return(prox)}))

  trip.path$ProximityIndex <- Proximity.index

  ### Jerk
  Jerk <- c(0, (trip.path$Acceleration[2:nnn] - trip.path$Acceleration[1:(nnn-1)]) / diff.Time[-1])
  trip.path$Jerk <- Jerk

  ### Bearing
  pts.df <- sfp2df(trip.path, rename.coords = c("x", "y"))

  Bearing <- c(TrackReconstruction::CalcBearing(pts.df$x[-nnn], pts.df$y[-nnn], pts.df$x[-1], pts.df$y[-1])*360/(2*pi), NA)
  trip.path$Bearing <- Bearing

  # bearing rate
  BearingRate <- c(NA, abs(Bearing[-1] - Bearing[-nnn]))
  trip.path$BearingRate <- BearingRate

  ### Speed Change
  SpeedChange <- c(abs((trip.path$SPEED.kn[-1] - trip.path$SPEED.kn[-nnn])/trip.path$SPEED.kn[-nnn]), NA)
  SpeedChange[1] <- NA
  SpeedChange[ SpeedChange %in% Inf] <- NA
  trip.path$SpeedChange <- SpeedChange

  # Q en 1, speed = 0, acc = 0? non, même pour Jerk, Bearing, SpeedChange

  ### Straigthness & Sinuosity
  trj <- Resample_Traj(trip.path, resampling = NULL, return.traj = TRUE)

  StraigthSinuos <- do.call(rbind, lapply( 2:(nnn-1), function(j) {

    strai <- trajr::TrajStraightness(trj[(j-1):(j+1),])
    sinuo <- trajr::TrajSinuosity2(trj[(j-1):(j+1),])

    return(data.frame(t(c(strai, sinuo))))

    }))

  StraigthSinuos <- rbind(c(NA, NA), StraigthSinuos, c(NA, NA))
  colnames(StraigthSinuos) <- c("Straigthness", "Sinuosity")
  StraigthSinuos$Sinuosity <- ifelse(StraigthSinuos$Sinuosity >1, 1, StraigthSinuos$Sinuosity)

  trip.path <- cbind(trip.path, StraigthSinuos)

  ### Turning angle
  TurningAngle <- c(NA, trajr::TrajAngles(trj, lag=1)*180/pi, NA)
  trip.path$TurningAngle <- TurningAngle

  ### Direction change
  DirectionChange <- c(NA, trajr::TrajDirectionalChange(trj)*180/pi, NA)
  trip.path$DirectionChange <- DirectionChange

  ### Covariates
  covar.names <- c("SPEED.kn", "Acceleration", "ProximityIndex", "Jerk", "Bearing", "BearingRate", "SpeedChange", "Straigthness",
                   "Sinuosity", "TurningAngle", "DirectionChange")

  if(movingWindow > 0){

    n.zeros <- ifelse(nchar(movingWindow) > 2, nchar(movingWindow), 2)
    TobeAdded <- sf::st_set_geometry(trip.path[, covar.names], NULL)

    for (mW in 1:movingWindow){

      Add.NA.rows <- stats::setNames(data.frame( matrix(NA, nrow = mW, ncol = length(covar.names))), covar.names)

      Add.previous <- rbind(Add.NA.rows ,
                            TobeAdded[-((nrow(TobeAdded)-mW+1):(nrow(TobeAdded))), ] )

      colnames(Add.previous) <- paste(covar.names, Harmonize_Ids("prev", mW, ID.length = n.zeros, sep =""), sep = "_")

      Add.next <- rbind( TobeAdded[-(1:mW), ] ,
                         Add.NA.rows )

      colnames(Add.next) <- paste(covar.names, Harmonize_Ids("next", mW, ID.length = n.zeros, sep =""), sep = "_")

      trip.path <- cbind(trip.path, Add.previous, Add.next)

    }

  }

  ### Point in Harbour
  if(!is.null(Harbours)){
    its <- apply(sf::st_intersects(trip.path, sf::st_buffer(Harbours, dist = buf2Harb * 1000 / 0.539957), sparse =  FALSE), 1, any)
    trip.path$InHarbour <- its
  }

  trip.path <- trip.path[, c(which( !colnames(trip.path) %in% "geometry"), which( colnames(trip.path) %in% "geometry"))]

  return(trip.path)

}
