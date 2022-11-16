#' Detect_FishingTrips
#'
#' \code{Detect_FishingTrips} Defines the fishing trip from positions.
#' The function first cleans the positions and calculates speed then defines the fishing trips.
#' 3 methods are available: it may either use a buffer with harbours or coasline and eventually add a decision rule based on speed threshold.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param Constantstring DEFAULT "FT", a string to be defined as a constant in fishing trip identifier
#' @param SpeedLimit DEFAULT = NA, speed threshold in nautic miles, if a value is defined,
#' then the positions which speed is below will be considered as a stop in Fishing trip within the limits eventually defined using harbour or coastline buffers.
#' @param MaxSpeed DEFAULT = 25, speed threshold in nautic miles, argument for CleanSpuriousSpeeds function
#' @param Harbours DEFAULT = NULL, a spatial object either points or polygons of class sf to describe the harbours
#' @param CoastLine DEFAULT = NULL, a spatial line objet of class sf describing the coastline. If set together to NULL with harbours and DetectCoastLine is TRUE, a coastline is downloaded
#' @param DetectCoastLine DEFAULT = TRUE, if set to TRUE with Harbours and CoastLine set to NULL a coastline is downloaded using geodata with PathStudyBoundaries function
#' @param buf.size.start DEFAULT = 2000, minimal distance to the harbour or coastline in meters to define the start/end of a fishing trip.
#' The buffer will be enlarged until an intersection is found or it reaches a maximal threshold of 2 nautic miles (3704 m)
#' @param min.stop DEFAULT = 3600, minimal duration in seconds to define a stop
#'
#' @return a list containing trip.path, a spatial points object of class sf with new columns: "FISHING_TRIP_FK", the fishing trip identifier, and the columns issued from application of CalcSpeed function
#' method, a character string indicating the method used: "Harbours", "Coastline" and or SpeedLimit
#' buf.size, a numeric returning the final value of the buffer used
#' SpeedLimit, a numeric returning the SpeedLimit used if not missing.
#'
#' @author ICES (2022). Workshop on Geo-Spatial Data for Small-Scale Fisheries (WKSSFGEO).
#' ICES Scientific Reports/Rapports scientifiques du CIEM , 4(10), 60pp.
#' Publisher's official version : https://doi.org/10.17895/ices.pub.10032
#' Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' require(mapview)
#' data(positions)
#' data(harbours)
#'
#' pos.sf <- df2sfp(positions,
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' pos.NewFt.Harb <- Detect_FishingTrips(pos.sf, Harbours = harbours)
#' pos.NewFt.Harb
#' table(pos.NewFt.Harb$trip.path$FISHING_TRIP_FK)
#' table(positions$FISHING_TRIP_FK)
#' pos.sf <- sf::st_join(pos.sf, pos.NewFt.Harb$trip.path[, "FISHING_TRIP_FK"])
#' with(pos.sf, table(FISHING_TRIP_FK.x, FISHING_TRIP_FK.y))
#'
#' trip <- "18529291"
#'
#' mapview(pos.sf[ pos.sf$FISHING_TRIP_FK.x %in% trip, ], zcol ="FISHING_TRIP_FK.y")
#'
#'
#' @export
#'

Detect_FishingTrips <- function(trip.path,
                                col.time = "DATE_TIME",
                                Constantstring = "FT",
                                SpeedLimit = NA,
                                MaxSpeed = 25,
                                Harbours = NULL,
                                CoastLine = NULL,
                                DetectCoastLine = TRUE,
                                buf.size.start = 2000,
                                min.stop = 3600) {

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  # Define.method
  # First: Harbour, third Coastline, second: SpeedLimit
  if( !is.null(Harbours) ){
    DetectCoastLine = FALSE
  }

  if( "FISHING_TRIP_FK" %in% colnames(trip.path) ){
    trip.path <- trip.path[,  !colnames(trip.path) %in% "FISHING_TRIP_FK" ]
  }

  ### Calc Speed and remove spurious Speeds

  trip.path.wspeed <- CleanSpuriousSpeeds(trip.path, col.time = col.time, MaxSpeed = MaxSpeed)

  colnames(trip.path.wspeed)[ colnames(trip.path.wspeed) %in% col.time ] <- "DATE_TIME"
  timestamp <- Char2Time(DateTime = trip.path.wspeed$DATE_TIME)
  colnames(trip.path.wspeed)[ colnames(trip.path.wspeed) %in% "DATE_TIME" ] <- col.time

  ### Define buffers
  obj.crs <- sf::st_crs(trip.path)
  bb <- sf::st_bbox(trip.path.wspeed)
  diag.length <- sf::st_distance(coord2sf(bb[c(1,2)], epsg = obj.crs),
                                 coord2sf(bb[c(3,4)], epsg = obj.crs))
  buf.size <- buf.size.start
  buf.size.max <- 2/0.539957*1000

  if ( as.numeric(diag.length)/100 > buf.size & as.numeric(diag.length)/100 < buf.size.max){
    buf.size <- as.numeric(diag.length)/100
  }

  ### use coastline

  if( DetectCoastLine ){

    method <- "CoastLine"

    if( is.null(CoastLine) ){

      CoastLine <- PathStudyBoundaries(trip.path.wspeed, CoastLine = TRUE, FineCountryLimits = TRUE, buffer = buf.size)

    }

    IsFishingTrip <- (1:nrow(trip.path.wspeed))
    if(nrow(CoastLine) > 0){

    its <- sf::st_intersects(trip.path.wspeed, sf::st_buffer(CoastLine, dist = buf.size), sparse = FALSE)
    it <- 1

    while( !any(its) & it < 6 & buf.size < buf.size.max){

      buf.size <- buf.size + as.numeric(diag.length)/100
      buf.size <- ifelse(buf.size > buf.size.max, buf.size.max, buf.size)
      its <- sf::st_intersects(trip.path.wspeed, sf::st_buffer(CoastLine, dist = buf.size), sparse = FALSE)
      it <- it + 1

    }

    IsFishingTrip <- (1:nrow(trip.path.wspeed))[!its]

    }

  }

  ### use Harbours

  if( !is.null(Harbours) ){

    method <- "Harbours"

    its <- apply(sf::st_intersects(trip.path.wspeed, sf::st_buffer(Harbours, dist = buf.size), sparse = FALSE), 1, any)
    it <- 1

    while( !any(its) & it < 6 & buf.size < buf.size.max){

      buf.size <- buf.size + as.numeric(diag.length)/100
      buf.size <- ifelse(buf.size > buf.size.max, buf.size.max, buf.size)
      its <- apply(sf::st_intersects(trip.path.wspeed, sf::st_buffer(Harbours, dist = buf.size), sparse = FALSE), 1, any)
      it <- it + 1

    }

    IsFishingTrip <- (1:nrow(trip.path.wspeed))[!its]

  }

  if( !anyNA(SpeedLimit) ){

    if(exists("IsFishingTrip")){

      method <- paste(method, "SpeedLimit", sep = "&")
      previous.IsFishingTrip <- IsFishingTrip
      IsntFishingTrip <- (1:nrow(trip.path.wspeed))[-previous.IsFishingTrip ]
      ind.belowSpeedLimit <- trip.path.wspeed$SPEED.kn[IsntFishingTrip] < SpeedLimit

      if(length(IsntFishingTrip[ind.belowSpeedLimit]) > 0){
        IsFishingTrip <- (1:nrow(trip.path.wspeed))[-IsntFishingTrip[ind.belowSpeedLimit]]
      }

    }else{

      method <- "SpeedLimit"
      ind.belowSpeedLimit <- trip.path.wspeed$SPEED.kn < SpeedLimit

      start.pos <- which(ind.belowSpeedLimit)[1]
      start.pol <- sf::st_buffer(trip.path[start.pos, ], dist = buf.size)

      end.pos <- which(ind.belowSpeedLimit)[length(which(ind.belowSpeedLimit))]
      end.pol <- sf::st_buffer(trip.path[end.pos, ], dist = buf.size)

      # Look for potential fishing positions
      # Remove positions intersecting with start.pol and end.pol (harbour) and below speed limit
      its.start <- sf::st_intersects(trip.path.wspeed, start.pol, sparse = FALSE)
      its.end <- sf::st_intersects(trip.path.wspeed, end.pol, sparse = FALSE)

      IsFishingTrip <- (1:nrow(trip.path.wspeed))[!ind.belowSpeedLimit & !its.start[,1] & !its.end[,1]]

      }

    }

  min.timelapse <- floor(mean(trip.path.wspeed$DIFFTIME.secs, na.rm = TRUE)/min.stop) + 1
  FT.Change <- c(1, IsFishingTrip[which(IsFishingTrip[-1] - IsFishingTrip[-length(IsFishingTrip)] > min.timelapse)])
  FT.Change <- unique(FT.Change[ FT.Change < (nrow(trip.path.wspeed) - min.timelapse)])
  l.FT <- length(FT.Change)
  if(l.FT > 1){
    TimeBreak <- c(NA, as.numeric(difftime(timestamp[FT.Change[2:l.FT]], timestamp[FT.Change[1:(l.FT-1)]], units = "sec")))
  }else{ TimeBreak <- NA}

  FT.Change <- FT.Change[ TimeBreak > min.stop | is.na(TimeBreak)]
  FT.names <- substr( gsub(timestamp, pattern = "-", replacement = ""), 1, 8)[FT.Change]
  ID.length <- ifelse(nchar(length(FT.names)) > 3, nchar(length(FT.names)), 3)
  FT.names <- Harmonize_Ids(paste0(Constantstring, FT.names), 1:length(FT.names), ID.length = ID.length)
  FT.Change <- c(FT.Change, nrow(trip.path.wspeed))

  trip.path.wspeed$FISHING_TRIP_FK <- FT.names[1]

  for(k in 1:length(FT.names)){
    trip.path.wspeed$FISHING_TRIP_FK[FT.Change[k]:FT.Change[k+1]] <- FT.names[k]
    FT.Change[k+1] <- FT.Change[k+1]+1
  }

  trip.path.wspeed <-  trip.path.wspeed[ IsFishingTrip,  ]
  trip.path.wspeed <- trip.path.wspeed[, c(which(!colnames(trip.path.wspeed) %in% "geometry"), which(colnames(trip.path.wspeed) %in% "geometry"))]

  return( list(trip.path = trip.path.wspeed, method = method, buf.size  = buf.size, SpeedLimit = SpeedLimit))

}



