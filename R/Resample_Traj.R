#' Resample_Traj
#'
#' \code{Resample_Traj} Resamples linearily the positions with a constant timelapse between them.
#' This function has to be applied by fishing trip, vessel and fishingtrip columns being identified as VESSEL_FK and FISHING_TRIP_FK.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param MaxSpeed DEFAULT = 25, speed threshold in nautic miles
#' @param resampling DEFAULT = NULL, the timelapse for resampling in seconds, if NULL it is resampled to average timelapse of the original dataset
#' @param return.traj DEFAULT = FALSE, if TRUE, returns the output of trajr::TrajResampleTime function as a data.frame projected in customized cartesian CRS
#' @param keep.var DEFAULT = NULL, adds additionnal columns to the resampled positions from the original dataset
#'
#' @return track, a spatial points object of class sf with columns identifying the timestamp, vessel and fishing trip and additional columns defined with keep.var argument.
#' if return.traj set to TRUE, the output of trajr::TrajResampleTime function as a data.frame projected in customized cartesian CRS
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' # Resample to 1 hour
#' pos.60min <- Resample_Traj(pos.sf, resampling = 3600,  keep.var = "FishingOperation" )
#' dim(pos.sf)
#' dim(pos.60min)
#' pos.60min
#'
#' @export
#'

Resample_Traj <- function(trip.path,
                          col.time = "DATE_TIME",
                          MaxSpeed = 25,
                          resampling = NULL,
                          return.traj = FALSE,
                          keep.var = NULL){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}
  if(!all(c("VESSEL_FK", "FISHING_TRIP_FK") %in% colnames(trip.path))) {stop("Vessel and fishingtrip columns not recognized: must be identified as VESSEL_FK and FISHING_TRIP_FK")}

  vessel.id <- unique(trip.path$VESSEL_FK)
  FT.id <- unique(trip.path$FISHING_TRIP_FK)

  if(length(vessel.id) > 1| length(FT.id) > 1) {stop("Resample_Traj function must be processed by vessel and fishing trip")}

  colnames(trip.path)[colnames(trip.path) %in% col.time] <- "DATE_TIME"
  timestamp <- Char2Time(trip.path$DATE_TIME)
  trip.path$DATE_TIME <- as.character(timestamp)
  trip.path <- trip.path[ order(timestamp), ]

  pos <- dplyr::distinct(trip.path)
  pos.speed <- CleanSpuriousSpeeds(pos[, !colnames(pos) %in% "SPEED.kn" ], col.time = "DATE_TIME", MaxSpeed = MaxSpeed)

  if(is.null(resampling)){
    resampling <- floor(mean(pos.speed$DIFFTIME.secs[-1]))
  }

  planar.coord <- CustomizedProjectedCRS(pos.speed)
  pos.planar <- sf::st_transform(pos.speed, planar.coord)
  pos.xy <- sfp2df(pos.planar, rename.coords = c("X", "Y"))
  pos.xy$Timer <- cumsum(pos.xy$DIFFTIME.secs)
  Interpolated.Track <- trajr::TrajResampleTime(
    trajr::TrajFromCoords(pos.xy[, c("X", "Y", "Timer")], xCol=1, yCol=2, timeCol=3, spatialUnits = "m", timeUnits = "secs"),
    stepTime = resampling)
  Interpolated.Track$DATE_TIME <- Char2Time(Char2Time(pos$DATE_TIME[1]) + Interpolated.Track$time)
  colnames(Interpolated.Track)[ colnames(Interpolated.Track) %in% "DATE_TIME"] <- col.time
  Interpolated.Track$VESSEL_FK <- vessel.id
  Interpolated.Track$FISHING_TRIP_FK <- FT.id

  col2sel <- c("x", "y", col.time, "VESSEL_FK", "FISHING_TRIP_FK")

  if(!is.null(keep.var)){

    if( !all(keep.var %in% col2sel)){

    colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"
    colnames(Interpolated.Track)[ colnames(Interpolated.Track) %in% col.time] <- "DATE_TIME"

    former.timestamp <- Char2Time(trip.path$DATE_TIME)
    new.timestamp <- Char2Time(Interpolated.Track$DATE_TIME)

    link2formerIndex <- sapply(1:length(new.timestamp), function(t){

      ind.before <- which(former.timestamp <= new.timestamp[t])
      ind.before <- ind.before[length(ind.before)]
      ind.after <- which(former.timestamp > new.timestamp[t])[1]
      ind2Select <- unique(c(ind.before, ind.after))

      if(length(ind2Select) > 1){
        sel.min <- which.min(abs(as.numeric(difftime(new.timestamp[t], former.timestamp[ind2Select]))))
        ind2Select <- ind2Select[sel.min]
      }

      return(ind2Select)

    })

    Tab2Add <- sf::st_set_geometry(trip.path[link2formerIndex, keep.var], NULL)
    Interpolated.Track <- cbind(Interpolated.Track, Tab2Add)

    colnames(Interpolated.Track)[ colnames(Interpolated.Track) %in% "DATE_TIME" ] <- col.time

    col2sel <- unique(c("x", "y", col.time, "VESSEL_FK", "FISHING_TRIP_FK", keep.var))

    }

  }

  if(!return.traj){
    track.sf <- df2sfp(Interpolated.Track[, col2sel], c("x", "y"), input.crs = planar.coord, output.crs = sf::st_crs(4326))
    track <-  track.sf
  }else{
    track <- Interpolated.Track
  }

  return(track)

}
