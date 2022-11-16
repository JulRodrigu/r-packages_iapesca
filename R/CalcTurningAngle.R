#' CalcTurningAngle
#'
#' \code{CalcTurningAngle} Calculate the turning angle between two positions relative to the previous one (in degrees).
#' This function has to be applied by fishing trip, vessel and fishingtrip columns being identified as VESSEL_FK and FISHING_TRIP_FK.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param MaxSpeed DEFAULT = 25, speed threshold in nautic miles, see CleanSpuriousSpeeds
#' @param keep.nas DEFAULT = FALSE, the first value being missing, it returns 0 instead if set to FALSE
#'
#' @return trip.path, a spatial points object of class sf with a new column "TURN.deg" in degrees
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' # The function CalcSpeed is applied by Fishing trip
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#' pos.turn <- CalcTurningAngle(pos.sf)
#' pos.turn
#'
#' @export
#'

CalcTurningAngle <- function(trip.path,
                             col.time = "DATE_TIME",
                             MaxSpeed = 25,
                             keep.nas = FALSE){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  if(!all(c("VESSEL_FK", "FISHING_TRIP_FK") %in% colnames(trip.path))) {
    stop("Vessel and fishingtrip columns not recognized: must be identified as VESSEL_FK and FISHING_TRIP_FK")}

  vessel.id <- unique(trip.path$VESSEL_FK)
  FT.id <- unique(trip.path$FISHING_TRIP_FK)

  if(length(vessel.id) > 1| length(FT.id) > 1) {
    stop("CalcTurningAngle function must be processed by vessel and fishing trip")}

  pos.speed <- CleanSpuriousSpeeds(trip.path, col.time = col.time, MaxSpeed = MaxSpeed)
  planar.coord <- CustomizedProjectedCRS(trip.path)
  pos.planar <- sf::st_transform(pos.speed, planar.coord)
  pos.xy <- sfp2df(pos.planar, rename.coords = c("X", "Y"))
  pos.xy$Timer <- cumsum(pos.xy$DIFFTIME.secs)

  TurningAngle <- c(NA, trajr::TrajAngles(trajr::TrajFromCoords(pos.xy[, c("X", "Y", "Timer")]), lag=1)*180/pi, NA)
  if(!keep.nas){
    TurningAngle[is.na(TurningAngle)] <- 0
  }
  trip.path$TURN.deg <- abs(TurningAngle)

  trip.path <- trip.path [, c(which(!colnames(trip.path) %in% "geometry"), which(colnames(trip.path) %in% "geometry"))]

  return(trip.path)

}
