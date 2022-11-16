#' CalcSpeed
#'
#' \code{CalcSpeed} Calculate speed between two positions. By convention the speed is calculated between the position and the previous one
#' Return the original sf object with a column "DIFFTIME.secs" in seconds, "SPEED.kn" in knots and eventually "DISTANCE.nm"
#' in nautic miles if CalcDist hasn't been applied before.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param verbose DEFAULT = FALSE, print non warning messages
#' @param method DEFAULT = "sf", two methods are available to calculate the distance: "sf" for longlat coordinates, "nn2" uses a reprojection in cartesian coordinates
#'
#' @return trip.path, a spatial points object of class sf with new columns "DISTANCE.nm" in nautic miles, DIFFTIME.secs" in seconds and "SPEED.kn" in knots
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
#' coords = c("LONGITUDE", "LATITUDE"))
#'
#' pos.sfWspeed <- CalcSpeed(pos.sf)
#' summary(pos.sfWspeed)
#'
#' all.equal(pos.sfWspeed$speed, pos.sfWspeed$SPEED.kn)
#' # Differences between speed is explained by the convention used:
#' # $speed is calculated between the position and the next one
#' # CalcSpeed returns a speed calculated between a position and the previous one.
#'
#' @export
#'

CalcSpeed <- function(trip.path,
                      col.time = "DATE_TIME",
                      verbose = FALSE,
                      method = "sf"){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  # Test wether points are duplicated
  colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"
  nr <- nrow(trip.path)

  timestamp <-   Char2Time(trip.path$DATE_TIME)
  test.dup <- duplicated(timestamp) & duplicated(sf::st_geometry(trip.path))

  if(any(test.dup)){
    trip.path <- trip.path[!test.dup, ]
    timestamp <-   Char2Time(trip.path$DATE_TIME)
    nr <- nrow(trip.path)
    }

  diff.Time <- c(NA, as.numeric(difftime(timestamp[2:nr], timestamp[1:(nr-1)], units = "sec")))

  if( !"DISTANCE.nm" %in% colnames(trip.path)) {
    trip.path <- CalcDist(trip.path = trip.path, col.time = "DATE_TIME", method = method)
    if(verbose) {print("Calculate Distance using CalcDist")}
  }
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}

  speed <- trip.path$DISTANCE.nm / (diff.Time/3600)
  speed[is.na(diff.Time)] <- 0
  diff.Time[is.na(diff.Time)] <- 0

  trip.path$DIFFTIME.secs <- diff.Time
  trip.path$SPEED.kn <- speed
  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]
  colnames(trip.path)[ colnames(trip.path) %in% "DATE_TIME"] <- col.time

  return(trip.path)

}
