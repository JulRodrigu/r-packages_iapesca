#' CalcAcceleration
#'
#' \code{CalcAcceleration} Calculate acceleration between two positions.
#' By convention the acceleration is calculated between the position and the previous one
#' Return the original sf object with a column "Acceleration" in m/s².
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct
#' @param col.speed DEFAULT = "SPEED.kn", name of the speed column
#' @param speed.units DEFAULT = "knots", if not, the function will consider speed unit as being m/s
#'
#' @return trip.path, a spatial points object of class sf with a new column "Acceleration" in m/s²
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' # The function CalcAcceleration is applied by Fishing trip
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' pos.sfWspeed <- CalcSpeed(pos.sf)
#' summary(pos.sfWspeed)
#' pos.sfWacc <- CalcAcceleration(pos.sfWspeed)
#' summary(pos.sfWacc)
#'
#'
#' @export
#'
CalcAcceleration <- function(trip.path, col.time =  "DATE_TIME", col.speed = "SPEED.kn", speed.units = "knots"){

  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(!col.speed %in% colnames(trip.path)) {stop("Speed column not found")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"

  timestamp <-   Char2Time(trip.path$DATE_TIME)
  vector.length <- length(timestamp)
  diff.Time <- c(NA, as.numeric(difftime(timestamp[2:vector.length], timestamp[1:(vector.length -1)], units = "sec")))

  if( inherits(trip.path, "sf") ) {
    speed <- unlist( sf::st_set_geometry(trip.path[, col.speed], NULL))
  }else{
    speed <- trip.path[, col.speed]
  }

  if( speed.units %in% "knots"){

    speed <- speed * 1852/3600

  }

  diff.Speed <- c(0, as.numeric(speed[2:vector.length] - speed[1:(vector.length -1)]))

  acceleration <- diff.Speed / diff.Time
  acceleration[is.na(diff.Time)] <- 0

  trip.path$Acceleration <- acceleration

  colnames(trip.path) [ colnames(trip.path) %in% "DATE_TIME"] <- col.time

  if( inherits(trip.path, "sf") ) {
    trip.path <- trip.path[, c(which( !colnames(trip.path) %in% "geometry"), which( colnames(trip.path) %in% "geometry"))]
  }

  return(trip.path)

}
