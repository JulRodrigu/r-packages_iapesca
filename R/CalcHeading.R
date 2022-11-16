#' CalcHeading
#'
#' \code{CalcHeading} Calculate heading between two positions in degrees.
#'
#' @param trip.path a spatial points object of class sf
#'
#' @return trip.path, a spatial points object of class sf with a new column "HEADING.deg " in degrees
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' # The function CalcHeading is applied by Fishing trip
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' pos.sfWHeading <- CalcHeading(pos.sf)
#' summary(pos.sfWHeading)
#'
#'
#' @export
#'

CalcHeading <- function(trip.path){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  planar.proj <- CustomizedProjectedCRS(trip.path)
  trip.planar <- sf::st_transform(trip.path, planar.proj)

  coords <- sf::st_coordinates(trip.planar)

  dir <- sapply(1:(nrow(trip.path)-1),
                function(i){ as.numeric(atan2((coords[i+1, 2] - coords[i, 2]),  (coords[i+1, 1] - coords[i, 1]))) })
  dir <- c(dir, dir[length(dir)])
  # Negative values of angle
  dir[ dir<0 ] <- 2 * pi + dir[ dir<0 ]
  # To degrees
  dir <- dir*180/pi

  trip.path$HEADING.deg <- dir
  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]

  return(trip.path)

}
