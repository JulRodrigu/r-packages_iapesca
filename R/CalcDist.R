#' CalcDist
#'
#' \code{CalcDist} Computes distances between positions in nautic miles, by convention the path is defined between the position and the previous one.
#' Two methods have been implemented: "sf" uses great circle distance calculations with sf::st_distance function for longlat coordinates,
#' "nn2" uses RANN::nn2 function for positions reprojected with cartesian coordinates.
#' Function to be applied by fishing trip.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param method DEFAULT = "sf", two methods are available to calculate the distance: "sf" for longlat coordinates, "nn2" uses a reprojection in cartesian coordinates
#'
#' @return trip.path, a spatial points object of class sf with a new column named DISTANCE.nm
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' # The function CalcDist is applied by Fishing trip
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#' coords = c("LONGITUDE", "LATITUDE"))
#'
#' # Compare default method (sf::st_distance) and "nn2" based on RANN::nn2
#' system.time(
#'   pos.sfMeth <- CalcDist(pos.sf)
#' )
#'
#' system.time(
#'   pos.nn2Meth <- CalcDist(pos.sf, method = "nn2")
#' )
#'
#' plot(pos.sfMeth$DISTANCE.nm ~ pos.nn2Meth$DISTANCE.nm, pch = "+")
#' abline(a = 0, b = 1, lty = 2)
#' all.equal(pos.sfMeth$DISTANCE.nm, pos.nn2Meth$DISTANCE.nm)
#'
#'
#' @export
#'

CalcDist <- function(trip.path, col.time = "DATE_TIME", method = "sf"){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  if(!method %in% c("nn2", "sf")) {stop("implemented methods for distance calculation: sf or nn2")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"

  trip.path <- trip.path[, !colnames(trip.path) %in% "DISTANCE.nm"]
  timestamp <-   Char2Time(trip.path$DATE_TIME)
  trip.path <- trip.path[ order(timestamp), ]

  colnames(trip.path)[ colnames(trip.path) %in% "DATE_TIME"] <- col.time

  if( method %in% "nn2"){

    crs.origin <- sf::st_crs(trip.path)
    if( crs.origin != sf::st_crs(4326) ){
      trip.path <- sf::st_transform(trip.path, sf::st_crs(4326))
    }
    planar.proj <- lonlat2UTM(apply(sf::st_coordinates(trip.path), 2, function(x){mean(x, na.rm = TRUE)}))
    trip.planar <- sf::st_transform(trip.path, planar.proj)
    coords <- sf::st_coordinates(trip.planar)

    nnn <- nrow(coords)
    dist.betweenPoints.m <- sapply(1:(nnn-1), function(k){
      dist <- RANN::nn2(coords[k:(k+1), ], k = 2)$nn.dists[2, 2]
      return(dist)})

    dist.betweenPoints <- c(0, dist.betweenPoints.m)* 0.539957/1000

  }else{

    dist.unit <- base::units(sf::st_distance(trip.path[1,], trip.path[2,]))[[1]]
    if( "m" %in% dist.unit) {
      dist.betweenPoints <- c(0, as.numeric(sf::st_distance(trip.path[-nrow(trip.path), ], trip.path[-1, ], by_element = TRUE)))* 0.539957/1000
    }else{ stop("distance unit not recognized")}

  }

  trip.path$DISTANCE.nm <-  dist.betweenPoints
  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]

  return(trip.path)

}

