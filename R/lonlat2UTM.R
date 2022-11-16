#' lonlat2UTM
#'
#' \code{lonlat2UTM} From Robin Lovelace, Geocomputation with R, see https://geocompr.robinlovelace.net/reproj-geo-data.html.
#' Calculates the UTM EPSG code associated with any point on the planet
#'
#' @param lonlat a vector of length 2 indicating the coordinates in WGS84 CRS as x an y
#'
#' @return a numeric, the UTM EPSG code associated to the coordinates.
#'
#' @author Robin Lovelace, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' # UTM EPSG code for Ifremer in Plouzané
#' lonlat2UTM(c(-4.7234, 48.4267))
#'
#' @export
#'


lonlat2UTM <- function( lonlat ){

  utm <- (floor((lonlat[1] + 180) / 6) %% 60) + 1

  if( lonlat[2] > 0){

    utm <- utm + 32600

  }else{

    utm <- utm + 32700

  }

  return(utm)

}

