#' SfBbox
#'
#' \code{SfBbox} Retrieves the bbox of a spatial object as a spatial polygon
#'
#' @param sfobj a spatial object of class sf
#'
#' @return the bounding box as a spatial polygon of class sf
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' pos.sf <- df2sfp(positions,
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' SpatialBBOX <- SfBbox(pos.sf)
#' SpatialBBOX
#'
#' @export
#'

SfBbox <- function(sfobj){

  if(!inherits(sfobj, "sf")) {stop("trip.path must be a valid sf object")}
  if(is.na(sf::st_crs(sfobj))) {stop("CRS must be defined for proper distance calculation")}

  if(nrow(sfobj) == 1){
    sfobj <- sf::st_buffer(sfobj, dist = 1)
  }
  bb <- sf::st_bbox(sfobj)
  bbox <- sf::st_sf("bb", geometry = coord2sf(as.matrix(expand.grid(bb[c(1,3)], bb[c(2,4)]))[c(1,3,4,2),], type = "polygon"))
  sf::st_crs(bbox) <- sf::st_crs(sfobj)

  return(bbox)

}
