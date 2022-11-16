#' CustomizedProjectedCRS
#'
#' \code{CustomizedProjectedCRS} Creates a customized planar crs for reprojecting the data in a cartesian system
#'
#' @param sfobj sf, sfc or sp spatial object with a valid CRS set
#'
#' @return an object of class crs
#'
#' @author Zivan Karaman and Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' # Customized planar crs for Ifremer in Plouzané
#' planar.crs <- CustomizedProjectedCRS(coord2sf(c(-4.7234, 48.4267), epsg = 4326))
#' planar.crs
#'
#' # Customized planar crs for an anomymized vessel positions
#' data(positions)
#'
#' pos.sf <- df2sfp(positions, coords = c("LONGITUDE", "LATITUDE"))
#'
#' planar.crs <- CustomizedProjectedCRS(pos.sf)
#' planar.crs
#'
#'
#' @export
#'

CustomizedProjectedCRS <- function(sfobj){

  if( inherits(sfobj, c("sfc", "Spatial"))) { sfobj <-  sf::st_as_sf(sfobj)}
  if( !inherits(sfobj, c("sf", "sfc"))) stop("sfobj must be a sf spatial Object with valid CRS set")
  if( is.na(sf::st_crs(sfobj))) stop("sfobj must be a sf spatial Object with valid CRS set")

  bb <- sf::st_bbox( sf::st_transform(sfobj, crs = sf::st_crs(4326)))

    if(nrow(sfobj) > 1){
      bbox <- sf::st_sf("bb", coord2sf(as.matrix(expand.grid(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")]))[c(1,3,4,2),], type = "polygon"))
      boxLL <- sf::st_bbox(bbox)
    }else{
      boxLL <- sf::st_bbox(sfobj)
    }

  llc <- c(mean( boxLL[c("xmin", "xmax")]),
         mean(boxLL[c("ymin", "ymax")]))

  angle <- sqrt( diff(boxLL[c("xmin", "xmax")])^2 +  diff(boxLL[c("ymin", "ymax")])^2)

  prj = paste0(
    "+proj=omerc +lat_0=",
    llc[2],
    " +lonc=",
    llc[1],
    " +alpha=",
    angle,
    " +gamma=0.0 +k=1.000000 +x_0=0.000 +y_0=0.000 +ellps=WGS84 +units=m "
    )

  return( sf::st_crs(prj))

}

