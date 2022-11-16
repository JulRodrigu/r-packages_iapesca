#' coord2sf
#'
#' \code{coord2sf} Spatializes coordinates.
#'
#' @param df data.frame or matrix of coordinates to be converted in sf object.
#' @param type DEFAULT "multipoint", type of sf object, "linestring" for line, "polygon" for polygon.
#' @param epsg either a valid "crs" object or a valid numeric espg code
#'
#' @return a spatial object of sf class
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @importFrom sf st_multipoint
#' @importFrom sf st_linestring
#' @importFrom sf st_polygon
#' @importFrom sf st_sfc
#' @importFrom sf st_cast
#'
#' @examples
#'
#' obj <- matrix(c( 181109.5, 182616.6, 333729.7, 331218.0), ncol = 2)
#'
#' obj.pol <- matrix( c(181414.4, 333221.7,  181473.8, 333122.7,
#' 179791.8, 330095.1,  179732.4, 330194.1),
#' byrow =TRUE, ncol =2)
#'
#' sf.pts <- coord2sf(obj)
#' sf.lin <- coord2sf(obj, type = "linestring")
#' sf.pol <- coord2sf(obj.pol, type = "polygon")
#'
#' plot( sf::st_geometry(sf.pol), axes = TRUE, ylim = c(330095.1, 334000))
#' plot( sf::st_geometry(sf.lin), add = TRUE, col ="red")
#' plot( sf::st_geometry(sf.pts), add = TRUE, col ="blue")
#'
#' @export
#'

coord2sf <- function(df, type = "multipoint", epsg = NA) {

  if( !type %in% c("multipoint", "linestring", "polygon")){stop("type is not recognized, must be multipoint, linestring or polygon")}

  if(is.vector(df)){

    if( type %in% c("linestring", "polygon")){stop("Only one point available, not enough for linestring or polygon type")}
    sf.obj <- sf::st_cast(sf::st_sfc(sf::st_multipoint(as.matrix(t(df)))),"POINT")

  }else{

    if( type == "multipoint") {sf.obj <- sf::st_cast(sf::st_sfc(sf::st_multipoint(as.matrix(df))),"POINT")}
    if( type == "linestring") {sf.obj <- sf::st_sfc(sf::st_linestring(as.matrix(df)))}
    if( type == "polygon") {

      if( sum(duplicated(df))!=1){
        df <- df[!duplicated(df), ]
        df <- rbind(df, df[1,])
        sf.obj <- sf::st_sfc(sf::st_polygon(list(df)))
      }else{ sf.obj <- sf::st_sfc(sf::st_polygon(list(df)))}
    }

  }

  if( !is.na(epsg)){

    if( !inherits(epsg, "crs") ){
      crs.obj <- try( sf::st_crs(epsg), silent = TRUE)
      if( inherits(crs.obj, "try-error") ){stop("epsg = either a valid sf crs object or an existing epsg")}
    }else{
      crs.obj <- epsg
    }
    sf::st_crs(sf.obj) <- crs.obj

  }
  return(sf.obj)
}
