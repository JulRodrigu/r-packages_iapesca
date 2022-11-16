#' sfp2df
#'
#' \code{sfp2df} Convert a sf multipoints object to a data.frame
#'
#' @param sf.points an object of class sf
#' @param rename.coords DEFAULT NULL, a vector of length 2 indicating the name of coordinates columns in df as x an y
#'
#' @return a data.frame
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' head(positions)
#'
#' # Default crs defined as WGS84 (EPSG 4326) for input and output of the function
#' pos.sf <- df2sfp(positions)
#' class(pos.sf)
#' st_crs(pos.sf)
#'
#' pos.df <- sfp2df(pos.sf, rename.coords = c("lon", "lat"))
#'
#' class(pos.df)
#' head(pos.df)
#'
#'
#' @export
#'

sfp2df <- function(sf.points, rename.coords = NULL){

  if(!inherits(sf.points, "sf")) stop("sf.points must be a valid sf object")

  class.columns <- lapply(sf.points, function(x) {class(x)})
  time.col <- unlist(lapply(class.columns, function(x) {all(x %in% c( "POSIXlt", "POSIXt" ))}))

  if(any(time.col)){

    for(i in 1:sum(time.col)){
      former.name <- colnames(sf.points)[which(time.col)[i]]
      new.name <- "VarTime2Change"
      colnames(sf.points)[ colnames(sf.points) %in% former.name] <- new.name
      sf.points$VarTime2Change <- as.character(Char2Time(sf.points$VarTime2Change))
      colnames(sf.points)[ colnames(sf.points) %in% new.name] <- former.name
    }

  }

  df <- cbind( sf::st_set_geometry(sf.points, NULL), sf::st_coordinates(sf.points))

  if(!is.null(rename.coords)){
   if(!(is.vector(rename.coords) & length(rename.coords) == 2)) stop("rename.coords must a character vector of length 2")
    xy.colnames <- colnames(sf::st_coordinates(sf.points))[1:2]
    colnames(df)[ colnames(df) %in% xy.colnames] <- rename.coords
  }

  return(df)

}
