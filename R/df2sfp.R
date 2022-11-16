#' df2sfp
#'
#' \code{df2sfp} Convert a data.frame to a sf multipoints object
#'
#' @param df an object coercible to data.frame class
#' @param coords DEFAULT  c("LONGITUDE", "LATITUDE"), a vector of length 2 indicating the name of coordinates columns in df as x an y
#' @param input.crs DEFAULT 4326, either the EPSG code of the coordinate reference system or an object of crs class describing df CRS
#' @param output.crs DEFAULT 4326, either the EPSG code of the coordinate reference system or an object of crs class describing the desired CRS for function output
#'
#' @return a sf multipoints object
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
#' pos.mercator <- df2sfp(positions, output.crs = 3395)
#' st_crs(pos.mercator)
#'
#'
#' @export
#'

df2sfp <- function(df, coords =  c("LONGITUDE", "LATITUDE"), input.crs = 4326, output.crs = 4326){

  if(!inherits(df, "data.frame")){
    df <- try(as.data.frame(df), silent = TRUE)
    if(inherits(df, "try-error")) stop("df must be coercible to data.frame class")
  }

  class.columns <- lapply(df, function(x) {class(x)})
  time.col <- unlist(lapply(class.columns, function(x) {all(x %in% c( "POSIXlt", "POSIXt" ))}))

  if(any(time.col)){

    for(i in 1:sum(time.col)){
      former.name <- colnames(df)[which(time.col)[i]]
      new.name <- "VarTime2Change"
      colnames(df)[ colnames(df) %in% former.name] <- new.name
      df$VarTime2Change <- as.character(Char2Time(df$VarTime2Change))
      colnames(df)[ colnames(df) %in% new.name] <- former.name
    }

  }

  nnai <- apply(df[, coords], 1, function(x){ !anyNA(x)})

  positions <- sf::st_as_sf(df[nnai, ], coords = coords)
  sf::st_crs(positions) <- sf::st_crs(input.crs)

  if(input.crs != output.crs){
    positions <- sf::st_transform(x = positions, crs = output.crs)
  }

  return(positions)

}
