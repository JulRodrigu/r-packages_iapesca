#' set_0nbr
#' 
#' \code{set_0nbr} Return a character string with zeros, the number of zeros is fixed so that nchar(paste0(set_0nbr(x), x)) is constant.
#' 
#' @param x a vector either numeric or character
#' @param ID.length DEFAULT NA, number of characters to be defined for each element of x so that nchar(paste0(set_0nbr(x), x)) = ID.length, if not specified, defined as max(nchar(x))
#'
#' @return seq0, a character strings with pasted zeros
#' 
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#' 
#' 
#' @examples
#'
#' set_0nbr(1:100)
#'
#' set_0nbr(c("toto", "turlututu", 1, 50000))
#' 
#' @export
#'

set_0nbr <- function(x, ID.length = NA) {
  
  if(is.na(ID.length)){ ID.length <- max(nchar(x))}
  if(ID.length < max(nchar(x))) stop("With ID.length as defined, a negative number of zeros would have to be set, review this argument")
  
  if( length(x) == 0){
    seq0 <- paste0( paste(rep(0, times = ID.length-nchar(x))), collapse = "")
  }else{
    seq0 <- do.call(c, 
                    lapply(x, function (arg) {
                      return(paste0( paste(rep(0, times = ID.length - nchar(arg))),collapse = ""))
                    }
                    ))
  }
    return(seq0)
  }
  
