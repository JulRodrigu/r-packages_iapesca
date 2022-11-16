#' Harmonize_Ids
#' 
#' \code{Harmonize_Ids} Returns unique identifiers having constant length
#' 
#' @param Constantstring DEFAULT "", a string to be defined as a constant in identifier
#' @param index a numeric index to be defined with unique values
#' @param ID.length DEFAULT NA, argument for set_0nbr function. Number of characters to be defined for each element of x so that nchar(paste0(set_0nbr(x), x)) = ID.length, if not specified, defined as max(nchar(x))
#' @param sep DEFAULT "_", separator between Constantstring and index
#' 
#' @return Harmonized.Ids
#' 
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#' 
#' 
#' @examples
#'
#' Harmonize_Ids("Boats", 1:130)
#' 
#' @export
#'
Harmonize_Ids <- function(Constantstring = "", index, ID.length = NA, sep = "_"){
  
  sep <- ifelse(Constantstring == "", "", sep)
  if(length(index) > length(unique(index))) stop("For the identifier to be unique, index has to be built with unique values, ex: 1:100")
  
  Harmonized.Ids <- paste0(Constantstring,
                            sep, 
                            set_0nbr(index, ID.length),
                            index)
  
  return(Harmonized.Ids)
  
}
