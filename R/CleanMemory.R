#' CleanMemory
#'
#' \code{CleanMemory} Cleans memory and temporary folder
#'
#' @param n number of times gc() is applied
#'
#' @author S.Demaneche & Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' CleanMemory()
#'
#'
#' @export
#'

CleanMemory <- function(n=3){

  for(i in 1:n){gc()}
  unlink(list.files(tempdir()), recursive = TRUE)

}

