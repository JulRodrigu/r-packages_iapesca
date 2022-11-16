#' CreateFormula
#'
#' \code{CreateFormula} Create a formula from character vectors
#'
#' @param y a character vector for variables to explain
#' @param x a character vector for variables to be used as descriptor
#'
#' @return form, an object with class "formula"
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#'
#' CreateFormula("MZ", c("toto", "titi"))
#'
#'
#' @export
#'

CreateFormula <- function(y, x) {

  form <- stats::as.formula(
                  paste(
                    paste(y, collapse = "+"),
                    paste(x, collapse = "+"),
                    sep = "~")
                      )

  return(form)

}
