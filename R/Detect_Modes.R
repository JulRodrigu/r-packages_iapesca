#' Detect_Modes
#'
#' \code{Detect_Modes} Detects the possibles modes in values
#'
#' @param vals a vector of numeric values
#' @param verbose DEFAULT FALSE, boolean argument to print messages
#'
#' @return a vector of numeric values with detected modes
#'
#' @author Julien Rodriguez \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' set.seed(10122)
#' samp.1 <- rnorm(4000, 12, 1)
#' samp.2 <- rnorm(1000, 3, 1)
#' samp.3 <- rnorm(100, 20, 1)
#' samp <- c(samp.1, samp.2, samp.3)
#'
#' Detect_Modes(samp.3, verbose = TRUE)
#'
#' modes <- Detect_Modes(samp, verbose = TRUE)
#' modes
#'
#' hist(samp, freq = FALSE, ylim = c(0,0.3))
#' lines(density(samp), col = "darkgray", lty = 2, lwd = 2)
#' abline(v = c(3, 12, 20), lty = 2, lwd = 2)
#' abline(v = modes, lty =2, col = "red")
#'
#' @export
#'
Detect_Modes <- function(vals, verbose = FALSE) {

  if(!is.numeric(vals)) stop("vals must be numeric")
  sorted.vals <- sort(vals, na.last = NA)
  dens <- stats::density(sorted.vals)
  dens.val <- dens$y
  coord.dens <- dens$x
  modes.index <- NULL

  modes.index <- do.call(c, sapply(2:(length(dens.val)-1), function(k) {
    if ( (dens.val[k] > dens.val[k-1]) & (dens.val[k] > dens.val[k+1]) ) {
      return(k)
    }else{return(NULL)}
  }))

  if ( length(modes.index) == 0 ) {
    modes <- 'This is a monotonic distribution'
    if(verbose){message(modes)}
  }else{
    modes <- coord.dens[modes.index]
    if(verbose){
      if(length(modes) == 1){
        message("One distribution detected")
      }else{
        message("More than one distribution detected")
        }
      }
  }

  return(modes)

}





