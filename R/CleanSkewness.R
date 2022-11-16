#' CleanSkewness
#'
#' \code{CleanSkewness} Removes the values that make a distribution skewed. At each step values over a threshold defined as min/max +/- range(values)/100 are dropped.
#'
#' @param vals a vector of numeric values
#' @param lim.abs.skew DEFAULT 0.5, absolute value of skewness, arbitrary threshold to consider a distribution is skewed and values have to be dropped
#' @param max.it  DEFAULT 30, maximal number of iterations used.
#'
#' @return vals, the same numeric vector filled with NA where values have been dropped
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' set.seed(10122)
#' samp.2 <- rnorm(1000, 3, 1)
#' samp <- c(samp.2, 25)
#' new.samp <- CleanSkewness(samp)
#' summary(new.samp)
#'
#' @export
#'
CleanSkewness <- function(vals,
                          lim.abs.skew = 0.5,
                          max.it = 30){

  skew.vals <- e1071::skewness(vals[!is.na(vals)])

  if(!is.na(skew.vals) & max.it > 0){

  n.data <- sum(!is.na(vals))
  lim.vals <- range(range(vals[!is.na(vals)]))

  if ( abs(skew.vals) > lim.abs.skew  & n.data >= 30){

    setup <- diff(lim.vals)/100
    if( skew.vals > 0) { lim.vals[2] <- lim.vals[2] - setup}
    if( skew.vals < 0) { lim.vals[1] <- lim.vals[1] + setup}

    vals[ vals < lim.vals[1] | vals > lim.vals[2]] <- NA

    skew.vals <- e1071::skewness(vals[!is.na(vals)])
    it.skew <- 1
    n.data <- sum(!is.na(vals))

      while( abs(skew.vals) > lim.abs.skew & it.skew < max.it & n.data >= 25 ){

        if( skew.vals > 0) { lim.vals[2] <- lim.vals[2] - setup}
        if( skew.vals < 0) { lim.vals[1] <- lim.vals[1] + setup}
        vals[ vals < lim.vals[1] | vals > lim.vals[2]] <- NA
        skew.vals <- e1071::skewness(vals[!is.na(vals)])
        n.data <- sum(!is.na(vals))

        it.skew <- it.skew + 1

      }

    }
  }

  return(vals)
}

