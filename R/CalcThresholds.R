#' CalcThresholds
#'
#' \code{CalcThresholds} Calculates thresholds for values considering their distribution should be unimodal and gaussian, function is used internally in Calc_NetsThresholds
#'
#' @param vals a vector of numeric values
#' @param lim.abs.skew DEFAULT = 0.05, absolute value of skewness, argument of CleanSkewness function
#' @param GaussianQt DEFAULT = NA, value for the quantile of Gaussian distribution. If missing, set to 1.96 for the 5% quantile
#'
#' @return LimVal, a vector of length two defining the thresholds for vals
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' set.seed(10122)
#' samp.2 <- rnorm(1000, 3, 1)
#' samp <- c(samp.2, 25)
#' CalcThresholds(samp, GaussianQt = 1.96)
#'
#' @export
#'

CalcThresholds <- function(vals,
                           lim.abs.skew = 0.05,
                           GaussianQt = NA){

  if(lim.abs.skew >= 0.5){
    message("lim.abs.skew defined as the maximum value allowed in the function: CleanSkewness won't be applied")
  }
  if(is.na(GaussianQt)){
    warning("GaussianQt not defined, its value will be arbitrarily set to 1.96 for the 5% quantile of Gaussian distribution")
  }

  n.vals <- length(vals)
  LimVal <- c(NA, NA)

  if(n.vals > 2){

    if(n.vals < 30){
      new.vals <- sample(vals, 30, replace = TRUE)
    }else{
      new.vals <- vals}

    y <- new.vals
    lim.skew <- lim.abs.skew

    while(anyNA(LimVal) & lim.skew < 0.5){
      lim.skew  <- lim.skew + 0.05
      y <- CleanSkewness(y, lim.abs.skew = lim.skew, max.it = floor(n.vals/10))
      mn.y <- mean(y, na.rm = TRUE)
      sd.y <- stats::sd(y, na.rm = TRUE)
      LimVal <- mn.y + sd.y*c(-1,1)*GaussianQt
    }
    if(anyNA(LimVal)){
      mn.y <- mean(y, na.rm = TRUE)
      sd.y <- stats::sd(y, na.rm = TRUE)
      LimVal <- mn.y + sd.y*c(-1,1)*GaussianQt
    }
  }

  return(LimVal)
}

