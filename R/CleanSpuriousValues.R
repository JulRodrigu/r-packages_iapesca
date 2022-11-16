#' CleanSpuriousValues
#'
#' \code{CleanSpuriousValues} Cleans the data supposing its distribution should be unimodal and gaussian.
#' Test wether the distribution is unimodal, if it isn't, the function will try to retrieve the dominant mode.
#' Skewness is eventually corrected.
#' This function having been created to establish average individual weight for various species, we consider with this approach that the first distribution found is not the most relevant one.
#'
#' @param vals a vector of numeric values
#' @param lim.abs.skew DEFAULT 0.05, absolute value of skewness, arbitrary threshold to consider a distribution is skewed and values have to be dropped
#' @param max.it DEFAULT 30, argument of CleanSkewness, maximal number of iterations used.
#' @param verbose DEFAULT FALSE, boolean argument to print messages
#' @param remove.first.distrib DEFAULT FALSE, if TRUE, remove the first distribution detected from the selection
#'
#' @return vals, the same numeric vector filled with NA where values are supposed spurious
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' set.seed(10122)
#' samp.1 <- rnorm(4000, 12, 1)
#' samp.2 <- rnorm(1000, 3, 1)
#' samp <- c(samp.1, samp.2, 25)
#'
#' hist(samp, freq=FALSE, ylim = c(0, 0.5))
#' lines(density(samp), col = "darkgray", lty = 2, lwd = 2)
#' abline(v = c(3, 12), lty = 2, lwd = 2)
#'
#' cl.samp <- CleanSpuriousValues(samp)
#' summary(samp)
#' summary(cl.samp)
#' lines(density(cl.samp[!is.na(cl.samp)]), col = "red", lwd = 2)
#' abline(v = mean(cl.samp[!is.na(cl.samp)]), lty = 2, lwd = 2, col = "red")
#'
#' @export
#'

CleanSpuriousValues <- function(vals, lim.abs.skew = 0.05, max.it = 30, verbose = FALSE, remove.first.distrib = FALSE){

  modes <- Detect_Modes(vals, verbose = verbose)

  if(length(modes) > 1){

    if(verbose){ message("More than one mode detected: two different distributions will be handled - better to check the data on your side!") }
    distrib.affectation <- Cluster_Distributions(vals)
    tab.clust <- table(distrib.affectation$Clust)

    if(remove.first.distrib){
      relevant.distribs <- prop.table(tab.clust) > 1/(2*length(tab.clust))
      tab.clust <- tab.clust[relevant.distribs]
      if(length(tab.clust) > 1){
        majority <- names(tab.clust)[-1][which.max(tab.clust[-1])]
      }else{
        majority <- names(tab.clust)[which.max(tab.clust)]
      }
    }else{
      majority <- names(tab.clust)[which.max(tab.clust)]
    }

    vals[ !is.na(distrib.affectation$Clust) & !distrib.affectation$Clust %in% majority] <- NA

  }

  vals <- CleanSkewness(vals, lim.abs.skew = lim.abs.skew, max.it = max.it)

  return(vals)
}



