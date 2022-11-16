#' Cluster_Distributions
#'
#' \code{Cluster_Distributions} Clusters the values from the distribution they may belong
#'
#' @param vals a vector of numeric values
#' @param verbose DEFAULT FALSE, boolean argument to print messages
#'
#' @return a data.frame with vals (values) and their cluster assignement to a distribution
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
#' mode.affectation <- Cluster_Distributions(samp)
#' table(mode.affectation$Clust)
#'
#' hist(samp, freq = FALSE, ylim = c(0,0.3))
#' lines(density(samp), col = "darkgray", lty = 2, lwd = 2)
#' abline(v = c(3, 12, 20), lty = 2, lwd = 2)
#' lines(density(mode.affectation[ mode.affectation$Clust == 1, "Vals"]),
#' col = "orange", lty = 2, lwd = 2)
#' lines(density(mode.affectation[ mode.affectation$Clust == 2, "Vals"]),
#'  col = "green", lty = 2, lwd = 2)
#' lines(density(mode.affectation[ mode.affectation$Clust == 3, "Vals"]),
#' col = "red", lty = 2, lwd = 2)
#'
#' @export
#'
Cluster_Distributions <- function(vals, verbose = FALSE) {

  if(!is.numeric(vals)) stop("vals must be numeric")

  modes <- Detect_Modes(vals, verbose = verbose)
  nnai <- which(!is.na(vals))
  df.distrib <- stats::setNames(
                      data.frame(
                        matrix(NA, nrow = length(vals), ncol = 2)),
                        c("Clust", "Vals"))
  df.distrib$Vals <- vals
  nb.class.max <- ifelse(length(modes) > 25, 25, length(modes))

  if(length(modes) > 1){
    Clust <- Clustering_KMeans(vals[nnai], nb.class.max = nb.class.max, tol = 0.05, threshold = 0.9, k.min = 2, decision = "MAX")
    df.distrib$Clust[nnai] <- Clust$data$Clust
  }else{
    df.distrib$Clust[nnai] <- 1
  }

  return(df.distrib)

}





