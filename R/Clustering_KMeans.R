#' Clustering_KMeans
#'
#' \code{Clustering_KMeans} Creates and chooses the optimal number of classes to cluster the data using Mac-Queen's K-means algorithm.
#'
#' @param v a vector of numeric values
#' @param nb.class.max DEFAULT 20, maximal number of clusters tested
#' @param tol DEFAULT 0.05, threshold for 1 - betweenss / totss for a number of class to remain a good candidate
#' @param threshold DEFAULT 0.9, threshold for betweenss / totss for a number of class to remain a good candidate
#' @param k.min DEFAULT 2, minimal number of clusters
#' @param decision DEFAULT "MAX", choose either the maximal "MAX" or minimal "MIN" number of classes
#'
#' @return a list containing data, a data.frame with v and its cluster assignement and clusters, a vector containing number of clusters candidates.
#'
#' @author Zivan Karaman & Julien Rodriguez \email{julien.rodriguez@ifremer.fr}
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
#' kclust <- Clustering_KMeans(samp)
#' hist(samp, freq = FALSE, ylim = c(0,0.3))
#' lines(density(samp), col = "darkgray", lty = 2, lwd = 2)
#' abline(v = c(3, 12, 20), lty = 2, lwd = 2)
#' table(kclust$data$Clust)
#' lines(density(kclust$data[ kclust$data$Clust == 1, "v"]), col = "orange", lty = 2, lwd = 2)
#' lines(density(kclust$data[ kclust$data$Clust == 2, "v"]), col = "green", lty = 2, lwd = 2)
#' lines(density(kclust$data[ kclust$data$Clust == 3, "v"]), col = "red", lty = 2, lwd = 2)
#'
#' @export
#'
Clustering_KMeans <-
function(v, nb.class.max = 20, tol = 0.05, threshold = 0.9, k.min = 2, decision = "MAX")
{
    kmax <- min(nb.class.max, length(unique(v)))
    # run kmeans for all possible numbers of clusters
    ss <- matrix(NA, ncol = 3, nrow = kmax)
    for (k in 2:kmax) {
        kk <- stats::kmeans(v, k, iter.max = 200, nstart = 10, algorithm = "MacQueen")
        ss[k, 1] <- kk$totss
        ss[k, 2] <- kk$betweenss
        ss[k, 3] <- kk$tot.withinss
    }
    # select optimal cluster number
    totss <- ss[2, 1]
    betweenss <- ss[-1, 2] # discard the k = 1 (value is NA)
    # discard last values for which betweenss / totss < tol
    # k1 <- sum(1 - betweenss / totss > tol) + 1 # add 1 to get the first that is < tol
    # k1 <- k1 + 1 # add 1 to find correct number of clusters since betweenss starts for k = 2
    k1 <- kmax - sum(1 - betweenss / totss < tol)
    # keep as many as required to attain sum(betweenss) >= t hreshold * totss
    k2 <- sum(betweenss / totss < threshold) + 1 # add 1 to get the first that is > threshold
    k2 <- k2 + 1 # add 1 to find correct number of clusters since betweenss starts for k = 2
    # choose the lowest number of groups
    if(decision %in% "MAX") {
      k <- max(k1, k2)
    }else{
      k <- min(k1, k2)
      }
    k <- max(k, k.min)
    # run final classification with optimal number of groups k
    kk <- stats::kmeans(v, k, iter.max = 200, nstart = 25, algorithm = "MacQueen")
    # sort clusters in ascending order of mean values
    ord <- order(kk$centers[, 1])
    nord <- match(1:k, ord)
    kk$cluster <- nord[kk$cluster]
    kk$centers[, 1] <- kk$centers[ord, 1]
    # return data.frame with group and central value, and clustering parameters
    out <- list(data = data.frame(Clust = kk$cluster, v), clusters = c(k.tol = k1, k.threshold = k2, k.min = k.min))
    return(out)
}
