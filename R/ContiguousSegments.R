#' ContiguousSegments
#'
#' \code{ContiguousSegments} Identifies contiguous segments from covariates, eg sequences of consecutive points which have the same combination of covariates.
#'
#' @param data either a data.frame or spatial points object of class sf
#' @param covar DEFAULT = NULL, a character string describing the covariates to be used. If null, all columns will be used.
#' @param na.rm DEFAULT = TRUE, if set to TRUE, returns NA if at least one of the covariates values is missing, else, the function will consider NA as a value.
#'
#' @return data, original data object with a new column named "Segments" which identifies the sequences of consecutive points
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' set.seed(220708)
#'
#' dat <- data.frame(clust.1 = sample((1:2), size = 100, replace = TRUE),
#'                   clust.2 = sample((1:2), size = 100, replace = TRUE))
#'
#' datWsegments <- ContiguousSegments(dat)
#'
#' head(datWsegments)
#'
#' @export
#'

ContiguousSegments <- function(data, covar = NULL, na.rm = TRUE){

  testIsSpatial <- inherits(data, "sf")
  if(testIsSpatial){ data <- sfp2df(data)}
  data <- try(as.data.frame(data), silent = TRUE)
  if(inherits(data, "try-error")) stop("data must be coercible to data.frame")

  if(is.null(covar)) { covar <- colnames(data)}

  if(length(covar) > 1){
    indWChange <- which(c(TRUE, sapply(2:nrow(data),
                                       function(k){ !all(apply(data[(k-1):(k), covar], 2, function(x) {duplicated(x)})[2, ])})))
  }else{
    indWChange <- which(c(TRUE, sapply(2:nrow(data),
                                       function(k){ (!duplicated(data[(k-1):(k), covar]))[2] })))
  }

  Segments <- rep(1, nrow(data))

  if(length(indWChange) > 1){

    indWChange <- c(indWChange, nrow(data))

    for( i in 2:(length(indWChange)-1)){

      Segments[indWChange[i]: indWChange[i+1]] <- Segments[indWChange[i]-1] + 1

    }

  }

  data$Segments <- Segments

  if(na.rm){
    if(length(covar) > 1){
      nai <- which(apply(data[, covar], 1, anyNA))
    }else{
      nai <- is.na(data[, covar])
    }
    nai <- nai[ nai!= 1]
    data$Segments[nai] <- NA
  }

  if(testIsSpatial){ data <- df2sfp(data, coords = c("X", "Y"))}

  return(data)

}

