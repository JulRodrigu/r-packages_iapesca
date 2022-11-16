#' novelty_svm
#'
#' \code{novelty_svm} Performs one-class svm for novelty detection
#'
#' @param data a data.frame, used to train the model, sf data points can also be handled
#' @param category DEFAULT NULL, the category to be predicted
#' @param y DEFAULT NULL, the name of the categorical variable to which the category belongs
#' @param covar a character string indicating the name of the covariables, must be coercible to numeric
#' @param kernel DEFAULT "radial", kernel argument for svm function
#' @param nu DEFAULT 0.1, nu argument for e1071::svm function
#' @param gamma DEFAULT NULL, gamma argument for e1071::svm function
#' @param cost DEFAULT 1, cost argument for e1071::svm function
#' @param pred DEFAULT TRUE, return predictions for the entire dataset
#'
#' @return data.output novelty.svm, a svm model of class svm or if pred is TRUE a list containing the model and predictions (svm.model and pred)
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.com}
#'
#' @examples
#'
#' data(positions)
#'
#' # Remove hauling identifications to leave one position by hauling operation only
#' hauling.events <- unique(positions$hauling[!is.na(positions$hauling)])
#' haul.index <- lapply(hauling.events, function(x) { which(positions$hauling %in% x)})
#' haul.ind2rm <- lapply(haul.index, function(x) { sample(x, length(x) -1 )})
#' positions$Fop.2 <- positions$FishingOperation
#' positions$Fop.2[do.call(c, haul.ind2rm)] <- NA
#'
#' # Create one-class svm model
#' # nu parameter to be optimized
#' hauling.svm <- novelty_svm(data = positions, category = "Hauling", y = "Fop.2",
#'                            covar = c( "turn" ,  "speed", "hdg"), pred = TRUE, nu = 0.01)
#' hauling.svm$svm.model
#' table(positions$Fop.2, hauling.svm$pred)
#' table(positions$FishingOperation, hauling.svm$pred)
#'
#'
#' @export
#'
novelty_svm <- function(data,
           category = NULL,
           y = NULL,
           covar,
           kernel = "radial",
           gamma = NULL,
           nu = 0.1,
           cost = 1,
           pred = TRUE)
  {

  if(inherits(data, "sf")){ data <- sfp2df(data) }

    test.cat <- sum(is.null(category), is.null(y))
    if(test.cat == 1) stop("If not null, arguments category and y must be both defined")
    test.cat <- test.cat == 0
    if(any(!covar %in% colnames(data))) stop("At least one the covariable is not recognized in data")

    covar.dat <- as.data.frame(apply(data[, covar], 2,
                                     function(x){return(as.numeric(x))}))
    nnai <- !apply(covar.dat, 1, anyNA)

    if(test.cat){

      index <- which(data[nnai, y] %in% category)
      if (length(index) == 0) stop(sprintf("%s does not belong to %s", category, y))
      train.dat <- covar.dat[nnai,][index, ]

    }else{

      train.dat <- covar.dat[nnai,]

    }

    if(is.null(gamma)){ gamma <- 1/length(covar)}

    novelty.svm <- e1071::svm( x = train.dat, y = NULL,
                        type = "one-classification",
                        kernel=kernel,
                        gamma = gamma,
                        nu = nu,
                        scale = FALSE
    )

    if(pred){

      pred.1KSVM <- rep(NA, length(nnai))
      pred.1KSVM[nnai] <- predict(novelty.svm, covar.dat[nnai,])
      novelty.svm <- list(svm.model = novelty.svm, pred = pred.1KSVM)

    }

    return(novelty.svm)

  }
