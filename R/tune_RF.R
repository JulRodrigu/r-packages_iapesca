#' tune_RF
#'
#' \code{tune_RF} Automatic optimization of hyperparameters used in a random forest model (mtry and min.node.size).
#' Two designs of nb.param X nb.param are tested using Out-of-Bag error estimate. The full design is then predicted from a random-forest model.
#'
#' @param formula a model formula built using stats::as.formula
#' @param data a data.frame, used to train the model
#' @param nb.param DEFAULT = 5, number of hyperparameters tested, two designs of nb.param X nb.param are tested
#' @param ntree.try DEFAULT = 50, Number of trees to grow in each random-forest used to test the combination of hyperparameters.
#'
#' @return a list containing optimal values for hyperparameters min.node.size and mtry
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @importFrom ranger ranger
#'
#' @examples
#'
#' require(ranger)
#' data(positions)
#'
#' optim.rf <- tune_RF(FishingOperation ~ turn + speed, positions)
#' optim.rf
#'
#' mod.rf <- ranger(FishingOperation ~ turn + speed,
#'                  positions,
#'                  importance = "impurity",
#'                  mtry =  optim.rf$mtry,
#'                  min.node.size =  optim.rf$min.node.size,
#'                  num.trees = 100,
#'                  write.forest = TRUE)
#'
#' print(sort(mod.rf$variable.importance))
#'
#' @export
#'
tune_RF <-
  function(formula, data, nb.param = 5, ntree.try = 50)
  {

    if(nrow(data) > 10000){ sel.data = sample(1:nrow(data), size = 10000, replace = FALSE)
    }else{sel.data = 1:nrow(data)}

    col.x <- as.character(formula)[3]
    n.x <- unlist(gregexpr(pattern = "\n", col.x))
    if(length(n.x) > 0){
      col.x <- gsub(col.x, pattern = "\n", replacement = "")}

    pos.x <- unlist(gregexpr(pattern = " ", col.x))
    nb.var <- length(pos.x)/2 + 1
    if(length(n.x) == 0 | n.x == -1){
      nb.var <- nb.var
    }else{ nb.var <- nb.var - 2*length(n.x)}
    y.is.factor <- is.factor(data[, as.character(formula)[2]])

    full.design <- expand.grid(min.node.size = seq( ifelse(y.is.factor, 1,5), floor(nrow(data)/3), by = 1),
                               mtry = seq( 1, nb.var-1, by = 1))
    design <- expand.grid(min.node.size = round(seq( ifelse(y.is.factor, 1,5), floor(nrow(data)/3), length.out = nb.param), digits = 0),
                          mtry = unique(c(floor(sqrt(nb.var)),round(seq( 1, nb.var, length.out = ifelse( nb.var < nb.param, nb.var, nb.param)), digits = 0))))
    design.mtry <- unique(design$mtry)
    design$mtry[ design$mtry >= nb.var ] <- min(floor(nb.var*0.9), design.mtry[length(design.mtry)-1] +1)

    mse.res <- sapply(1:nrow(design), function(k) {
      ranger::ranger(formula,
             data[sel.data, ],
             num.trees = ntree.try,
             min.node.size = design[k, 1],
             mtry = design[k, 2],
             write.forest = FALSE,
             verbose = FALSE)$prediction.error
    })
    design <- cbind(design, mse.res)

    param.res <- lapply( list( with(design, aggregate(mse.res, by = list(min.node.size), mean)),
                               with(design, aggregate(mse.res, by = list(mtry), mean))), function(x){ x[order(x[,2]),] })
    n.mtry <- ifelse(nrow(param.res[[2]]) <3, nrow(param.res[[2]]), 3)

    new.design <- expand.grid(min.node.size = unique(round(seq( min(param.res[[1]][1:2,1])+1, max(param.res[[1]][1:2,1])-1, length.out = nb.param), digits = 0)),
                              mtry = unique(round(seq( min(param.res[[2]][1:n.mtry,1]), max(param.res[[2]][1:n.mtry,1]), length.out = nb.param), digits = 0)))

    mse.res <- sapply(1:nrow(new.design), function(k) {
      ranger::ranger(formula,
             data[sel.data, ],
             num.trees = ntree.try,
             min.node.size = new.design[k, 1],
             mtry = new.design[k, 2],
             write.forest = FALSE,
             verbose = FALSE)$prediction.error
    })

    design <- do.call(rbind, list(design, cbind(new.design, mse.res)))
    mod.optim <- ranger::ranger( mse.res ~ min.node.size + mtry,
                         design,
                         num.trees = ifelse(ntree.try < 100, 100, ntree.try),
                         write.forest = TRUE,
                         verbose = FALSE)

    full.design$mse.pred <- stats::predict(mod.optim, full.design)$predictions

    return(list( min.node.size =   full.design[which.min( full.design$mse.pred), "min.node.size"],
                 mtry = full.design[which.min( full.design$mse.pred), "mtry"]))

  }
