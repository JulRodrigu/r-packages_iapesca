#' Pivot_Table
#'
#'
#' \code{Pivot_Table} Create different columns from a categorical variable
#'
#' @param tab a table coercible to data.frame with as.data.frame function
#' @param col.pattern a character string identifying the row identifier
#' @param column the column identifier where the categories have to be retrieved to create new columns
#' @param vals the column containg numeric values to retrieve
#' @param keep.nas DEFAULT TRUE, if FALSE, replace missing values by 0
#'
#'
#' @return pivot.tab, a pivotted version of tab with categories in columns
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#'
#' set.seed(220719)
#'
#' n <- 100
#' dat <- data.frame(
#'            clust = letters[sample((1:2), size = n, replace = TRUE)],
#'            val = round(runif(n, 0, 100), digits = 0))
#' dup.id <- sapply(1:(n-1), function(k){ dat$clust[k] %in% "a" & dat$clust[k+1] %in% "b"})
#' dat$id <- rep(NA, nrow(dat))
#' dat$id[1] <- 1
#'
#' for(i in 2:n){
#'
#'   if(dup.id[i-1]){
#'      dat$id[i] <- dat$id[i-1]
#'   }else{
#'      dat$id[i] <- dat$id[i-1]+1
#'   }
#'
#' }
#'
#' head(dat)
#'
#' pivot.tab <- Pivot_Table(dat, "id", "clust", "val")
#'
#' dim(pivot.tab)
#' head(pivot.tab)
#'
#'
#' @export
#'

Pivot_Table <- function(tab, col.pattern, column, vals, keep.nas = TRUE){

  tab.df <- try( as.data.frame(tab), silent = TRUE)
  if (inherits(tab.df, "try-error")) stop("tab must be an object coercible to data.frame class")
  if (!is.numeric(as.numeric(tab.df[, vals]))) stop(sprintf("%s should contain numeric values", vals))

  pattern <- stats::setNames(dplyr::distinct(as.data.frame(tab.df[, col.pattern])), col.pattern)
  categories <- sort(unique(tab.df[, column]))

  Prep_Synth <- function(cat){

    Synth <- stats::setNames(doBy::summaryBy(CreateFormula(vals, col.pattern),
                                           tab.df[ tab.df[, column] %in% cat, ],
                                           FUN = function(x) sum(x, na.rm = TRUE)
    ), c(col.pattern, paste0(cat, "_sum")))

    Synth.fullPattern <- merge(pattern,
                               Synth,
                                    by = col.pattern,
                                    all.x = TRUE
    )

    if(!keep.nas){
      nai <- is.na(Synth.fullPattern[, paste0(cat, "_sum")])
      Synth.fullPattern[nai, paste0(cat, "_sum")] <- 0
    }

    return(Synth.fullPattern)

  }

  pivot.tab <- do.call(cbind,
                            lapply(categories , function(x) {Prep_Synth(x)}))
  pivot.tab <- pivot.tab[, !duplicated(colnames(pivot.tab))]

  return(pivot.tab)

  }

