#' Set_NetThresholds
#'
#' \code{Set_NetThresholds} seeks for the best combination of arguments qt and iter to be used in Calc_NetsThresholds.
#' Tests iteratively designs of values for qt and iter and selects their combination that maximizes the decay of thresholds.
#'
#' @param ls.Nets object x$Nets, output of Create_NetsByBoat function, a list of spatial linestring objects of class sf, each of them being the output of Create_Nets function applied to the fishing trips
#' or Nets, a spatial linestring object of class sf, output of Create_Nets
#'
#' @return comb.test, a data-frame with 1 line and 2 columns named "qt" and "iter"
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' run.example <- FALSE
#'
#' if(run.example){
#'
#'   require(mapview)
#'   data(positions)
#'
#'   pos <- df2sfp(positions, coords = c("LONGITUDE", "LATITUDE"))
#'
#'   # Add erroneous fishing operations values for testing the automated consolidation process
#'   pos$FishingOperation[which(pos$FISHING_TRIP_FK %in% "18483452")][97:103] <-  "NotFishing"
#'   pos$FishingOperation[which(pos$FISHING_TRIP_FK %in% "18483452")][6:9] <- "Hauling"
#'
#'   Nets.list <- Create_NetsByBoat(traj = pos, Use.BehaviourChanges = TRUE)
#'   set.seed(221107)
#'   Set_NetThresholds(ls.Nets = Nets.list$Nets)
#'
#' }
#'
#' @export
#'
Set_NetThresholds <- function(ls.Nets){

  if(!(inherits(ls.Nets, "list")|inherits(ls.Nets, "sf"))) stop("ls.Nets is a list or sf object, output of Create_NetsByBoat or Retrieve_SettingOperations functions")

  if(inherits(ls.Nets, "list")){

    if(!all(unlist(lapply(ls.Nets, function(x){inherits(x, "sf") & inherits(sf::st_geometry(x), "sfc_LINESTRING")})))) stop("Invalid ls.Nets format, output of Create_NetsByBoat function")
    if(!all(unlist(lapply(ls.Nets, function(x){all(c("length", "heading", "speed") %in% colnames(x))})))) stop("Invalid ls.Nets format, output of Create_NetsByBoat function")

    Nets <- do.call(rbind, ls.Nets)

  }else{

    if(!inherits(sf::st_geometry(ls.Nets), "sfc_LINESTRING")) stop("Invalid ls.Nets format, output of Create_NetsByBoat function")
    Nets <- ls.Nets

  }

  n.vals <- ifelse(is.null(Nets), 0, nrow(Nets))

  if(n.vals > 10){

    first.design <- expand.grid(qt = seq(0.5999, 0.9999, by = 0.05), iter = 1:2)
    test.design <- cbind(first.design,
                         as.data.frame(do.call(rbind,
                                               lapply(
                                                 lapply(1:nrow(first.design), function(k) {

                                                   Nets.thresh <- Calc_NetsThresholds(ls.Nets = Nets,
                                                                                      qt = first.design$qt[k],
                                                                                      iter = first.design$iter[k])

                                                   n.hdg <- nrow(Nets.thresh$LimVal.Heading)
                                                   if(is.null(n.hdg)){
                                                     diff.hdg <- diff(Nets.thresh$LimVal.Heading)
                                                   }else{
                                                     diff.hdg <- mean(apply(Nets.thresh$LimVal.Heading, 1, diff))
                                                   }

                                                   return(list(diff.sp = diff(Nets.thresh$LimVal.Speed),
                                                               diff.lg = diff(Nets.thresh$LimVal.Length),
                                                               n.hdg = n.hdg ,
                                                               diff.hdg = diff.hdg
                                                   ))

                                                 }),
                                                 function(x){ do.call(c, x) }))))

    test.design <-   test.design[ order(test.design$diff.lg, test.design$diff.hdg, test.design$diff.sp), ]

    n.design <- nrow(test.design)
    ind.thresh.lg <- which.max(
      (sort(test.design$diff.lg)[3:(n.design)] - sort(test.design$diff.lg)[2:(n.design-1)])/sort(test.design$diff.lg)[2:(n.design-1)])+2
    ind.test.lg <- which(test.design$diff.lg == sort(test.design$diff.lg)[ind.thresh.lg])
    ind.thresh.hdg <- which.max(
      (sort(test.design$diff.hdg)[3:(n.design)] - sort(test.design$diff.hdg)[2:(n.design-1)])/sort(test.design$diff.hdg)[2:(n.design-1)])+2
    ind.test.hdg <- which(test.design$diff.hdg == sort(test.design$diff.hdg)[ind.thresh.hdg])
    ind.thresh.spd <- which.max(
      (sort(test.design$diff.sp)[3:(n.design)] - sort(test.design$diff.sp)[2:(n.design-1)])/sort(test.design$diff.sp)[2:(n.design-1)])+2
    ind.test.spd <- which(test.design$diff.sp == sort(test.design$diff.sp)[ind.thresh.spd])
    ind.new.design <- unique(c(ind.test.lg[1] + -1:1, ind.test.lg[1] + -1:1, ind.test.lg[1] + -1:1))
    ind.new.design <- ind.new.design[ !ind.new.design < 1]
    ind.new.design <- ind.new.design[ !ind.new.design > n.design]
    if(length(ind.new.design) < 3){
      if(any(ind.new.design == 1)){
        ind.new.design <- c(ind.new.design, max(ind.new.design)+1)
      }else{
        ind.new.design <- c(min(ind.new.design)-1, ind.new.design)
      }
    }
    comb.test <- test.design[ind.new.design, c("qt", "iter")]

    new.design <- expand.grid(qt = seq(min(comb.test$qt), max(comb.test$qt), length.out = 15), iter = min(comb.test$iter):max(comb.test$iter))

    test.design <- cbind(new.design,
                         as.data.frame(do.call(rbind,
                                               lapply(
                                                 lapply(1:nrow(new.design), function(k) {

                                                   Nets.thresh <- Calc_NetsThresholds(ls.Nets = Nets,
                                                                                      qt = new.design$qt[k],
                                                                                      iter = new.design$iter[k])

                                                   n.hdg <- nrow(Nets.thresh$LimVal.Heading)
                                                   if(is.null(n.hdg)){
                                                     diff.hdg <- diff(Nets.thresh$LimVal.Heading)
                                                   }else{
                                                     diff.hdg <- mean(apply(Nets.thresh$LimVal.Heading, 1, diff))
                                                   }

                                                   return(list(diff.sp = diff(Nets.thresh$LimVal.Speed),
                                                               diff.lg = diff(Nets.thresh$LimVal.Length),
                                                               n.hdg = n.hdg ,
                                                               diff.hdg = diff.hdg ))

                                                 }),
                                                 function(x){ do.call(c, x) }))))

    test.design <-   test.design[ order(test.design$diff.lg, test.design$diff.hdg, test.design$diff.sp), ]

    n.design <- nrow(test.design)
    ind.thresh.lg <- which.max(
      (sort(test.design$diff.lg)[3:(n.design)] - sort(test.design$diff.lg)[2:(n.design-1)])/sort(test.design$diff.lg)[2:(n.design-1)])+2
    ind.test.lg <- which(test.design$diff.lg == sort(test.design$diff.lg)[ind.thresh.lg])
    ind.thresh.hdg <- which.max(
      (sort(test.design$diff.hdg)[3:(n.design)] - sort(test.design$diff.hdg)[2:(n.design-1)])/sort(test.design$diff.hdg)[2:(n.design-1)])+2
    ind.test.hdg <- which(test.design$diff.hdg == sort(test.design$diff.hdg)[ind.thresh.hdg])
    ind.thresh.spd <- which.max(
      (sort(test.design$diff.sp)[3:(n.design)] - sort(test.design$diff.sp)[2:(n.design-1)])/sort(test.design$diff.sp)[2:(n.design-1)])+2
    ind.test.spd <- which(test.design$diff.sp == sort(test.design$diff.sp)[ind.thresh.spd])
    ind.new.design <- unique(c(ind.test.lg[1] + -1:1, ind.test.lg[1] + -1:1, ind.test.lg[1] + -1:1))
    ind.new.design <- ind.new.design[ !ind.new.design < 1]
    ind.new.design <- ind.new.design[ !ind.new.design > n.design]
    if(length(ind.new.design) < 3){
      if(any(ind.new.design == 1)){
        ind.new.design <- c(ind.new.design, max(ind.new.design)+1)
      }else{
        ind.new.design <- c(min(ind.new.design)-1, ind.new.design)
      }
    }
    comb.test <- test.design[ind.new.design, c("qt", "iter")]

  }else{

    comb.test <- stats::setNames(as.data.frame(matrix(NA, nrow = 3, ncol = 2)),
                                 c("qt", "iter"))

  }

  return(comb.test[2, ])

}
