#' Create_NetsByBoat
#'
#' \code{Create_NetsByBoat} Creates nets from positions flagged by an operation.
#' This function uses Create_Nets and Calc_NetsThresholds, it must be applied by fishing vessel for many fishing trips for the consolidation process to be relevant.
#' This process may be parallelized by Fishing trip using parallelize argument if dedicated packages are available.
#'
#' @param traj a spatial points object of class sf
#' @param Col.VesselId DEFAULT = "VESSEL_FK", name of the column identifying the vessel identifier
#' @param Col.TripId DEFAULT = "FISHING_TRIP_FK", name of the column identifying the fishing trips
#' @param Col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param Col.Fop DEFAULT = "FishingOperation", the name of the column identifying the fishing operations.
#' @param Fop.category DEFAULT = "Hauling", the fishing operation category used to build the fishing gear
#' @param Use.BehaviourChanges DEFAULT = TRUE, if set to TRUE, a clustering of behaviour will be used together with the Fop.category sequences to build the nets, see ?Detect_BehaviourChanges
#' @param Auto.ThreshHolds.Detection DEFAULT = TRUE, if set to TRUE, nets Threshold will be set from the first nets created using Calc_NetsThresholds function
#' @param qt DEFAULT = NULL, see ?Calc_NetsThreshold, value for the quantile of Gaussian distribution, if set to NULL it will be automatically defined from a design using Set_NetThresholds
#' @param iter DEFAULT = NULL, see ?Calc_NetsThreshold, number of iterations of CalcThresholds function, if set to NULL it will be automatically defined from a design using Set_NetThresholds
#' @param length.lim DEFAULT = NULL, a vector of length 2 identifying the range of length for a net to be valid
#' @param av.speed.lim DEFAULT = NULL, a vector of length 2 identifying the range of average speeds for a net to be valid
#' @param heading.lim DEFAULT = NULL, a matrix with 2 columns identifying different ranges of heading for a net to be valid
#' @param verbose DEFAULT = FALSE, if TRUE, prints messages.
#' @param parallelize DEFAULT = FALSE, optional argument for parallelizing the processing of positions
#' @param nCores DEFAULT = NULL, number of cores used, if NULL set to floor(parallel::detectCores()/2)
#'
#'
#' @return Nets, a list of spatial linestring objects of class sf, each of them being the output of Create_Nets function applied to the fishing trips
#' Nets.Thresh, a list of thresholds, output of Calc_NetsThresholds function
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @importFrom foreach %dopar%
#'
#' @examples
#'
#' run.example = FALSE
#'
#' if(run.example){
#'
#'   require(mapview)
#'   data(positions)
#'   set.seed(221104)
#'
#'   pos <- df2sfp(positions, coords = c("LONGITUDE", "LATITUDE"))
#'   # Add erroneous fishing operations values for testing the automated consolidation process
#'   pos$FishingOperation[which(pos$FISHING_TRIP_FK %in% "18483452")][97:103] <-  "NotFishing"
#'   pos$FishingOperation[which(pos$FISHING_TRIP_FK %in% "18483452")][6:9] <- "Hauling"
#'
#'  # Create Nets from column "FishingOperation" and category "Hauling"
#'   Nets.list.raw <- Create_NetsByBoat(traj = pos,
#'                                      Use.BehaviourChanges = TRUE,
#'                                      verbose = TRUE,
#'                                      parallelize = TRUE)
#'
#'   Calc_NetsThresholds(Nets.list.raw$Nets)
#'
#'   # Create and consolidate Nets (Auto.ThreshHolds.Detection set to TRUE) from column "FishingOperation"
#'   Nets.list.cleaned <- Create_NetsByBoat(traj = pos,
#'                                          Use.BehaviourChanges = TRUE,
#'                                          Auto.ThreshHolds.Detection = TRUE,
#'                                          verbose = TRUE,
#'                                          parallelize = TRUE)
#'
#'   Nets.list.cleaned$Nets.Thresh
#'
#'   mapview(pos, zcol = "FishingOperation", cex = 2)+
#'     mapview(list(raw = do.call(rbind, Nets.list.raw$Nets),
#'                  cleaned = do.call(rbind, Nets.list.cleaned$Nets)),
#'                  lwd  = 3, color = c("darkred", "green"))
#'
#' }
#'
#' @export
#'
Create_NetsByBoat <- function(traj,
                        Col.VesselId = "VESSEL_FK",
                        Col.TripId = "FISHING_TRIP_FK",
                        Col.time = "DATE_TIME",
                        Col.Fop = "FishingOperation",
                        Fop.category = "Hauling",
                        Use.BehaviourChanges = TRUE,
                        Auto.ThreshHolds.Detection = FALSE,
                        qt = NULL,
                        iter = NULL,
                        length.lim = NULL,
                        av.speed.lim = NULL,
                        heading.lim = NULL,
                        verbose = FALSE,
                        parallelize = FALSE,
                        nCores = NULL
                        ){


  if(!inherits(traj, "sf")) stop("traj must be a sf points object")
  if(is.na(sf::st_crs(traj))) {stop("CRS must be defined for proper distance calculation")}
  if(!Col.VesselId %in% colnames(traj)) stop("Vessel identifier column not found")
  if(!Col.TripId %in% colnames(traj)) stop("Fishing trip identifier column not found")

  if( !Col.VesselId %in% "VESSEL_FK"){
    traj <- traj[, !colnames(traj) %in% "VESSEL_FK" ]
    colnames(traj)[ colnames(traj) %in% Col.VesselId] <- "VESSEL_FK"
  }

  if( !Col.TripId %in% "FISHING_TRIP_FK" ){
    traj <- traj[, !colnames(traj) %in% "FISHING_TRIP_FK" ]
    colnames(traj)[ colnames(traj) %in% Col.TripId ] <- "FISHING_TRIP_FK"
  }
  if( !Col.Fop %in% "FishingOperation" ){
    traj <- traj[, !colnames(traj) %in% "FishingOperation" ]
    colnames(traj)[ colnames(traj) %in% Col.Fop ] <- "FishingOperation"
  }
  if( !Col.time %in% "DATE_TIME" ){
    traj <- traj[, !colnames(traj) %in% "DATE_TIME" ]
    colnames(traj)[ colnames(traj) %in% Col.time] <- "DATE_TIME"
  }

    VesselId <- unique(traj$VESSEL_FK)
    if(length(VesselId) > 1) stop("Create_NetsByBoat function must be processed by vessel")

    traj.desc <- Traj_Desc(traj)

    if(parallelize){
      test.packages <- c("foreach", "parallel", "doParallel") %in% utils::installed.packages()
      if(all(!test.packages)){
        parallelize <- FALSE
        message(sprintf("Package(s) %s missing for using parallelize option", paste(c("foreach", "parallel", "doParallel")[!test.packages], collapse = ", ")))
      }
    }

    if(!parallelize){

      Nets.list <- lapply(traj.desc$FISHING_TRIP_FK, function(trip){

        Nets <- Create_Nets(traj = traj,
                            tripId = trip,
                            Col.TripId = "FISHING_TRIP_FK",
                            Col.time = "DATE_TIME",
                            Col.Fop = "FishingOperation",
                            Fop.category = Fop.category,
                            Use.BehaviourChanges = Use.BehaviourChanges,
                            length.lim = length.lim,
                            av.speed.lim = av.speed.lim,
                            heading.lim = heading.lim)

      })



    }else{

      nCores <- ifelse(is.null(nCores), floor(parallel::detectCores()/2), nCores)
      if(nCores > nrow(traj.desc)){ nCores <- nrow(traj.desc)}
      cl <- parallel::makeCluster(nCores)
      doParallel::registerDoParallel(cl)

      Nets.list <- foreach::foreach(k = iterators::iter(1:nrow(traj.desc)),
                                  .combine = "c",
                                  .packages = c("sf", "iapesca")) %dopar%
         {
           suppressWarnings(list(Nets <- Create_Nets(traj = traj,
                                                     tripId = traj.desc$FISHING_TRIP_FK[k],
                                                     Col.TripId = "FISHING_TRIP_FK",
                                                     Col.time = "DATE_TIME",
                                                     Col.Fop = "FishingOperation",
                                                     Fop.category = Fop.category,
                                                     Use.BehaviourChanges = Use.BehaviourChanges,
                                                     length.lim = length.lim,
                                                     av.speed.lim = av.speed.lim,
                                                     heading.lim = heading.lim)
         ))

         }

         parallel::stopCluster(cl)

    }

    names(Nets.list) <- traj.desc$FISHING_TRIP_FK
    Nets.list <- Nets.list[!unlist(lapply(Nets.list, is.null))]
    n.nets <- do.call(sum, lapply(Nets.list, nrow))

    if(verbose){
      message(sprintf("%s nets created", n.nets))
    }

    null.lims <- unlist(lapply(list(length.lim, av.speed.lim, heading.lim ), is.null))
    Nets.stats <- NULL

    if(!all(null.lims)){

      Nets.stats <- list(LimVal.Speed = av.speed.lim,
           LimVal.Length = length.lim,
           LimVal.Heading = heading.lim,
           LimVal.SoakTime = NULL
      )

    }

    if(all(!null.lims)|n.nets == 0){ Auto.ThreshHolds.Detection <- FALSE}

    if(Auto.ThreshHolds.Detection){

      if(verbose){
        if(any( c(is.null(qt), is.null(iter)))){
          message("Auto.ThreshHolds.Detection option selected: retrieve statistics on nets using Calc_NetsThresholds with automated thresholds detection")
        }else{
          message(sprintf("Auto.ThreshHolds.Detection option selected: retrieve statistics on nets using Calc_NetsThresholds with qt defined as %s and %s iteration(s)", qt, iter))
        }
      }

      Nets.stats.2 <- Calc_NetsThresholds(Nets.list, qt = qt, iter = iter, verbose = verbose)

      if(!is.null(Nets.stats)){

        Nets.stats.2[[which(!null.lims)]] <- Nets.stats[[which(!null.lims)]]

      }

      Nets.stats <- Nets.stats.2

      if(!parallelize){

      Nets.list <- lapply(traj.desc$FISHING_TRIP_FK, function(trip){

        Nets <- Create_Nets(traj = traj,
                            tripId = trip,
                            Col.TripId = "FISHING_TRIP_FK",
                            Col.time = "DATE_TIME",
                            Col.Fop = "FishingOperation",
                            Fop.category = Fop.category,
                            Use.BehaviourChanges = Use.BehaviourChanges,
                            length.lim = Nets.stats$LimVal.Length,
                            av.speed.lim = Nets.stats$LimVal.Speed,
                            heading.lim = Nets.stats$LimVal.Heading
                            )

      })

      }else{

        cl <- parallel::makeCluster(nCores)
        doParallel::registerDoParallel(cl)

        Nets.list <- foreach::foreach(k = iterators::iter(1:nrow(traj.desc)),
                                      .combine = "c",
                                      .packages = c("sf", "iapesca")) %dopar%
          {
            suppressWarnings(list(Nets <- Create_Nets(traj = traj,
                                                      tripId = traj.desc$FISHING_TRIP_FK[k],
                                                      Col.TripId = "FISHING_TRIP_FK",
                                                      Col.time = "DATE_TIME",
                                                      Col.Fop = "FishingOperation",
                                                      Fop.category = Fop.category,
                                                      Use.BehaviourChanges = Use.BehaviourChanges,
                                                      length.lim = Nets.stats$LimVal.Length,
                                                      av.speed.lim = Nets.stats$LimVal.Speed,
                                                      heading.lim = Nets.stats$LimVal.Heading
            )
            ))

          }

        parallel::stopCluster(cl)



      }

      names(Nets.list) <- traj.desc$FISHING_TRIP_FK
      Nets.list <- Nets.list[!unlist(lapply(Nets.list, is.null))]

      if(verbose){
        n.nets <- do.call(sum, lapply(Nets.list, nrow))
        message(sprintf("%s nets consolidated", n.nets))
      }


    }

  return(list(Nets = Nets.list, Nets.Thresh = Nets.stats))

}

