#' Retrieve_SettingOperations
#'
#' \code{Retrieve_SettingOperations} Retrieve setting operations from the positions using the nets.
#' Retrieves the gear identifier from Nets, output of Create_Nets function then identifies the related setting events.
#' Defines new columns in Nets objects to identify the setting: Fishing trip identifier, start and end and soaking time in hours
#'
#' @param traj a spatial points object of class sf
#' @param ls.nets object x$Nets, output of Create_NetsByBoat function, a list of spatial linestring objects of class sf, each of them being the output of Create_Nets function applied to the fishing trips
#' or Nets, a spatial linestring object of class sf, output of Create_Nets
#' @param Col.VesselId DEFAULT = "VESSEL_FK", name of the column identifying the vessel identifier
#' @param Col.TripId DEFAULT = "FISHING_TRIP_FK", name of the column identifying the fishing trips
#' @param Col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param Col.Fop DEFAULT = "FishingOperation", the name of the column identifying the fishing operations.
#' @param Setting.category DEFAULT = "Setting", the fishing operation category describing setting events
#' @param Hauling.category DEFAULT = "Hauling", the fishing operation category describing hauling events
#' @param buffer.max DEFAULT = 550, maximal buffer to retrieve the setting operations. Starts to 330 m to 550 m until intersections are found.
#' @param update.Fop DEFAULT = FALSE, if set to TRUE, column defined with Col.Fop argument will be updated.
#' @param tol.soaktime DEFAULT = 240, maximal soaktime in hours for a positions to be a potential setting candidate
#' @param remove.orphans DEFAULT = FALSE, if set to TRUE, nets without associated setting events are removed
#'
#' @return nets, a spatial linestring object of class sf with additional columns to describe setting events: setting trip, start and end and soaking time.
#' Nets.Thresh, a list of thresholds, output of Calc_NetsThresholds function
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' run.example = FALSE
#'
#' if(run.example){
#'
#' require(mapview)
#' data(positions)
#' set.seed(221104)
#'
#' pos.sf <- df2sfp(positions, coords = c("LONGITUDE", "LATITUDE"))
#'
#' # Create and consolidate Nets from column "FishingOperation" and category "Hauling"
#' Nets.list.cleaned <- Create_NetsByBoat(traj = pos.sf,
#'                                        Use.BehaviourChanges = TRUE,
#'                                        verbose = TRUE,
#'                                        Auto.ThreshHolds.Detection = TRUE,
#'                                        parallelize = TRUE)
#' Nets.list$Nets.Thresh
#'
#' # Retrieve setting operations from positions using the nets
#' Nets.op <- Retrieve_SettingOperations(traj = pos.sf, ls.nets = Nets.list.cleaned$Nets)
#' names(Nets.op)
#' head(Nets.op$nets)
#'
#' # Save results in a new column named Fop.2
#' # (FishingOperation column might be updated if update.Fop set to TRUE)
#'
#' pos.qualif <- Nets.op$traj
#' pos.qualif$Fop.2 <- rep(NA, nrow(pos.qualif))
#' pos.qualif$Fop.2[!is.na(pos.qualif$Hauling_GearId)] <-  "Hauling"
#' pos.qualif$Fop.2[!is.na(pos.qualif$Setting_GearId)] <-  "Setting"
#' pos.qualif$Fop.2[is.na(pos.qualif$Fop.2)] <- "NotFishing"
#'
#' # Compare operations
#'
#' with(pos.qualif, table(FishingOperation, Fop.2))
#' diff.diagnosis <- pos.qualif$Fop.2!=pos.qualif$FishingOperation
#' summary(Nets.op$nets$soaking.time.hours)
#' summary(Nets.op$nets$length)
#'
#' with(pos.qualif, table(FishingOperation, Fop.2))
#'
#' mapview(Nets.op$nets) +
#'   mapview(pos.qualif, zcol = "FishingOperation", cex  = 2.2) +
#'   mapview(pos.qualif[diff.diagnosis, ])
#'
#' # Different diagnosis related to ambiguous points being either described as "NotFishing" or "Setting".
#' # Because settings are processed quickly, they are actually ambiguous because these points
#' #straddle two operations.
#'
#' }
#'
#' @export
#'


Retrieve_SettingOperations <- function(traj,
         ls.nets,
         Col.VesselId = "VESSEL_FK",
         Col.TripId = "FISHING_TRIP_FK",
         Col.time = "DATE_TIME",
         Col.Fop = "FishingOperation",
         Setting.category = "Setting",
         Hauling.category = "Hauling",
         buffer.max = 550,
         update.Fop = FALSE,
         tol.soaktime = 240,
         remove.orphans = FALSE
         ){

  if(!inherits(traj, "sf")) stop("traj must be a sf points object")
  if(is.na(sf::st_crs(traj))) {stop("CRS must be defined for proper distance calculation")}
  if(!Col.VesselId %in% colnames(traj)) stop("Vessel identifier column not found")
  if(!Col.TripId %in% colnames(traj)) stop("Fishing trip identifier column not found")
  if(!Col.Fop %in% colnames(traj)) stop("Fishing operation identifier column not found")
  if(!Col.time %in% colnames(traj)) stop("Time column not found")
  if(!inherits(ls.nets,"list")) stop("ls.nets must be a list containing nets objects for each Fishing trip, output of Create_NetsByBoat")

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
  if(length(VesselId) > 1) stop("Retrieve_SettingOperations function must be processed by vessel")

  traj.desc <- Traj_Desc(traj)

  if("Nets" %in% names(ls.nets)){
    nets <- do.call(rbind, ls.nets$Nets)
  }else{
    nets <- do.call(rbind, ls.nets)
  }

  nets$setting.start <- rep(NA, nrow(nets))
  nets$setting.end <- rep(NA, nrow(nets))
  colnames(nets)[colnames(nets) %in% "FISHING_TRIP_FK"] <- "FT_Hauling"
  nets$FT_Setting <- rep(NA, nrow(nets))
  nets$soaking.time.hours <- rep(NA, nrow(nets))

  # First define Hauling events
  colGear.name <- paste(Hauling.category, "GearId", sep = "_")
  traj <- traj[, !colnames(traj) %in% colGear.name]
  index.FTS <- unique(unlist(gregexpr(nets$Net, pattern = "_")))-1

  traj.wHaul <- lapply(traj.desc$FISHING_TRIP_FK, function(trip){

    nets2retrieve <- nets[substr(nets$Net, 1, index.FTS) %in% trip, ]

    if(nrow(nets2retrieve) > 0){

      new.traj <- Retrieve_Gear(traj = traj[traj$FISHING_TRIP_FK %in% trip, ],
                                nets2retrieve,
                                buffer = 100,
                                Col.Fop = "FishingOperation",
                                Fop.category = Hauling.category)

    }else{

      new.traj <- traj[traj$FISHING_TRIP_FK %in% trip, ]
      new.traj$empty.col <- rep(NA, nrow(new.traj))
      colnames(new.traj) [colnames(new.traj) %in% "empty.col"] <- colGear.name

    }

    return(new.traj)

  })

  traj <- do.call(rbind, traj.wHaul)
  colnames(traj)[ colnames(traj) %in% colGear.name] <- "Hauling_GearId"

  # Look for setting events
  traj$Setting_GearId <- rep(NA, nrow(traj))

  buffer <- 330
  its <- as.data.frame(sf::st_intersects(traj, sf::st_buffer(nets, dist = buffer), sparse = FALSE) & !traj$FishingOperation %in% Hauling.category)
  test.its <- !apply(its, 2, function(x){ sum(x) > 0})

  while(any(test.its) & buffer < buffer.max){

    buffer <- buffer + 10
    its <- as.data.frame(sf::st_intersects(traj, sf::st_buffer(nets, dist = buffer), sparse = FALSE) & !traj$FishingOperation %in% Hauling.category)
    test.its <- !apply(its, 2, function(x){ sum(x) > 0})

  }

  colnames(its) <- nets$Net

  score.its <- its
  its$timestamp <- Char2Time(traj$DATE_TIME)
  its$FTid <- traj$FISHING_TRIP_FK

  for(nt in nrow(nets):1){

    net <- nets$Net[nt]
    index.nnet <- unlist(gregexpr(net, pattern = "_"))
    index.nnet <- index.nnet[length(index.nnet)]
    index.Ft <- which(traj.desc$FISHING_TRIP_FK %in% substr(net, 1, index.nnet-1))
    sel.FTs <- ifelse(index.Ft-2 < 1, 1, index.Ft-2):index.Ft

    if(length(sel.FTs) == 0) next(nt)

    FtsCandidate <- traj.desc$FISHING_TRIP_FK[sel.FTs]
    diffT.Fts <- c(sapply(1:(length(FtsCandidate)-1), function(k){ difftime(traj.desc$Start.FT[traj.desc$FISHING_TRIP_FK %in% FtsCandidate[length(FtsCandidate)]],
                                                                            traj.desc$Start.FT[traj.desc$FISHING_TRIP_FK %in% FtsCandidate[k]],
                                                                            units = "hours")
    }), 0)

    FtsCandidate <- FtsCandidate[ diffT.Fts < tol.soaktime ]
    TimeCandidate <- its$timestamp < Char2Time(nets$hauling.start[nt])

    its.net <- its[, colnames(its) %in% net]
    its.net[ !its$FTid %in% FtsCandidate] <- FALSE
    its.net[!TimeCandidate] <- FALSE
    its.net <- ContiguousSegments(its.net)
    Seg.Candidates <- sort(unique(its.net$Segments[its.net$data]))

    if(length(Seg.Candidates) == 0) next(nt)

    score.HaulingProximity <- (1:length(Seg.Candidates))/ length(Seg.Candidates)
    score.HaulingProximity[ score.HaulingProximity < 0.25] <- 0.25
    tab.segs <- table(its.net$Segments[its.net$data])
    score.Freq <- tab.segs
    score.Freq[ score.Freq > 4] <- 4
    score.Freq <- score.Freq / max(score.Freq)
    Seg.setting <- traj$FishingOperation[ its.net$Segments %in% Seg.Candidates] %in% Setting.category

    if(any( Seg.setting)){
      tab.setting <- table(its.net$Segments[its.net$data], Seg.setting)
      if(ncol(tab.setting) > 1){
        score.setting <- tab.setting[, 2]
        score.setting[ score.setting > 4] <- 4
      }else{
        score.setting <- rep(1, length(Seg.Candidates))
      }
    }else{ score.setting <- rep(1, length(Seg.Candidates)) }

    score.setting <- score.setting / max(score.setting)
    score.total <- score.HaulingProximity + score.Freq + score.setting

    index.BestSeg <- which(its.net$Segments %in% Seg.Candidates[ which.max(score.total)])
    score.its[ its.net$Segments %in% Seg.Candidates, colnames(its) %in% net] <- do.call(c, lapply(1:length(Seg.Candidates), function(i){ rep(score.total[i], tab.segs[i])}))

    traj$Setting_GearId[index.BestSeg] <- net
    nets$setting.start[nt] <- as.character(min(its$timestamp[index.BestSeg]))
    nets$setting.end[nt] <- as.character(max(its$timestamp[index.BestSeg]))
    nets$FT_Setting[nt] <- unique(its$FTid[index.BestSeg])[1]
    nets$soaking.time.hours[nt] <- difftime(nets$hauling.end[nt], nets$setting.start[nt], units = "hours")

  }

  nets$hauling.start <- as.character(nets$hauling.start)
  nets$hauling.end <- as.character(nets$hauling.end)

  if(remove.orphans){

    nets.orphans <- nets$Net[is.na(nets$soaking.time.hours)]
    nets <- nets[!is.na(nets$soaking.time.hours), ]
    if(update.Fop){
      traj$FishingOperation[ traj$Hauling_GearId %in% nets.orphans ] <- NA
    }
    traj$Hauling_GearId[ traj$Hauling_GearId %in% nets.orphans] <- NA

  }

  if(update.Fop){

      traj$FishingOperation[!is.na(traj$Hauling_GearId)] <- Hauling.category
      traj$FishingOperation[!is.na(traj$Setting_GearId)] <- Setting.category

  }

  if( !Col.VesselId %in% "VESSEL_FK"){
    colnames(traj)[ colnames(traj) %in% "VESSEL_FK"] <- Col.VesselId
  }
  if( !Col.TripId %in% "FISHING_TRIP_FK" ){
    colnames(traj)[ colnames(traj) %in% "FISHING_TRIP_FK"] <- Col.TripId
  }
  if( !Col.Fop %in% "FishingOperation" ){
    colnames(traj)[ colnames(traj) %in%  "FishingOperation"] <- Col.Fop
  }
  if( !Col.time %in% "DATE_TIME" ){
    colnames(traj)[ colnames(traj) %in%  "DATE_TIME"] <-Col.time
  }

  traj <- traj[, c(which(!colnames(traj) %in% "geometry"), which(colnames(traj) %in% "geometry"))]
  nets <- nets[, c(which(!colnames(nets) %in% "geometry"), which(colnames(nets) %in% "geometry"))]

  return(list(nets = nets, traj = traj))


}

