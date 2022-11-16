#' Consolidate_Nets
#'
#' \code{Consolidate_Nets} Consolidates Nets, output of Create_Nets function using thresholds on length, speed and heading
#' This function must be applied by fishing trip.
#'
#' @param traj a spatial points object of class sf
#' @param nets a spatial linestring object of class sf, output of Create_Nets function
#' @param tripId DEFAULT = NULL, the fishing trip identifier to be retrieved from Col.TripId
#' @param Col.TripId DEFAULT = "FISHING_TRIP_FK", name of the column identifying the fishing trips
#' @param Col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param Col.Fop DEFAULT = "FishingOperation", the name of the column identifying the fishing operations.
#' @param Fop.category DEFAULT = "Hauling", the fishing operation category used to build the fishing gear
#' @param Use.BehaviourChanges DEFAULT = TRUE, if set to TRUE, a clustering of behaviour will be used together with the Fop.category sequences to build the nets, see ?Detect_BehaviourChanges
#' @param length.lim DEFAULT = NULL, a vector of length 2 identifying the range of length for a net to be valid
#' @param av.speed.lim DEFAULT = NULL, a vector of length 2 identifying the range of average speeds for a net to be valid
#' @param heading.lim DEFAULT = NULL, a matrix with 2 columns identifying different ranges of heading for a net to be valid
#' @param verbose DEFAULT = FALSE, if TRUE, prints messages.
#'
#' @return Nets, a spatial linestring object of class sf with a unique identifier, the length, average heading and speed, hauling start and end.
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' require(mapview)
#' data(positions)
#'
#' fishTrip <- 18483452
#'
#' pos <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% fishTrip, ],
#'               coords = c("LONGITUDE", "LATITUDE"))
#'
#' pos$FishingOperation[97:103] <-  "NotFishing"
#' pos$FishingOperation[6:9] <- "Hauling"
#' spurious <- c(6:9, 97:103)
#'
#' Nets <- Create_Nets(pos,
#'                     Use.BehaviourChanges = TRUE)
#'
#' Consolidated.nets <- Consolidate_Nets(pos,
#'                                       Nets,
#'                                       length.lim = c(7956, 11512),
#'                                       av.speed.lim = c(1.6, 2.3),
#'                                       heading.lim = matrix(c(222.9, 230, 41.8, 50.2),
#'                                       byrow = TRUE, ncol = 2),
#'                                       verbose = TRUE)
#'
#' mapview(pos, zcol = "FishingOperation", cex  = 2) +
#'   mapview(pos[spurious, ], color = "darkred", zcol = "FishingOperation")+
#'   mapview(Nets, zcol = "Net", lwd  = 3)+
#'   mapview(Consolidated.nets, zcol = "Net", lwd = 3)
#'
#'
#' @export
#'
Consolidate_Nets <- function(traj,
         nets,
         tripId = NULL,
         Col.TripId = "FISHING_TRIP_FK",
         Col.time = "DATE_TIME",
         Col.Fop = "FishingOperation",
         Fop.category = "Hauling",
         Use.BehaviourChanges = TRUE,
         length.lim = NULL,
         av.speed.lim = NULL,
         heading.lim = NULL,
         verbose = FALSE
         ){

  if(!inherits(traj, "sf")) stop("traj must be a sf points object")
  if(is.na(sf::st_crs(traj))) {stop("CRS must be defined for proper distance calculation")}
  if(!Col.TripId %in% colnames(traj)) stop("Fishing trip identifier column not found")
  if(!Col.Fop %in% colnames(traj)) stop("Fishing operation identifier column not found")
  if(!Col.time %in% colnames(traj)) stop("Time column not found")
  if(!inherits(nets,"sf") | !inherits(sf::st_geometry(nets), "sfc_LINESTRING")) stop("net must be an object of class sf LINESTRING, output of Create_Nets function")

  null.lims <- unlist(lapply(list(length.lim, av.speed.lim, heading.lim ), is.null))

  if(all(null.lims)) stop("length.lim, av.speed.lim and heading.lim necessary for net consolidation")

  na.vals <- unlist(lapply(list(length.lim, av.speed.lim, heading.lim ), function(x){ any(is.na(x))}))

  if(any(null.lims)|any(na.vals)){

    if(null.lims[1]|na.vals[1]){ length.lim <- c(0, 100000)}
    if(null.lims[2]|na.vals[2]){ av.speed.lim <- c(0, 25)}
    if(null.lims[3]|na.vals[3]){ heading.lim <- matrix(c(0, 360), ncol = 2)}

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

  if(is.null(tripId)){

    tripId <- unique(traj$FISHING_TRIP_FK)
    if(length(tripId) > 1) stop("Consolidate_Nets function must be processed by vessel and fishing trip")

  }
  if(!tripId %in% traj$FISHING_TRIP_FK) stop(sprintf("tripId not found in %s", Col.TripId))
  if(verbose){ message(sprintf("Consolidating nets for FishingTripId %s", tripId))}

  traj <- traj[traj$FISHING_TRIP_FK %in% tripId, ]

  traj <- traj [, !colnames(traj) %in% c(paste(Fop.category, "GearId", sep = "_"), "GearId")]
  traj <- Retrieve_Gear(traj, nets, buffer = 50, Col.Fop = "FishingOperation", Fop.category = Fop.category)
  colnames(traj) [ colnames(traj) %in% paste(Fop.category, "GearId", sep = "_")] <- "GearId"

  if(sum(!is.na(traj$GearId)) != sum(traj$FishingOperation %in% Fop.category)){
    traj <- traj[, !colnames(traj) %in% "GearId"]
    nets <- Create_Nets(traj[, !colnames(traj) %in% c("Net", "Clust.Pass")], Use.BehaviourChanges = Use.BehaviourChanges)
    traj <- Retrieve_Gear(traj, nets, buffer = 100, Col.Fop = "FishingOperation", Fop.category = Fop.category)
    colnames(traj) [ colnames(traj) %in% paste(Fop.category, "GearId", sep = "_")] <- "GearId"
  }

  Gear.ids <- unique(traj$GearId[!is.na(traj$GearId)])
  Gear2rm <- nets$Net[ !nets$Net %in% Gear.ids]

  if(length(Gear2rm) > 0){

    nets <- nets[ !nets$Net %in% Gear2rm, ]

  }

  if( !"its" %in% colnames(nets)){

    planar.crs <- CustomizedProjectedCRS(nets)
    Nets.planar <- sf::st_transform(nets, planar.crs)
    nets$its <- sapply(1:nrow(Nets.planar), function(k){
      its <- sum(sf::st_crosses(Nets.planar[k, ], Nets.planar[-k, ], sparse = FALSE))
    })

    }

  score.length <- as.numeric(nets$length) >= length.lim[1] & as.numeric(nets$length) <= length.lim[2]
  score.speed <- as.numeric(nets$speed) >= av.speed.lim[1] & as.numeric(nets$speed) <= av.speed.lim[2]
  score.heading <- apply(do.call(rbind, lapply(1:nrow(heading.lim), function(k){
    as.numeric(nets$heading) >= heading.lim[k, 1] & as.numeric(nets$heading) <= heading.lim[k, 2]
  })), 2 , any)
  score.its <- nets$its == 0

  score.total <- score.length + score.speed + score.heading + score.its
  NetsOK <- score.total == 4

  if(verbose){ message(sprintf("%s nets retrieved on %s", length(NetsOK), nrow(nets)+length(Gear2rm)))}
  if(verbose){ message(sprintf("%s nets to be consolidated", sum(!NetsOK)))}

  # Paste Nets and remove spurious
  if(any(!NetsOK)){

    thresh.lim <- diff(length.lim)/4
    lg.limsplit <- c(length.lim[2], 2 * length.lim[2] + thresh.lim)
    lg.limpaste <- c(length.lim[1]/2 - thresh.lim, length.lim[1] + thresh.lim)
    lg.limpaste[ lg.limpaste < 0] <- 0

    Candidate.Nets2Paste <- as.numeric(nets$length) >= lg.limpaste[1] & as.numeric(nets$length) <= lg.limpaste[2]
    Candidate.Nets2Split <- as.numeric(nets$length) >= lg.limsplit[1] & as.numeric(nets$length) <= lg.limsplit[2]

    if( !"Clust.Pass" %in% colnames(traj)){
      DirChanges <- Detect_BehaviourChanges(traj)
      traj <- DirChanges$trip.path
    }
    traj <- ContiguousSegments(traj, "Clust.Pass")

    tab.Behaviour <- with(traj, table(GearId, Clust.Pass))
    Fishing.Behaviours <-   colnames(tab.Behaviour)[ apply(tab.Behaviour, 2, function(x){ sum(x) > 0}) ]
    spurious.behaviours <- NULL
    if(length(Fishing.Behaviours) > 2 ){
      score.behaviours <- apply(tab.Behaviour, 2, sum)
      sorted.behaviours <- colnames(tab.Behaviour)[order(score.behaviours, decreasing = TRUE)]
      spurious.behaviours <- sorted.behaviours[-(1:2)][ sorted.behaviours[-(1:2)] %in% Fishing.Behaviours]
    }

    tab.segments <- with(traj, table(GearId, Segments))
    segments.fop <- sort(unique(traj$Segments))[ apply(tab.segments, 2, function(x){ sum(x) > 0})]
    shared.segs <- sort(unique(traj$Segments))[ apply(tab.segments, 2, function(x){ sum(sum(x) > 0) > 1})]
    missing.NetsInSegs <- sort(unique(traj$Segments[ traj$Segments %in% segments.fop & is.na(traj$GearId)]))

    for(ind.net in (1:sum(!NetsOK))){

      net.index <- which(!NetsOK)[ind.net]
      seg.tab <- tab.segments[net.index, ][ tab.segments[net.index, ] > 0]
      Segs <- names(seg.tab)
      if(verbose){ message(sprintf("Consolidating net %s", net.index))}

      if(!score.heading[net.index]){

        if(length(spurious.behaviours) > 0){

          ind.spuriousBehaviour <- which(traj$GearId %in% nets$Net[net.index] & traj$Clust.Pass %in% spurious.behaviours)
          if(length(ind.spuriousBehaviour) > 0){
            traj$FishingOperation[ traj$GearId %in% nets$Net[net.index] & traj$Clust.Pass %in% spurious.behaviours] <- NA
            if(verbose){ message("Correction of heading based on spurious behaviours")}
          }

        }

      }

      if(Candidate.Nets2Split[net.index]){

        if(length(Segs) > 1){

          majority.report <- names(seg.tab[which.max(seg.tab)])
          traj$FishingOperation[ traj$GearId %in% nets$Net[net.index] & !traj$Segments %in% majority.report] <- NA
          if(verbose){ message("Split correction based on majority report")}

        }else{

          if(length(spurious.behaviours) > 0){

            ind.spuriousBehaviour <- which(traj$GearId %in% nets$Net[net.index] & traj$Clust.Pass %in% spurious.behaviours)
            if(length(ind.spuriousBehaviour) > 0){
              traj$FishingOperation[ traj$GearId %in% nets$Net[net.index] & traj$Clust.Pass %in% spurious.behaviours] <- NA
              if(verbose){ message("Correction split based on spurious behaviours")}
            }
          }

        }

      }

      if(Candidate.Nets2Paste[net.index]){

        if( length(missing.NetsInSegs) > 0){

          if(any(Segs %in% missing.NetsInSegs)){

            traj$FishingOperation[ traj$Segments %in% Segs[Segs %in% missing.NetsInSegs]] <- Fop.category
            if(verbose){ message("Paste correction based on orphan points from segments")}

          }
        }

        if( length(shared.segs) > 0){

          if(any(Segs %in% shared.segs)){

            traj$FishingOperation[ traj$Segments %in% Segs[Segs %in% shared.segs]] <- Fop.category
            if(verbose){ message("Paste correction based on shared segments")}

          }
        }

      }


    }

    new.nets <- Create_Nets(traj[, !colnames(traj) %in% c("Net", "Clust.Pass")], Use.BehaviourChanges = Use.BehaviourChanges)
    buf <- 1
    traj <- Retrieve_Gear(traj[, !colnames(traj) %in%  "GearId" ], new.nets, buffer = buf, Col.Fop = "FishingOperation", Fop.category = Fop.category)
    colnames(traj) [ colnames(traj) %in% paste(Fop.category, "GearId", sep = "_")] <- "GearId"

    missingFopInNets <- traj$FishingOperation %in% Fop.category & is.na(traj$GearId)
    missingNetsInFop <- !new.nets$Net %in% traj$GearId

    while((any(missingFopInNets)|any(missingNetsInFop)) & buf <= 101){
      buf <- buf + 20
      traj <- Retrieve_Gear(traj[, !colnames(traj) %in%  "GearId" ], new.nets, buffer = buf, Col.Fop = "FishingOperation", Fop.category = Fop.category)
      colnames(traj) [ colnames(traj) %in% paste(Fop.category, "GearId", sep = "_")] <- "GearId"
      missingFopInNets <- traj$FishingOperation %in% Fop.category & is.na(traj$GearId)
      missingNetsInFop <- !new.nets$Net %in% traj$GearId
    }
    if(any(missingNetsInFop)){
      new.nets <- new.nets[!missingNetsInFop, ]
    }

    former.sc.lg <- score.length
    former.sc.hdg <- score.heading

    score.length <- as.numeric(new.nets$length) >= length.lim[1] & as.numeric(new.nets$length) <= length.lim[2]
    score.speed <- as.numeric(new.nets$speed) >= av.speed.lim[1] & as.numeric(new.nets$speed) <= av.speed.lim[2]
    score.heading <- apply(do.call(rbind, lapply(1:nrow(heading.lim), function(k){
      as.numeric(new.nets$heading) >= heading.lim[k, 1] & as.numeric(new.nets$heading) <= heading.lim[k, 2]
    })), 2 , any)
    score.its <- new.nets$its < 2

    if(length(former.sc.lg) == length(score.length)){

      test.net <- !former.sc.hdg & former.sc.lg & score.heading & score.its

      if(any(test.net)){
        score.length[test.net] <- TRUE
        val.lg <- as.numeric(new.nets$length[test.net])
        length.lim[1] <- ifelse(length.lim[1] <  val.lg, length.lim[1], val.lg*0.99)
        length.lim[2] <- ifelse(length.lim[2] >  val.lg, length.lim[2],  val.lg*1.01)
      }

    }

    score.total <- score.length + score.speed + score.heading
    lg.limsplit <- c(length.lim[2], 2 * length.lim[2] - thresh.lim)

    Candidate.Nets2Split <- as.numeric(new.nets$length) >= lg.limsplit[1] & as.numeric(new.nets$length) <= lg.limsplit[2]
    Candidate.Nets2Split [score.total < 2] <- FALSE

    tab.Behaviour <- data.frame(rbind(with(traj, table(GearId, Clust.Pass))))
    colnames(tab.Behaviour) <- sort(unique(traj$Clust.Pass))
    maj.spurious <- apply(tab.Behaviour, 1, function(x){
      sum(x[as.numeric(spurious.behaviours)])/sum(x) > 0.5
    })
    Candidate.SpuriousBehaviour <- !score.its & maj.spurious

    net2rm <- score.total < 3 & !Candidate.Nets2Split
    net2rm[Candidate.SpuriousBehaviour] <- TRUE

    Nets <- new.nets[!net2rm, ]

    if(nrow(Nets) > 0){

      Candidate.Nets2Split <- as.numeric(Nets$length) >= lg.limsplit[1] & as.numeric(Nets$length) <= lg.limsplit[2]

      if(any(Candidate.Nets2Split)){

        for(ind.net in (1:sum(Candidate.Nets2Split))){

          net.index <- which(Candidate.Nets2Split)[ind.net]
          ind.traj <- which(traj$GearId %in% Nets$Net[net.index])
          n.pts <- length(ind.traj)
          ind2rm <- floor(n.pts/2) + 1
          traj$FishingOperation[ind.traj[ind2rm]] <- NA

        }
      }

      traj <- Retrieve_Gear(traj[, !colnames(traj) %in%  "GearId" ], Nets, buffer = buf, Col.Fop = "FishingOperation", Fop.category = Fop.category)
      colnames(traj) [ colnames(traj) %in% paste(Fop.category, "GearId", sep = "_")] <- "GearId"

      traj$FishingOperation[ is.na(traj$GearId) & traj$FishingOperation %in% Fop.category] <- NA

      new.nets <- Create_Nets(traj[, !colnames(traj) %in% c("Net", "Clust.Pass")], Use.BehaviourChanges = Use.BehaviourChanges)

      score.length <- as.numeric(new.nets$length) >= length.lim[1] & as.numeric(new.nets$length) <= length.lim[2]
      score.speed <- as.numeric(new.nets$speed) >= av.speed.lim[1] & as.numeric(new.nets$speed) <= av.speed.lim[2]
      score.heading <- apply(do.call(rbind, lapply(1:nrow(heading.lim), function(k){
        as.numeric(new.nets$heading) >= heading.lim[k, 1] & as.numeric(new.nets$heading) <= heading.lim[k, 2]
      })), 2 , any)
      score.total <- score.length + score.speed + score.heading

      new.nets <- new.nets[ score.total == 3, ]

      if(nrow(new.nets) > 0){
        Net.idx <- 1:nrow(new.nets)
        n.zeros <- ifelse(nrow(new.nets) < 99, 2, max(nchar(Net.idx)))
        ID <- sapply(  1:nrow(new.nets), function(k){paste(tripId,
                                                           paste0(paste(rep(0, n.zeros - nchar(Net.idx[k])), collapse = ""), Net.idx[k]), sep ="_")})
        new.nets$Net <- ID

        if(verbose){ message(sprintf("%s nets consolidated", nrow(new.nets))) }
      }else{
        new.nets <- NULL
        if(verbose){ message("no nets consolidated") }
      }

    }else{
      new.nets <- NULL
      if(verbose){ message("no nets consolidated") }
    }


  }else{

    new.nets <- nets
    if(verbose){ message("no nets consolidated") }

  }

  return(new.nets)

}

