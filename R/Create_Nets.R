#' Create_Nets
#'
#' \code{Create_Nets} Creates nets from positions flagged by an operation.
#' Each net is identified as a spatial linestring with a unique identifier, the length, average heading and speed, hauling start and end.
#' This function must be applied by fishing trip.
#'
#' @param traj a spatial points object of class sf
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
#' Traj_Desc(positions)
#'
#' fishTrip <- 18483452
#' pos <- df2sfp(positions, coords = c("LONGITUDE", "LATITUDE"))
#'
#' Nets.raw <- Create_Nets(pos,
#'                         tripId = fishTrip,
#'                         Use.BehaviourChanges = FALSE)
#'
#'
#' Nets.Behav <- Create_Nets(pos,
#'                           tripId = fishTrip,
#'                           Use.BehaviourChanges = TRUE)
#'
#' Nets.Thresh <- Create_Nets(pos,
#'                            tripId = fishTrip,
#'                            Use.BehaviourChanges = TRUE,
#'                            length.lim = c(8743.294, 11496.073),
#'                            av.speed.lim = c(1.642265, 2.231296),
#'                            heading.lim = matrix(c(219.05, 232.97, 25.87, 70.26),
#'                            byrow = TRUE, ncol = 2))
#'
#'
#' mapview(pos[pos$FISHING_TRIP_FK %in% fishTrip, ], zcol = "FishingOperation") +
#'   mapview(Nets.raw, zcol = "Net", lwd  = 3)+
#'   mapview(Nets.Behav, zcol = "Net", lwd  = 3)+
#'   mapview(Nets.Thresh, zcol = "Net", lwd  = 3)
#'
#'
#' @export
#'
Create_Nets <- function(traj,
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
    if(length(tripId) > 1) stop("Create_Nets function must be processed by vessel and fishing trip")

  }
  if(!tripId %in% traj$FISHING_TRIP_FK) stop(sprintf("tripId not found in %s", Col.TripId))
  traj <- traj[traj$FISHING_TRIP_FK %in% tripId, ]

  timestamp <- Char2Time(traj$DATE_TIME)
  laps.resamp <-  mean(difftime(timestamp[2:length(timestamp)], timestamp[1:(length(timestamp)-1)], units = "secs"), na.rm = TRUE)/2
  traj <- traj[ order(timestamp), ]

  traj$Net <- traj$FishingOperation %in% Fop.category

  if(Use.BehaviourChanges){
    DirChanges <- Detect_BehaviourChanges(traj)
    traj <- sf::st_join(traj[, !colnames(traj) %in% "Clust.Pass"],  DirChanges$trip.path[, "Clust.Pass"])
    traj <- ContiguousSegments(traj, covar = c("Net", "Clust.Pass"), na.rm = FALSE)
  }else{
    traj <- ContiguousSegments(traj, covar = "Net", na.rm = FALSE)
  }

  Candidate.Nets <- unique(traj$Segments[traj$Net])
  length.nets <- table(traj$Segments[ traj$Segments %in% Candidate.Nets])
  contiguous.nets <- (duplicated(c(Candidate.Nets-1, Candidate.Nets))|duplicated(c(Candidate.Nets, Candidate.Nets-1)))[(length(Candidate.Nets)+1):(2*length(Candidate.Nets))]

  Nets2Paste <- Candidate.Nets[ contiguous.nets & length.nets == 1]

  if(length(Nets2Paste) > 0 & any(length.nets > 1) & Use.BehaviourChanges){

    traj.2 <- ContiguousSegments(traj, covar = "Net", na.rm = FALSE)
    colnames(traj.2)[ colnames(traj.2) %in% "Segments"] <- "Seg.Nets"
    traj.2 <- sf::st_join(traj.2, traj[, "Segments"])
    tab.TransCode <- with(traj.2[traj.2$Net, ], table(Segments, Seg.Nets))
    TransCode <- colnames(tab.TransCode)[apply(tab.TransCode, 1, which.max)]

    Nets2Keep <- sapply(1:length(Nets2Paste), function(k){
      TransSeg <- TransCode[ which(Candidate.Nets %in% Nets2Paste[k]) ]
      score <- tab.TransCode[, colnames(tab.TransCode) %in% TransSeg]
      Candidate.Net2Keep <- Candidate.Nets[ TransCode %in% TransSeg & contiguous.nets]
      Net2Keep <- Candidate.Net2Keep [which.max(score[names(score) %in% Candidate.Net2Keep])]
      return(Net2Keep)
    })

    traj$Segments <- factor(as.numeric(traj$Segments))
    levels(traj$Segments)[ levels(traj$Segments) %in% Nets2Paste] <-  Nets2Keep
    traj$Segments <- as.character(traj$Segments)
    Candidate.Nets <- unique(traj$Segments[traj$Net])
    Candidate.Nets <- Candidate.Nets[!is.na(Candidate.Nets)]
  }

  n.Nets <- length(Candidate.Nets)
  n.zeros <- ifelse(n.Nets < 99, 2, max(nchar(Candidate.Nets)))
  epsg <- sf::st_crs(traj)

  if(length(Candidate.Nets) > 0){

    Nets <- do.call(rbind,
                    lapply(Candidate.Nets,
                           function(x) {
                             Net.idx <- which(Candidate.Nets %in% x)
                             index <- which(traj$Segments %in% x)
                             index.around <- unique(c(ifelse(index[1]-1 < 1, index[1], index[1]-1),
                                                      index,
                                                      ifelse(index[length(index)]+1 <= nrow(traj), index[length(index)]+1, index[length(index)])))

                             if(length(index.around) > 2){

                               net2retrieve <- traj[index.around, ]
                               net.resamp <- CalcSpeed(
                                 CalcHeading(
                                   Resample_Traj(net2retrieve, col.time = Col.time, resampling = as.numeric(laps.resamp))
                                 ))
                               net2retrieve <- net.resamp[-(1), ]
                               spurious.Heading <- which(abs(net2retrieve$HEADING.deg - mean(net2retrieve$HEADING.deg[-c(1, nrow(net2retrieve))], na.rm = TRUE)) > 10)

                               if(length(spurious.Heading) > 0){
                                 spurious.Heading <- spurious.Heading[ spurious.Heading %in% c(1, nrow(net2retrieve))]
                               }
                               if(length(spurious.Heading) > 0){
                                 net2retrieve <- net2retrieve[ -spurious.Heading, ]
                               }

                               net.geom <- coord2sf(sf::st_coordinates(net2retrieve), type = "linestring", epsg = epsg)
                               ID <- Harmonize_Ids(tripId, Net.idx, n.zeros)
                               net.line <- sf::st_sf(val =  data.frame(Net = ID),
                                                     geometry = net.geom)
                               net.line$length <- sf::st_length(net.geom)
                               net.line$heading <- mean(net2retrieve$HEADING.deg, na.rm = TRUE)
                               net.line$speed <- mean(net2retrieve$SPEED.kn, na.rm = TRUE)
                               net.line$hauling.start <- min(Char2Time(net2retrieve$DATE_TIME))
                               net.line$hauling.end <- max(Char2Time(net2retrieve$DATE_TIME))

                             }else{

                               net.line <- NULL

                             }

                             return(net.line)}))

    null.lims <- unlist(lapply(list(length.lim, av.speed.lim, heading.lim ), is.null))

    if(!all(null.lims)){

      new.nets <- try(Consolidate_Nets(traj = traj,
                                   nets = Nets,
                                   tripId = tripId,
                                   Col.TripId = "FISHING_TRIP_FK",
                                   Col.time = "DATE_TIME",
                                   Col.Fop = "FishingOperation",
                                   Fop.category = Fop.category,
                                   Use.BehaviourChanges = Use.BehaviourChanges,
                                   length.lim = length.lim,
                                   av.speed.lim = av.speed.lim,
                                   heading.lim = heading.lim,
                                   verbose = verbose
      ), silent = TRUE)

      if(inherits(new.nets, "try-error")){
        warning("Consolidation process failed")
      }else{Nets <- new.nets}

    }

    if(!is.null(Nets)){
      if(nrow(Nets) > 0){

        Net.idx <- 1:nrow(Nets)
        Nets$Net <- Harmonize_Ids(Constantstring = tripId, index = Net.idx, ID.length = n.zeros)

        planar.crs <- CustomizedProjectedCRS(Nets)
        Nets.planar <- sf::st_transform(Nets, planar.crs)

        Nets$its <- sapply(1:nrow(Nets.planar), function(k) its <- sum(sf::st_crosses(Nets.planar[k, ], Nets.planar[-k, ], sparse = FALSE)))
        Nets$FISHING_TRIP_FK <- tripId
        Nets <- Nets[, c(which(!colnames(Nets) %in% "geometry"),
                         which(colnames(Nets) %in% "geometry"))]

      }else{

        Nets <- NULL

      }
    }

  }else{ Nets <- NULL}

  return(Nets)

}

