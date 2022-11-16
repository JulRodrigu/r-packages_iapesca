#' plot_Feature
#'
#' \code{plot_Feature} plots boat paths described by a quantitative variable and optionally by a qualitative variable or the fishing operations.
#'
#' @param boat.path a spatial points object of class sf
#' @param col.feature name of the column of the feature to plot
#' @param col.FishingOp DEFAULT = NULL, the name of the column identifying the fishing operations.
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param cex.Fop DEFAULT = NULL, size of points for FishingOp column
#' @param Fishing DEFAULT = "Fishing", category of fishing events, not relevant if ActiveGear is FALSE and Hauling/Setting are identified distinctly
#' @param Hauling DEFAULT = "Hauling", category of hauling events, relevant if ActiveGear is TRUE
#' @param Setting DEFAULT = "Setting", category of setting events, relevant if ActiveGear is TRUE
#' @param Fishing.op DEFAULT = NULL, a data.frame describing the fishing operations in time with columns FishingOperation, FISHING_START_DATE_TIME and FISHING_END_DATE_TIME, if set to NULL will be built from boat.paths
#' @param ActiveGear DEFAULT = FALSE, identifies if gear is active or not prints rectangles when gear is operating if set to TRUE.
#' @param main DEFAULT = NA, The graphic's title.
#'
#' @return plots a map
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#'
#' plot_Feature(boat.path = df2sfp(positions[positions$FISHING_TRIP_FK %in% "18529291", ]),
#'              col.feature = "speed", col.FishingOp = "FishingOperation", cex.Fop = 1.5)
#' plot_Feature(boat.path = df2sfp(positions[positions$FISHING_TRIP_FK %in% "18529291", ]),
#'              col.feature = "turn", col.FishingOp = "FishingOperation", cex.Fop = 1.5)
#'
#' @export
#'

plot_Feature <- function(boat.path,
                       col.feature,
                       col.FishingOp,
                       col.time = "DATE_TIME",
                       cex.Fop = NULL,
                       Fishing = "Fishing",
                       Hauling = "Hauling",
                       Setting = "Setting",
                       Fishing.op = NULL,
                       ActiveGear = FALSE,
                       main = NA
                       ){

  if(!inherits( boat.path, "sf")) {
    stop("boat.path must be a valid sf object")
  }

  if(!col.time %in% colnames(boat.path)) {stop("Time column not found")}
  if(!col.feature %in% colnames(boat.path)) {stop("feature column not found")}
  if(!col.FishingOp %in% colnames(boat.path)) {stop("Fishing operation column not found")}
  if(is.na(sf::st_crs(boat.path))) {stop("CRS must be defined for proper distance calculation")}
  if(!is.null(Fishing.op)){
    if(any(!c("FishingOperation", "FISHING_START_DATE_TIME", "FISHING_END_DATE_TIME") %in%    colnames(Fishing.op))){
      stop("FishingOperation, FISHING_START_DATE_TIME and FISHING_END_DATE_TIME columns expected in Fishing.op")
    }
  }

  colnames(boat.path)[ colnames(boat.path) %in% col.time] <- "DATE_TIME"
  colnames(boat.path)[ colnames(boat.path) %in% col.FishingOp] <- "FishingOperation"
  colnames(boat.path)[ colnames(boat.path) %in% col.feature] <- "Feature"

  boat.path$timestamp <- as.POSIXct(Char2Time(boat.path$DATE_TIME))
  boat.path <- boat.path [ order(as.numeric(boat.path$timestamp)), ]
  # boat.path$timestamp <- substr(boat.path$timestamp,12,20)

  nnai <- !is.na(boat.path$FishingOperation)

  boat.path$FishingOperation <- as.character(boat.path$FishingOperation)
  lev.FoP <- unique(boat.path$FishingOperation[nnai])

  if(Fishing %in% lev.FoP){

    boat.path$FishingOperation[ boat.path$FishingOperation %in% Fishing] <- "Fishing"
    boat.path$FishingOperation[ !boat.path$FishingOperation %in% "Fishing"] <- "NotFishing"

  }

  lev.FoP <- unique(boat.path$FishingOperation[nnai])

  if(Hauling %in% lev.FoP | Setting %in% lev.FoP){

    boat.path$FishingOperation[ boat.path$FishingOperation %in% Hauling] <- "Hauling"
    boat.path$FishingOperation[ boat.path$FishingOperation %in% Setting] <- "Setting"
    boat.path$FishingOperation[ !boat.path$FishingOperation %in% c("Setting", "Hauling")] <- "NotFishing"
    ActiveGear <- FALSE

  }

  if(any("Fishing" %in% boat.path$FishingOperation)){

    boat.path$FishingOperation[!nnai] <- "NotFishing"
    nnai <- !is.na(boat.path$FishingOperation)

  }

  if(is.null(Fishing.op)){

    boat.dt <- data.table::data.table(sfp2df(ContiguousSegments(boat.path, covar = "FishingOperation")))
    retrieve_startFT <- function(x){ min(Char2Time(x)) }
    retrieve_endFT <- function(x){ max(Char2Time(x)) }
    sel.FishingSegs <- boat.dt$Segments[ !boat.dt$FishingOperation %in% "NotFishing"]
      Fishing.op <- merge(dplyr::distinct(boat.dt[Segments %in% sel.FishingSegs,,], Segments, FishingOperation),
                          merge(boat.dt[Segments %in% sel.FishingSegs, retrieve_startFT(DATE_TIME), by = Segments],
                          boat.dt[Segments %in% sel.FishingSegs, retrieve_endFT(DATE_TIME), by = Segments],
                          by = "Segments"), by = "Segments")
      colnames(Fishing.op)[!colnames(Fishing.op) %in% c( "Segments", "FishingOperation")] <- c("FISHING_START_DATE_TIME", "FISHING_END_DATE_TIME")
      Fishing.op <- Fishing.op[order(FISHING_START_DATE_TIME), ]

  }

  lev.FoP <- unique(as.character(boat.path$FishingOperation[nnai]))

  col.P <- c("blue", "darkred")

  if(length(lev.FoP) > 2){

    col.P <- lev.FoP
    col.P[ col.P %in% "Hauling"] <- "darkred"
    col.P[ col.P %in% "Setting"] <- "blue"
    col.P[ col.P %in% "NotFishing"] <- "darkolivegreen"
    if(is.null(cex.Fop)){ cex.Fop <- 1.5}

  }else{

    if("NotFishing" %in% lev.FoP){col.P <- rev(col.P)
    }else{ col.P <- lev.FoP
    col.P[ col.P %in% "Hauling"] <- "darkred"
    col.P[ col.P %in% "Setting"] <- "blue"
    }
  }

  if(is.null(cex.Fop)){ cex.Fop <- ifelse(!ActiveGear & any(!nnai), 3, 1.5)}

  plot( Feature  ~ timestamp, boat.path, pch = "+", ylab = col.feature, xlab = "Time", xaxt="n", main = ifelse(is.na(main), "", main))

  if(ActiveGear & !is.null(Fishing.op)){

    y.range <- range(boat.path$Feature)
    n.rect <- nrow(Fishing.op)
    enlarge.rect <- median(abs(boat.path$Feature))*0.2
    y.range <- y.range + c(-1,1)*enlarge.rect

    for (i in 1:n.rect){

      graphics::rect(as.POSIXct(Char2Time(Fishing.op$FISHING_START_DATE_TIME))[i], y.range[1],
                      as.POSIXct(Char2Time(Fishing.op$FISHING_END_DATE_TIME))[i], y.range[2], col = "deepskyblue")

    }

    graphics::points( Feature  ~ timestamp, boat.path, pch = "+")

  }

  graphics::axis.POSIXct(side=1, at=cut(boat.path$timestamp, "hours"), format="%H:%M")
  graphics::lines( Feature ~ timestamp, boat.path)

  if(any(nnai)){
    index.FoP <- lapply(lev.FoP, function(x){ which(boat.path$FishingOperation %in% x)})

    for( i in 1:length(lev.FoP)){
        graphics::points( Feature ~ timestamp, boat.path[index.FoP[[i]], ], col = col.P [i], pch = "+", cex = cex.Fop)
    }
  }

  if(!is.null(Fishing.op) & !ActiveGear){

     graphics::abline(v = Char2Time(Fishing.op$FISHING_START_DATE_TIME[ Fishing.op$FishingOperation %in% "Setting"]), lty = 2, col = "blue")
     graphics::abline(v = Char2Time(Fishing.op$FISHING_START_DATE_TIME[ Fishing.op$FishingOperation %in% "Hauling"]), lty = 2, col = "darkred")

   }

  }
