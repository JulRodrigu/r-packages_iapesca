#' Detect_BehaviourChanges
#'
#' \code{Detect_BehaviourChanges} Defines a non-supervised behavioural clustering based on speed, heading and turning angle.
#' This function has to be applied by fishing trip.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.Dir DEFAULT = NULL, name of the direction column, if set to NULL, it is calculated using CalcHeading
#' @param col.Speed DEFAULT = NULL, name of the speed column, if set to NULL, it is calculated using CalcSpeed
#' @param col.HeadingChange DEFAULT = NULL, name of the turning angle column, if set to NULL, it is calculated using CalcTurningAngle
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param dirChange.Lim DEFAULT = NA, user defined threshold for turning angle to define a direction change
#'
#' @return a list with trip.path, a spatial points object of class sf with new columns named "Clust.Pass", value of behavioural cluster and "Pass.number" identifying contiguous positions with similar behavior
#' and ClustDesc, a data.frame with clusters characteristics.
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' require(mapview)
#' data(positions)
#'
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' pos.DirChange <- Detect_BehaviourChanges(pos.sf)
#' names(pos.DirChange)
#'
#' head(pos.DirChange$trip.path)
#' pos.DirChange$ClustDesc
#'
#' mapview(pos.DirChange$trip.path, zcol = "Clust.Pass")
#'
#' @export
#'
Detect_BehaviourChanges <- function(trip.path,
                                    col.Dir = NULL,
                                    col.Speed = NULL,
                                    col.HeadingChange = NULL,
                                    col.time = "DATE_TIME",
                                    dirChange.Lim = NA){

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}

  if(is.null(col.Speed)){
    trip.path <- CalcSpeed(trip.path)
    col.Speed <- "SPEED.kn"
  }else{
    if(!col.Speed %in% colnames(trip.path)) {stop("Speed column not found")}
  }
  if(is.null(col.Dir)){
    trip.path <- CalcHeading(trip.path)
    col.Dir <- "HEADING.deg"
  }else{
    if(!col.Dir %in% colnames(trip.path)) {stop("Heading column not found")}
  }
  if(is.null(col.HeadingChange)){
    trip.path <- CalcTurningAngle(trip.path, col.time = col.time)
    col.HeadingChange <- "TURN.deg"
  }

  heading <- unlist( sf::st_set_geometry(trip.path[, col.Dir], NULL) )
  speed <- unlist( sf::st_set_geometry(trip.path[, col.Speed], NULL) )
  abs.HeadingChange <- unlist( sf::st_set_geometry(trip.path[, col.HeadingChange], NULL) )

  Pass.number <- rep(NA, length(heading))
  km.clust <- Clustering_KMeans(cbind(heading, abs.HeadingChange, speed),
                                              nb.class.max = ifelse(length(Pass.number)>20, 20, length(Pass.number)))$data
  km.desc <- doBy::summaryBy( stats::as.formula( paste(paste( colnames(km.clust)[-1], collapse = "+" ), "Clust",  sep ="~")),
                              data =   km.clust,
                              FUN = mean
                              )
  km.desc$eff <- table(km.clust$Clust)

  if( is.na(dirChange.Lim)){
    km.dirchange <- Clustering_KMeans(abs.HeadingChange)$data
    class.change <- which.min(with(km.dirchange, aggregate(v, by = list(Clust), mean))[,2])
    dirChange.Lim <- stats::quantile(km.dirchange[ km.dirchange$Clust %in% class.change, "v"], probs = 0.95)
  }

  dir.change  <- c(1, which(abs.HeadingChange > dirChange.Lim), length(heading))
  Pass.number[ dir.change] <- 1:(length(dir.change))

  for (k in 1:(length(dir.change) -1) ){
    index <- dir.change[k] : (dir.change[k+1]-1)
    Pass.number[index] <- k
  }

  Pass.number <- factor(as.character(Pass.number))
  levels(Pass.number) <- paste0(set_0nbr(levels(Pass.number), max(nchar(levels(Pass.number)))),
                                levels(Pass.number))

  trip.path$Pass.number <- as.character(Pass.number)
  trip.path$Clust.Pass <- as.character(km.clust$Clust)

  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]

  return( list(trip.path = trip.path, ClustDesc = km.desc))

}
