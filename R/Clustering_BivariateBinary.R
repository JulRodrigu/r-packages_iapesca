#' Clustering_BivariateBinary
#'
#' \code{Clustering_BivariateBinary} Performs gaussian binary clustering applied to fishing trips
#'
#' @param data a data.frame, used to train the model, sf data points can also be handled, crs being set as WGS84
#' @param time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param lon DEFAULT = "LONGITUDE", name of the longitude column in WGS84
#' @param lat DEFAULT = "LATITUDE", name of the latitude column in WGS84
#' @param columns.ref DEFAULT = c("VESSEL_FK",  "FISHING_TRIP_FK"), character string describing the columns with vessel and fishing trips identifiers
#' @param speed.unit DEFAULT = "knots", if set to "knots", speed will be converted to knots
#'
#' @return data.output, input data.frame with new columns named "turn", "speed", "hdg" and "BinClust", outputs of EMbC::stbc function
#'
#' @author WKSSFGEO 2021, inspired from Tania Mendo and Einar Hjorleifsson, Julien Rodriguez, \email{julien.rodriguez@ifremer.com}
#'
#' @examples
#'
#' data(positions)
#'
#' traj.desc <- Traj_Desc(positions)
#' FishingTrips <- unique(traj.desc$FISHING_TRIP_FK)
#'
#' # Function to be applied by fishing trips
#'
#' positions <- do.call(rbind, lapply(FishingTrips, function(trip){
#'
#'   pos.trip <- positions[ positions$FISHING_TRIP_FK %in% trip, ]
#'   binclust <- Clustering_BivariateBinary(pos.trip)
#'
#'   return(binclust)
#'
#' }))
#'
#' head(positions)
#'
#' with(positions, table(BinClust, FishingOperation))
#'
#'
#' @export
#'

Clustering_BivariateBinary <- function(data = positions,
                                       time = "DATE_TIME",
                                       lon = "LONGITUDE",
                                       lat = "LATITUDE",
                                       columns.ref = c("VESSEL_FK",  "FISHING_TRIP_FK"),
                                       speed.unit = "knots"){


  if(inherits(data, "sf")){ data <- sfp2df(data) }

  if(!all(columns.ref %in% c("VESSEL_FK",  "FISHING_TRIP_FK"))){
    colnames(data)[ colnames(data) %in% columns.ref] <- c("VESSEL_FK",  "FISHING_TRIP_FK")
  }
  vessel.id <- unique(data$VESSEL_FK)
  FT.id <- unique(data$FISHING_TRIP_FK)
  if(length(vessel.id) > 1| length(FT.id) > 1) {
    stop("Clustering_BivariateBinary function must be processed by vessel and fishing trip")}

  d <- data[, c(time, lon, lat)]
  colnames(d) <- c("time", "lon", "lat")
  d$time <- strptime(Char2Time(d$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  stbc.output <- EMbC::stbc(d, info = -1)
  fun.output <- data.frame(turn = EMbC:::getTurns(stbc.output),
                           speed = EMbC:::getSpeed(stbc.output),
                           hdg = stbc.output@hdg,
                           BinClust = stbc.output@A
  )

  if( speed.unit %in% "knots"){
    fun.output$speed <- fun.output$speed *1.943844
  }

  data.output <- cbind(data[ , !colnames(data) %in% colnames(fun.output)],
                       fun.output)

  colnames(data.output)[colnames(data.output) %in% c("VESSEL_FK",  "FISHING_TRIP_FK")] <- columns.ref

  return(data.output)

}

