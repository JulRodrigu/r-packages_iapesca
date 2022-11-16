#' Process_TripPositions
#'
#' \code{Process_TripPositions} A wrapper function to process the positions by fishing trips:
#' Calculates speed and cleans the spurious positions, eventually redefines the fishing trips ore samples the positions.
#' At last the heading is calculated and a linear path with linestrings is optionnaly retrieved from positions
#'
#' @param trip.path a spatial points object of class sf
#' @param col.clust DEFAULT = NULL, optional argument to define a column in Pos2Path
#' @param val DEFAULT = NULL, see ?Pos2Path, optional value to set a name associated to the object. Not relevant if col.clust is defined
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param state DEFAULT = "forward", see ?Pos2Path, the segment is built from the position to its next neighbour, otherwise its previous one.
#' @param MaxSpeed DEFAULT = 25, see ?CleanSpuriousSpeeds, speed threshold in nautic miles
#' @param SpeedLimit DEFAULT = NA,
#' @param Harbours DEFAULT = NULL, see ?Detect_FishingTrips, a spatial object either points or polygons of class sf to describe the harbours
#' @param CoastLine DEFAULT = NULL, see ?Detect_FishingTrips, a spatial line objet of class sf describing the coastline.
#' @param DetectCoastLine = FALSE, see ?Detect_FishingTrips
#' @param buf.size.start = 1000, see ?Detect_FishingTrips
#' @param min.stop = 3600, see ?Detect_FishingTrips
#' @param resampling DEFAULT = NULL, see ?Resample_Traj, the timelapse for resampling in seconds, if NULL positions won't be resampled
#' @param keep.var DEFAULT = NULL, see ?Resample_Traj, select columns to keep if resampling is performed.
#' @param create.paths = TRUE, returns the positions as a linear path using Pos2Path function
#' @param columns.ref DEFAULT = c("VESSEL_FK",  "FISHING_TRIP_FK"), character string describing the columns with vessel and fishing trips identifiers
#' @param CalcFeatures DEFAULT = FALSE, calculate additional features if requested, see ?CalcFeatures
#' @param movingWindow DEFAULT = 1, number of neighbours to consider when calculating the movingWindow, see ?CalcFeatures
#'
#' @return trip.path, the processed positions and traj, a spatial linestring object of class sf or NULL if create.paths is set to FALSE
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
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
#' head(pos.sf)
#' # Processing trip positions while checking the fishing trips (argument harbours)
#' # and downgrading to 1000 seconds (argument resampling).
#'
#' posPro <- Process_TripPositions(pos.sf,
#' col.clust = "FishingOperation",
#' Harbours = harbours,
#' keep.var = c("setting", "hauling"),
#' resampling = 1000)
#'
#' head(posPro)
#'
#' mapview(posPro$traj) + mapview(pos.sf, zcol = "FishingOperation")
#'
#' @export
#'
Process_TripPositions <- function(trip.path,
                                  col.clust = NULL,
                                  val = NULL,
                                  col.time = "DATE_TIME",
                                  state = "forward",
                                  MaxSpeed = 25,
                                  SpeedLimit = NA,
                                  Harbours = NULL,
                                  CoastLine = NULL,
                                  DetectCoastLine = FALSE,
                                  buf.size.start = 1000,
                                  min.stop = 3600,
                                  resampling = NULL,
                                  keep.var = NULL,
                                  create.paths = TRUE,
                                  columns.ref = c("VESSEL_FK",  "FISHING_TRIP_FK"),
                                  CalcFeatures = FALSE,
                                  movingWindow = 1
                                  ) {


  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}
  if(!all(columns.ref %in% colnames(trip.path))) {stop("Vessel and/or fishingtrip columns not recognized")}

  if(!all(columns.ref %in% c("VESSEL_FK",  "FISHING_TRIP_FK"))){
    colnames(trip.path)[ colnames(trip.path) %in% columns.ref] <- c("VESSEL_FK",  "FISHING_TRIP_FK")
  }
  vessel.id <- unique(trip.path$VESSEL_FK)
  FT.id <- unique(trip.path$FISHING_TRIP_FK)
  if(length(vessel.id) > 1| length(FT.id) > 1) {stop("Process_TripPositions function must be processed by vessel and fishing trip")}

  # Remove potential duplicates
  trip.path <- dplyr::distinct(trip.path)

  # Detect fishing trips if requested (if already detected fishing trips are spurious)
  # Then calculate and clean spurious speeds
  if( !anyNA(SpeedLimit)| !is.null(Harbours) | DetectCoastLine){

    trip.path <- Detect_FishingTrips(trip.path,  col.time = col.time,
                                            SpeedLimit = SpeedLimit,
                                            MaxSpeed = MaxSpeed,
                                            Harbours = Harbours,
                                            CoastLine = CoastLine,
                                            DetectCoastLine = DetectCoastLine,
                                            buf.size.start = buf.size.start,
                                            min.stop = min.stop)$trip.path

    FT.id <- unique(trip.path$FISHING_TRIP_FK)
    # Resample if requested
    if(!is.null(resampling)|CalcFeatures){

      if(!is.null(col.clust)) { keep.var <- unique(c(keep.var, col.clust)) }

      trip.path <- do.call(rbind,
                           lapply(FT.id, function(tripID){

                             FT.path <- Resample_Traj(trip.path = trip.path[ trip.path$FISHING_TRIP_FK %in% tripID, ],
                                           col.time = col.time,
                                           MaxSpeed = MaxSpeed,
                                           resampling = resampling,
                                           keep.var = keep.var)
                             FT.path.wspeed <- CalcSpeed(trip.path = FT.path, col.time = col.time)
                             FT.path.w.heading <- CalcHeading(FT.path.wspeed)

                             if(CalcFeatures){
                               FT.path <- CalcFeatures(trip.path = FT.path.w.heading,
                                                       col.time = col.time,
                                                       Harbours = Harbours)
                             }else{
                               FT.path <- FT.path.w.heading
                             }

                             return(FT.path)

                             })
      )

    }else{

      trip.path <- do.call(rbind,
                           lapply(FT.id, function(tripID){

                             FT.path.wspeed <- CleanSpuriousSpeeds(trip.path = trip.path[ trip.path$FISHING_TRIP_FK %in% tripID, ],
                                                                   col.time = col.time,
                                                                   MaxSpeed = MaxSpeed)
                             FT.path.w.heading <- CalcHeading(FT.path.wspeed)
                             FT.path <- trip.path.w.heading

                             return(FT.path)

                           })
      )

    }



  }else{

    # Resample if requested
    if(!is.null(resampling)|CalcFeatures){

      if(!is.null(col.clust)) { keep.var <- unique(c(keep.var, col.clust)) }
      trip.path <- Resample_Traj(trip.path = trip.path,
                                 col.time = col.time,
                                 MaxSpeed = MaxSpeed,
                                 resampling = resampling,
                                 keep.var = keep.var)

      }

    trip.path.wspeed <- CleanSpuriousSpeeds(trip.path,
                                            col.time = col.time,
                                            MaxSpeed = MaxSpeed)
    trip.path.w.heading <- CalcHeading(trip.path.wspeed)

    if(CalcFeatures){
      trip.path <- CalcFeatures(trip.path = trip.path.w.heading,
                                col.time = col.time,
                                Harbours = Harbours,
                                movingWindow = movingWindow)
    }else{
      trip.path <- trip.path.w.heading
    }

  }

  if( is.null(val) & is.null(col.clust)) { val = data.frame( Trip = 1)}

  if(create.paths){
    traj <- sf::st_make_valid(Pos2Path( trip.path,
                                        col.clust = col.clust,
                                        val = val,
                                        epsg = 4326,
                                        state = state))
  }else{
    traj <- NULL
  }

  return( list(trip.path = trip.path, traj = traj))

}
