#' Positions2Tacsat
#'
#' \code{Positions2Tacsat} Converts fishing vessels positions to tacsat format used in vmstools. Positions are cleaned from duplicates and spurious positions based on speed.
#' Speed and heading are calculated in knots and degrees.
#'
#' @param positions either a data.frame with coordinates or a sf spatial points object. CRS must be defined as WGS84 (EPSG 4326)
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param MaxSpeed DEFAULT = 25, speed threshold in nautic miles, used as argument for CleanSpuriousSpeeds
#' @param coords DEFAULT  c("LONGITUDE", "LATITUDE"), a vector of length 2 indicating the name of coordinates columns in df as x an y
#' @param epsg DEFAULT = 4326, EPSG code describing positions CRS (4326 for WGS84 crs)
#' @param columns.ref DEFAULT = c("VESSEL_FK",  "FISHING_TRIP_FK"), character string describing the columns to be converted as c("VE_REF", "FT_REF"): vessel and fishing trips identifiers
#' @param parallelize DEFAULT = FALSE, optional argument for parallelizing the processing of positions
#' @param nCores DEFAULT = NULL, number of cores used, if NULL set to floor(parallel::detectCores()/2)
#' @param keep.columns = NULL, character string describing optionnal columns to be kept in output
#' @param as.sf DEFAULT = FALSE
#'
#' @return positions.TacsatFormat, a data.frame compatible with tacsat format used in vmstools
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @importFrom foreach %dopar%
#'
#' @examples
#'
#' data(positions)
#' head(positions)
#'
#' pos.tacsat <- Positions2Tacsat(positions, keep.columns = "FishingOperation")
#'
#' head(pos.tacsat)
#'
#' @export
#'

Positions2Tacsat <- function( positions,
                              col.time = "DATE_TIME",
                              MaxSpeed = 25,
                              coords = c("LONGITUDE", "LATITUDE"),
                              epsg = 4326,
                              columns.ref  = c("VESSEL_FK",  "FISHING_TRIP_FK"),
                              keep.columns = NULL,
                              parallelize = FALSE,
                              nCores = NULL,
                              as.sf = FALSE){

    if(!col.time %in% colnames(positions)) {stop("Time column not found")}
    if(!all(columns.ref %in% colnames(positions))) {stop("Vessel and/or fishingtrip columns not recognized")}

    if(inherits(positions, "sf")){
      positions <- sfp2df(positions, rename.coords = coords)
    }

    if(any(!coords %in% colnames(positions))) {stop("LongLat coordinates not found")}

    if(!all(columns.ref %in% c("VESSEL_FK",  "FISHING_TRIP_FK"))){

      positions <- positions[ , c(which(colnames(positions) %in% columns.ref[1]),
                                  which(colnames(positions) %in% columns.ref[2]),
                                  which(!colnames(positions) %in% c(columns.ref)))]

      colnames(positions)[ colnames(positions) %in% columns.ref] <- c("VESSEL_FK",  "FISHING_TRIP_FK")
      columns.ref <- c("VESSEL_FK",  "FISHING_TRIP_FK")

    }

    # Define timestamp
    colnames(positions) [ colnames(positions) %in% col.time] <- "DATE_TIME"
    positions$SI_DATIM  <- as.POSIXct(Char2Time(positions$DATE_TIME), tz = "UTC", format = "%d/%m/%Y  %H:%M")
    SI_DATE <- as.character(as.Date(positions$SI_DATIM))
    positions$SI_DATE <- as.character( paste(substr(SI_DATE, 9, 10), substr(SI_DATE, 6, 7), substr(SI_DATE, 1, 4), sep = "/"))
    positions$SI_TIME <- substr(positions$SI_DATIM, 12, 19)
    # Remove spurious lonlat and duplicated pings
    spurious.LonLat <- which(abs(positions[, coords[2]]) > 90 | abs(positions[, coords[1]]) > 180)
    uniqueTacsat    <- paste(positions$VESSEL_FK, positions$SI_DATE, positions[, coords[1]], positions[, coords[2]])
    dup.Tacsat <- which(duplicated(uniqueTacsat))
    to.rm <- unique(c(spurious.LonLat, dup.Tacsat))
    if(any(to.rm)){
      positions <- positions[ -to.rm, ]
    }
    # Summarize FTs
    traj.desc <- Traj_Desc(positions)
    # Define as sf object
    positions.sf <- sf::st_as_sf(positions, coords = coords)
    sf::st_crs(positions.sf) <- sf::st_crs(epsg)

    trips <- unique(traj.desc$FISHING_TRIP_FK)
    if(length(trips) != nrow(traj.desc)) stop("Fishing trip IDs are not unique")
    # Calc speed and heading and remove speed over SpeedMax iteratively by trip id.

    if(parallelize){
      test.packages <- c("foreach", "parallel", "doParallel", "iterators") %in% utils::installed.packages()
      if(all(!test.packages)){
        parallelize <- FALSE
        message(sprintf("Package(s) %s missing for using parallelize option", paste(c("foreach", "parallel", "doParallel", "iterators")[!test.packages], collapse = ", ")))
      }
    }

    Process_Positions <- function(trip){

      processed.data <- suppressMessages(try(Process_TripPositions(positions.sf [ positions.sf$FISHING_TRIP_FK %in% trip, ],
                                                                   col.time = "DATE_TIME",
                                                                   SpeedLimit = NA,
                                                                   MaxSpeed = MaxSpeed,
                                                                   create.paths = FALSE,
                                                                   CalcFeatures = FALSE), silent = TRUE))

      if( inherits(processed.data, "try-error")){

        processed.data <- suppressMessages(try(CalcHeading(CalcSpeed(positions.sf[ positions.sf$FISHING_TRIP_FK %in% trip, ])), silent = TRUE))

        if( inherits(processed.data, "try-error")){

          processed.data <- positions.sf [ positions.sf$FISHING_TRIP_FK %in% trip, ]
          processed.data$DISTANCE.nm <- NA
          processed.data$DIFFTIME.secs <- NA
          processed.data$SPEED.kn <- NA
          processed.data$HEADING.deg <- NA}

      }else{

        processed.data <- processed.data$trip.path

      }
      return(processed.data)
    }

    if(!parallelize){

      pos.CalcSpeedHeading.ls <- lapply(trips, Process_Positions)

    }else{

      nCores <- ifelse(is.null(nCores), floor(parallel::detectCores()/2), nCores)
      if(nCores > length(trips)){ nCores <- length(trips)}
      cl <- parallel::makeCluster(nCores)
      doParallel::registerDoParallel(cl)

      pos.CalcSpeedHeading.ls <- foreach::foreach(k = iterators::iter(1:length(trips)),
                                    .combine = "c",
                                    .packages = c("sf", "iapesca")) %dopar%
        {
          suppressWarnings(list(posPro <- Process_Positions(trips[k])))

        }

      parallel::stopCluster(cl)

    }

    valid.Trip <- unlist(lapply(pos.CalcSpeedHeading.ls, function(x) {inherits(x, "sf")}))
    pos.CalcSpeedHeading.ls <- pos.CalcSpeedHeading.ls[valid.Trip]

    positions.CalcSpeedHeading <- do.call(rbind, pos.CalcSpeedHeading.ls)

    positions.preprocessed  <- cbind( sf::st_set_geometry(positions.CalcSpeedHeading, NULL), sf::st_coordinates(positions.CalcSpeedHeading))
    colBasics <- c(columns.ref, "SPEED.kn", "HEADING.deg", "X",  "Y")
    colBasics <- colBasics[colBasics %in% colnames(positions.preprocessed)]
    otherCol <- colnames(positions.preprocessed)[! colnames(positions.preprocessed) %in% colBasics]
    positions.preprocessed <- positions.preprocessed[, c(colBasics, otherCol)]

    colnames(positions.preprocessed ) [ colnames(positions.preprocessed) %in% colBasics] <- c("VE_REF", "FT_REF", "SI_SP", "SI_HE", "SI_LONG", "SI_LATI")
    colRef <- c("VE_REF", "FT_REF",  "DEPARTURE_DATE_TIME", "RETURN_DATE_TIME", "DEPARTURE_LOCATION_FK", "RETURN_LOCATION_FK",  "PROGRAM_FK",
                "SI_LATI", "SI_LONG", "SI_DATE", "SI_TIME", "SI_SP", "SI_HE", "SI_DATIM")
    if(!is.null(keep.columns)){
      colRef <- c(colRef, keep.columns)
    }
    col2Keep <-  colRef[  colRef %in% colnames(positions.preprocessed )]

    positions.TacsatFormat <- positions.preprocessed [, col2Keep]

    if("vmstools" %in% utils::installed.packages()){
      positions.TacsatFormat <- vmstools::sortTacsat(vmstools::formatTacsat(positions.TacsatFormat))
    }
    if(as.sf){
      positions.TacsatFormat  <- df2sfp(positions.TacsatFormat , coords = c("SI_LONG", "SI_LATI"))
    }

  return(positions.TacsatFormat)

}
