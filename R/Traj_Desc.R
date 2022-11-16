#' Traj_Desc
#'
#' \code{Traj_Desc} Summarizes and sorts vessels and fishing trips by fishing trip starts
#'
#' @param positions either a data.frame with coordinates or a sf spatial points object.
#' @param col.time DEFAULT "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param columns.ref DEFAULT c("VESSEL_FK",  "FISHING_TRIP_FK"), character string describing the vessel and fishing trips identifiers
#' @param add.group DEFAULT = NULL, additional column used to group the fishing trips
#'
#' @return traj.desc, a summary describing the vessels and fishing trips
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @importFrom data.table data.table
#'
#' @examples
#'
#' data(positions)
#'
#' Traj_Desc(positions)
#' Traj_Desc(positions, add.group = "FishingOperation")
#' Traj_Desc(positions, add.group = c("setting", "hauling"))
#'
#' @export
#'

Traj_Desc <- function(positions,
                      col.time = "DATE_TIME",
                      columns.ref  = c("VESSEL_FK", "FISHING_TRIP_FK"),
                      add.group = NULL
                      ){

  if(inherits(positions, "sf")){ positions <- sfp2df(positions)}
  if(!col.time %in% colnames(positions)) {stop("Time column not found")}
  if(!all(columns.ref %in% colnames(positions))) {stop("Vessel and/or fishingtrip columns not recognized")}

  positions <- positions[ , c(which(colnames(positions) %in% columns.ref[1]),
                              which(colnames(positions) %in% columns.ref[2]),
                              which(colnames(positions) %in% col.time ),
                              which(!colnames(positions) %in% c(columns.ref, col.time)))
                              ]
  colnames(positions)[ colnames(positions) %in% c(columns.ref, col.time)] <- c("VESSEL_FK", "FISHING_TRIP_FK", "DATE_TIME")

  if(!is.null(add.group)){
    positions <- ContiguousSegments(positions, covar = add.group, na.rm = FALSE)
  }else{
    positions$Segments <- rep(1, nrow(positions))
  }

  pos.dt <- data.table::data.table(positions)
  retrieve_startFT <- function(x){ min(Char2Time(x)) }
  retrieve_endFT <- function(x){ max(Char2Time(x)) }
  boats <- sort(unique(pos.dt$VESSEL_FK))

  traj.desc <- merge(pos.dt[, retrieve_startFT(DATE_TIME), by =c("VESSEL_FK", "FISHING_TRIP_FK", "Segments")],
                     pos.dt[, retrieve_endFT(DATE_TIME), by =c("VESSEL_FK", "FISHING_TRIP_FK", "Segments")],
                     by = c("VESSEL_FK", "FISHING_TRIP_FK", "Segments"))
  traj.desc <- traj.desc[order(VESSEL_FK, V1.x, Segments), ]

  if(!is.null(add.group)){

    traj.desc <- merge(traj.desc,
                       dplyr::distinct(positions[, c("Segments", add.group)]),
                       by = "Segments")

  }

  traj.desc <- as.data.frame(traj.desc)
  traj.desc <- traj.desc[, c("VESSEL_FK", "FISHING_TRIP_FK", add.group,  "V1.x", "V1.y" )]
  colnames(traj.desc) <- c(columns.ref, add.group, "Start.FT", "End.FT")

  return(traj.desc)

}

