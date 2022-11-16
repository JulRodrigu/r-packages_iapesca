#' Retrieve_Gear
#'
#' \code{Retrieve_Gear} Retrieves the gear identifier from Nets, output of Create_Nets function.
#'
#' @param traj a spatial points object of class sf
#' @param nets a spatial linestring object of class sf, output of Create_Nets function
#' @param buffer DEFAULT = 50, buffer applied to the nets to retrieve the fishing operations. This buffer has to be adjusted depending on the fishing operation.
#' @param Col.Fop DEFAULT = "FishingOperation", the name of the column identifying the fishing operations.
#' @param Fop.category the fishing operation category used to build the fishing gear
#'
#' @return traj, the initial spatial points object with a new column named "Fop.category_GearId" identifying the gear linked to a fishing operation
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
#' fishTrip <- 18529291
#' pos <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% fishTrip, ],
#'               coords = c("LONGITUDE", "LATITUDE"))
#'
#' Nets <- Create_Nets(pos,
#'                     Use.BehaviourChanges = TRUE)
#' Nets$Net
#'
#' pos.hauling <- Retrieve_Gear(pos[pos$FISHING_TRIP_FK %in% fishTrip, ],
#'                              Nets,
#'                              buffer = 50,
#'                              Col.Fop = "FishingOperation",
#'                              Fop.category = "Hauling")
#'
#' sum(!is.na(pos.hauling$hauling))
#' sum(!is.na(pos.hauling$Hauling_GearId))
#' with(pos.hauling, table(hauling, Hauling_GearId))
#'
#'
#' @export
#'
Retrieve_Gear <- function(traj,
                          nets,
                          buffer = 50,
                          Col.Fop = "FishingOperation",
                          Fop.category){

  if(!inherits(traj, "sf")) stop("traj must be a sf points object")
  if(is.na(sf::st_crs(traj))) {stop("CRS must be defined for proper distance calculation")}
  if(!inherits(nets,"sf") | !inherits(sf::st_geometry(nets), "sfc_LINESTRING")) stop("Nets must be an object of class sf LINESTRING, output of Create_Nets function")
  if(!Col.Fop %in% colnames(traj)) stop("Fishing operation identifier column not found")

  if( !Col.Fop %in% "FishingOperation" ){
    traj <- traj[, !colnames(traj) %in% "FishingOperation" ]
    colnames(traj)[ colnames(traj) %in% Col.Fop ] <- "FishingOperation"
  }
  colGear.name <- paste(Fop.category, "GearId", sep = "_")
  traj <- traj[, !colnames(traj) %in% colGear.name]

  its <- sf::st_intersects(traj, sf::st_buffer(nets, dist = buffer), sparse = FALSE) & traj$FishingOperation %in% Fop.category
  colnames(its) <- nets$Net

  GearId <- apply(its, 1, function(x){
    gear.id <- names(x)[x]
    if(length(gear.id) == 0){
      gear.id <- NA
    }
    return(gear.id)
  })

  if(inherits(GearId, "list")){

    spurious <- which(unlist(lapply(GearId, length)) > 1)

    for (i in 1:length(spurious)){

      tab <- table(unlist(GearId[(spurious[i]-1):(spurious[i]+1)]))
      choice <- names(tab)[which.max(tab)]
      GearId[[spurious[i]]] <- choice

    }

  }

  traj$Gear.id <- unlist(GearId)

  colnames(traj)[ colnames(traj) %in% "Gear.id"] <- colGear.name
  colnames(traj)[ colnames(traj) %in% "FishingOperation"] <- Col.Fop

  traj <- traj[, c(which(!colnames(traj) %in% "geometry"), which(colnames(traj) %in% "geometry"))]

  return(traj)

}
