#' Pos2Path
#'
#' \code{Pos2Path} Creates a linear path with linestrings from positions.
#'
#' @param sf.obj a spatial points object of class sf
#' @param col.clust DEFAULT = NULL, optional argument to define a column
#' @param val DEFAULT = NULL, optional value to set a name associated to the object. Not relevant if col.clust is defined
#' @param epsg either a valid "crs" object or a valid numeric espg code
#' @param state DEFAULT = "forward", the segment is built from the position to its next neighbour, otherwise its previous one.
#'
#' @return Path, a spatial linestring object of class sf
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' require(mapview)
#' data(positions)
#'
#' # The function Pos2Path is applied by Fishing trip
#' tripId <- 18529291
#' # Select the fishing trip and convert to sf object
#' pos.sf <- df2sfp(positions[ positions$FISHING_TRIP_FK %in% tripId, ],
#'                  coords = c("LONGITUDE", "LATITUDE"))
#'
#' path <- Pos2Path(pos.sf)
#' path.forward <- Pos2Path(pos.sf, col.clust = "FishingOperation", state = "forward")
#' path.backward <- Pos2Path(pos.sf, col.clust = "FishingOperation", state = "backward")
#'
#' mapview::mapview(pos.sf, zcol = "FishingOperation")+
#'   mapview(path)+
#'   mapview(path.backward, zcol = "FishingOperation")+
#'   mapview(path.forward, zcol = "FishingOperation")
#'
#' @export
#'

Pos2Path <- function(sf.obj, col.clust = NULL, val = NULL, epsg = NA, state = "forward"){

  if(!inherits(sf.obj, "sf")) {stop("sf.obj must be a valid sf object")}
  if(is.na(epsg)) {epsg <- sf::st_crs(sf.obj)}

  if(is.null(col.clust)){

    if(is.null(val)){
      val = data.frame(ID = 1)
    }
    Path <- sf::st_sf(val, geometry = coord2sf( df = sf::st_coordinates(sf.obj), type = "linestring", epsg = epsg ))

  }else{

    if(!state %in% c("forward", "backward")) {stop("state can be defined as forward or backward")}

    sf.obj <- ContiguousSegments(sf.obj, covar = col.clust)

    Path <- do.call(rbind, lapply(unique(sf.obj$Segments), function(Seg) {

      ind.seg <- sort(which(sf.obj$Segments %in% Seg))
      clust.val <- unique(sf::st_set_geometry(sf.obj[ind.seg, col.clust], NULL))

      if(state %in% "forward"){
        ind.seg <- c(ind.seg, ind.seg[length(ind.seg)]+1)
        ind.seg <- ind.seg[ ind.seg <= nrow(sf.obj)]
      }else{
        ind.seg <- c(ind.seg[1]-1, ind.seg)
        ind.seg <- ind.seg[ ind.seg > 0]
      }

      ls.line <- sf::st_sf(val = data.frame(clust = clust.val),
                           geometry = coord2sf(df = sf::st_coordinates(sf.obj)[ind.seg, ], type = "linestring", epsg = epsg))
      return(ls.line)
    }))

  }

  return(Path)

}
