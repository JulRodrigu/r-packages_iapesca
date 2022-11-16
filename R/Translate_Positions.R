#' Translate_Positions
#'
#' \code{Translate_Positions} Translate geographic positions by x and y after reprojecting the data in a cartesian system
#'
#' @param sf.points a spatial points object of class sf
#' @param Key.x translation of x coordinates in meters
#' @param Key.y translation of y coordinates in meters
#' @param proj DEFAULT = "UTM", the cartesian coordinates system to be used for reprojection. Choices between "WorldMercator", "UTM" uses lonlat2UTM, "Customized" uses CustomizedProjectedCRS
#'
#' @return pos.trans, the translated dataset with WGS84 CRS
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
#' user.proj <- "WorldMercator"
#' user.proj <- "Customized"
#' user.proj <- "UTM"
#'
#' # Use an appropriate coordinates system depending on the translation performed
#'
#' pos.trans <- Translate_Positions(pos.sf, Key.x = 500000, Key.y = 500000,  proj = user.proj)
#' pos.rev <- Translate_Positions(pos.trans, Key.x = -500000, Key.y = -500000,  proj = user.proj)
#'
#' mapview(pos.sf) +
#' mapview(pos.trans, col.regions = "darkred") +
#' mapview(pos.rev, col.regions = "darkred")
#'
#' # Check distances
#' pos.trans <- CalcDist(pos.trans)
#' pos.sf <- CalcDist(pos.trans)
#' summary(pos.trans$DISTANCE.nm)
#' summary(pos.sf$DISTANCE.nm)
#' cor(pos.trans$DISTANCE.nm, pos.sf$DISTANCE.nm)
#'
#' @export
#'

Translate_Positions <- function(sf.points, Key.x = NULL, Key.y = NULL, proj = "UTM"){

  if(!inherits(sf.points, "sf")) stop("sf.points must be a valid sf object")
  if(!proj %in% c("UTM", "WorldMercator", "Customized")) stop("proj must be defined as UTM, WorldMercator or Customized")

  coord.names <- colnames(sf::st_coordinates(sf.points))[1:2]

  if(proj %in% "WorldMercator"){
    planar.proj <- sf::st_crs(3395)
  }
  if(proj %in% "UTM"){
    planar.proj <- suppressWarnings(lonlat2UTM(sf::st_coordinates(sf::st_centroid(sf.points))))
  }
  if(proj %in% "Customized"){
    planar.proj <- CustomizedProjectedCRS(sf.points)
  }

  pos.planar <- sf::st_transform(sf.points, planar.proj)
  pos.planar.df <- sfp2df(pos.planar, rename.coords = c("X", "Y"))

  if(!is.null(Key.x)){
    pos.planar.df$X <- pos.planar.df$X + Key.x
  }
  if(!is.null(Key.y)){
    pos.planar.df$Y <- pos.planar.df$Y + Key.y
  }

  colnames(pos.planar.df)[ colnames(pos.planar.df) %in% c("X", "Y")] <- coord.names
  pos.trans <- df2sfp(pos.planar.df, coords = coord.names, input.crs = planar.proj, output.crs = sf::st_crs(4326))

  return(pos.trans)

}
