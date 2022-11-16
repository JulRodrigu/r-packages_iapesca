#' CleanSpuriousSpeeds
#'
#' \code{CleanSpuriousSpeeds} Calculates speed and cleans the spurious positions, a used-defined threshold being defined for speed.
#' The positions are removed iteratively until the speed remains below the threshold.
#'
#' @param trip.path a spatial points object of class sf
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param MaxSpeed DEFAULT = 25, speed threshold in nautic miles
#'
#' @return trip.path.wspeed, a spatial points object of class sf with new columns "DISTANCE.nm" in nautic miles, DIFFTIME.secs" in seconds and "SPEED.kn" in knots
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
#' # Calculates and cleans speeds below threshold
#' dim(pos.sf)
#' pos.raw <- CleanSpuriousSpeeds(pos.sf, MaxSpeed = 20)
#' dim(pos.raw)
#' # No positions removed
#'
#' # Translate randomly selected positions
#' set.seed(100522)
#' pos.spurious <- pos.sf
#' sel.spurious <- sample(1:nrow(pos.sf), size = 3)
#' trans <- sample(seq(-30000, 30000, by = 10000), size = 3)
#'
#' for(i in 1:3){
#'
#'   pos.spurious[sel.spurious[i], ] <-  Translate_Positions(pos.spurious[sel.spurious[i], ],
#'   Key.x = trans[i], Key.y = trans[i])
#'
#' }
#'
#' pos.spurious <- CalcSpeed(pos.spurious)
#' summary(pos.spurious$SPEED.kn)
#'
#' pos.cleaned <- CleanSpuriousSpeeds(pos.spurious, MaxSpeed = 20)
#'
#' dim(pos.cleaned)
#' dim(pos.sf)
#' # 3 spurious positions have been removed
#' summary(pos.spurious$SPEED.kn)
#' summary(pos.cleaned$SPEED.kn)
#' summary(pos.raw$SPEED.kn)
#'
#' pal_cols <- function(n){ rev(rainbow(n = n, start = 0, end = 4/6))}
#' mapview::mapview(pos.raw, zcol = "SPEED.kn", col.regions = pal_cols )+
#'   mapview::mapview(pos.cleaned, zcol = "SPEED.kn", col.regions = pal_cols)
#'
#' @export
#'

CleanSpuriousSpeeds <- function(trip.path,
                                col.time = "DATE_TIME",
                                MaxSpeed = 25) {

  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  if(nrow(trip.path) == 1) {stop("Only one position available: can't be processed")}

  if(! "SPEED.kn" %in% colnames(trip.path)){
    trip.path.wspeed <- CalcSpeed(trip.path, col.time = col.time)
  }else{
    trip.path.wspeed <- trip.path
  }

  it <- 1

  while( any(!trip.path.wspeed$SPEED.kn < MaxSpeed & !is.na(trip.path.wspeed$SPEED.kn)) & it < 6){

    spurious.speed <- which(!trip.path.wspeed$SPEED.kn < MaxSpeed & !is.na(trip.path.wspeed$SPEED.kn))

    spuriousIsNotUnique <- c()
    if(length(spurious.speed) > 1){
      spuriousIsNotUnique <- which((spurious.speed[2:length(spurious.speed)] - spurious.speed[1:(length(spurious.speed)-1)]) == 1) + 1
    }
    if(length(spuriousIsNotUnique) > 0){
      spurious.speed <- spurious.speed[ -spuriousIsNotUnique ]
    }
    valid.speed <- 1:nrow(trip.path)

    if(length(spurious.speed) > 0){
      valid.speed <- valid.speed[-spurious.speed]
    }

    trip.path <- trip.path.wspeed[valid.speed, ]
    trip.path.wspeed <- CalcSpeed(trip.path[, !colnames(trip.path) %in% c("DIFFTIME.secs", "SPEED.kn", "DISTANCE.nm")], col.time = col.time)
    it <- it + 1
  }

  return(trip.path.wspeed)

}
