#' BackgroundMapWithCountries
#'
#' \code{BackgroundMapWithCountries} Download tiles using geodata package to create a background with country limits
#'
#' @param SfObj a spatial points object of class sf
#' @param WorldMap.sf DEFAULT = NULL, the world map tile from geodata if already loaded in memory, if NULL, it is downloaded.
#' @param save2Path DEFAULT = NULL, optional argument to save the object as a rds file. Must be definied as "mypath/myobject.rds"
#' @param FineCountryLimits DEFAULT = TRUE, if TRUE, for the countries touching the object bounding box, finer tiles will be downloaded from geodata
#' @param SelectedCountriesOnly DEFAULT = TRUE, if set to TRUE, only the countries touching the object bounding box will be kept
#'
#' @return WorldMap.cor, a spatial polygons object of class sf with country boundaries
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' run.example <- FALSE
#' # downloading the tiles may be long....
#'
#' if(run.example){
#'
#'   pos.sf <- df2sfp(data.frame(LONGITUDE = -4.552, LATITUDE = 48.36132))
#'   bgMap <- BackgroundMapWithCountries(pos.sf)
#'   plot(sf::st_geometry(pos.sf), axes = TRUE, col = "red", lwd = 5, cex = 3)
#'   plot(sf::st_geometry(bgMap), add = TRUE)
#'
#' }
#'
#' @export
#'

BackgroundMapWithCountries <- function(SfObj,
                                       WorldMap.sf = NULL,
                                       save2Path = NULL,
                                       FineCountryLimits = TRUE,
                                       SelectedCountriesOnly = TRUE){

  if(!inherits(SfObj, "sf")) {
    stop("SfObj must be a valid sf object")
  }
  if(is.na(sf::st_crs(SfObj))) {
    stop("Coordinate reference system must be defined using sf::st_crs")
  }
  if(is.null(WorldMap.sf)){

    world.map <- geodata::world(resolution = 1, path = tempdir())
    WorldMap.sf  <- sf::st_make_valid(sf::st_as_sf(world.map))
    unlink(list.files(tempdir()), recursive = TRUE)

    }

  bbox <- SfBbox(SfObj)

  its <- apply(sf::st_intersects(WorldMap.sf, bbox, sparse = FALSE), 1, any)
  buf.size <- 0

  while(!any(its) & buf.size < 100000){
    buf.size <- buf.size + 1000
    its <- apply(sf::st_intersects(WorldMap.sf, sf::st_buffer(bbox, dist = buf.size), sparse = FALSE), 1, any)
  }

  GIDs <- WorldMap.sf$GID_0[its]

  if(FineCountryLimits){

    Co <- try(do.call(rbind,
                      lapply(GIDs,
                             function(x){ sf::st_as_sf(geodata::gadm(country = x, level = 0, path = tempdir()))} )),
              silent = TRUE)

    unlink(list.files(tempdir()), recursive = TRUE)

    if( !inherits(Co, "try-error")){
      colnames(Co) <- colnames(WorldMap.sf)
      WorldMap.fine <- rbind(WorldMap.sf[!WorldMap.sf$GID_0 %in% WorldMap.sf$GID_0[its], ], Co)
      WorldMap.fine <- try(sf::st_make_valid(WorldMap.fine), silent = TRUE)
      if(!inherits(WorldMap.fine, "try-error")){
        WorldMap.sf <- WorldMap.fine
      }
    }
  }

  WorldMap.cor <- try(suppressWarnings(sf::st_make_valid(WorldMap.sf)), silent = TRUE)
  if(inherits(WorldMap.cor, "try-error")){ WorldMap.cor <- WorldMap.sf }

  if(SelectedCountriesOnly){

    WorldMap.cor <- suppressWarnings(sf::st_cast(WorldMap.cor[WorldMap.sf$GID_0 %in% GIDs, ], "POLYGON"))
    WorldMap.cor <- WorldMap.cor[WorldMap.cor$GID_0 %in% GIDs, ]

  }

  if(!is.null(save2Path)){
    saveRDS(WorldMap.cor, save2Path)
  }

  return(WorldMap.cor)


}

