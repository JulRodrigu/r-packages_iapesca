#' PathStudyBoundaries
#'
#' \code{PathStudyBoundaries} Creates path boundaries with country or coastline limits defined as a polygon or linestring object.
#' If country.ISO3 is defined, imports country boundaries tiles using geodata package and removes intersection with inland geometries
#'
#' @param trip.path a spatial points object of class sf
#' @param country.ISO3 DEFAULT = NA, ISO3 Country code, see geodata::country_codes()
#' @param WorldMap.sf DEFAULT = NULL, may be defined if country.ISO3 is missing. The world map tile from geodata if already loaded in memory, if NULL, it is downloaded.
#' @param plot.option DEFAULT = FALSE, if TRUE plot the path boundaries retrieved by the function
#' @param buffer DEFAULT = 0, if positive the bounding box will be enlarged with a buffer of "buffer" m.
#' @param CoastLine DEFAULT = TRUE, if true, the path boundaries are returned as a linestring instead of polygon object.
#' @param save2Path DEFAULT = NULL, optional argument to save the object as a rds file. Must be definied as "mypath/myobject.rds"
#' @param FineCountryLimits DEFAULT = TRUE, may be defined if country.ISO3 is missing. If TRUE, for the countries touching the object bounding box, finer tiles will be downloaded from geodata
#' @param SelectedCountriesOnly DEFAULT = TRUE, , may be defined if country.ISO3 is missing. If set to TRUE, only the countries touching the object bounding box will be kept
#'
#' @return study.area, a spatial polygons or linestring object of class sf with country boundaries
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
#'   bgMap <- PathStudyBoundaries(pos.sf,
#'   country.ISO3 = "FRA",
#'   plot.option = TRUE,
#'   buffer = 30000,
#'   CoastLine = TRUE)
#'
#' }
#'
#' @export
#'

PathStudyBoundaries <- function(trip.path,
                                country.ISO3 = NA,
                                WorldMap.sf = NULL,
                                plot.option = FALSE,
                                buffer = 0,
                                CoastLine = TRUE,
                                save2Path = NULL,
                                FineCountryLimits = FALSE,
                                SelectedCountriesOnly = TRUE){


  if(!inherits(trip.path, "sf")) {
    stop("SfObj must be a valid sf object")
  }
  if(is.na(sf::st_crs(trip.path))) {
    stop("Coordinate reference system must be defined using sf::st_crs")
  }
  if(!country.ISO3 %in% geodata::country_codes()$ISO3 & !is.na(country.ISO3)) {
    stop("country.ISO3s must be a valid country ISO3 code, see geodata::country_codes()")
  }

  bbox <- SfBbox(trip.path)
  if( buffer != 0) { bbox <- suppressWarnings(sf::st_buffer(bbox, dist = buffer))}

  study.area <- bbox

  if( !is.na(country.ISO3)){

      Country.lim <- sf::st_as_sf(geodata::gadm(country = country.ISO3, level = 0, path = tempdir()))

    }else{

      Country.lim <- BackgroundMapWithCountries(trip.path,
                                                WorldMap.sf = WorldMap.sf,
                                                save2Path = save2Path,
                                                FineCountryLimits = FineCountryLimits,
                                                SelectedCountriesOnly = SelectedCountriesOnly)

    }

    study.area <- suppressWarnings(sf::st_difference(bbox, Country.lim))
    study.area <- suppressWarnings(sf::st_make_valid(sf::st_cast(study.area, "POLYGON")))
    colnames(study.area)[1] <- "ID"
    attr(study.area, "sf_column") <- "geometry"
    sf::st_geometry(study.area) <- "geometry"
    colnames(study.area)[ncol(study.area)] <- "geometry"

    if( CoastLine ){

      LineObj <- suppressWarnings(sf::st_cast(study.area, "LINESTRING"))
      LineObj$ID <- Harmonize_Ids("Ln", 1:nrow(LineObj))
      Line.BB <- sf::st_boundary(bbox)
      LineObj.coast <- suppressWarnings(sf::st_difference(LineObj,
                                                          sf::st_buffer(Line.BB, dist = 0.1)))
      study.area <- LineObj.coast

    }

  if(!is.null(save2Path)){
      saveRDS(study.area, save2Path)
  }

  if( plot.option ){

    plot( sf::st_geometry(bbox), axes = TRUE, lwd = 2, lty = 2, main = "Study area definition")
    plot( sf::st_geometry(Country.lim), add = TRUE)
    plot( sf::st_geometry(study.area), add = TRUE, col = "red" )

  }

  return(study.area)

}
