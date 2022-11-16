#' plot_Trip
#'
#' \code{plot_Trip} plots boat paths described by a quantitative variable and the fishing operations and the evolution of features in time for the same trip
#' see ?plot_Paths and ?plot_Feature
#'
#' @param boat.path a spatial points object of class sf
#' @param col.features vector containing the name(s) of the column(s) of the feature(s) to plot
#' @param col.FishingOp DEFAULT = NULL, the name of the column identifying the fishing operations.
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param Fishing.op DEFAULT = NULL, a data.frame describing the fishing operations in time with columns FishingOperation, FISHING_START_DATE_TIME and FISHING_END_DATE_TIME, if set to NULL will be built from boat.paths
#' @param Fishing DEFAULT = "Fishing", category of fishing events, not relevant if ActiveGear is FALSE and Hauling/Setting are identified distinctly
#' @param Hauling DEFAULT = "Hauling", category of hauling events, relevant if ActiveGear is TRUE
#' @param Setting DEFAULT = "Setting", category of setting events, relevant if ActiveGear is TRUE
#' @param ActiveGear DEFAULT = FALSE, identifies if gear is active or not prints rectangles when gear is operating if set to TRUE.
#' @param z.col DEFAULT = NULL, print the fishing trip paths by a qualitative variable, eg the gear
#' @param quanti DEFAULT = "SPEED.kn", quantitative variable to plot on the map
#' @param main DEFAULT = NA, The graphic's title. Is also used to define the path where to save the background map
#' @param trip_ID DEFAULT = "FISHING_TRIP_FK", the name of the column identifying the fishing trips.
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param FishingOp DEFAULT = NULL, the name of the column identifying the fishing operations.
#' @param path DEFAULT = getwd(), path where to save the background map created
#' @param col.pal DEFAULT = NULL, the color palette to print the quantitative variable, if null set a palette from grDevices::rainbow
#' @param Create.BgMap DEFAULT = FALSE, if set to TRUE, download a background map using PathStudyBoundaries
#' @param rm.BgMap DEFAULT = FALSE, if set to TRUE, removes the Background map saved to path
#' @param expandBy DEFAULT = 0.2, expansion coefficient to create the background map (X5)
#' @param Path2BgMap DEFAULT = NULL, used defined path to load the background map.
#' @param cex.FoP DEFAULT = NULL, size of points for FishingOp column
#' @param gears.set DEFAULT = NULL, gears set as a spatial linestring object output of Create_Nets function
#' @param gears.hauled DEFAULT = NULL, gears hauled as a spatial linestring object output of Create_Nets function
#' @param lwd.gears DEFAULT = c(3, 3), lwd parameters, a vector of length two (set, hauled) for plotting gears
#' @param credits DEFAULT = "Ifremer, HISSEO, RECOPESCA program" writes the source below the map
#'
#' @return plots a map
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' data(positions)
#' plot_Trip(boat.path = df2sfp(positions[positions$FISHING_TRIP_FK %in% "18529291", ]),
#'           col.features = c("speed"), col.FishingOp = "FishingOperation",
#'           quanti = "speed", Create.BgMap = FALSE,
#'           main = "FT_18529291")
#'
#' @export
#'
plot_Trip <- function(boat.path,
                       col.features,
                       col.FishingOp,
                       col.time = "DATE_TIME",
                       Fishing.op = NULL,
                       Fishing = "Fishing",
                       Hauling = "Hauling",
                       Setting = "Setting",
                       ActiveGear = FALSE,
                       z.col = NULL,
                       quanti =  "SPEED.kn",
                       main = NA,
                       trip_ID = "FISHING_TRIP_FK",
                       path = getwd(),
                       col.pal = NULL,
                       Create.BgMap = FALSE,
                       rm.BgMap = FALSE,
                       expandBy = 0.2,
                       Path2BgMap = NULL,
                       cex.FoP = NULL,
                       gears.set = NULL,
                       gears.hauled = NULL,
                       lwd.gears = c(3,3),
                       credits = "Ifremer, HISSEO, RECOPESCA program"
                       ){

  if(!inherits( boat.path, "sf")) {
    stop("boat.path must be a valid sf object")
  }

  if(!col.time %in% colnames(boat.path)) {stop("Time column not found")}
  if(any(!col.features %in% colnames(boat.path))) {stop("features column not found")}
  if(!col.FishingOp %in% colnames(boat.path)) {stop("Fishing operation column not found")}
  if(is.na(sf::st_crs(boat.path))) {stop("CRS must be defined for proper distance calculation")}
  if(!quanti %in% colnames(boat.path)) {stop("quanti column not found")}
  if(!trip_ID %in% colnames(boat.path)) {stop("Fishing trip ID column not found")}

  colnames(boat.path)[ colnames(boat.path) %in% col.time] <- "DATE_TIME"
  if(col.FishingOp != "FishingOperation"){ boat.path <- boat.path[, !colnames(boat.path) %in% "FishingOperation"]}
  colnames(boat.path)[ colnames(boat.path) %in% col.FishingOp] <- "FishingOperation"

  bb <- sf::st_bbox(boat.path)
  trips <- unique(sort(unlist(sf::st_set_geometry(boat.path[, trip_ID], NULL))))

  if(length(trips) > 1){stop("plot_Trip function is designed to be run on one trip only")}

  size.window <- sqrt(length(col.features)+1)

  if(floor(size.window) == size.window){
    dim.window <- rep(size.window, 2)
  }else{
    test.quadrat <- round(size.window,0) == floor(size.window)
    dim.window <- rep(floor(size.window)+1,2)
    dim.window[1] <- ifelse(test.quadrat, floor(size.window), dim.window[1])
  }

  graphics::par(mfrow = dim.window)

  for(i in 1:length(col.features)){

    plot_Feature(boat.path = boat.path,
                 col.feature = col.features[i],
                 col.FishingOp = "FishingOperation",
                 Fishing = Fishing,
                 Hauling = Hauling,
                 Setting = Setting,
                 col.time = "DATE_TIME",
                 cex.Fop = NULL,
                 Fishing.op = Fishing.op,
                 ActiveGear = ActiveGear,
                 main = NA)

  }

  plot_Paths(boat.paths = boat.path,
             z.col = z.col,
             quanti =  quanti,
             main = ifelse(is.na(main), trips, main),
             trip_ID =  trip_ID ,
             col.time = "DATE_TIME",
             FishingOp = "FishingOperation",
             path = path,
             col.pal = col.pal,
             Create.BgMap = Create.BgMap,
             rm.BgMap = rm.BgMap,
             expandBy = expandBy,
             Path2BgMap = Path2BgMap ,
             cex.FoP = cex.FoP,
             gears.set = gears.set,
             gears.hauled = gears.hauled,
             lwd.gears = lwd.gears,
             credits = credits)

  graphics::par(mfrow = c(1,1))

  }
