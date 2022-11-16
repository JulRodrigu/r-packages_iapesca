#' plot_Paths
#'
#' \code{plot_Paths} plots boat paths described by a quantitative variable and optionally by a qualitative variable or the fishing operations.
#'
#' @param boat.paths a spatial points object of class sf
#' @param z.col DEFAULT = NULL, print the fishing trip paths by a qualitative variable, eg the gear
#' @param quanti DEFAULT = "SPEED.kn", quantitative variable to plot, can be set to NULL
#' @param main DEFAULT = NA, The graphic's title. Is also used to define the path where to save the background map
#' @param trip_ID DEFAULT = "FISHING_TRIP_FK", the name of the column identifying the fishing trips.
#' @param col.time DEFAULT = "DATE_TIME", name of the timestamp column coercible to POSIXct, time zone must be set to UTC
#' @param FishingOp DEFAULT = NULL, the name of the column identifying the fishing operations.
#' @param path DEFAULT = getwd(), path where to save the background map created
#' @param col.pal DEFAULT = NULL, the color palette to print the quantitative variable, if null set a palette from grDevices::rainbow
#' @param Create.BgMap DEFAULT = TRUE, if set to TRUE, download a background map using PathStudyBoundaries
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
#'
#' plot_Paths(boat.paths = df2sfp(positions), quanti = "speed", Create.BgMap = FALSE)
#'
#' plot_Paths(boat.paths = df2sfp(positions[positions$FISHING_TRIP_FK %in% "18529291", ]),
#'            quanti = "speed", FishingOp = "FishingOperation", Create.BgMap = FALSE, cex.FoP = 0.9,
#'            main = "FT_18529291")
#'
#' @export
#'

plot_Paths <- function(boat.paths,
                       z.col = NULL,
                       quanti =  "SPEED.kn",
                       main = NA,
                       trip_ID = "FISHING_TRIP_FK",
                       col.time = "DATE_TIME",
                       FishingOp = NULL,
                       path = getwd(),
                       col.pal = NULL,
                       Create.BgMap = TRUE,
                       rm.BgMap = FALSE,
                       expandBy = 0.2,
                       Path2BgMap = NULL,
                       cex.FoP = NULL,
                       gears.set = NULL,
                       gears.hauled = NULL,
                       lwd.gears = c(3,3),
                       credits = "Ifremer, HISSEO, RECOPESCA program"
                       ){

  if(!inherits( boat.paths, "sf")) {
    stop("boat.paths must be a valid sf object")
  }

  if(!col.time %in% colnames(boat.paths)) {stop("Time column not found")}
  if(!is.null(quanti)){
    if(!quanti %in% colnames(boat.paths)) {stop("quanti column not found")}
  }
  if(!trip_ID %in% colnames(boat.paths)) {stop("Fishing trip ID column not found")}
  if(is.na(sf::st_crs(boat.paths))) {stop("CRS must be defined for proper distance calculation")}

  trips <- unique(sort(unlist(sf::st_set_geometry(boat.paths[, trip_ID], NULL))))
  colnames(boat.paths)[ colnames(boat.paths) %in% col.time] <- "DATE_TIME"

  # Make map bbox
  bb <- sf::st_bbox(boat.paths)
  bbox <- SfBbox(boat.paths)
  diag.length <- as.numeric(sf::st_distance( sf::st_transform(coord2sf(bb[c(1,2)], epsg = 4326), sf::st_crs(3857)),
                                             sf::st_transform(coord2sf(bb[c(3,4)], epsg = 4326), sf::st_crs(3857))))

  if(!is.null(z.col)){
    col2plot <- z.col
  }else{
    col2plot <- "Trip"
  }

  if(!is.null(col.pal)){
    lev.fac <- unique(sf::st_set_geometry(boat.paths[, col2plot], NULL))
    col.pal <- grDevices::rainbow( n = length(lev.fac), start = 0, end =1)
  }
  if(!is.null(quanti)){
    nr <- nrow(boat.paths)
    col.quanti <- rev(grDevices::rainbow( n = ifelse(nr > 10, 10,nr), start = 0, end = 4/6))
  }

  if(Create.BgMap){

    if(!is.null(Path2BgMap)){
      Path2File <- Path2BgMap
    }else{
      Path2File <- sprintf("%s/BgMap%s.rds", path, ifelse(is.na(main), "", paste0("_", main)))
    }

    if(file.exists(Path2File)){

      bg.map <- readRDS(Path2File)

    }else{

      bg.map <- PathStudyBoundaries(boat.paths,
                                    CoastLine = TRUE,
                                    FineCountryLimits = diag.length < 100000,
                                    buffer = diag.length/(1/expandBy))
      saveRDS(bg.map, Path2File)

      if(rm.BgMap){ unlink(Path2File, recursive = TRUE)}
    }

    if(nrow(bg.map) == 0){ Create.BgMap <- FALSE}

  }

  if(!Create.BgMap){

    bg.map <-  suppressWarnings(sf::st_boundary(sf::st_buffer(bbox, dist = diag.length/(1/expandBy))))

  }

  trajectories <- do.call( rbind, lapply(1:length(trips), function(k) {

    boat.path <- boat.paths[ unlist(sf::st_set_geometry(boat.paths[, colnames(boat.paths) %in% trip_ID], NULL)) %in% trips[k], ]
    colnames(boat.path)[colnames(boat.path) %in% col.time] <- "DATE_TIME"
    timestamp <- Char2Time(boat.path$DATE_TIME)
    boat.path <- boat.path [order(timestamp), ]
    traj <- sf::st_make_valid(Pos2Path(boat.path, val = data.frame( Trip = k) , epsg = 4326 ))
    return(traj)

  }))

  if(!is.null(z.col)){
    trajectories$Trip <- trips
    trajectories <- merge(trajectories,
                          dplyr::distinct(sf::st_set_geometry(boat.paths[, c(trip_ID, z.col)], NULL)),
                          by.x = "Trip",
                          by.y = trip_ID
    )
  }

  plot(sf::st_geometry(bg.map), axes = TRUE,
       lwd = 1.5, col = "darkgray", xlim = bb[c(1,3)], ylim = bb[c(2,4)]
  )
  if(!is.null(z.col)){
    mapsf::mf_map(trajectories[, z.col], type = "typo", add = TRUE, var = col2plot, leg_pos = "left")
  }else{
    plot(sf::st_geometry(trajectories), add = TRUE,
         lwd = 1, lty = 2)
  }

  gears.available <- any(ifelse(!is.null(gears.hauled), inherits(gears.hauled, "sf"), FALSE),
                         ifelse(!is.null(gears.set), inherits(gears.set, "sf"), FALSE))

  if(gears.available){

    if(!is.null(gears.set)){
      mapsf::mf_map(gears.set, type = "base", leg_pos = "right", col = "blue", lwd  = lwd.gears[1], add = TRUE)
    }
    if(!is.null(gears.hauled)){
      mapsf::mf_map(gears.hauled, type = "base", leg_pos = "right", col = "tomato4", lwd  = lwd.gears[2], add = TRUE)
    }

    mapsf::mf_legend(type = "typo", pos = "bottom", val = c( "hauling", "setting"), pal = c("tomato4", "blue"), border = "white", title = "Gears", cex = 0.5)

  }

  if(!is.null(quanti)){
    mapsf::mf_map(boat.paths, type = "choro", nbreaks = 10, add = TRUE, var = quanti, leg_pos = "right", pal = col.quanti, cex = 0.7)
  }

  mapsf::mf_layout(title = ifelse(is.na(main), "", main),
                   credits = paste0("Sources: ", credits, ", ", substr(Sys.Date(),1,4)), scale = FALSE)



  if(!is.null(FishingOp) & !gears.available){

    nnai <- !is.na(sf::st_set_geometry(boat.paths[, FishingOp], NULL))
    if(sum(nnai) > 0){
      if(is.null(cex.FoP)){ cex.FoP <- ifelse(any(!nnai), 1.2, 0.7)}
      mapsf::mf_map(boat.paths[nnai, ], type = "typo", add = TRUE, var = FishingOp, leg_pos = "bottomleft", pch = "+", cex = cex.FoP, leg_val_cex = 0.7)
    }
  }



  }
