#' Calc_NetsThresholds
#'
#' \code{Calc_NetsThresholds} Calculates plausible thresholds for speed, length and heading and, if available, soaking time.
#'
#' @param ls.Nets object x$Nets, output of Create_NetsByBoat function, a list of spatial linestring objects of class sf, each of them being the output of Create_Nets function applied to the fishing trips
#' or Nets, a spatial linestring object of class sf, output of Create_Nets
#' @param qt DEFAULT = NULL, value for the quantile of Gaussian distribution, if set to NULL it will be automatically defined from a design using Set_NetThresholds
#' @param lim.abs.skew DEFAULT = 0.05, absolute value of skewness, argument of CleanSkewness function
#' @param iter DEFAULT = NULL, number of iterations of CalcThresholds function, if set to NULL it will be automatically defined from a design using Set_NetThresholds
#' @param verbose DEFAULT = FALSE, if TRUE, prints messages.
#'
#' @return comb.test, a data-frame with 1 line and 2 columns named "qt" and "iter"
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#'
#' @examples
#'
#' run.example <- FALSE
#'
#' if(run.example){
#'
#'   require(mapview)
#'   data(positions)
#'
#'   pos <- df2sfp(positions, coords = c("LONGITUDE", "LATITUDE"))
#'
#'   # Add erroneous fishing operations values for testing the automated consolidation process
#'   pos$FishingOperation[which(pos$FISHING_TRIP_FK %in% "18483452")][97:103] <-  "NotFishing"
#'   pos$FishingOperation[which(pos$FISHING_TRIP_FK %in% "18483452")][6:9] <- "Hauling"
#'
#'   Nets.list <- Create_NetsByBoat(traj = pos, Use.BehaviourChanges = TRUE)
#'   summary(do.call(rbind, Nets.list$Nets))
#'
#'   set.seed(221107)
#'
#'   Nets.stats.auto <- Calc_NetsThresholds(ls.Nets = Nets.list$Nets, verbose = TRUE)
#'   Nets.stats.auto
#'   Nets.stats <- Calc_NetsThresholds(ls.Nets = Nets.list$Nets, qt = 0.8, iter = 1)
#'   Nets.stats
#'
#' }
#'
#' @export
#'
Calc_NetsThresholds <- function(ls.Nets,
                                qt = NULL,
                                lim.abs.skew = 0.05,
                                iter = NULL,
                                verbose = FALSE){

  if(!(inherits(ls.Nets, "list")|inherits(ls.Nets, "sf"))) stop("ls.Nets is a list or sf object, output of Create_NetsByBoat or Retrieve_SettingOperations functions")

  if(inherits(ls.Nets, "list")){

    if(!all(unlist(lapply(ls.Nets, function(x){inherits(x, "sf") & inherits(sf::st_geometry(x), "sfc_LINESTRING")})))) stop("Invalid ls.Nets format, output of Create_NetsByBoat function")
    if(!all(unlist(lapply(ls.Nets, function(x){all(c("length", "heading", "speed") %in% colnames(x))})))) stop("Invalid ls.Nets format, output of Create_NetsByBoat function")

    Nets <- do.call(rbind, ls.Nets)

  }else{

    if(!inherits(sf::st_geometry(ls.Nets), "sfc_LINESTRING")) stop("Invalid ls.Nets format, output of Create_NetsByBoat function")
    Nets <- ls.Nets

  }

  n.vals <- ifelse(is.null(Nets), 0, nrow(Nets))

  if(n.vals > 10){

  if(any(is.null(qt), is.null(iter))){

      now <- Sys.time()

      threshs.ls <- do.call(rbind, lapply(1:30, function(k){
        set.seed(now + k)
        Set_NetThresholds(ls.Nets = Nets)
      }))

      threshs <- threshs.ls[1, ]
      threshs[c(1,2)] <- c(round(stats::median(CleanSpuriousValues(threshs.ls$qt), na.rm = TRUE), digits = 4),
                           round(mean(CleanSpuriousValues(threshs.ls$iter), na.rm = TRUE), digits = 0))

    qt <- ifelse(is.null(qt), threshs$qt, qt)
    iter <- ifelse(is.null(iter), threshs$iter, iter)
    if(verbose){message(sprintf("Nets thresholds set to qt = %s, iter = %s", qt, iter))}
  }

  mult.sd <- stats::qnorm(qt + (1-qt)/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

  LimVal.Sp <- c(NA, NA)
  LimVal.Lg <- c(NA, NA)


    LimVal.Sp <- CalcThresholds(Nets$speed, GaussianQt = mult.sd, lim.abs.skew = lim.abs.skew)

    if(iter > 1){
      for(i in 2:iter){
        new.vals <- Nets$speed
        new.vals[ new.vals < LimVal.Sp[1] | new.vals > LimVal.Sp[2]] <- NA
        LimVal.Sp <- CalcThresholds(new.vals, GaussianQt = mult.sd, lim.abs.skew = lim.abs.skew)
      }
    }

    laps.10 <- mean(LimVal.Sp)/10
    LimVal.Sp <- LimVal.Sp + laps.10*c(-1,1)

    LimVal.Lg <- CalcThresholds(as.numeric(Nets$length), GaussianQt = mult.sd, lim.abs.skew = lim.abs.skew)

    if(iter > 1){
      for(i in 2:iter){
        new.vals <- as.numeric(Nets$length)
        new.vals[ new.vals < LimVal.Lg[1] | new.vals > LimVal.Lg[2]] <- NA
        LimVal.Lg <- CalcThresholds(new.vals, GaussianQt = mult.sd, lim.abs.skew = lim.abs.skew)
      }
    }

      y.alternatif <- suppressWarnings(try(CleanSpuriousValues(vals = as.numeric(Nets$length),
                                                               lim.abs.skew = lim.abs.skew,
                                                               max.it = floor(n.vals/10),
                                                               remove.first.distrib = TRUE
                                                               ), silent = TRUE))

      if(inherits(y.alternatif, "try-error")){
        new.vals <- as.numeric(Nets$length)
        new.vals[ new.vals < LimVal.Lg[1] | new.vals > LimVal.Lg[2]] <- NA
      }

      mn.alternatif <- mean(y.alternatif, na.rm = TRUE)

      if( (mn.alternatif > LimVal.Lg[2]) | (mn.alternatif < LimVal.Lg[1])){

        y <- y.alternatif
        mn.y <- mean(y, na.rm = TRUE)
        sd.y <- stats::sd(y, na.rm = TRUE)
        LimVal.Lg <- mn.y + sd.y*c(-1,1)*mult.sd

      }

    heading <- Nets$heading
    clust.heading <-  Cluster_Distributions(heading)

    tab.clust <- table(clust.heading$Clust)

    LimVal.Hdg <- lapply(names(tab.clust), function(kk){
        hdg.k <- clust.heading$Vals[ clust.heading$Clust %in% kk]
        rg <- suppressMessages(CalcThresholds(hdg.k, GaussianQt = mult.sd, lim.abs.skew = lim.abs.skew*2))
        rg[ rg < 0] <- 0
        rg[rg > 360] <- 360
        return(rg)
      })

    test.nas <- !unlist(lapply(LimVal.Hdg , anyNA))
    LimVal.Hdg  <- do.call(rbind, LimVal.Hdg [test.nas])
    if(nrow(LimVal.Hdg) > 3){
      LimVal.Hdg  <- matrix(c(0, 360), nrow = 1)
    }
    if(nrow(LimVal.Hdg) > 2 & !any(LimVal.Hdg %in% c(0,360))){
      laps.10 <- mean(apply(LimVal.Hdg, 1, function(x){ diff(x)/10}))
      LimVal.Hdg[,1]  <- LimVal.Hdg[,1]-laps.10
      LimVal.Hdg[,2]  <- LimVal.Hdg[,2]+laps.10
      LimVal.Hdg[LimVal.Hdg[,1] < 0, 1] <- 0
      LimVal.Hdg[LimVal.Hdg[,2] > 360, 2] <- 360
    }

    LimVal.Sp[1] <- ifelse(LimVal.Sp[1] < min(Nets$speed, na.rm = TRUE), min(Nets$speed, na.rm = TRUE), LimVal.Sp[1])
    LimVal.Sp[2] <- ifelse(LimVal.Sp[2] > max(Nets$speed, na.rm = TRUE), max(Nets$speed, na.rm = TRUE), LimVal.Sp[2])
    LimVal.Lg[1] <- ifelse(LimVal.Lg[1] < min(as.numeric(Nets$length), na.rm = TRUE), min(as.numeric(Nets$length), na.rm = TRUE), LimVal.Lg[1])

    LimVal.Sp[ LimVal.Sp < 0] <- 0
    LimVal.Lg [ LimVal.Lg < 0] <- 0

    if( "soaking.time.hours" %in% colnames(Nets)){

      LimVal.Skh <- CalcThresholds(as.numeric(Nets$soaking.time.hours), GaussianQt = mult.sd, lim.abs.skew = lim.abs.skew)
      LimVal.Skh[1] <- ifelse(LimVal.Skh[1] < min(as.numeric(Nets$soaking.time.hours), na.rm = TRUE), min(as.numeric(Nets$soaking.time.hours), na.rm = TRUE), LimVal.Skh[1])
      LimVal.Skh[2] <- ifelse(LimVal.Skh[2] > max(as.numeric(Nets$soaking.time.hours), na.rm = TRUE), max(as.numeric(Nets$soaking.time.hours), na.rm = TRUE), LimVal.Skh[2])
      LimVal.Skh[ LimVal.Skh < 0] <- 0

    }else{

      LimVal.Skh <- NULL

    }

    }else{

    LimVal.Sp <- c(NA, NA)
    LimVal.Lg <- c(NA, NA)
    LimVal.Hdg <- c(NA, NA)
    LimVal.Skh <- c(NA, NA)

  }

  return(Nets.Thresholds = list(LimVal.Speed = LimVal.Sp,
                                LimVal.Length = LimVal.Lg ,
                                LimVal.Heading = LimVal.Hdg,
                                LimVal.SoakTime = LimVal.Skh
                                ))

}

