#' iapesca package data
#'
#' @description
#' A subset of datasets from the French Fisheries Information System (FIS, https://sih.ifremer.fr/),
#' the Ifremer permanent, operational and multidisciplinary national monitoring network for the observation of marine resources and their uses.
#'
#' @format ## 'positions'
#' Geolocation information from a fishing vessel operating nets.
#' This dataset is a subset of Ifremer, RECOPESCA database for which fishing operations have been identified and validated with the fisherman.
#' This database has been anonymized: identity have been changed and spatial geolocation randomly translated.
#' A data frame with 1,937 rows and 11 columns
#' \describe{
#'   \item{VESSEL_FK}{Fishing vessel identifier, character}
#'   \item{FISHING_TRIP_FK}{Fishing trip identifier, character}
#'   \item{DATE_TIME}{timestamp, character coercible to POSIXct}
#'   \item{turn}{turning angle in radians, numeric}
#'   \item{speed}{speed in knots, numeric}
#'   \item{hdg}{heading in radians, numeric}
#'   \item{setting}{Fishing gear identifier for setting, a character string with NAs when the vessel isn't fishing}
#'   \item{hauling}{Fishing gear identifier for hauling, a character string with NAs when the vessel isn't fishing}
#'   \item{FishingOperation}{a character string describing the fishing activity for each position}
#'   \item{LONGITUDE}{longitude of the the translated position in WGS84 CRS}
#'   \item{LATITUDE}{latitude of the the translated position in WGS84 CRS}
#' }
#'
#' @source <https://sih.ifremer.fr/>
#'
#' @author Ifremer HISSEO, Marial LAURANS, François DANHIEZ, Mathieu Woillez, Julien RODRIGUEZ, \email{julien.rodriguez@ifremer.fr}
#'
"positions"

#' @rdname positions
#'
#' @format ## 'harbours'
#' Anonymized harbour table (French fleet file) from FIS database stored in Harmonie.
#' A sf object with 1 row and 7 columns
#'
#' \describe{
#'   \item{LOCATION_LABEL}{Anonymized harbour label, character}
#'   \item{LOCATION_FK}{Anonymized harbour identifier, character}
#'   \item{LOCATION_NAME}{Anonymized harbour name, character}
#'   \item{EXTERNAL_CODE}{Anonymized harbour Locode identifier, character}
#'   \item{COUNTRY_CODE}{Country code, character}
#'   \item{geometry}{object geometry, a sfc}
#' }
#'
"harbours"

#' @rdname harbours

