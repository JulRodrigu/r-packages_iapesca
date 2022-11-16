#' Char2Time
#'
#' \code{Char2Time} Convert a character string to valid date-time format in the desired time zone
#'
#' @param DateTime a character vector coercible to "POSIXct" class
#' @param input.tz DEFAULT "UTC", time zone name for the DateTime object. See OlsonNames().
#' @param output.tz DEFAULT "UTC", desired time zone name for the output. See OlsonNames().
#'
#' @return a vector of class POSIXct
#'
#' @author Julien Rodriguez, \email{julien.rodriguez@ifremer.fr}
#'
#' @examples
#'
#' now <- Sys.time()
#' now
#'
#' # Warning: default input and output tz are defined as UTC!
#' Char2Time(now)
#' Char2Time(as.character(now))
#'
#' # Correctly define the input time zone using input.tz argument
#' Char2Time(now, input.tz = Sys.timezone())
#'
#' # Eventually set the output.tz argument to be defined in another time zone instead of UTC
#' Char2Time(now, input.tz = Sys.timezone(), output.tz = Sys.timezone())
#' Char2Time(now, input.tz = Sys.timezone(), output.tz =  "America/Martinique")
#'
#'
#'
#' @export
#'

Char2Time <- function(DateTime, input.tz = "UTC", output.tz = "UTC"){

  timestamp <- as.POSIXct(strptime(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = input.tz))
  if(input.tz != output.tz){
    timestamp <- lubridate::as_datetime(timestamp, tz = output.tz)
  }
  return(timestamp)

}

