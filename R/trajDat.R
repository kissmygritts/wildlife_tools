#' A data.frame of 2 mule deer
#'
#' A data.frame of two mule deer movements.
#'
#' @format A data.table with 2121 rows and 9 variables:
#' \describe{
#'    \item{locid}{location ID for the coordinate}
#'    \item{ndowid}{NDOW ID for the animal}
#'    \item{timestamp}{chr string of the timestamp}
#'    \item{long_x}{longitude of the location (WGS 84)}
#'    \item{lat_y}{latitude of the location (WGS 84)}
#'    \item{hdop}{horizontal displacement}
#'    \item{species}{species of the animal}
#'    \item{mgmtarea}{management area of the animal}
#'    \item{deviceid}{gps collar device ID}
#'    }
#'
"trajDat"
