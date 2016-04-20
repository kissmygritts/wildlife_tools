#' Estimate movement parameters from timestamped xy data
#'
#' The function will estimate common movement parameters from the given xy coordinates and
#' timestamp data. The data can be returnded by itself as a data.table or added to an
#' existing data.table.
#'
#' @param x The x component of the xy data
#' @param y The y component of the xy data
#' @param timestamp The timestamp of the xy data
#' @param dat Additional data for the returned data to be added to (optional)
#' @param format Format of the date time string to be passed to the \code{as.POSIXct}.
#' The default is \code{NULL} assuming that timestamps will be in the "standard unambiguous format",
#' i.e. YYYY-MM-DD HH:MM:SS. If the timestamp is already of class \code{POSIXct}, this isn't required.
#'
#' @return a \code{data.table} with the following columns:
#' \item{dist}{the distance between xy locations (meters)}
#' \item{nsd}{net squared displacement (meters)}
#' \item{dt}{duration between xy locations (seconds)}
#' \item{speed}{velocity between xy locations (km/hr)}
#' \item{phi, theta}{absolute and turning angle (respectively)}
#' \item{vp}{persistence velocity}
#' \item{vt}{turning velocity}
#'
#' @author Mitchell Gritts
#'
#' @importFrom fasttime fastPOSIXct
#'
#' @export
#'
#' @examples
#' df <- moveParams(df$X, df$Y, df$timestamp, dat = df, format = '%Y-%m-%d %H:%M:%S')

moveParams <- function(df, xyti = c('x', 'y', 'timestamp', 'ndowid'),
                       format = NULL) {
  df <- df[order(df[, xyti[4]], df[, xyti[3]]), ]
  x <- df[, xyti[1]]
  y <- df[, xyti[2]]
  timestamp <- df[, xyti[3]]

  if (class(timestamp) != 'POSIXct') {
    if (is.null(format)) {
      timestamp <- fasttime::fastPOSIXct(timestamp)
    } else {
      timestamp <- as.POSIXct(timestamp, format = format)
    }
  }
  df$timestamp <- timestamp

  dist <- c(0, sqrt((x[-1] - x[-length(x)])**2 +
                      (y[-1] - y[-length(y)])**2))
  nsd <- (x - x[1])**2 + (y - y[1])**2
  dt <- c(0, unclass(timestamp[-1]) - unclass(timestamp[-length(timestamp)]))
  speed <- (dist / 1000) / (dt / 3600)
  speed[1] <- 0

  z <- x + (0 +1i) * y
  phi <- c(0, Arg(diff(z)))
  theta <- c(0, diff(phi))

  vp <- speed * cos(theta)
  vt <- speed * sin(theta)

  params <- data.frame(cbind(dist, nsd, dt, speed, phi, theta, vp, vt))

  if (is.null(dat) == TRUE) {
    return(params)
  } else {
    dat$timestamp <- timestamp
    return(cbind(df, params))
  }
}
