#' Estimate movement parameters from timestamped xy data
#'
#' The function will estimate common movement parameters from the given xy coordinates and
#' timestamp data. The data can be returnded by itself as a data.table or added to an
#' existing data.table.
#'
#' @param x the x component of the xy data
#' @param y the y component of the xy data
#' @param timestamp the timestamp of the xy data
#' @param dat additional data for the returned data to be added to (optional)
#' @param isPOSIXct whether or not the timestamp data is of class \code{POSIXct}
#' the default value is \code{TRUE}
#'
#' @return a \code{data.table} with the following columns:
#' \item{dist}{the distance between xy locations (meters)}
#' \item{nsd}{net squared displacement (meters)}
#' \item{dt}{duration between xy locations (seconds)}
#' \item{speed}{velocity between xy locations (km/hr)}
#' \item{phi, theta}{absolute and turning angle (respectively)}
#' \item{vp}{persistence velocity}
#' \item{vt}{turning velocity}

moveParams <- function(x, y, timestamp, dat = NULL, isPOSIXct = TRUE) {
  if (isPOSIXct == FALSE) {
    timestamp <- fasttime::fastPOSIXct(timestamp)
  }
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
    return(cbind(dat, params))
  }
}
