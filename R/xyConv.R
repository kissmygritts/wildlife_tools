#' Convert xy coordinates from one projection to another.
#'
#' Takes a dataframe with XY coordinates as input and converts the coordinates to
#' another projection system. The function defaults to converting longlat to UTM zone 11,
#' however any properly formated CRS string is acceptable for input.
#'
#' @param df A dataframe that contains x, y coordinates.
#' @param xy A vector of the names for the input x and y coordinates. Default is long_x and lat_y
#' @param CRSin The CRS string of the input coordinates, defaults to longlat
#' @param CRSout The CRS string of the output coordinates, defaults to UTM Zone 11
#' @param outclass The class of the data object to be returned. Options include \code{'data.frame'},
#' \code{'data.table'}, and \code{'spdf'}. \code{'spdf'} returns an object of class
#' \code{sp::SpatialPointsDataFrame}.
#'
#' @return A \code{data.frame} of the input data with the converted XY coordinates. 
#'  the \code{data.frame} removes rows with \code{Null} or \code{NA} values in 
#'  the XY columns.
#'
#' @author Mitchell Gritts
#'
#' @import sp
#'
#' @export
#'
#' @examples
#' df <- xyConv(dat, xy = c('geox', 'geoy'))

xyConv <- function(df, xy = c('long_x', 'lat_y'), CRSin = '+proj=longlat',
                   CRSout = '+proj=utm +zone=11') {
  df <- df[complete.cases(df[, xy]), ]
  coord <- data.frame(df[, xy])
  colnames(coord) <- c('x', 'y') 
  coord[, 1] <- as.numeric(coord[, 1])
  coord[, 2] <- as.numeric(coord[, 2])
  conv <- SpatialPoints(coordinates(coord),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  return(df)
}

