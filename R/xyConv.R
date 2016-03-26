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
#' @return the original \code{data.table}, \code{data.frame} or \code{SpatialPointsDataFrame}
#'  with converted XY coordinates added to the object.
#'
#' @author Mitchell Gritts
#'
#' @import data.table
#' @import sp
#'
#' @export
#'
#' @examples
#' df <- xyConv(dat, xy = c('geox', 'geoy'))

xyConv <- function(df, xy = c('long_x', 'lat_y'), CRSin = '+proj=longlat', CRSout = '+proj=utm +zone=11',
                    outclass = 'data.table') {
  if (class(df)[1] == 'data.frame') {
    df <- as.data.table(df)
  }

  df <- na.omit(df, cols = xy)
  df <- df[order(timestamp)]
  conv <- SpatialPoints(cbind('X' = as.numeric(df[[xy[1]]]),
                              'Y' = as.numeric(df[[xy[2]]])),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  df <- cbind(df, as.data.frame(conv))

  if (outclass == 'data.frame') {
    df <- as.data.frame(df)
  } else if (outclass == 'spdf') {
    df <- SpatialPointsDataFrame(conv, df, proj4string = conv@proj4string@projargs)
  }
  return(df)
}
