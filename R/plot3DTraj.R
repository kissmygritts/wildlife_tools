#' 3D plot of animal trajectory
#'
#' @param x The x geographic coordinate to plot
#' @param y The y geographic coordinate to plot
#' @param z The z value for the z axis
#' @param colval The value you want to plot as points along the trajectory
#'
#'
#' @return A 3D plot of the the animal trajectory. Each point is connected by a
#' line. The color of the points is value of the colval argument. Purple is for low
#' values, Yellow is for high levels.
#'
#' @note A good use for the 3D plot is to construct a spacetime cube. In a spacetime cube
#' the z-axis is the timestamp of the points. There is potential to visualize seasonal use
#' and revisitation rates. It is useful to set the \code{colval} argument equal to the
#' timestamp as well to generate a color ramp for the timestamp. The Purple will be for the first
#' XY locations, Yellow for the last.
#'
#' @author Mitchell Gritts
#'
#' @importFrom rgl plot3d play3d spin3d
#'
#' @examples
#' plot3DTraj(df$X, df$Y, df$timestamp, df$timestamp)
#'
#' @export

plot3DTraj <- function(x, y, z, colval) {
  myColorRamp <- function(colors, values) {
    v <- (values - min(values)) / diff(range(values))
    x <- colorRamp(colors)(v)
    return(rgb(x[, 1], x[, 2], x[, 3], maxColorValue = 255))
  }
  if ('POSIXct' %in% class(colval)) {
    colval <- 1:length(colval)
  }
  xyPanel <- min(z, na.rm = T) - 10

  cols <- myColorRamp(c('purple', 'springgreen', 'yellow'), colval)

  plot3d(x, y, z, type = 'l', col = 'darkgrey')
  plot3d(x, y, z, type = 'p', col = cols, add = T, size = 5)
  plot3d(x, y, xyPanel, type = 'l', col = 'lightgrey', add = T)


  play3d(spin3d(rpm = 3), duration = 20)
}
