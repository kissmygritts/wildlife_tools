#' Plot an animal trajectory
#'
#' @param x The x geographic coordinate to plot
#' @param y the y geographic coordinate to plot
#'
#' @return A plot of the the animal trajectory. Each point is connected by a
#' line. The start of the trajectory is shown as the green circle, the end
#' as the red circle.
#'
#' @author Mitchell Gritts
#'
#' @examples
#' plotTraj(df$X, df$Y)
#'
#' @export

plotTraj <- function(x, y) {
  plot(x, y, asp = 1, type = 'o', pch = 19, cex = .5, col = rgb(0, 0, 0, .2))
  points(x[1], y[1], col = 'green', pch = 19, cex = 1.25)
  points(x[length(x)], y[length(y)], col = 'red', pch = 19, cex = 1.25)
}
