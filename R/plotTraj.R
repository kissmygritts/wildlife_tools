#' Plot an animal trajectory
#'
#' @param x The x geographic coordinate to plot
#' @param y the y geographic coordinate to plot
#' @param id The column of the individual IDs.
#'
#' @return A plot of the animal trajectory. Each point is connected by a
#' line. The start of the trajectory is shown as the green circle, the end
#' as the red circle.
#'
#' @author Mitchell Gritts
#'
#' @examples
#' plotTraj(df$X, df$Y, df$ndowid)
#'
#' @export

plotTraj <- function(x, y, id) {
  df <- data.frame('x' = x, 'y' = y, 'id' = id)
  unq <- unique(df$id)
  print(head(df))
  xmin <- min(x) + 20
  xmax <- max(x) + 20
  ymin <- min(y) + 20
  ymax <- max(y) + 20

  plot(c(xmin, xmax), c(ymin, ymax), pch = 19, col = NA, asp = 1)

  for (i in seq_along(unq)) {
    d <- df[df$id == unq[i], ]
    print(unq[i])
    lines(d$x, d$y, type = 'o', pch = 19, cex = .5, col = rgb(0, 0, 0, .2))
    points(d$x[1], d$y[1], col = 'green', pch = 19)
    points(d$x[length(d$x)], d$y[length(d$y)], col = 'red', pch = 19)
  }
}
