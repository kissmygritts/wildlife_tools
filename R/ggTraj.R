#' Plot animal trajectories (ggplot)
#'
#' @param df The dataframe of the animals GPS points
#' @param xyi A character vector of the x, y coordinates and animal ID. Default
#' values are c('x', 'y', 'ndowid')
#'
#' @return A ggplot of the animal trajectory. The plot has basic styling applied for
#' consistency. The plot can be saved to a variable and styling can be added with ggplot2 functions.
#'
#' @author Mitchell Gritts
#'
#' @examples
#' ggTraj(df, c('x', 'y', 'ndowid'))
#'
#' @export
#'
#' @import ggplot2

ggTraj <- function(df, xyi = c('x', 'y', 'ndowid')) {
  x <- xyi[1]
  y <- xyi[2]
  id <- xyi[3]

  df[, id] <- factor(df[, id])
  ggplot(df, aes_string(x = x, y = y, color = id)) +
    geom_point(alpha = .4) +
    geom_path(aes_string(group = id), alpha = .3) +
    theme_void() +
    ggthemes::scale_color_gdocs() +
    coord_equal()
}
