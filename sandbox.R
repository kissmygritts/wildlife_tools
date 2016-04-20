# SANDBOX
source('R/ggTraj.R')
source('R/moveParams.R')
source('R/xyConv.R')
library(rNDOW)
dat <- data("trajDat")
library(ggplot2)
library(sp)

dat_conv <- xyConv(trajDat)
dat_move <- moveParams(dat_conv)


ggplot(dat_move, aes(x, y, color = as.factor(ndowid))) +
  geom_point(alpha = .2) +
  geom_path(aes(group = ndowid), alpha = .5) +
  ggthemes::scale_color_gdocs() +
  facet_wrap(~ndowid) +
  coord_equal() +
  theme_void()


ggTraj <- function(df, x, y, fid) {
  df[, fid] <- as.factor(df[, fid])

  ggplot(df, aes_q(x = ~x, y = ~y, color = ~df[, fid])) +
    geom_point() +
    coord_equal() +
    theme_void() +
    ggthemes::scale_color_gdocs() +
    facet_wrap(~ndowid)
}

ggTraj(dat_move, 'long_x', 'lat_y', 'ndowid')

ggplot(dat_move, aes(x = x, y = y, color = factor(ndowid))) + geom_point()

ggTraj <- function(df, geox, geoy, nid) {
  df[, nid] <- factor(df[, nid])
  ggplot(df, aes_string(x = geox, y = geoy, color = nid)) +
    geom_point() +
    theme_void() +
    ggthemes::scale_color_gdocs()
}

ggTraj(dat_move, 'x', 'y', 'ndowid')

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

trj <- ggTraj(dat_move, c('x', 'y', 'ndowid'))
trj + facet_wrap(~ndowid)
