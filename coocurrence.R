dat <- read.csv('data/CollarData26.csv')
df <- xyConv(dat)
plotTraj(df$x, df$y, df$ndowid)

df <- moveParams(df$x, df$y, df$timestamp, df)

df30 <- df[df$ndowid == 10330, ]
df20 <- df[df$ndowid == 10320, ]

plot3DTraj(df30$x, df30$y, df30$timestamp, df30$timestamp)
plot3DTraj(df20$x, df20$y, df20$timestamp, df20$timestamp, add = TRUE)

plot3d(df30$x, df30$y, df30$timestamp, type = 'l', col = 'springgreen')
plot3d(df20$x, df20$y, df20$timestamp, type = 'l', col = 'purple', add = T)

plot3DTraj <- function(x, y, z, colval, add = FALSE) {
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

  plot3d(x, y, z, type = 'l', col = 'darkgrey', add = add)
  plot3d(x, y, z, type = 'p', col = cols, add = T, size = 5)
  plot3d(x, y, xyPanel, type = 'l', col = 'lightgrey', add = T)


  play3d(spin3d(rpm = 3), duration = 20)
}
