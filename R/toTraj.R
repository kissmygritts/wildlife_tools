toTraj <- function(dat, fields = c('X', 'Y', 'timestamp', 'ndowid')) {
  traj <- adehabitatLT::as.ltraj(dat[, fields[1:2]], date = dat[, fields[3]],
                                 id = dat[, fields[4]])
  return(traj)
}
