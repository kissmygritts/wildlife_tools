#' Sample Stratified Data
#'
#' @param dat dataframe to be used in this function
#' @param colName the name of the column containing the stratified data
#' @param pctVector vector of percentages to sample the stratified column by. These should add to 1 and have the names of all the unique values in the colName
#'
#' @return a random sub sample of the original dataframe (data_table).
#'
#' @author Mitchell Gritts
#' @author Cody McKee
#'
#' @examples
#' sampleStratified(df, 'species', pctVector = c('dog' = .2, 'cat' = .6, 'rat' = .2))
#'
#'  @export

sampleStratified <- function(dat, colName = 'stratifi', pctVector = c('high' = .5, 'med' = .3, 'low' = .2)) {
  dat$idx <- 1:nrow(dat)
  r <- dat %>%
    group_by_(colName) %>%
    summarize(n = n())
  strat <- r %>% select_(colName) %>% extract2(1) %>% as.vector()

  vr <- vector()
  for (i in seq_along(strat)) {
    criteria <- interp(~x == strat[i], .values = list(x = as.name(colName)))
    stratIdx <- dat %>% filter_(criteria) %>% select(idx) %>% extract2(1) %>% as.vector()
    rdmIdx <- sample(stratIdx, ceiling(pctVector[strat[i]] * length(stratIdx)))
    vr <- c(vr, rdmIdx)
  }

  return(dat %>% filter(idx %in% vr))
}
