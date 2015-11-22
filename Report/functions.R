## Function to split the rr-values
getSplitFactor <- function(x, init, n.obs = length(x), n.fctr, n.ovrlap){
  getFactor <- list()
  i <- 1
  while (init + n.fctr < n.obs) {
    getFactor[[i]] <- seq(init, length.out = n.fctr)
    init <- init + n.ovrlap
    i <- i + 1
  }
  return(getFactor)
}