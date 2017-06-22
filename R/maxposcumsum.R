max.pos.cumsum <- function(x) {
  n <- length(x)
  i <- 1:n
  y <- cumsum(x)
  while (min(y)<0) {
    neg <- which(y<0)[1]
    pos <- which(i>neg & x>0)[1]
    if (is.na(pos)) {
      y[neg:n]       <- 0
    } else {
      y[neg:(pos-1)] <- 0
      y[pos:n]       <- cumsum(x[pos:n])
    }
  }
  return(max(y))
}
