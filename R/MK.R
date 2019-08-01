#' @importFrom stats pnorm median

#Mann Kendall test
MK <- function(x) {
  names(x) <- NULL
  es <- function(x) {
    ln <- length(x)
    mks <- 0
    for (i in 1:(ln-1))
      for (j in (i+1):ln) mks <- mks + ifelse(is.na(x[j]-x[i]),0,sign(x[j]-x[i]))
    return(mks)
  }
  vars <- function(x) {
    ln <- length(x)
    li <- length(which(!is.na(x)))
    g  <- rep(0,ln)
    for (i in 1:ln) g[i] <- max(0,length(which(x == x[i])) - 1)
    ti <- length(which(g>0))
    return((li*(li-1)*(2*li+5)-sum(g*(g-1)*(2*g+5)))/18)
  }
  slope <- function(x) {
    sl <- NULL
    ln <- length(x)
    for (i in 1:(ln-1)) sl <- c(sl,(x[(i+1):ln]-x[i])/(c((i+1):ln)-i))
    return(median(sl,na.rm=T))
  }
  xs <- es(x)
  xv <- vars(x)
  xz <- suppressWarnings((xs>0)*(xs-1)/sqrt(xv) + (xs<0)*(xs+1)/sqrt(xv))
  p <- 1-pnorm(abs(xz)) # Signifikanzniveau
  m <- slope(x)         # Trend
  return(c(m=m,p=p))
}