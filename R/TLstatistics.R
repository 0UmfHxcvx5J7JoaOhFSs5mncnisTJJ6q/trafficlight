#' TLstatistics
#' 
#' Calculates several statistical measures to compare model results with data.
#' 
#' Details about the employed tests can be found here:
#' \url{https://redmine.pik-potsdam.de/projects/magpie-intern/wiki/Traffic_light_validation}
#' 
#' @param x model output which should be tested in magclass format. Only a single element in data dimension is allowed
#' @param xc comparison data on which x should be tested (magclass format)
#' @return A matrix containing results of multiple tests
#' for all available comparison datasets.
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @seealso
#' \code{\link{TLevaluate}},\code{\link{trafficlight}},\code{\link{TLplot}}
#' @examples
#' 
#' library(magclass)
#' data("population_magpie")
#' x <- population_magpie
#' xc <- x + runif(length(x)[1],0,10^5)
#' TLstatistics(x[,,1],xc)
#' 
#' @importFrom magclass as.array getRegions getRegions<- mbind getNames
#' @export 
TLstatistics <- function(x,xc) {
  if(dim(x)[3]!=1) {
    tl <- NULL
    for(i in 1:dim(x)[3]) {
      tmp <- TLstatistics(x[,,i],xc)
      if(ncol(tmp)==1) {
        colnames(tmp) <- getNames(x)[i]
      } else {
        colnames(tmp) <- paste(colnames(tmp),getNames(x)[i],sep=".")
      }
      tl <- cbind(tl,tmp)
    }
    return(tl)
  }  
  dimnames(x)[[3]] <- "data"
  dimnames(xc)[[3]] <- paste0("data",1:dim(xc)[3]) 

  if(!all(getRegions(x)%in%getRegions(xc))) {
    missing_regions <- getRegions(x)[!(getRegions(x)%in%getRegions(xc))]
    dummy <- xc[rep(1,length(missing_regions)),,]
    dummy[,,] <- NA
    getRegions(dummy) <- missing_regions
    xc <- mbind(xc,dummy)
  }
  
  xc <- xc[getRegions(x),,]
  
  x <- as.array(x)  
  xc <- as.array(xc)  
  
  dimnames(x)[[2]]  <- substr(dimnames(x)[[2]],2,5)
  dimnames(xc)[[2]] <- substr(dimnames(xc)[[2]],2,5)
  
  tmpfunc <- function(i,x,xc) {
    a <- x[i,,]
    b <- xc[i,,]
    return(suppressWarnings(c(testLevel(a,b),
             testOverlap(a,b),
             testTrend(a,b))))
  }

  out <- NULL
  for(i in 1:dim(xc)[3]) {
    out <- rbind(out, sapply(dimnames(x)[[1]],tmpfunc,x,xc[,,i,drop=FALSE]))
  }
  dimnames(out)[[1]] <- paste0(rep(paste0("data",1:dim(xc)[3]),each=dim(out)[1]/dim(xc)[3]),".",dimnames(out)[[1]])
  out[is.nan(out)] <- NA
  return(out)
}

