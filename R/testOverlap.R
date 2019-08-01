#' testOverlap
#' 
#' A collection of tests to compare the overlap of data with comparison data
#' 
#' Details about the employed tests can be found here:
#' \url{http://redmine.pik-potsdam.de/projects/x-intern/wiki/Traffic_light_validation}
#' 
#' @param x a data vector of a single data set named and sorted by years
#' @param xc a data vector of a single data set named and sorted by years
#' @return A named vector containing results of multiple tests
#' for the single combination of data and comparison data.
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @seealso
#' \code{\link{TLstatistics}},\code{\link{trafficlight}}
#' @importFrom magclass as.magpie as.array time_interpolate
#' @examples
#' 
#' #create some test data
#' test <- 1:40; names(test) <- 1995:2034
#' x <- test[20:40]
#' xc <- test[1:30] + rnorm(30)
#' 
#' testOverlap(x,xc)
#' 
#' @export 
testOverlap <- function(x,xc) {
  
  out <- c("Overlap.MannKendall m_annual"=NA, "Overlap.MannKendall p"=NA)
  
  x <- x[!is.na(x)]
  xc <- xc[!is.na(xc)]
  
  if(length(x) < 2 | length(xc) < 2) return(out)
  
  yx <- as.integer(names(x))
  yc <- as.integer(names(xc))

  #determine overlapping range  
  minx <- max(c(min(yc), min(yx)))
  maxx <- min(c(max(yc), max(yx)))
  overlap <- yc[yc>=minx & yc<=maxx]
  
  if(length(overlap) >= 2){
    #Do the interpolation
    overlap <- fillequidistant(overlap)
    i_data <- interpoldata(xc, overlap)
    i_x    <- interpoldata(x, overlap)
    
    dist <- unique(diff(overlap))
    if(length(dist)>1) stop("Data is not equidistant even though fillequidistant has been executed before. Something went wrong here!")
    
    #Correct the data trend to x start value
    i_data <- i_data - i_data[1] + i_x[1]
    ###############################################

    #Difference between timeseries
    tmp <- i_x - i_data  
    #Divide by mean value
    tmp<-tmp/mean(i_data)
    tmp<-MK(tmp)
    out["Overlap.MannKendall m_annual"] <-  tmp["m"]/dist
    out["Overlap.MannKendall p"]        <-  tmp["p"]
  }
  return(out)
}