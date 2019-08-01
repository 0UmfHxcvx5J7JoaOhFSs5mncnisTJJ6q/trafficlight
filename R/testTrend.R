#' testTrend
#' 
#' A collection of tests to compare the trend of data with comparison data
#' 
#' Details about the employed tests can be found here:
#' \url{http://redmine.pik-potsdam.de/projects/x-intern/wiki/Traffic_light_validation}
#' 
#' @param x a data vector of a single data set named and sorted by years
#' @param xc a data vector of a single data set named and sorted by years
#' @param t window size in years for which the trend should be tested. It is 
#' tried to create a window -t/2 to t/2 around the start year for trend testing.
#' If this does not work the window can be moved in the range of -t to t around
#' the start year to find comparison data. Comparison data is shifted from the given
#' window to start year to start year + t for analysis
#' @return A named vector containing results of multiple tests
#' for the single combination of data and comparison data.
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @seealso
#' \code{\link{TLstatistics}},\code{\link{trafficlight}}
#' @importFrom magclass as.magpie as.array time_interpolate
#' @examples
#' 
#' #create some test data
#' test <- 1:80; names(test) <- 1995:2074
#' x <- test
#' xc <- test + rnorm(30)
#' names(xc) <- 1925:2004
#' 
#' testOverlap(x,xc)
#' 
#' @export 

testTrend <- function(x, xc, t=40) {
  
  # in Markus original work there was a distinction in treatment
  # for historic data and projections in which treatment for projections
  # was basically identical with overlapping tests except that only 40
  # years were taken into consideration.
  # for historic data comparison data was shifted to the first 40 years of simulation
  # and a overlapping test was performed afterwards. For the moment this function
  # just sticks to the approach for historic data but that should be thought through 
  # again at some point!
  
  out <- c("Trend.MannKendall m_annual"=NA, "Trend.MannKendall p"=NA)
  
  
  x <- x[!is.na(x)]
  xc <- xc[!is.na(xc)]
  
  if(length(x) < 2 | length(xc) < 2) return(out)
  
  yx <- as.integer(names(x))  
  yc <- as.integer(names(xc))
  # only consider years close to start year of x. In the best case t/2 before to t/2 after start year.
  # if this is not available, shift the window to max t before or t after to get a t-size window
  # (or a windows close to t)
  yc <- yc[abs(yc-yx[1]) < t]
  range <- max(yc)-min(yc)
  if(range > t) {
    #reduce window to t
    if(max(yc) - yx[1] <= t/2) {
      yc <- yc[yc >= (max(yc) - t)]
    } else if(yx[1] - min(yc) <= t/2){
      yc <- yc[yc <= (min(yc) + t)]
    } else {
      yc <- yc[yc <= yx[1] + t/2 & yc >= yx[1] - t/2]
    }
  }
  
  if(length(yc) >= 2){
    #construct artificial data timeseries starting at x start point
    overlap <- yc - yc[1] + yx[1]
    i_data <- xc[as.character(yc)]
    names(i_data) <- overlap
    
    overlap <- fillequidistant(overlap)
    
    i_data <- interpoldata(i_data, overlap)
    i_x    <- interpoldata(x, overlap)
    
    dist <- unique(diff(overlap))
    if(length(dist)>1) stop("Data is not equidistant even though fillequidistant has been executed before. Something went wrong here!")
    
    #Correct the data trend to x start value
    i_data <- i_data - i_data[1] + i_x[1]

    #Difference between timeseries
    tmp <- i_x - i_data  
    #Divide by mean value
    tmp <- tmp/mean(i_data)
    tmp <- MK(tmp)
    out["Trend.MannKendall m_annual"] <-  tmp["m"]/dist
    out["Trend.MannKendall p"]        <-  tmp["p"]
  }
  return(out)
}

