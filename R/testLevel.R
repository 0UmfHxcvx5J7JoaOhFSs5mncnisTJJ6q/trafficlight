#' testLevel
#' 
#' A collection of tests to compare the level of data with comparison data
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
#' @examples
#' 
#' #create some test data
#' test <- 1:40; names(test) <- 1995:2034
#' x <- test[20:40]
#' xc <- test[1:30] + rnorm(30)
#' 
#' testLevel(x,xc)
#' 
#' @importFrom qualV GRI
#' @export 
testLevel <- function(x,xc) {
  
  x <- x[!is.na(x)]
  xc <- xc[!is.na(xc)]
  
  #determine model data reference year
  yref  <- as.integer(names(x))[1]
  #determine data comparison years (10 years around start year)
  ycomp <- as.integer(names(xc))
  ycomp <- ycomp[which(-5<=(ycomp-yref)&(ycomp-yref)<=5)]
  
  out <- NULL
  if(length(ycomp)>0) {
    #get vectors with simulated and observed values
    sim <- rep(x[as.character(yref)],length(ycomp))      
    obs <- xc[as.character(ycomp)]
    
    ##############################################################
    #Leggett Williams
    out <- c(Level.GeometricReliability=GRI(p=sim,o=obs))
    
    ###############################################################
    #MRSR
    out <- c(out, Level.ModifiedStandardDeviationRatio=MRSR(obs=obs,sim=sim,sigma=NA))
    
    #############################################3
    #relative difference
    ###############################################
    out <- c(out, Level.RelativeDifference=(mean(sim,na.rm=TRUE)-mean(obs,na.rm=TRUE))/mean(obs,na.rm=TRUE))
  } else {
    out <- c(Level.GeometricReliability=NA,Level.ModifiedStandardDeviationRatio=NA,Level.RelativeDifference=NA)
  }
  return(out)
}