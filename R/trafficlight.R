#' trafficlight
#' 
#' Function to perform an automatic validation routine
#' 
#' The procedure is explained in detail here:
#' \url{https://redmine.pik-potsdam.de/projects/magpie-intern/wiki/Traffic_light_validation}
#' 
#' @param x model output which should be tested in magclass format
#' @param xc comparison data on which x should be tested (magclass format)
#' @param plot TRUE or FALSE. If TRUE, a plot with a visualization of te
#' treffic light is returned. If FALSE, a MAgPIE object with the test result is
#' returned.
#' @param ... Additional arguments provided to \code{\link{TLplot}}
#' @return If \code{plot==TRUE}, a ggplot object. If \code{plot==FALSE}, a
#' matrix with traffic light result (0 = red, 1 = yellow, 2 = green)
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @seealso
#' \code{\link{TLevaluate}},\code{\link{TLstatistics}},\code{\link{TLplot}}
#' @examples
#' library(magclass)
#' data("population_magpie")
#' x <- population_magpie
#' xc <- x + runif(length(x)[1],0,10^5)
#' 
#' trafficlight(x[,,1], xc)
#' trafficlight(x[,,1], xc, detailed=FALSE)
#' 
#' @export
trafficlight<-function(x, xc, plot=TRUE, ...){
  #Calculate the Traffic Light indicators
  indicators <- TLstatistics(x=x, xc=xc)
  #Evaluate the thresholds
  TL <- TLevaluate(indicators)
  if(!plot){
    return(TL)
  } else {
    p <- TLplot(TL,...)
    attr(p,"data") <- TL
    return(p)
  }
}
