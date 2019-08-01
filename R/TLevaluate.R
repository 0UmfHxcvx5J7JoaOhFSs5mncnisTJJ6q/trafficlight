#' TLevaluate
#' 
#' Function to evaluate statistical tests concerning model-data compatibility
#' with respect to predefined thresholds to determine agreement classes.
#' 
#' Details about the thresholds and the aggregation can be found here:
#' \url{https://redmine.pik-potsdam.de/projects/magpie-intern/wiki/Traffic_light_validation}
#' 
#' @param TLstatistics MAgPIE object containing statistical test results as
#' returned by \code{\link{TLstatistics}}.
#' @return matrix containing agreement classes (0 = red, 1 = yellow, 2 =
#' green).
#' @author Markus Bonsch
#' @seealso
#' \code{\link{TLstatistics}},\code{\link{trafficlight}},\code{\link{TLplot}}
#' @examples
#' library(magclass)
#' data("population_magpie")
#' x <- population_magpie
#' xc <- x + runif(length(x)[1],0,10^5)
#' tl <- TLstatistics(x[,,1],xc)
#' 
#' TLevaluate(tl)
#' 
#' @importFrom utils read.csv
#' @export
TLevaluate<-function(TLstatistics){

  thresholds <- read.csv(system.file("extdata","thresholds.csv",package = "trafficlight"),stringsAsFactors = F)
  
  ####################################################
  #calculate traffic light for individual tests 
  #0 = red
  #1 = yellow
  #2 = green
  #####################################################
  
  test <- function(i,x,thresholds) {
    x <- x[grepl(i,dimnames(x)[[1]],fixed=TRUE),,drop=FALSE]
    j <- sub("^.*\\.","",i)
    green <- thresholds[j,"green"]
    yellow <- thresholds[j,"yellow"]
    origin_check <- thresholds[j,"check"]
    tmp <- x
    tmp[] <- 0
    if(yellow>green) {
      tmp[abs(x) <= yellow] <- 1
      tmp[abs(x) <= green] <- 2
    } else {
      tmp[abs(x) >= yellow] <- 1
      tmp[abs(x) >= green] <- 2
    }
    tmp[is.na(x)] <- -1
    tmp <- apply(tmp,2,max,na.rm=TRUE)
    
    if(origin_check) {
      #check there is a change in sign of the tests
      #(e.g. test with data1 was positive and with data2 negative)
      #if so, set trafficlight to green as this indicates that the 
      #behavior of the data to be tested is somewhere in between
      #the behavior of comparison sets
      tmpf <- function(x) return(any(diff(x)!=0,na.rm=TRUE))
      tmp[apply(sign(x),2,tmpf)] <- 2
    }
    tmp[tmp==-1] <- NA
    return(tmp)
  }
  
  i <- unique(sub("^.*\\.(.*\\..*)$","\\1",dimnames(TLstatistics)[[1]]))
  out <- as.data.frame(sapply(i,test,TLstatistics,thresholds,simplify = FALSE)) 
  
  #aggregate results: take best result of each category and calculate the mean over all categories
  tmp <- NULL
  for(i in unique(sub("\\..*$","",dimnames(out)[[2]]))) {
    tmp <- suppressWarnings(cbind(apply(out[,grepl(i,dimnames(out)[[2]])],1,max, na.rm=TRUE),tmp))
    dimnames(tmp)[[2]][1] <- i
  }
  tmp[is.infinite(tmp)] <- NA
  return(as.matrix(cbind(Total=round(rowMeans(tmp, na.rm = TRUE)),tmp,out)))
}
