#' TLplot
#' 
#' Function to plot the results of the automatic validation procedure as a
#' traffic light symbol
#' 
#' 
#' @param mode A matrix containing traffic light values
#' returned by \code{\link{TLevaluate}}.
#' @param detailed boolean deciding whether detailed information should be plotted or not.
#' In detailed mode each block represents a test group with the first traffic light showing
#' the overall performance in this group and the following traffic lights showing the results
#' of single tests.
#' @param linesize line size of the traffic light borders
#' @return A ggplot object.
#' @author Jan Philipp Dietrich, Markus Bonsch
#' @seealso
#' \code{\link{TLevaluate}},\code{\link{TLstatistics}},\code{\link{trafficlight}}
#' @examples
#' library(magclass)
#' data("population_magpie")
#' x <- population_magpie
#' xc <- x + runif(length(x)[1],-10^5,10^5)
#' tl <- TLstatistics(x[,,1],xc)
#' tle <- TLevaluate(tl)
#' 
#' TLplot(tle)
#' 
#' @importFrom ggplot2 ggplot theme_void coord_fixed geom_rect aes_string expand_limits annotate
#' @export
#' 

TLplot <- function(mode, detailed=TRUE, linesize=0.5) {
  
  if(!detailed) mode <- t(mode[,"Total",drop=FALSE])
  
  #if(length(mode)!=1) stop("length of mode different to 1!")
  mode[is.na(mode)] <- -1
  p <- ggplot() + theme_void() + coord_fixed() + expand_limits(x = -1 ,y = 0)
  
  types <- unique(sub("\\..*$","",dimnames(mode)[[2]]))
  
  for(j in 1:dim(mode)[1]) {
    p <- p + annotate("text",angle=90,x=1.2,y=-4*j+1.5, vjust=-1, hjust=0.5, label=dimnames(mode)[[1]][j])
    tleft <- 0
    for(t in types) {
      subtypes <- grep(t,dimnames(mode)[[2]],value=TRUE)
      if(j==1) p <- p + annotate("text", x=tleft+1.1 + 0.6*length(subtypes),y=3-4*j, vjust=-1, hjust=0.5, label=t)
      for(i in 1:length(subtypes)) {
        k <- subtypes[i]
        left <- tleft+1.2*i; right <- tleft + 1 + 1.2*i; top <- 3-4*j; bottom <- 0-4*j
        height <- (top-bottom)/3
        p <- p + geom_rect(mapping = aes_string(xmin=left,xmax=right,ymin=top-height,ymax=top),color="black",fill=ifelse(mode[j,k]==0, "red", "grey20"),size=linesize) + geom_rect(mapping = aes_string(xmin=left,xmax=right,ymin=top-2*height,ymax=top-height),color="black",fill=ifelse(mode[j,k]==1, "yellow", "grey20"),size=linesize) + geom_rect(mapping = aes_string(xmin=left,xmax=right,ymin=bottom,ymax=bottom+height),color="black",fill=ifelse(mode[j,k]==2, "green", "grey20"),size=linesize)
        
        if(!grepl(".",k,fixed = TRUE)) {
          label <- "Sigma"; parse=TRUE
        } else if(grepl("MannKendall.m_annual", k, fixed=TRUE)) {
          label <- "m"; parse=FALSE
        } else if(grepl("MannKendall.p", k, fixed=TRUE)) {
          label <- "p"; parse=FALSE
        } else if(grepl("GeometricReliability", k, fixed=TRUE)) {
          label <- "gr"; parse=FALSE
        } else if(grepl("ModifiedStandardDeviationRatio", k, fixed=TRUE)) {
          label <- "sigma"; parse=TRUE
        } else if(grepl("RelativeDifference", k, fixed=TRUE)) {
          label <- "delta"; parse=TRUE
        } else {
          label <- ""; parse=FALSE
        }
        if(j==dim(mode)[1]) p <- p + annotate("text", x=left+0.1 ,y=bottom-0.7, vjust=0, hjust=0, label=label, parse=parse)
      }
      tleft <- right
    }
  }
  return(p)
}
