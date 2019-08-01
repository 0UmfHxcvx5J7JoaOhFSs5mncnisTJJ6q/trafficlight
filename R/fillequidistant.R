
fillequidistant <- function(years) {
  if(!all(round(years)==years)) stop("Input must be full years!")
  df <- unique(diff(years))
  if(length(df)==1) {
    return(years)
  } else if(sum(df %% min(df))==0) {
    return(seq(min(years),max(years),by = min(df))) 
  } else {
    return(seq(min(years),max(years),by = 1))
  }
}