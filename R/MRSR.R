#define MRSR
MRSR<-function(obs,sim,sigma=NA){
  if(length(obs)!=length(sim)) stop("Number of observations and simulations do not fit together")
  num<-sqrt(1/length(obs)*sum((obs-sim)^2))
  if(!any(is.na(sigma))){
    den<-1/length(obs)*sum(sigma)
    if(den==0)return(NA)
  } else {
    den<-sqrt(1/length(obs)*sum((obs-mean(obs))^2))
    if(den==0)return(NA)
  }
  return(num/den)        
}