## hydromad: Hydrological Modelling and Analysis of Data
##
## Copyright (c) Joseph Guillaume <josephguillaume@gmail.com>
##

crossValidate <- function(MODEL,start.dates,end.dates,fitBy,...){
  cv.set <- splitData(MODEL,start.dates,end.dates)
  runs <- runlist()
  for(n in names(cv.set)){
    if(isTRUE(hydromad.options("trace")$trace)) cat("\nFitting period",n,"\n")
    fitx <- fitBy(cv.set[[n]],...)
    new.runs <- update(cv.set,newpars=coef(fitx))
    names(new.runs) <- sprintf("%s_cal%s",names(cv.set),n)
    ## Preserve fit attributes
    new.runs[[sprintf("%s_cal%s",n,n)]] <- fitx
    ## for(m in names(new.runs)){
    ##   new.runs[[m]]$objective <- fitx$objective
    ## }
    runs <- c(runs,new.runs)
  }
  class(runs) <- unique(c("crossvalidation",class(runs),"runlist", "list"))
  runs
}

summary.crossvalidation <- function(object, ...){
  s=NextMethod(runs,...)
  s$sim.period <- sub("_cal.*","",rownames(s))
  s$calib.period <- sub(".*_cal","",rownames(s))
  s
}  
