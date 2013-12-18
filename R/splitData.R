splitData<-function(object,start.dates,end.dates){
  ## Split data according to start.dates and end.dates
  cv.data <- mapply(window,start=start.dates,end=end.dates,MoreArgs=list(x=observed(object,all=TRUE,select=TRUE)),SIMPLIFY=FALSE)
  rl=as.runlist(mapply(update,newdata=cv.data,MoreArgs=list(object=object),SIMPLIFY=FALSE))
  cv.names=paste(start.dates,end.dates,sep="_")
  names(rl)<-cv.names
  return(rl)
}

