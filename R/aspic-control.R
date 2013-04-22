setMethod('setControl<-', signature(object='aspic',value="FLPar"), function(object,value,min=0.1,max=10.0) {
  
  nms=dimnames(object@params)$params
  object@control=FLPar(array(rep(c(1,NA,NA,NA,1),each=length(nms)), dim=c(length(nms),5,1), dimnames=list(params=nms,option=c("fit","min","val","max","lambda"),iter=1)))
  object@control[nms,"val"]=value
  object@control[nms,"min"]=value[nms,]*min
  object@control[nms,"max"]=value[nms,]*max
  
  
  return(object)})
