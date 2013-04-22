#setGeneric("params<-",  function(object,value,...) standardGeneric('params<-'))
# 
setGeneric("control",    function(object,...)       standardGeneric('control'))
setGeneric("control<-",  function(object,value,...) standardGeneric('control<-'))
#setGeneric("catch<-",   function(object,value,...) standardGeneric('catch<-'))

setMethod('control',  signature(object='aspic'),
          function(object)  object@control)
           
setMethod('params<-',  signature(object='aspic',value="character"),
          function(object,value) {
            
            coerceDP=function(x)  FLPar(unlist(c(t(x))),params=names(x),iter=dim(x)[1])
            
            det=aspicDet(value)
            parNms=dimnames(object@params)$params
            object@params=coerceDP(det[,parNms])
            
            return(object)
          })
#params(object)<-"/home/laurie/Desktop/gcode/gbyp-sam/data/ASPIC/albs/2011/run2/aspic.det"

setMethod('control<-',  signature(object='aspic',value="FLPar"),
          function(object,value,min=0.1,max=10.0,fix=T) {
            
            if (fix) nms=dimnames(value)$params[object@control[,"fit"]==1] else
                     nms=dimnames(value)$params[object@control[,"fit"]==1]
            
            object@control[nms,"val"]=value[nms]
            object@control[nms,"min"]=value[nms]*min
            object@control[nms,"max"]=value[nms]*max
            
            return(object)
          })
#control(object)<-object@params

setMethod('catch<-',  signature(object='aspic',value="character"),
          function(object,value) {
            object@index=readCpue(value,"aspic")
            
            dat=ddply(object@index[object@index$code %in% c("CC","CE"),],.(year), with, data.frame(data=sum(catch)))
            
            object@catch=as.FLQuant(dat)

            return(object)})
#catch(object)="/home/laurie/Desktop/gcode/gbyp-sam/data/ASPIC/albs/2011/run2/aspic.inp"




