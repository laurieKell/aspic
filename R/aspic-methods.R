#setGeneric("params<-",  function(object,value,...) standardGeneric('params<-'))
# 
setGeneric("control",    function(object,...)       standardGeneric('control'))
setGeneric("control<-",  function(object,value,...) standardGeneric('control<-'))
#setGeneric("catch<-",   function(object,value,...) standardGeneric('catch<-'))

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))

setMethod('control',  signature(object='aspic'),
          function(object)  object@control)
           
setMethod('params<-',  signature(object='aspic',value="character"),
          function(object,value) {
         
            if (getExt(value)=="det"){
                coerceDP=function(x)  FLPar(unlist(c(t(x))),params=names(x),iter=dim(x)[1])
                det=aspicDet(value)
                parNms=dimnames(object@params)$params
                object@params=coerceDP(det[,parNms])}
            if (getExt(value)=="rdat"){
                estimates=aspic(value)$estimates
                names(estimates)=tolower(names(estimates))
                params(object)["b0"] =estimates["b1.k"]
                params(object)["msy"]=estimates["msy"]
                params(object)["k"]  =estimates["k"]
               
                q1=dimnames(params(object))$params[substr(dimnames(params(object))$params,1,1)=="q"]
                q2=names(estimates)[substr(names(estimates),1,2)=="q."]
                params(object)[q1]  =estimates[q2]
                }  
            
            return(object)})

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
            object@index=iUAspic(value) #,"aspic")
            
            dat=ddply(object@index[object@index$code %in% c("CC","CE"),],.(year), with, data.frame(data=sum(catch)))
            
            object@catch=as.FLQuant(dat)

            return(object)})
#catch(object)="/home/laurie/Desktop/gcode/gbyp-sam/data/ASPIC/albs/2011/run2/aspic.inp"




