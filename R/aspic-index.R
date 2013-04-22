setGeneric("index<-",    function(object,value,...) standardGeneric('index<-'))

validIndex=function(x){
   if (!("data.frame" %in% is(x)))                         stop("not a data.frame")
   if (!all(c("name","year","code") %in% names(x)))        stop("data frame has to have columns 'name', 'year' and 'code'")   
   if (!any(x$code %in% dimnames(aspic:::indexCode)[[1]])) stop("invalid code")
   if (!any(c("index","catch","effor") %in% names(x)))     stop("data frame has to have a column 'index', 'catch' or 'effort'")
   
   return(TRUE)}

setMethod('index<-',  signature(object='aspic',value="character"),
          function(object,value) {
            object@index=readCpue(value,"aspic")
            
            if (!validIndex(object@index)) stop()
            
            return(object) })

setMethod('index<-',  signature(object='aspic',value="data.frame"),
          function(object,value) {
            object@index=value
            
            return(object)})


setMethod('index',  signature(object='aspic'),
          function(object,df=TRUE) {
            
            if (df) return(object@index)
            
            if (length(unique(object@index[,"name"]))==1) return(FLQuant(object@index$index,dimnames=list(year=object@index$year)))
            else {res=dlply(object@index,.(name), with, FLQuant(index, dimnames=list(year=object@index$year)))
                  names(res)=unique(object@index[,"name"])
              }
            
            
            })
