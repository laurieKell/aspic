setMethod('setParams<-', signature(object='aspic',value="data.frame"), function(object,value) {
  #LOGISTIC 
  nms=c("b0","msy","k")
  
  object@params=object@params[nms]
  
  params =biodyn:::setQ(object,value)
  
  params =params[dimnames(params)$params[substr(dimnames(params)$params,1,5)!="sigma"]]
  
  qs=substr(dimnames(params)$params,1,1)=="q"
  dimnames(params)$params[seq(length(qs))[qs]]=
    paste(substr(dimnames(params)$params[seq(length(qs))[qs]],1,1),seq(sum(qs)),sep="")
  
  object@params=params
  
  return(object)})

setIndexFn=function(object,value){
  res=aspic()
  
  ### New index #################################
  ## aspic
  # a) expunge old index
  # b) count number of indices
  # c) expand relevant slots
  # d) calculate default values

  idxs=unique(value$name)

  ## a) expunge old index
  # index 
  res@index=value

  # stopmess  
  res@stopmess="not ran"

  # catch
  tmp=ddply(res@index, .(year), with, sum(catch,na.rm=TRUE))
  res@catch=as.FLQuant(tmp[,"V1"], dimnames=list(year=tmp[,"year"]))
  dmns=dimnames(res@catch)
  dmns$year=c(dmns$year,as.numeric(max(dmns$year))+1)
 
  # stock        
  res@stock=FLQuant(NA,dimnames=dmns)

  range(res)=range(as.numeric(dimnames(res@catch)$year))

  # diags       
  res@diags=data.frame(NULL)

  # params      
  res@params =FLPar(as.numeric(NA),dimnames=list(params=c("b0","msy","k"),iter=1))

  params(asp)

  # control 
  res@control=FLPar(as.numeric(NA),c(length(c(c("b0","msy","k"),paste("q",seq(length(idxs)),sep=""))),5),
                       dimnames=list(params=c(c("b0","msy","k"),paste("q",seq(length(idxs)),sep="")),
                                   c("fit","min","val","max","lambda"),iter=1))

  # vcov      
  res@vcov=FLPar(as.numeric(NA),dimnames=list(params=c("b0","msy","k"),param=c("b0","msy","k"),iter=1))

  # hessian       
  res@hessian=FLPar(as.numeric(NA),dimnames=list(params=c("b0","msy","k"),param=c("b0","msy","k"),iter=1))

  # objFn
  res@objFn  =FLPar(array(as.numeric(NA),dim=c(2,1),dimnames=list("value"=c("rss","rsq"),iter=1)))

  # mng 
  res@rnd=object@rnd
  
  # mng 
  res@mng=FLPar()
  
  # mngVcov      
  res@mngVcov=FLPar()

  # profile         
  res@profile=data.frame(NULL)

  # desc        
  res@desc=paste(res@desc,"new index")

  nms=dimnames(res@params)$params[dimnames(res@params)$params %in% dimnames(object@params)$params]
  
  res@params[ nms]=object@params[ nms]
  res@control[nms]=object@control[nms]
  
  res@stock=object@stock
  
  ## add q??s
  setParams( res)=value
  setControl(res)=params(res)
  
  res}

setGeneric("setIndex<-",    function(object,value,...)   standardGeneric('setIndex<-'))
setMethod('setIndex<-', signature(object='aspic',value="data.frame"), function(object,value) 
  setIndexFn(object,value))


# 
# object=asp
# value =rbind(asp@index,sdz)
# setIndex(object)=value  