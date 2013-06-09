uAspic=function(object,index,code="I1",bound=c(.01,100),add=TRUE){

  ## create new indices
  res=rbind(cbind(expand.grid(year=index(object)$year,name=unique(index$name),index=0)),index)
  res=ddply(res,.(name,year), function(x) data.frame(index=sum(x$index)))
  res$index[res$index<=0]=NA
  res=cbind(res,code=code,catch=0)[,names(index(object))]
  if (add)
    res=rbind(index(object),res)
  
  res=merge(res,FLCore:::as.data.frame(stock(object),drop=T),by=c("year"),all=T)

  res=res[order(res[,"name"]),]

  nidx =length(unique(res$name))
  qs =ddply(res,.(name), with, biodyn:::calcQ(data,index))
  qnm=paste("q",seq(dim(qs)[1]),sep="")
  
  par=params(object)
  par=par[dimnames(par)$params[substr(dimnames(par)$params,1,1)!="q"],]
  dmns=dimnames(par)
  dmns$params=c(dmns$params,paste("q",seq(nidx),sep=""))
  
  rtn=object
  rtn@params=FLPar(array(NA,unlist(laply(dmns,length)),dmns))
  
  ctl=object@control
  ctl=ctl[dimnames(ctl)$params[substr(dimnames(ctl)$params,1,1)!="q"],]
  dmns=dimnames(ctl)
  dmns$params=c(dmns$params,paste("q",seq(nidx),sep=""))
  ctl.=FLPar(array(NA,unlist(laply(dmns,length)),dmns))
  ctl.[dimnames(ctl)$params,]=ctl
  ctl.[qnm,"fit"]   =1
  ctl.[qnm,"lambda"]=1
  ctl.[qnm,"val"]=qs[,"q"]
  ctl.[qnm,"min"]=ctl.[qnm,"val"]*bound[1]
  ctl.[qnm,"max"]=ctl.[qnm,"val"]*bound[2]
  
  dmns=dimnames(rtn@ll)
  dmns$params=paste("u",seq(nidx),sep="")
  rtn@ll=FLPar(array(NA,unlist(laply(dmns,length)),dmns))

  rtn@control=ctl.
  rtn@index=res[,-6]
  
  return(rtn)}

