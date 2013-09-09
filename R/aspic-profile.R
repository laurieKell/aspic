#' profile
#'
#' @description 
#' Performs a profile using residual sum of squares, fixes some parameters for a range of values 
#' and then estimate the others 
#'
#' @param fitted: an \code{aspic} object
#' @param which: \code{character} giving the parameters to do the profile for, i.e. to fix.
#' @param range; \code{numeric} relative values by which to vary parameter values, default seq(0.5,1.5,length.out=21). 
#' @param fn: \code{function} that gives values to be profiled.
#' @param run: \code{logical} if \code{TRUE} then returns profile, otherwise it just sets the control object-
#' 
#' @return a \code{data frame} with results turned by \code{fn} by values in \code{which}. 
#' @seealso \code{\link{biodyn},\link{fit}}
#'
#' @export
#' @docType methods
#' @rdname profile
#'
#' @examples
#' /dontrun{
#' data(asp)
#' res=profile(asp,which="msy",range=seq(0.75,1.25,length.out=21))
#' ggplot(res)+geom_line(aes(k,rss))
#' 
#' fn =function(x) cbind( data.frame(model.frame(params(x)),rss=sum(diags(x)$residual^2,na.rm=T)))
#' res=profile(asp,which="msy",length.out=3,range=c(0.5,1.1),fn=fn)
#' 
#' fn=function(x) ddply(x@diags, .(name), with, sum(residual^2,na.rm=T)/sum(count(!is.na(residual))))
#' 
#' }       
setMethod("profile", signature(fitted="aspic"),
      function(fitted,which,
                   range=seq(0.5,1.5,length.out=21),
                   fn   =function(x) cbind(model.frame(params(x)), 
                                           model.frame(refpts(x)),
                                           model.frame(x@objFn)[,-3],
                                           t(unlist(x@ll)[1,drop=T])),
                   run=TRUE,...){
  
        if (dims(fitted)$iter>1) stop("can only be done for a single iter")
        
        if (length(range)==1) range=c(range,2-range)
        
        if (dim(fitted@control)[3]==1){
          sq=list(range)
          sq=do.call("expand.grid",sq[rep(1,length(which))])
          names(sq)=which
          
          fitted@control=propagate(fitted@control,dim(sq)[1])
          
          for (i in which)
             fitted@control[i,"val"]=params(fitted)[i,]*sq[,i]
          
          fitted@control[which,"fit"]=0
            
          if (!run) return(fitted)
          
          res=fit(fitted)
          
          fitted=fn(res)}
        else
          fitted@control=profileGrid(fitted@control,which,range)
          
        return(fitted)})

setMethod('profile',  signature(fitted='aspics'),
          function(fitted,which,
                   range=seq(0.5,1.5,length.out=21),
                   fn   =function(x) cbind(model.frame(params(x)), 
                                           model.frame(refpts(x)),
                                           model.frame(x@objFn)[,-3],
                                           t(unlist(x@ll)[1,drop=T])),
                   run=TRUE,
                   .combine=rbind.fill,
                   .multicombine=T,.maxcombine=10,.packages="aspic"){
            
            if (!run) .combine=list

            res=foreach(i=names(fitted), .combine=.combine,
                        .multicombine=.multicombine,
                        .maxcombine  =.maxcombine,
                        .packages    =.packages) %dopar% {  
                        
                          profile(fitted[[i]],which=which,range=range,fn=fn,run=run)}
            
            if (!run) {
              res=aspics(res)
              names(res)=names(fitted)}
            else
              res=cbind(.id=rep(names(fitted),each=length(range)*length(which)),res)
            
            res})

#' @description 
#' Outputs as a data.frame a summary of parameters and RSS etc by data component 
#' generated when do a profile
#' 
#' @param x: an \code{aspic} object
#' 
#' @return a \code{data frame} with results by data component. 
#' @seealso \code{\link{biodyn},\link{profile} \link{fit}}
#'
#' @export
#' @docType methods
#' @rdname fnPiner
#'
#' @examples
#' /dontrun{
#' data(asp)
#' dcK=profile(asp,which=c("k"),range=seq(0.2,2.0,length.out=21),fn=fnPiner)
#' ggplot(dcK)+geom_line(aes(k,value,group=variable,col=variable))+
#'  theme_ms(12,legend.position="bottom")+
#'  ylab("Residual Sum of Squares")+xlab("K")
#' 
#' }       
fnPiner=function(x) {
  
  res=cbind(model.frame(params(x),drop=T)[,-(dim(params(x))[1]+1)],
            model.frame(refpts(x),drop=T)[,-c(1,4)],
            model.frame(FLQuants(stock  =stock(  x)[,ac(range(x)["maxyear"])]%/%bmsy(x),
                                 harvest=harvest(x)[,ac(range(x)["maxyear"])]%/%fmsy(x)),drop=T)[,-1],
            model.frame(x@ll[,"ss"])[,seq(dim(x@ll)[1])]
  )
  
  nms=dimnames(params(x))$params
  res=melt(res,id=c(dimnames(refpts(x))$refpts[-1],nms,c("stock","harvest")))
  
  res=transform(res,name=unique(index(x)$name[res$variable]))
  res}

# ### debugging stuff
# data(bd)
# fitted=biodyn(factor("pellat"),params(bd),catch=catch(bd))
# cpue=rlnorm(1,log(stock(bd)),.2)[,-60]
# setParams(fitted)     =cpue
# 
# 
# attach(list(length.out=11, range=0.5, ci=c(0.25, 0.5, 0.75, 0.95),
#             plot=TRUE,fixed=c()))
# which="r"
# fixed=c("p","b0")
# ###
# rtn=profile(swon[[1]],which="k",fixed="b0",length.out=31,range=c(.75,1.5))
# ggplot(rtn)+geom_line(aes(msy,rss))