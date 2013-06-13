#' profile
#'
#' @description 
#' Performs a profile using residual sum of squares, fixes some parameters for a range of values 
#' and then estimate the others 
#'
#' @param fitted: an \code{aspic} fitted
#' @param which: \code{character} giving the parameters to do the profile for, i.e. to fix.
#' @param range; \code{numeric} relative values by which to vary parameter values, default seq(0.5,1.5,length.out=21). 
#' @param fn: \code{function} that gives values to be profiled.
#' @param run: \code{logical} if \code{TRUE} then returns profile, otherwise it just sets the control fitted-
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
                                       rev(rev(model.frame(x@ll))[-1])),
                   run=TRUE,...){
  
        if (dims(fitted)$iter>1) stop("can only be done for a single iter")
                 
        if (length(range)==1) range=c(range,2-range)
        
        sq=list(range)
        sq=do.call("expand.grid",sq[rep(1,length(which))])
        names(sq)=which
        
        fitted@control=propagate(fitted@control,dim(sq)[1])
        
        for (i in which)
           fitted@control[i,"val"]=params(fitted)[i,]*sq[,i]
        
        fitted@control[which,"fit"]=0
          
        if (!run) return(fitted)
        
        res=fit(fitted)
        
        rtn=fn(res)
        
        return(rtn)})


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