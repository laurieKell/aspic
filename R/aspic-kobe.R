utils::globalVariables(c('ddply','.','year','pctl','cast','kobeP','sims'))

#' kobe
#' 
#' @description Creates time series of stock relative to BMSY and harvest rate relative
#' to FMSY
#' 
#' @name kobe
#' 
#' @param object aspic object
#' @param method missing
#' @param ... other arguments
#' 
#' @return data.frame or list of data.frames
#' 
#' @aliases kobe kobe,aspics,missing-method kobe,aspics,ANY-method kobe,aspic,missing-method 
#' 
#' @export
#' 
#' @rdname kobe
#' 
#' @examples 
#' \dontrun{
#' sim()
#' }

#if (!isGeneric('kobe')) 
setGeneric('kobe',  function(object,method,...) standardGeneric('kobe'))

setMethod('kobe', signature(object='aspic',method='missing'),
          function(object,what=c('sims','trks','pts','smry','wrms')[1],probs=c(0.75,0.5,.25),
                   year=NULL,nwrms=10,sim=NULL){
            
            if (substr(sim,1,4)=="jack"){
              stock  =stock(  object)%/%bmsy(object)
              harvest=harvest(object)%/%fmsy(object)
            }else if ((dims(ple4)$iter)==1){
              stock  =stock(  object)%/%bmsy(object)
              harvest=harvest(object)%/%fmsy(object)
            }else{          
              res=model.frame(mcf(FLQuants(stock   =stock(  object)%/%bmsy(object),
                                           stock   =stock(  object)%/%bmsy(object))))
            }
            
            if ('pts' %in% what & is.null(year)) year=range(object)['maxyear']-1
            biodyn:::kobeFn(res,what,probs,year,nwrms)})

setMethod('kobe', signature(object='aspics'),
          function(object,what=c('sims','trks','pts','smry','wrms')[1],probs=c(0.75,0.5,.25),year=NULL,nwrms=10){
            
            res=ldply(object,  function(x) model.frame(mcf(FLQuants(stock  =stock(  x)%/%bmsy(x),
                                                                    harvest=harvest(x)%/%fmsy(x)))))
            
            biodyn:::kobeFn(res,what,probs,year,nwrms)})
