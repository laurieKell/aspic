########################################################################################
#### aspic stuff for Kobe #############################################################
########################################################################################

utils::globalVariables(c("ddply",".","year","pctl","cast","kobeP","sims"))
# 
# #' @export
# setMethod('kobe', signature(object="aspics",method="missing"),  function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
#      if (is.null(year)) year=range(object[[1]])["maxyear"]
#  
#      res=llply(object, function(x,what=what,prob=prob,year=year,nwrms=nwrms)
#               kobe(model.frame(mcf(FLQuants(stock  =stock(  x)%/%bmsy(x),
#                                             harvest=harvest(x)%/%fmsy(x))),drop=T),
#                        what=what,prob=prob,year=year,nwrms=nwrms),
#                        what=what,prob=prob,year=year,nwrms=nwrms)
# 
#      res=list(trks=ldply(res, function(x) x$trks),
#               pts =ldply(res, function(x) x$pts),
#               smry=ldply(res, function(x) x$smry),
#               wrms=ldply(res, function(x) x$wrms),
#               sims=ldply(res, function(x) x$sims))
#      
#      if (length(what)==1)
#         return(res[[what]])
#      else
#         return(res[what]) })
# 
# setMethod('kobe',  signature(object="aspic",method="missing"),  
#   function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
#     if (is.null(year)) year=range(object)["maxyear"]
#     dat=model.frame(mcf(FLQuants(stock  =stock(  object)%/%bmsy(object),
#                                  harvest=harvest(object)%/%fmsy(object))),drop=T)
#     res=kobe:::kobeFn(dat,what=what,prob=prob,year=year,nwrms=nwrms)
#     if (length(what)==1)
#          return(res[[what]])
#     else
#          return(res[what])})
# 
# 
# setMethod('kobe',  signature(object="data.frame",method="missing"), 
#           function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){ 
#             kobe:::kobeFn(object,what=what,prob=prob,year=year,nwrms=nwrms)})
# # 
# # kobeFn=function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){         
# #   
# #   trks. =NULL
# #   pts.  =NULL
# #   smry. =NULL
# #   wrms. =NULL
# #   sims. =NULL
# #   
# #   ## trks
# #   if ("trks" %in% what){
# #     
# #     trks.=rbind(ddply(object,.(year), function(x) data.frame(quantity="stock",  pctl=prob,value=quantile(x$stock,    prob, na.rm=T))),
# #                 ddply(object,.(year), function(x) data.frame(quantity="harvest",pctl=prob,value=quantile(x$harvest,  prob, na.rm=T))))
# #     
# #     trks.=transform(trks.,pctl=paste(substr(ac(signif(pctl,2)),3,nchar(ac(signif(pctl,2)))),ifelse(nchar(ac(trks.$pctl))==3,"0",""),"%",sep=""))
# #     trks.=cast(trks.,year+pctl~quantity,value="value") 
# #   }
# #   
# #   if ("pts" %in% what & !is.null(year))
# #     pts. =object[object$year==year,]
# #   
# #   
# #   if ("smry" %in% what)
# #     smry. =ddply(kobeP(sims), .(year), function(x) data.frame(stock      =median(stock(object),       na.rm=T),
# #                                                               harvest    =median(harvest(object),     na.rm=T),
# #                                                               red        =mean(  x$red,         na.rm=T),
# #                                                               yellow     =mean(  x$yellow,      na.rm=T),
# #                                                               green      =mean(  x$green,       na.rm=T),
# #                                                               overFished =mean(  x$overFished,  na.rm=T),
# #                                                               overFishing=mean(  x$overFishing, na.rm=T)))
# #   if ("wrms" %in% what){          
# #     wrms =sample(unique(res$iter),nwrms)
# #     wrms.=sims[sims$iter %in% wrms,]
# #   }
# #   
# #   if ("sims" %in% what)     
# #     sims. =object
# #   
# #   res=list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.)
# #   
# #   res}

#' kobePhase 
#' 
#' @description 
#' produces the kobe Phase plot background, i.e. green, red and yellow quadrants to which 
#' layers can be added
#'
#' @param object; a \code{aspic} object 
#' @return A ggplot2 object 
#' @seealso \code{\link{kobe}}
#' @export
#' @examples
#' \dontrun{
#'     data(asp)
#'     kobePhase(asp)+geom_path( aes(stock,harvest)) +
#'     geom_point(aes(stock,harvest))
setMethod('kobe', signature(object='aspic'),
          function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
            
            res=model.frame(mcf(FLQuants(stock   =stock(  object)%/%bmsy(object),
                                          harvest=harvest(object)%/%fmsy(object))))
                    
            kobeFn(res,what,prob,year,nwrms)})

setMethod('kobe', signature(object='aspics'),
          function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
            
          res=ldply(object,  function(x) model.frame(mcf(FLQuants(stock  =stock(  x)%/%bmsy(x),
                                                                  harvest=harvest(x)%/%fmsy(x)))))
         
          kobeFn(res,what,prob,year,nwrms)})


kobeMar=function(x,ds=seq(0,4,.001)){
  
  mar=rbind(cbind(what="stock",
                  data.frame(value  =ds,
                             density=dnorm(ds,x@mng["bbmsy","hat"],x@mng["bbmsy","sd"]))),
            cbind(what="harvest",
                  data.frame(value  =ds,
                             density=dnorm(ds,x@mng["ffmsy","hat"],x@mng["ffmsy","sd"]))))
  return(mar)}

