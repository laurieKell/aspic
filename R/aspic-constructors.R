#' aspic Class
#'
#' @description Creates an object of the \pkg{aspic} class that implements a biomass dynamic stock assessment model.
#' @name aspic
#' @param object, a factor or string that specifies the model type, has to be one of "fox", "schaefer", "pellat", "gulland", "fletcher", "shepherd", "logistic", "genfit"
#' @param params, an \code{FLPar}  object with model parameters
#' @return an \code{aspic} object
#' @export
#' @doctype methods
#' @examples \dontrun{bd=biodyn("logistic",FLPar(k=50000,msy=1000,b0=1))}
setMethod('aspic', signature(object='missing'),
          function(...)
          {
            #args <- list(...)
            
            # if no FLQuant argument given, then use empty FLQuant
            #slots <- lapply(args, class)
            #slots <- names(slots)[slots == 'FLQuant']
            
          return(new("aspic"))
          })

setMethod('aspic', signature(object="data.frame"),
    function(object,r=0.25,...){
         
            args <- list(...)
            
            nms=names(object)
         
            res=new("aspic")
            
            if (all(c("year","catch") %in% nms)){
              o=ddply(object, .(year), with, sum(catch))
              res@catch <- FLQuant(o$V1,dimnames=list(year=o$year))
              }
            
            ## CC
            if (all(c("year","catch") %in% nms) & !("index" %in% nms))
               object=transform(object, index=catch/effort)
            range(res)=unlist(list(minyear=min(object$year), maxyear=max(object$year)))
            
            res@index=object
            # Load given slots
            for(i in names(args))
              slot(res, i) <- args[[i]]
            
            nms=dimnames(res@params)
            nms$params=c(nms$params,paste("q",seq(length(unique(object$name))),sep=""))
            
            res@params=FLPar(NA,dimnames=nms)
            
            nms=dimnames(res@control)
            nms$params=dimnames(res@params)[[1]]
            nms$params=nms$params
            
            res@control=array(as.numeric(NA),dim=laply(nms[-3],length),dimnames=nms[-3])
            res@control["b0", "val"]=1.0
            res@control["msy","val"]=mean(res@catch,na.rm=T)
            res@control["k",  "val"]=mean(res@control["msy","val"])*4.0/r
     
            res@control[-(1:3),"val"]=daply(res@index, .(name), with, 2*mean(index,na.rm=T))/res@control["k",  "start"]
            
            res@control[,"min"] = res@control[,"val"]*0.01
            res@control[,"max"] = res@control[,"val"]*100.0
            res@control[,"fit"]    =1
            res@control[,"lambda"] =1
               
            res@control["b0","fit"]   =0
            res@control[1:3, "lambda"]=0
            
            res@rnd=2062012
            
            return(res)})

setMethod('aspic', signature(object='character'),
          function(object,...)
          {
          return(readAspic(object))
          })
