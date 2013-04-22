#### Convergence ###############################################################
# 1 100000     					## 0=no MC search, 1=search, 2=repeated srch; N trials #
# 1.00000e-08 					## Convergence crit. for simplex                       #  
# 3.00000e-08 6					## Convergence crit. for restarts, N restarts          #
# 1.00000e-04 0 				## Convergence crit. for estimating effort; N steps/yr #
# 8.00000 							## Maximum F allowed in estimating effort              #
################################################################################

model=factor(c("LOGISTIC", #Schaefer
               "GENGRID",  #generalized model at grid of values or at one specified value
               "FOX",      #Fox
               "GENFIT"))  #Fit the generalized model and estimate its exponent directly.

conditioning=c("YLD", #Condition fitting on yield (recommended for most analyses).
               "EFT") #Condition fitting on fishing-effort rate

objFn=c("SSE",     #Sum of squared errors (recommended default).
        "WTDSSE",  #SSE with annual data weighting
        "LAV")     #Least absolute values (robust objective function).

indexCode=c("CE", "Fishing effort rate, catch (weight)",                  "Effort rate: annual average,Catch: annual total",
            "CC", "Index (weight-based), catch (weight)",                 "Index: annual average,Catch: annual total",
            "B0", "Estimate of biomass Effort rate: annual average",      "Start of year",
            "B1", "Estimate of biomass: annual total",                    "Annual average",
            "B2", "Estimate of biomass: annual average",                  "End of year",
            "I0", "Index of biomass: annual total",                       "Start of year",
            "I1", "Index of biomass Start of year",                       "Annual average", 
            "I2", "Index of biomass Annual average",                      "End of year")

indexCode=t(array(indexCode, dim=c(3,8),dimnames=list(c("code","desc","timing"),NULL)))
dimnames(indexCode)[[1]]=indexCode[,1]
indexCode=transform(indexCode[,-1],startf=c(0,0,0,0,1,0,0,1),
                                 endf  =c(1,1,0,1,1,0,1,1),
                                 ncol  =c(3,3,2,2,2,2,2,2),
                                 col2  =c("effort","index","biomass","biomass","biomass","index","index","index"),
                                 col3  =c("catch", "catch","",       "",       "",       "",     "",     ""))


validAspic <- function(object) {
  ## Catch must be continous
  yrs<-dimnames(catch(object))$year
  
  if (!all(yrs == ac(dims(catch(object))$minyear:dims(catch(object))$maxyear)))
      return("years in catch not continous")
  
  #model         ="factor"
  if (!("factor" %in% is(object@model)) || !(model %in% model))
    stop()
  
  #obj           ="factor",
  #conditioning  ="factor",
  #options       ="numeric",

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)}


#' ASPIC Biomass Dynamic Model Class
#' 
#' @description A class that represents the ASPIC biomass dynamic stock assessment model.
#' @return biodyn object
#' @export
#' @examples
#' \dontrun{aspic()}
#' 
#'@section Slots:                                 
#' \describe{                                  
#' \item{\code{obj}:          \code{factor} .}          
#' \item{\code{conditioning}: \code{factor} .}
#' \item{\code{options}:      \code{numeric} .}     
#' \item{\code{index}:        \code{data.frame} .}   
#' \item{\code{stopmess}:     \code{character} .}  
#' \item{\code{rnd}:          \code{numeric} .}       
#' \item{\code{model}:        \code{factor} .}        
#' \item{\code{catch}:        \code{FLQuant} .}      
#' \item{\code{stock}:        \code{FLQuant} .}       
#' \item{\code{diags}:        \code{data.frame} .}   
#' \item{\code{params}:       \code{FLPar} .}    
#' \item{\code{control}:      \code{FLPar} .}      
#' \item{\code{priors}:       \code{array} .}        
#' \item{\code{vcov}:         \code{FLPar} .}         
#' \item{\code{hessian}:      \code{FLPar} .}       
#' \item{\code{objFn}:        \code{FLPar} .}        
#' \item{\code{mng}:          \code{FLPars} .}          
#' \item{\code{name}:         \code{character} .}     
#' \item{\code{desc}:         \code{character} .}      
#' \item{\code{range}:        \code{numeric} .}      
#' }

setClass('aspic', representation(
    "biodyn",
    obj           ="factor",
    conditioning  ="factor",
    options       ="numeric",     
    
    index          ="data.frame",
    
    stopmess      ="character",
    rnd           ="numeric"),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA),   maxyear=as.numeric(NA))),
    model         =factor("LOGISTIC",levels=model,       labels=model),
    obj           =factor("SSE",     levels=objFn,       labels=objFn),
    conditioning  =factor("YLD",     levels=conditioning,labels=conditioning),
    options       =c(search=1,trials=100000,simplex=1e-8,restarts=3e-8,nrestarts=6,effort=1e-4,nsteps=0,maxf=8.0),
   
    params        =FLPar(NA,dimnames=list(params=c("b0","msy","k"),iter=1)),
    control       =FLPar(NA,c(length(c(c("b0","msy","k"),paste("q",seq(1),sep=""))),5),
                         dimnames=list(params=c(c("b0","msy","k"),paste("q",seq(1),sep="")),
                                              c("fit","min","val","max","lambda"),iter=1)),
    objFn         =FLPar(array(NA,dim=c(2,1),dimnames=list("value"=c("rss","rsq"),iter=1))),
    vcov          =FLPar(NA,dimnames=list(params=c("b0","msy","k"),param=c("b0","msy","k"),iter=1)),
    hessian       =FLPar(NA,dimnames=list(params=c("b0","msy","k"),param=c("b0","msy","k"),iter=1)),
    stopmess      ="not ran"),
  validity=validAspic)

# printSlot=function(x){
#    res=data.frame(getSlots(x))
#    c("#'@section Slots:", "#'  \\describe{",
#      paste("#' \\item{\\code{", dimnames(res)[[1]], "}: \\code{", as.character(res[,1]),"} .}",sep=""),
#      "#'  }")}




