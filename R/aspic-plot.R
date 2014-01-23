utils::globalVariables(c("ggplot","geom_line","aes","yield","geom_point","cast","xlab","ylab"))

##############################################################
#' Create a \code{ggplot} plot
#'
#' Creates a \code{ggplot2} object that plots time series of biomass, harvest rate and catch. The basic object can then be modified by adding ggpot2 layers.
#'
#' @param  \code{x}, an object of class \code{aspics} 
#'
#' @return an \code{ggplot2} object
#' 
#' @seealso \code{\link{plotSP}} 
#' @method plot
#' @export
#' @docType methods
#' @rdname plot
#'
#' @examples
#' refpts("logistic",FLPar(msy=100,k=500))
#'  
setMethod("plot", signature(x="aspics", y="missing"),
  function(x, y, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free",ncol=1),
    fn=list("Stock"  =function(x) stock(x), 
            "Harvest"=function(x) harvest(x),
            "Yield"  =function(x) catch(x)),...)
   
    plotComps(x,fn,probs,size,lty,facet))

# @param  \code{fn}, a list of functions that estimate the quantities for plotting
# @param  \code{probs}, a vector specifying the percentiles for plotting, these are c(0.95,0.50,0.05) by default.
# @param  \code{size}, thinkness of percentile lines
# @param  \code{lty}, line type for percentiles
# @param \code{facet}, a layer that determines the facetting of the plot


plotSP=function(object,biomass=FLQuant(seq(0,max(params(object)["k"]),length.out=101))) {
  object=as(object,"biodyn")
  if ((dims(object)$iter>1 | dims(params(object))$iter>1) & dims(biomass)$iter==1) 
    biomass=propagate(biomass,max(dims(object)$iter,dims(params(object))$iter))
  
  p <-  ggplot(model.frame(FLQuants(stock=biomass, yield=FLQuant(computeSP(object,biomass))))) +
    geom_line(aes(stock, yield, group=iter, col=iter)) +
    geom_point(aes(bmsy,msy,col=iter),size=2,data=cast(as.data.frame(refpts(object)),iter~refpts,value="data")) +
    xlab("Stock") + ylab("Surplus Production")
  print(p)
  invisible(p)} 
