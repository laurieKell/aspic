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
    fn=list("Stock"=stock, "Harvest"=function(x) catch(x)/stock(x)[,dimnames(catch(x))$year],"Yield"=catch),...)
   
    plotComps(x,fn,probs,size,lty,facet))

# @param  \code{fn}, a list of functions that estimate the quantities for plotting
# @param  \code{probs}, a vector specifying the percentiles for plotting, these are c(0.95,0.50,0.05) by default.
# @param  \code{size}, thinkness of percentile lines
# @param  \code{lty}, line type for percentiles
# @param \code{facet}, a layer that determines the facetting of the plot