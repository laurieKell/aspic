library(FLBioDym)
library(aspic)

scenMdl=list("schaefer"=FLPar(r=0.5, k=100, b0=1.0,           q=1,sigma=0.3),
             "fox"     =FLPar(r=0.5, k=100, b0=1.0,           q=1,sigma=0.3),
             "pellat"  =FLPar(r=0.5, k=100, b0=1.0,p=1,       q=1,sigma=0.3),
             "shepherd"=FLPar(r=0.5, k=100,            m=1,   q=1,sigma=0.3),
             "gulland" =FLPar(r=0.5, k=100),
             "fletcher"=FLPar(       k=100,        p=1,msy=25,q=1,sigma=0.3))

tstMsy=function(mdl,pars=scenMdl[[mdl]],...){
    args=list(...)
    print(args)
    args=args[names(args) %in% parLst[[mdl]]]
  
    if (length(args)>0) pars[names(args)]=unlist(args)
             
    res=c(" msy"=msy( mdl, pars),
          "bmsy"=bmsy(mdl, pars),
          "fmsy"=fmsy(mdl, pars))
    return(res)}

tstMsy("schaefer",k=2)

tstSP=function(mdl,pars=scenMdl[[mdl]],...){
  args=list(...)
  print(args)
  args=args[names(args) %in% parLst[[mdl]]]
  
  if (length(args)>0) pars[names(args)]=unlist(args)
  
  spFn(mdl,pars,biomass=seq(0,1,.01)*pars["k"])}

object,
k=100
biomass=FLQuant(seq(0,k,length.out=101))
yield  =sp(object,biomass)
                
  p <-  ggplot(model.frame(FLQuants(stock=biomass, yield=sp(object,biomass)))) +
    geom_line(aes(stock, yield)) +
    geom_point(aes(bmsy,msy),data=cast(as.data.frame(refpts(object)),iter~refpts,value="data")) +
    xlab("Stock") + ylab("Surplus Production")
  print(p)
  invisible(p)}


object=simFLBioDym("schaefer")
plot(object)
plotSP(object)

refpts(object)

as=asAspic(object)
plot(as)
plotSP(as)




t.=cpueBiodym2Aspic(bd,"CC",n=5)

tail(t.)



# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-class.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-generics.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-methods.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-constructors.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-sp.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-msy.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-fwd.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-sim.R")
# source("/home/laurie/Desktop/FLBioDymV2/R/FLBioDym-createAccessors.R")
# 
# source("/home/laurie/Desktop/aspicV2/R/aspic-class.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-coerce.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-constructors.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-diags.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-exe.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-fwd.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-generics.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-io.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-msy.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-OEM.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-plot.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-sp.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspic-accessors.R") 
# source("/home/laurie/Desktop/aspicV2/R/aspics.R")
