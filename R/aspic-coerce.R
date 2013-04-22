setMethod('aspic', signature(object="FLStock"),
    function(object){
                  
      res      =new("aspic")
      res@catch=catch(object)
      res@stock=window(catch(object),end=dims(object)$maxyear+1)
      res@index =data.frame(model.frame(FLQuants(catch=catch(object),index=catch(object)/fbar(object)/mean(catch(object)/fbar(object))),drop=T),type="CC",name="1")
      
      dmns=dimnames(res@params)
      dmns$params=c(dmns$params,"q1")
      
      res@params=FLPar(NA,dimnames=dmns)
      res@params[]=c(1,mean(res@catch),4*mean(res@catch), mean(res@index$index/res@index$catch)*.2)
      
      res@control[,"val"]=res@params
      res@control[,"min"]=res@control[,"val"]*.1
      res@control[,"max"]=res@control[,"val"]*10
      res@control[,"lambda"]=1.0
      res@control[,"fit"]=c(0,1,1,1)
      
      range(res)[]=range(res@index$year)
      
      res@rnd=99999
      res})


#bdModel=attributes(model(new("FLBioDym")))$levels
  # 
  # setMethod('aspic', signature(object="data.frame"),
  # asAspic=function(object,...){
  #   
  #             args <- list(...)
  #             
  #             res=new("aspic")
  #             
  #             ## The same
  #             slot(res,"desc")    =slot(object,"desc")
  #             slot(res,"name")    =slot(object,"name")
  #             slot(res,"range")   =slot(object,"range")
  #             
  #             slot(res,"catch")   =slot(object,"catch")
  #             slot(res,"stock")   =slot(object,"stock")
  #             
  #             slot(res,"stopmess")=slot(object,"stopmess")
  #       
  #             ## model
  #             slot(res,"model")=switch(model(object),
  #                                      schaefer=factor("LOGISTIC"),
  #                                      fox     =factor("FOX"),
  #                                      pellat  =factor("GENFIT"))
  #           
  #             slot(res,"params")  =paramFn(object)
  #             slot(res,"control")  =controlFn(res)
  #  
  #             # Load given slots
  #             for(i in names(args))
  #               slot(res, i) <- args[[i]]
  #                         
  #             return(res)}

paramFn=function(object){
  
#   fox       =c("r","K")
#   schaefer  =c("r","K")
#   pellat    =c("r","K","p")
  
  b2aParams=function(model,params) {
    
    model=model(bd)
    
    if(!(model %in% bdModel)) stop("has to be one of", bdModel)
    
    schaeferFn = function(biomass, params) { #logistic
      params["r"]=params["r"]*params["K"]/4
      dimnames(params)$params[1]="msy"
      
      params}
    
    foxFn =function(biomass, params) params
    
    pellatFn = function(biomass, params) params
    
    res = switch(model,
                 "fox"     =foxFn(     biomass,params),
                 "schaefer"=schaeferFn(biomass,params),
                 "pellat"  =pellatFn(  biomass,params),
                 stop("has to be either 'fox', 'schaefer' or 'pellat'"))
    
    res@rnd=9999
    return(res)}
   
   b2aParams(model(object),params(object))}
     
controlFn=function(object){
  
  schaefer2Logistic=function(x){FLPar()}
  fox2fox          =function(x){FLPar()}
  pellat2Genfit    =function(x){FLPar()}
  
  switch(model(object),
         pellat  =schaefer2Logistic(params(object)),
         fox     =fox2fox(          params(object)),
         schaefer=pellat2Genfit(    params(object)),
         gulland =stop("Gulland not available in ASPIC"),
         fletcher=stop("Gulland not available in ASPIC"),
         shepherd=stop("Gulland not available in ASPIC"))
  }

setAs('biodyn','aspic',
      function(from){
        sA=getSlots("aspic")
        sB=getSlots("biodyn")
        
        sA=sA[!(names(sA) %in% c("model","params"))]
        sB=sB[!(names(sB) %in% c("model","params"))]
        
        res=aspic()
        
        model(res) =aspic:::model[1]
        params(res)=FLPar("msy"=msy(from),"k"=c(params(from)["k"]),"b0"=c(params(from)["b0"]))
        
        control(res)[c("b0","k"),c("min","val","max")]=control(from)[c("b0","k"),c("min","val","max")]
        control(res)["msy",c("min","val","max")]=control(from)["r",-1]/c(control(from)["r",3])*c(msy(from))
        
        for (i in names(sA[(names(sA) %in% names(sB))]))
          slot(res,i)=slot(from,i)
        
        dimnames(res@objFn)$value=c("rss","ll")
        
        return(res)})

setAs('aspic', 'biodyn',
   function(from){
        sA=getSlots("aspic")
        sB=getSlots("biodyn")
        
        sA=sA[!(names(sA) %in% c("model","params"))]
        sB=sB[!(names(sB) %in% c("model","params"))]
       
        par=FLPar("r"=.6,"k"=c(params(from)["k"]),"b0"=c(params(from)["b0"]),"p"=1)
        par["r"]=c(params(from)["msy"]/(par["k"]*(1/(1+par["p"]))^(1/par["p"]+1)))
        res=biodyn(factor("pellat"),par)
       
        res@control[c("p","b0"),1,1]=-1
        
        for (i in names(sA[(names(sA) %in% names(sB))]))
          slot(res,i)=slot(from,i)
       
        cpue=index(from,F)              
        setParams( res)            =cpue
        setControl(res)            =params(res)
        
        dimnames(res@objFn)$value=c("rss","ll")
        
        return(res)}
      )


