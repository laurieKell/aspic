testRuns=function(){ 

require(plyr)

    dirAss=data.frame(stockId   = c("albs","bet","bum","swon","whm","yft"),
                      assessment=c(2011,2010,2011,2009,2012,2011),stringsAsFactors=FALSE)
      
    runs=list(albs=paste("run",c(2,6,7,8),sep=""),
              bet =paste("run",c(3,5,6),sep=""),
              bum =c("highProd","lowProd"),
              swon=c("highProd","lowProd","run9"),
              whm =c("noBrazll"),
              yft =paste("run",c(9,10,11,12),sep=""))
    
    runs=mdply(names(runs), function(x) data.frame(stockId=x,run=runs[[x]]))[,-1]
    runs=merge(dirAss,runs)
    runs=mdply(runs,function(stockId,assessment,run) data.frame(dir=paste(stockId,assessment,run,sep="/")))
    
    return(runs)}

testTac=function(){ 
  
    tacs=list(albs=seq(15000,35000,5000),
              bet =seq(50,120,10),
              bum =seq(0,6000,500),
              swon=seq(10,15),
              whm =seq(200,1600,200),
              yft =seq(50,150,10))
    
    mdply(names(tacs), function(x) data.frame(stock=x,tac=tacs[[x]]))[,-1]}

