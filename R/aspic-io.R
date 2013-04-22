#' read ASPIC text files.
#' @description 
#' Reads the the various \code{ASPIC} files and returns if possible an \code{FLR} object, otherwise returns 
#' a \code{data frame}.
#'       
#' @param file; the name of the file which the data are to be read from. 
#' @relative logical; by default \code{TRUE}, i.e. returns stock and harvest time series relative to $F_{MSY}$ and  $B_{MSY}$. 
#' @return The object returned depends upon what is being read in.
#' @seealso \code{\link{writeAspic},\link{biodyn}}
#' @export
#' 

#'@section Longwinded Explanation: 
#'  The executable version of ASPIC uses a number of text files i.e.
#'  \describe{
#'   \item{\code{inp}  the input file with data, starting guesses, and run settings and for output,}
#'   \item{\code{bio}  estimated stock and F trajectory for each bootstrap trial,}        
#'   \item{\code{prb}  as \code{bio} but with projection results,}         
#'   \item{\code{rdat} inputs and estimates specially formatted for R, }       
#'   \item{\code{det}  parameter estimates by bootstrap trial.}
#'   \item{\code{fit}  data.frame with fiitted time series.}
#'   }

#' 
#' @examples
#' \dontrun{
#'    dirMy="www.iccat.int/stocka/Models/ASPIC/albs/2011/run2"
#'    
#'    ## list of file types
#'    aspic:::files
#'
#'    is(readAspic(paste(dirMy,"aspicb.inp",sep="/")))    
#'    is(readAspic(paste(dirMy,"aspic.bio",sep="/")))
#'    is(readAspic(paste(dirMy,"aspic_20000.prb",sep="/")))
#'    is(readAspic(paste(dirMy,"aspic.det",sep="/")))
#'    is(readAspic(paste(dirMy,"aspic.rdat",sep="/")))
#'    }
setMethod("readAspic",  signature(object="character"),   
           function(object,relative=TRUE,...)  .readAspic(object,relative,...))

#' \code[writeAspic} Writes the ASPIC text input file \cod{inp} to a file or connection.
#'
#' @param object; of type \code{aspic}
#' @seealso \code{\link{readAspic},\link{aspic:::files}}
#' @export
#' @note
#' The executable version of ASPIC uses an input file, this method generates that file
#' 
#' @examples
#' \dontrun{
#'     data(asp)
#'     writeAspic(asp,"aspic.inp")}
#'     
setMethod("writeAspic",    signature(object="aspic"),       function(object,index=object@index,what="FIT",niter=1,fl="aspic.inp",...)        .writeAspicInp(object,index,what,niter,fl=fl,...))

utils::globalVariables(c("aspicCtl"))

checkFile=function(x){
  
  ln=tolower(scan(x,nlines=1,what=character(),sep="\n"))

  if (any(maply(data.frame(pattern=c("trial","loss","msy","bmsy","brel","frel"),stringsAsFactors=F), grep, x=ln)>0))
    return("det")
  
  }
  
.writeAspicInp<-function(object,index=object@index,what="FIT",niter=ifelse(what=="FIT",1,501),fl="aspic.inp"){
   dgts=options()$digits
   options(digits=22)
   
   dmmy=expand.grid(year=min(as.numeric(as.character(index$year))):max(as.numeric(as.character(index$year))),
                    name=unique(index$name))[,2:1]
  
   u=merge(dmmy,index,all=TRUE,sort=FALSE)   
   u$index[is.na(u$index)]=-9999
   u$catch[is.na(u$catch)]=0
      
    comment=rep("",22)
    comment[ 1]= "\n"                                                                                               
    comment[ 2]= "\n"                                                                                               
    comment[ 3]= "\n"                                                                                                            
    comment[ 4]= "\t212 ## Verbosity\n"                                                                                                                  
    comment[ 5]= "\t## Number of bootstrap trials, <= 1000\n"                                                                                     
    comment[ 6]= "\t## 0=no MC search, 1=search, 2=repeated srch; N trials\n"                                                                  
    comment[ 7]= "\t## Convergence crit. for simplex\n"                                                                                      
    comment[ 8]= "\t## Convergence crit. for restarts, N restarts\n"                                                                      
    comment[ 9]= "\t## Conv. crit. for F; N steps/yr for gen. model\n"                                                                    
    comment[10]= "\t## Maximum F when cond. on yield\n"                                                                                          
    comment[11]= "\t## Stat weight for B1>K as residual (usually 0 or 1)\n"                                                                         
    comment[12]= "\t## Number of fisheries (data series)\n"                                                                                           
    comment[13]= "\t## Statistical weights for data series\n"      
    comment[14]= "\t## B1/K (starting guess, usually 0 to 1)\n"                                                                                 
    comment[15]= "\t## MSY (starting guess)\n"                                                                                               
    comment[16]= "\t## K (carrying capacity) (starting guess)\n"                                                                             
    comment[17]= "\t## q (starting guesses -- 1 per data series)\n"
    comment[18]= "\t## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)\n"                                                   
    comment[19]= "\t## Min and max constraints -- MSY\n"                                                                         
    comment[20]= "\t## Min and max constraints -- K\n"                                                                           
    comment[21]= "\t## Random number seed\n"                                                                                                    
    comment[22]= "\t## Number of years of data in each series\n" 

    # search    trials   simplex  restarts nrestarts    effort    nsteps      maxf 
    #1e+00     1e+05     1e-08     3e-08     6e+00     1e-04     0e+00     8e+00 
    cat(what                                             ,comment[ 1],file=fl,append=FALSE)
    cat("FLR generated"                                  ,comment[ 2],file=fl,append=TRUE)
    cat(ac(object@model), ac(object@conditioning), ac(object@obj)  ,comment[ 3],file=fl,append=TRUE)
    cat(                                                  comment[ 4],file=fl,append=TRUE)
    cat(niter                                            ,comment[ 5],file=fl,append=TRUE)
    cat(as.integer(object@options[c("search","trials")])             ,comment[ 6],file=fl,append=TRUE)
    cat(object@options["simplex"]                        ,comment[ 7],file=fl,append=TRUE)
    cat(object@options["restarts"],as.integer(object@options["nrestarts"]),comment[ 8],file=fl,append=TRUE)
    cat(object@options["effort"],object@options["nsteps"],comment[ 9],file=fl,append=TRUE)
    cat(object@options["maxf"]                           ,comment[10],file=fl,append=TRUE)
    cat(0                                                ,comment[11],file=fl,append=TRUE)
    cat(dim(object@control)[1]-3                         ,comment[12],file=fl,append=TRUE)
    cat(object@control[-(1:3),"lambda"]                  ,comment[13],file=fl,append=TRUE)
    cat(object@control["b0",  "val"]                     ,comment[14],file=fl,append=TRUE)
    cat(object@control["msy", "val"]                     ,comment[15],file=fl,append=TRUE)
    cat(object@control["k",   "val"]                     ,comment[16],file=fl,append=TRUE)
    cat(object@control[-(1:3),"val"]                     ,comment[17],file=fl,append=TRUE)
    cat(object@control[      ,"fit"]                     ,comment[18],file=fl,append=TRUE)
    cat(object@control["msy",c("min","max")]             ,comment[19],file=fl,append=TRUE)
    cat(object@control["k",  c("min","max")]             ,comment[20],file=fl,append=TRUE)
    cat(as.integer(object@rnd)                           ,comment[21],file=fl,append=TRUE)
 
    cat(daply(u,.(name), with, length(name)),comment[22],file=fl,append=TRUE)
    d_ply(u,.(name), function(x) {
      cat(as.character(unique(x$name)),"\n",file=fl,append=TRUE)
      cat("CC\n",file=fl,append=TRUE)
      cat(apply(x[,c("year","index","catch")],1,paste, collapse=" "),sep="\n",file=fl,append=TRUE)})
    
#     l_ply(index, 
#         function(idx){
#             cat(name(idx)                                       ,"\n",file=fl,append=TRUE)
#             cat(type(idx)                                       ,"\n",file=fl,append=TRUE)
#             
#             mm=model.frame(FLQuants(col2=index(idx),col3=catch.n(idx)),drop=T)
#             mm[is.na(mm)]=-1
#                  
#             cat(paste(t(apply(as.matrix(mm),1,paste,collapse=" ")),"\n"),file=fl,append=TRUE)})

   
    options(digits=dgts)
   
    return()}


# ggplotFL/R/diags.R
# 
# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC, Laurie Kell, ICCAT
# $Id:  $

## local function to calculated expected QQ line
qqLine <- function(x,y){ 
  qtlx <- quantile(x, prob=c(0.25,0.75), na.rm=T)
  qtly <- quantile(y, prob=c(0.25,0.75), na.rm=T)
  
  a <- (qtly[1]- qtly[2]) / (qtlx[1] - qtlx[2])
  b <- qtly[1] - qtlx[1] * a
  
  res <- c(a,b)
  
  names(res) <- NULL
  names(res) <- c("a","b")
  
  return(res)}

fnDiags=function(res){
  res$residualLag <- c(res$residual[-1],NA)
  
  qq.     <- qqnorm(res$residual,plot.it=FALSE,na.rm=T)
  res$qqx <- qq.$x
  res$qqy <- qq.$y
  
  qqpar <- qqLine(qq.$x,qq.$y)[c("a","b")]
  
  res$qqHat=qqpar["a"]*res$qqx+qqpar["b"]
  
  res}

files=data.frame(ext =c("inp","bio","prb","rdat","det","prn","fit"),
                      desc=c("Input file with data, starting guesses, and run settings",
                             "Estimated B and F trajectory for each bootstrap trial",
                             "As .bio but with projection results",
                             "Inputs and estimates specially formatted for R",
                             "Estimates from each bootstrap trial",
                             "index fitted and observed",
                             "results in print format"),stringsAsFactor=FALSE)

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))

checkExt=function(x) (tolower(getExt(x)) %in% aspic:::files[,"ext"])

#### Aspic #####################################################################################
.readAspic<-function(x,relative=TRUE){
  
  if (!checkExt(x)) stop(cat("File", x, "does not have a valid extension")) 
  type=getExt(x)

  return(switch(tolower(type),
                "inp" =aspicInp(x),
                "bio" =aspicBio(x,relative),
                "prb" =aspicPrb(x,relative),
                "rdat"=aspicRdat(x),
                "ctl" =aspicCtl(x),
                "det" =aspicDet(x),
                "prn" =aspicPrn(x),
                "fit" =aspicFit(x)))


}

################################################################################
aspicBio =function(file,relative=TRUE){
  t.  <-scan(file,skip=4)
  nits<-scan(file,skip=1,nmax=1)
  yrs <-scan(file,skip=2,nmax=2)
  nyrs<-diff(yrs)
  nval<-nyrs*2+3
  
  yrs <-yrs[1]:yrs[2]
  
  b.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+2,     1:nits,function(x,y=nyrs+1) x:(x+y-1)))],dimnames=list(year=yrs,               iter=1:nits))
  f.  <-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+4,1:nits,function(x,y=nyrs)   x:(x+y-1)))],dimnames=list(year=yrs[-length(yrs)], iter=1:nits))
  
  bmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+1,     1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
  fmsy<-FLQuant(t.[unlist(tapply(((1:nits)-1)*nval+nyrs+3,1:nits,function(x,y=1)      x:(x+y-1)))],dimnames=list(                        iter=1:nits))
  
  if (relative){
    b.=b.%/%bmsy
    f.=f.%/%fmsy}
    
  return(FLQuants(stock=b.,harvest=f.,bmsy=bmsy,fmsy=fmsy))}

aspicPrb =function(file,relative=TRUE){
  ## Stuff
  nits<-scan(file,skip=1,nmax=1)
  yrs <-scan(file,skip=2,nmax=2)
  t.  <-scan(file,skip=4)
  ncol<-yrs[2]-yrs[1]+2
  
  ## stock
  first<-rep((1:nits-1)*ncol*2,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
  b.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))
  if (relative){
    first<-((1:nits-1)*ncol*2)+1
    bmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
    b.   <-sweep(b.,6,bmsy,"/")
    }
  
  ## F
  first<-rep((1:nits-1)*ncol*2+ncol,each=yrs[2]-yrs[1]+1)+(1:(ncol-1))+1
  f.   <-FLQuant(t.[first],dimnames=list(year=yrs[1]:yrs[2],iter=1:nits))[,ac(yrs[1]:(yrs[2]-1))]
  if (relative)
    {
    first<-((1:nits-1)*ncol*2)+ncol+1
    fmsy <-FLQuant(t.[first],dimnames=list(iter=1:nits))
    f.   <-sweep(f.,6,fmsy,"/")
    }
  
  return(FLQuants(harvest=f.,stock=b.))}

aspicRdat=function(file){            
  return(dget(file))}

aspicDet =function(x){  
  det=read.table(x,header=TRUE)
  
  nms=names(det)
  names(det)[seq(length(nms))[nms=="trial"]]="iter"
  names(det)[seq(length(nms))[nms=="b1.k"]]="b0"
  names(det)[seq(length(nms))[nms=="brel"]]="stock"
  names(det)[seq(length(nms))[nms=="frel"]]="harvest"
  
  if (all(c("msy","k") %in% nms) & !("r" %in% nms))  det=transform(det,r=4*msy/k)
  
  dim(det)[2]
  
  det=det[,c(1:8,dim(det)[2],10:dim(det)[2]-1)]
  
  dNms=names(det)
  dNms=gsub("q\\.0","q",dNms)
  dNms=gsub("q\\.","q",dNms)
  
  names(det)=dNms
  
  det}

aspicPrn =function(x){
  #x="/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/aspic/Base case runs/whmrun1bb.prn"
  res=read.table(x,header=TRUE)
  
  res=res[,seq(dim(res)[2]-2)]
  
  obs=melt(res[,seq((dim(res)[2]-1)/2+1)],id.var="year")
  est=melt(res[,c(1,((dim(res)[2]-1)/2+2):dim(res)[2])],id.var="year")
  
  res=data.frame(transform(obs,obs=value,index=gsub(".obs","",obs$variable))[,c("year","index","obs")],
                 hat=est$value)
  
  res$residual=log(res$obs/res$hat)
  
  res=ddply(res,.(index),fnDiags)
  
  names(res)[2]="index"
  
  res}


aspicInp =function(x){

  res=aspic()
  
  aspicC=function(file) {  
    inp=scan(file,sep="\n",what=character())
    ctrl=mlply(inp[5:21], function(x) {
      tmp=strsplit(x," ")
      tmp=unlist(tmp)[nchar(unlist(tmp))>0]})
    ctrl=llply(ctrl,as.numeric)
    ctrl=llply(ctrl,function(x) x[!is.na(x)])
    
    return(ctrl)}
  
  ctrl=suppressWarnings(aspicC(x))
  ops =strsplit(scan(x,sep="\n",what=character(),skip=2,nlines=1), " ")[[1]]
  ops =ops[nchar(ops)>0]
  model(res)      =factor(ops[1])
  res@conditioning=factor(ops[2])
  res@obj         =factor(ops[3])
  
  #  [8] "7  ## Number of fisheries (data series)"                                                                                           
  n     =ctrl[[8]]  
  params=FLPar("b0"=NA,"k"=NA,"msy"=NA)
  parNms=c(c("b0","msy","k"),paste("q",seq(n),sep=""))
  res@params=FLPar(NA,parNms,iter=1)
  
  res@control=FLPar(array(NA,c(length(c(c("b0","msy","k"),paste("q",seq(n),sep=""))),5,1),dimnames=list(params=parNms,c("fit","min","val","max","lambda"),iter=1)))
  
  # [10] "1.00000  ## B1/K (starting guess, usually 0 to 1)"                                                                                 
  res@control["b0", "val"]=ctrl[[10]][1]
  # [11] "3.0000E+04  ## MSY (starting guess)"                                                                                               
  res@control["msy","val"]=ctrl[[11]]
  # [12] "2.6700E+05  ## K (carrying capacity) (starting guess)"                                                                             
  res@control["k", "val"]=ctrl[[12]]
  # [13] "2.1126E-06  6.0195E-06  9.7627E-06  1.4944E-04  2.9980E-06  4.2138E-04  8.3406E-04    ## q (starting guesses -- 1 per data series)"
  res@control[parNms[-(1:3)],"val"]=ctrl[[13]][1:n]
  
  # [14] "0  1  1  1  1  1  1  1  1  1    ## Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)"                                                   
  res@control[,"fit"]=ctrl[[14]]
  
  # [15] "1.0000E+02  1.0000E+07  ## Min and max constraints -- MSY"                                                                         
  res@control["msy",c("min","max")]=ctrl[[15]]
  # [16] "1.0000E+04  2.0000E+07  ## Min and max constraints -- K"                                                                           
  res@control["k",  c("min","max")]=ctrl[[16]]
  
  res@control[parNms[-(1:3)],"min"]=res@control[parNms[-(1:3)],"val"]*0.01  
  res@control[parNms[-(1:3)],"max"]=res@control[parNms[-(1:3)],"val"]*100  
  res@control["b0","min"]=0.01  
  res@control["b0","max"]=1  
  
  #  [9] "1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E-02  1.0000E+00  1.0000E-02    ## Statistical weights for data series"      
  if (length(ctrl[[9]])==0)
    res@control[parNms[-(1:3)],"lambda"][]=1.0 else
      res@control[parNms[-(1:3)],"lambda"]=ctrl[[9]]
  
  # [17] "6745260  ## Random number seed
  res@rnd   =ctrl[[17]]
  #     
  #   #  [3] "1.0000E-08  ## Convergence crit. for simplex"  
  #   if (length(ctrl[[3]])==0) ctrl[[3]]=1.0-08  
  #   res@conv["simplex"]=ctrl[[3]]
  #     
  #   #  [4] "3.0000E-08  6  ## Convergence crit. for restarts, N restarts"                                                                      
  #   res@conv["restart"]=ctrl[[4]][1]
  #   res@nRestart=ctrl[[4]][1]
  #  
  #   #  [5] "1.0000E-04  0  ## Conv. crit. for F; N steps/yr for gen. model"                                                                    
  #   res@conv["F"]=ctrl[[5]][1]
  #   res@nSteps   =ctrl[[5]][1]
  #     
  #   #  [6] "8.0000  ## Maximum F when cond. on yield"                                                                                          
  #   res@maxF    =ctrl[[6]]
  #   #  [7] "0.0  ## Stat weight for B1>K as residual (usually 0 or 1)" 
  #   res@wt      =ctrl[[7]][1]
  
  res@index=readCpue(x,"aspic")
  
  rng       =range(res@index$year)
  names(rng)=c("minyear","maxyear")
  res@range =rng

  return(res)}

aspicPrn =function(x){
  #x="/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/aspic/Base case runs/whmrun1bb.prn"
  res=read.table(x,header=TRUE)
  
  res=res[,seq(dim(res)[2]-2)]
  
  obs=melt(res[,seq((dim(res)[2]-1)/2+1)],id.var="year")
  est=melt(res[,c(1,((dim(res)[2]-1)/2+2):dim(res)[2])],id.var="year")
  
  res=data.frame(transform(obs,obs=value,index=gsub(".obs","",obs$variable))[,c("year","index","obs")],
                 hat=est$value)
  
  res$residual=log(res$obs/res$hat)
  
  names(res)[2:3]=c("name","index")

  res=ddply(res,.(name),fnDiags)
  
  res}

#x="/home/laurie/Desktop/flr/tests/aspic/Inputs/swon/2009/run9/aspic.fit"
aspicFit=function(x){
  txt=str_trim(scan(x,sep="\n",what=as.character()))
  
  start=seq(length(txt))[substr(txt,1,9)=="ESTIMATED"]+5
  end  =seq(length(txt))[substr(txt,1,7)=="RESULTS"]  -3
  
  txt=txt[start:end]
  
  res=as.data.frame(t(matrix(as.numeric(unlist(strsplit(txt," +"))),ncol=end-start+1,nrow=10)))[,-1]
  names(res)=c("year","harvest","biomass","biomassMn","yield","yieldHat","sp","harvestMSY","biomassMSY")
  
  res}