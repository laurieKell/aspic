### R code from vignette source 'aspic.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: prelim
###################################################
options(digits=2)

library(aspic)

stampIt=function(...){
   require(plyr)

   args=list(...)
   
   ldply(args, function(x) { res=packageDescription(x)
                            c(Package=x,
                              Version=packageDescription(x)$Version,
                              Date   =packageDescription(x)$Date)})}

smry=stampIt("kobe","aspic")


###################################################
### code chunk number 2: aspic.Rnw:118-133 (eval = FALSE)
###################################################
## library(FLAdvice)
## 
## ### Assessments
## ## 1 file
## aspic=readASPIC(paste(dirAspic,"/",scen=scen[1],".bio",sep=""))
## class(aspic)
## names(aspic)
## 
## aspic=readASPIC(paste(dirAspic,"/",scen=scen[1],".bio",sep=""),data.frame=T)
## class(aspic)
## names(aspic)
## 
## ## many files
## aspics=readASPIC(dirAspic,scen=scen,type="b",data.frame=T)
## 


###################################################
### code chunk number 3: aspic.Rnw:139-153 (eval = FALSE)
###################################################
## #### Projections
## ## 1 file
## prj=readASPIC(paste(dirAspic,"/","bumcont1bproj500",".prj",sep=""))
## class(prj)
## names(prj)
## 
## prj=readASPIC(paste(dirAspic,"/","bumcont1bproj500",".prj",sep="",data.frame))
## class(prj)
## names(prj)
## 
## ## many
## prjs=readASPIC(dirAspic,scen=expand.grid(scen=c("bumcont1bproj","bumhighpproj"),TAC=seq(0,6000,500)))
## class(prjs)
## names(prjs)


###################################################
### code chunk number 4: aspic.Rnw:161-178 (eval = FALSE)
###################################################
## 
## dirInp=paste(system.file(package="aspic"),"extdata",sep="/")
## asp=aspic(paste(dirInp,"albn.inp",sep="/"))
## asp=fit(asp)
## 
## key=data.frame(
##   name   =c("Troll Composite CPUE","JLL Old","JLL Modern","CT Old","CT Modern"), 
##   series=c("I","I","II","I","II"),
##   flag  =c("OT","JA","JA","CT","CT"),
##   gear  =c("TR","LL","LL","LL","LL"))
## 
## dimnames(key)[[1]]=c("Troll Composite CPUE","JLL Old","JLL Modern","CT Old","CT Modern") 
## 
## wts=t(array(c(1,1,1,1,1,
##               1,0,0,0,0,
##               0,1,1,0,0,
##               0,0,0,1,1),c(5,4),list(name=key$name,Scenario=1:4)))


###################################################
### code chunk number 5: aspic.Rnw:183-188 (eval = FALSE)
###################################################
## cpue=subset(diags(asp),!is.na(obs))[,c("year","name","obs")]
## ggplot(aes(year,obs,group=name,col=name),data=cpue)+
##   geom_point()+
##   stat_smooth()+
##   theme_ms(legend.position="bottom")


###################################################
### code chunk number 6: aspic.Rnw:194-210 (eval = FALSE)
###################################################
## library(gam)
## gm  =gam(log(obs)~lo(year)+name,data=cpue)
## cpue=data.frame(cpue,gam=predict(gm),gamRsdl=residuals(gm))
## scl =coefficients(gm)[3:9]
## names(scl)=substr(names(scl),5,nchar(names(scl)))
## cpue=transform(cpue,scl=scl[as.character(name)])
## cpue[is.na(cpue$scl),"scl"]=0
## 
## cpue=cbind(cpue,key[cpue$name,])[,-2]
## cpue$name=factor(cpue$name, levels=c("Troll Composite CPUE","JLL Old","JLL Modern","CT Old","CT Modern"))
## ggplot(cpue)+ geom_line(aes(year,exp(gam)),col="red")  +
##               geom_smooth(aes(year,obs),se=FALSE)      +           
##               geom_point( aes(year,obs,col=name))       +
##               facet_wrap(~name,ncol=1,scale="free_y")  +
##               theme_ms(legend.position="none")         +
##               xlab("Year") + ylab("Index")


###################################################
### code chunk number 7: aspic.Rnw:215-226 (eval = FALSE)
###################################################
## uMat=ddply(cpue,.(name),transform, obs=stdz(obs))
## uMat=cast(uMat,year~name,value="obs")
## uMat=uMat[apply(uMat,1,function(x) !all(is.na(x))),]
## 
## pM=plotmatrix(uMat[,-1])
## pM$layers[[2]]=NULL
## mns=ddply(subset(pM$data,!(is.na(x) & !is.na(y))),.(xvar,yvar), function(x) mean(x$y,na.rm=T))
## pM+geom_hline(aes(yintercept=V1),data=mns,col="red") +
##    geom_smooth(method="lm",se=F)  + 
##    theme(legend.position="bottom")                   +
##    xlab("Index")+ylab("Index")      


###################################################
### code chunk number 8: aspic.Rnw:231-236 (eval = FALSE)
###################################################
## cr=cor(uMat[,-1],use="pairwise.complete.obs")
## dimnames(cr)=list(gsub("_"," ",names(uMat)[-1]),gsub("_"," ",names(uMat)[-1]))
## cr[is.na(cr)]=0
## corrplot(cr,diag=F,order="hclust",addrect=2)  +          
##              theme(legend.position="bottom")  


###################################################
### code chunk number 9: aspic.Rnw:244-245 (eval = FALSE)
###################################################
## asp=fit(asp)


###################################################
### code chunk number 10: aspic.Rnw:251-252 (eval = FALSE)
###################################################
## plot(asp)


###################################################
### code chunk number 11: aspic.Rnw:257-257 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 12: aspic.Rnw:264-264 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 13: aspic.Rnw:270-270 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 14: aspic.Rnw:277-277 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 15: aspic.Rnw:282-282 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 16: aspic.Rnw:304-304 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 17: aspic.Rnw:314-314 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 18: aspic.Rnw:321-321 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 19: aspic.Rnw:328-328 (eval = FALSE)
###################################################
## 


###################################################
### code chunk number 20: aspic.Rnw:337-337 (eval = FALSE)
###################################################
## 


