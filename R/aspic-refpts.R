#object=asp[[1]]

refpts<-function(object){
  msy=fmsy(object)*bmsy(object)
  dimnames(msy)$params="msy"
  rbind(msy,
        bmsy(object),
        fmsy(object))}
