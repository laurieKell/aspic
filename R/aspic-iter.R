if (!isGeneric("iter")) setGeneric("iter", function(obj, ...)
  standardGeneric("iter"))

setMethod("iter", signature(obj="aspic"),
          function(obj, iter) {
            
            # copy the iterate into the new slots
            names. <- c(getSlotNamesClass(obj, 'FLArray'),getSlotNamesClass(obj, 'FLPar'))
            for(s. in names.)
            {
              if(dims(slot(obj, s.))$iter == 1)
                slot(obj, s.) <- iter(slot(obj, s.), 1)
              else
                slot(obj, s.) <- iter(slot(obj, s.), iter)
            }
            
            
          if ("iter" %in% names(obj@index)){
          flag=obj@index$iter %in% iter
          obj@index=obj@index[flag,]}
          
          return(obj)})
            