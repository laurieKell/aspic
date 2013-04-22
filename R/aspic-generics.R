setGeneric('aspic',        function(object,...)   standardGeneric('aspic'))

# setGeneric('diagPlot',     function(object,...)   standardGeneric('diagPlot'))
# 
# setGeneric('survey',       function(object,...)   standardGeneric('survey'))
# setGeneric('index',         function(object,...)   standardGeneric('index'))
# 
setGeneric("readAspic",    function(object,...)   standardGeneric('readAspic'))
setGeneric("writeAspic",   function(object,...)   standardGeneric('writeAspic'))
# 
# setGeneric("index<-",       function(object,value,...) standardGeneric('index<-'))

#if (!isGeneric("fwd"))       setGeneric("fwd",      function(object, ctrl, ...) standardGeneric("fwd"))
#if (!isGeneric("hcr"))       setGeneric("hcr",      function(object, ctrl, ...) standardGeneric("hcr"))
#if (!isGeneric("tac"))       setGeneric("tac",      function(object, ctrl, ...) standardGeneric("tac"))

setGeneric('boot',  function(object,...)     standardGeneric('boot'))
setGeneric('jk',    function(object,...)     standardGeneric('jk'))


