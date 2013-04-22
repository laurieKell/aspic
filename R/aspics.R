#' aspics list 
#' 
#' @description A class that contains a list of ASPIC biomass dynamic stock assessment model classes
#' @return aspics object
#' @export
#' @examples
#' \dontrun{aspics()}
setClass("aspics",
   representation(
      "FLlst"))

      setGeneric('aspics', function(object, ...)
  	standardGeneric('aspics'))

#' constructs a list of aspic Biomass Dynamic Model Classes
#'
#' @description A class that contains a list of ASPIC biomass dynamic stock assessment model classes
#' @return aspics object
#' @export
#' @param  object which can be missing, a list of aspic or a sigle aspic object
#' @examples
#' \dontrun{aspics()}
# setMethod("aspics", signature(object="aspic"), function(object, ...) {
#     lst <- c(object, list(...))
#     aspics(lst)
# })

setMethod("aspics", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
	  	new("aspics")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('aspics',  c(list(object=object), args))
	  }
  }
)

setMethod("aspics", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="aspics", .Data=object, names=names),
      args[!names(args) %in% 'names'])

    return(
      do.call('new', args)
      )}) 

setMethod("aspics", signature(object="character"),
          function(object) {
           
          aspics(mlply(object,aspic))})
