
setClass("R6",
  representation(),
  prototype = prototype()
)

setMethod("asJSON", "R6", function(x, cody = 1, ...) {
  if (R6::is.R6(x)) {
    if(is.function(x$print)) {
      return(asJSON(x$print(...)))
    } else {
      fields = names(x$.__enclos_env__$self) #names(get(class(x))$public_fields)
      to.print = list()

      for(f in fields) {
        val = x[[f]]
        if(!is.function(val) &&!is.environment(val)) {
          to.print[[f]] = val
        }
      }
      return(asJSON(to.print))
    }
  }
})


