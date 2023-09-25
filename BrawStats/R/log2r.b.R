
# This file is a generated template, your changes will not be overwritten

log2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "log2rClass",
    inherit = log2rBase,
    private = list(
        .run = function() {

          rsqr<-self$options$rsqr

          self$results$r_text$setContent(paste0("Rsquare = ",format(rsqr,digits=3)))
          
          r<-sqrt(rsqr)
          self$results$r_text$setContent(paste0("        r = ",format(r,digits=3)))
          
          # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
