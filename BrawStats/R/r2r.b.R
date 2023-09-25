
# This file is a generated template, your changes will not be overwritten

r2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "r2rClass",
    inherit = r2rBase,
    private = list(
        .run = function() {

          r<-self$options$rvalue
          sdDV<-self$options$DVsd
          
          msd<-sdDV*r
          sdd<-sqrt(sdDV^2-msd^2)

          self$results$sd_text$setContent(paste0("sd(model)=",format(msd,digits=3)
                                                 ,"\nsd(residuals)=",format(sdd,digits=3)
                                                 ,"\nsd(total)=",format(sqrt(sdd^2+msd^2),digits=3)))
          self$results$d_text$setContent(paste0("Cohen's d = ",format(2*msd/sdd,digits=3)))
          self$results$f_text$setContent(paste0("Cohen's f = ",format(msd/sdd,digits=3)))
          self$results$r_text$setContent(paste0("        r = ",format(msd/sqrt(sdd^2+msd^2),digits=3)))
          # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
