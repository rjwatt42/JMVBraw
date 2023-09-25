
# This file is a generated template, your changes will not be overwritten

chi2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "chi2rClass",
    inherit = chi2rBase,
    private = list(
        .run = function() {

          chisqr<-self$options$chisqr
          n1<-self$options$group1sz
          n2<-self$options$group2sz
          
          if (n1==0 || n2==0) {
            self$results$r_text$setContent("waiting...")
            return()
          }
          
          self$results$chi_text$setContent(paste0("chisquare = ",format(chisqr,digits=3)))
          
          ncorrection<-max(n1,n2)/min(n1,n2)
          r<-sqrt(chisqr/(n1+n2)/ncorrection)
          self$results$r_text$setContent(paste0("        r = ",format(r,digits=3)))
          
          # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
