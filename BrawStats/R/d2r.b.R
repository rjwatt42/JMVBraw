
# This file is a generated template, your changes will not be overwritten

d2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "d2rClass",
    inherit = d2rBase,
    private = list(
        .run = function() {

          n1<-self$options$group1sz
          m1<-self$options$group1mn
          sd1<-self$options$group1sd
          
          n2<-self$options$group2sz
          m2<-self$options$group2mn
          sd2<-self$options$group2sd
          
          if (n1==0 || n2==0) {
            self$results$r_text$setContent("waiting...")
            return()
          }

          md<-m1-m2
          sdd<-sqrt( ((n1)*sd1^2 + (n2)*sd2^2 )/(n1+n2))
          self$results$d_text$setContent(paste0("Cohen's d = ",format(md/sdd,digits=3)))
          
          mn<-(m1*n1+m2*n2)/(n1+n2)
          msd<-sqrt( (n1*(m1-mn)^2 + n2*(m2-mn)^2)/(n1+n2))
          sdd<-sqrt( (n1*sd1^2 + n2*sd2^2 )/(n1+n2))
          self$results$r_text$setContent(paste0("        r = ",format(msd/sqrt(sdd^2+msd^2),digits=3)))
          # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
