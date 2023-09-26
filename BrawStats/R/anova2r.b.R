
# This file is a generated template, your changes will not be overwritten

anova2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "anova2rClass",
    inherit = anova2rBase,
    private = list(
        .run = function() {

          
          n1<-self$options$group1sz
          m1<-self$options$group1mn
          sd1<-self$options$group1sd
          
          n2<-self$options$group2sz
          m2<-self$options$group2mn
          sd2<-self$options$group2sd
          
          n3<-self$options$group3sz
          m3<-self$options$group3mn
          sd3<-self$options$group3sd
          
          if (n1==0 || n2==0) {
            self$results$r_text$setContent("waiting...")
            return()
          }
          
          mn<-(m1*n1+m2*n2+m3*n3)/(n1+n2+n3)
          msd<-sqrt( (n1*(m1-mn)^2 + n2*(m2-mn)^2 + n3*(m3-mn)^2)/(n1+n2+n3))
          sdd<-sqrt( (n1*sd1^2 + n2*sd2^2 + n3*sd3^2 )/(n1+n2+n3))
          
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
