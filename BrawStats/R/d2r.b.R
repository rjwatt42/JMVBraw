
# This file is a generated template, your changes will not be overwritten

d2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "d2rClass",
    inherit = d2rBase,
    private = list(
        .run = function() {

          getGlobals()
          
          n1<-self$options$group1sz
          m1<-self$options$group1mn
          sd1<-self$options$group1sd
          
          n2<-self$options$group2sz
          m2<-self$options$group2mn
          sd2<-self$options$group2sd
          
          if (sd1==0 || sd2==0) {
            self$results$reportPlot$setState(NULL)
            return()
          }

          md<-m2-m1
          sdd<-sqrt( ((n1)*sd1^2 + (n2)*sd2^2 )/(n1+n2))
          
          mn<-(m1*n1+m2*n2)/(n1+n2)
          msd<-sqrt( (n1*(m1-mn)^2 + n2*(m2-mn)^2)/(n1+n2))
          # sdd<-sqrt( (n1*sd1^2 + n2*sd2^2 )/(n1+n2))
          
          output<-c(" "," "," "," ",
                    "!jsd(model) = ",brawFormat(msd,digits=3)," "," ",
                    "!jsd(residuals) = ",brawFormat(sdd,digits=3)," "," ",
                    "!jsd(total) = ",brawFormat(sqrt(sdd^2+msd^2),digits=3)," "," ",
                    "-","  "," "," ",
                    "!j        r = ",brawFormat(msd/sqrt(sdd^2+msd^2),digits=3)," "," ",
                    "!jCohens d = ",brawFormat(md/sdd,digits=3)," "," ",
                    "!jCohens f = ",brawFormat(msd/sdd,digits=3)," "," "
          )
          outputText<-list(outputText=output,nc=4,nr=length(output)/4)
          self$results$reportPlot$setState(outputText)
          
          # self$results$sd_text$setContent(paste0("sd(model)=",brawFormat(msd,digits=3)
          #                                        ,"\nsd(residuals)=",brawFormat(sdd,digits=3)
          #                                        ,"\nsd(total)=",brawFormat(sqrt(sdd^2+msd^2),digits=3)))
          # self$results$d_text$setContent(paste0("Cohen's d = ",brawFormat(md/sdd,digits=3)))
          # self$results$f_text$setContent(paste0("Cohen's f = ",brawFormat(msd/sdd,digits=3)))
          # self$results$r_text$setContent(paste0("        r = ",brawFormat(msd/sqrt(sdd^2+msd^2),digits=3)))

        },
        .plotReport=function(image, ...) {
          
          outputText <- image$state
          if (is.null(outputText)) {
            plot<-plotBlankTheme
          } else {
            plot<-reportPlot(outputText$outputText,outputText$nc,outputText$nr)        
          }
          print(plot)
          TRUE
        }
        
        )
)
