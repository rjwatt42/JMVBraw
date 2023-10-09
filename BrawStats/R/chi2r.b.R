
# This file is a generated template, your changes will not be overwritten

chi2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "chi2rClass",
    inherit = chi2rBase,
    private = list(
        .run = function() {

          getGlobals()
          usingShiny<<-FALSE
          
          chisqr<-self$options$chisqr
          n1<-self$options$group1sz
          n2<-self$options$group2sz
          
          if (n1==0 || n2==0) {
            self$results$reportPlot$setState(NULL)
            return()
          }

          ncorrection<-max(n1,n2)/min(n1,n2)
          r<-sqrt(chisqr/(n1+n2)/ncorrection)
          
          f<-r/sqrt(1-r^2)
          
          output<-c(" "," "," "," ",
                    "!jsd(model) = ",brawFormat(r,digits=3)," "," ",
                    "!jsd(residuals) = ",brawFormat(sqrt(1-r^2),digits=3)," "," ",
                    "!jsd(total) = ",brawFormat(1,digits=3)," "," ",
                    "-","  "," "," ",
                    "!j        r = ",brawFormat(r,digits=3)," "," ",
                    "!jCohens d = ",brawFormat(2*f,digits=3)," "," ",
                    "!jCohens f = ",brawFormat(f,digits=3)," "," "
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
          })
)
