
# This file is a generated template, your changes will not be overwritten

r2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "r2rClass",
    inherit = r2rBase,
    private = list(
        .run = function() {

          getGlobals()
          r<-self$options$rvalue
          sdDV<-self$options$DVsd
          
          msd<-sdDV*r
          sdd<-sqrt(sdDV^2-msd^2)
          md<-msd*2
          
          output<-c(" "," "," "," ",
                    "!jsd(model) = ",format(msd,digits=3)," "," ",
                    "!jsd(residuals) = ",format(sdd,digits=3)," "," ",
                    "!jsd(total) = ",format(sqrt(sdd^2+msd^2),digits=3)," "," ",
                    "-","  "," "," ",
                    "!j        r = ",format(msd/sqrt(sdd^2+msd^2),digits=3)," "," ",
                    "!jCohens d = ",format(md/sdd,digits=3)," "," ",
                    "!jCohens f = ",format(msd/sdd,digits=3)," "," "
          )
          outputText<-list(outputText=output,nc=4,nr=length(output)/4)
          self$results$reportPlot$setState(outputText)
          
          # self$results$sd_text$setContent(paste0("sd(model)=",format(msd,digits=3)
          #                                        ,"\nsd(residuals)=",format(sdd,digits=3)
          #                                        ,"\nsd(total)=",format(sqrt(sdd^2+msd^2),digits=3)))
          # self$results$d_text$setContent(paste0("Cohen's d = ",format(md/sdd,digits=3)))
          # self$results$f_text$setContent(paste0("Cohen's f = ",format(msd/sdd,digits=3)))
          # self$results$r_text$setContent(paste0("        r = ",format(msd/sqrt(sdd^2+msd^2),digits=3)))
          
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
