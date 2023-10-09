
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
