
# This file is a generated template, your changes will not be overwritten

log2rClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "log2rClass",
    inherit = log2rBase,
    private = list(
        .run = function() {

          getGlobals()
          usingShiny<<-FALSE
          
          rsqr<-self$options$rsqr
          r<-sqrt(rsqr)
          f<-r/sqrt(1-r^2)

          output<-c(" "," "," "," ",
                    "!jsd(model) = ",format(r,digits=3)," "," ",
                    "!jsd(residuals) = ",format(sqrt(1-rsqr),digits=3)," "," ",
                    "!jsd(total) = ",format(1,digits=3)," "," ",
                    "-","  "," "," ",
                    "!j        r = ",format(r,digits=3)," "," ",
                    "!jCohens d = ",format(2*f,digits=3)," "," ",
                    "!jCohens f = ",format(f,digits=3)," "," "
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
        })
)
