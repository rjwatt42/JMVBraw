
# This file is a generated template, your changes will not be overwritten

ESnormClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ESnormClass",
    inherit = ESnormBase,
    private = list(
        .run = function() {

          getGlobals()

          if (is.null(self$options$IV) || is.null(self$options$DV)) {
            self$results$reportPlot$setState(NULL)
            return()
          }
          defaults<-getDefaults()
          dataFull<-self$data

                    
          IV<-getVariable(self$options$IV,dataFull)     
          IV2<-NULL
          DV<-getVariable(self$options$DV,dataFull)          

          dataHold<-list(participant=1:length(IV$data),iv=IV$data,iv2=IV$data*0,dv=DV$data)
          result<-analyseSample(IV,IV2,DV,defaults$effect,
                                            defaults$design,
                                            defaults$evidence,
                                 dataHold)

          r<-result$rFull
          rci<-result$rFullCI
          rse<-result$rFullse
          d<- 2*r/sqrt(1-r^2)
          dci<-2*rci/sqrt(1-rci^2)
          dse<-rse*(2*r^2)/(1 - r^2)^(3/2) + 2/(1 - r^2)^(1/2)
          outputText<-list(outputText=c("Effect size (r):",
                                        paste0(format(r,digits=report_precision),"+/-",format(rse,digits=report_precision)),
                                        paste0("CI: (",format(rci[1],digits=report_precision),",",format(rci[2],digits=report_precision),")"),
                                        
                                        "Effect size (d):",
                                        paste0(format(d,digits=report_precision),"+/-",format(dse,digits=report_precision)),
                                        paste0("CI: (",format(dci[1],digits=report_precision),",",format(dci[2],digits=report_precision),")")
          ),
                           nc=3,nr=2)
                   
          self$results$reportPlot$setState(outputText)
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
