
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

          if (DV$type=="Categorical") {
            output<-c(" "," "," "," ",
                      "!jdeviance(model) = ",format(result$rawModel$deviance,digits=3)," "," ",
                      "!jdeviance(null) = ",format(result$rawModel$null.deviance,digits=3)," "," ",
                      "!chisqr = ",format(result$rawModel$null.deviance-result$rawModel$deviance,digits=3)," "," ",
                      "-","  "," "," "
            )
          } else {
            output<-c(" "," "," "," ",
              "!jsd(model) = ",format(sd(result$rawModel$fitted.values),digits=3)," "," ",
              "!jsd(residuals) = ",format(sd(result$rawModel$residuals),digits=3)," "," ",
              "!jsd(total) = ",format(sd(result$dv),digits=3)," "," ",
              "-","  "," "," "
            )
          }
          
          r<-result$rFull
          rci<-result$rFullCI
          rse<-result$rFullse
          d<- 2*r/sqrt(1-r^2)
          dci<-2*rci/sqrt(1-rci^2)
          dse<-rse*((2*r^2)/(1 - r^2)^(3/2) + 2/(1 - r^2)^(1/2))
          output<-c(output,
            "!j        r = ",
            paste0(format(r,digits=report_precision),"+/-",format(rse,digits=report_precision)),
            paste0("CI: (",format(rci[1],digits=report_precision),",",format(rci[2],digits=report_precision),")"),
            " ",
            "!jCohens d = ",
            paste0(format(d,digits=report_precision),"+/-",format(dse,digits=report_precision)),
            paste0("CI: (",format(dci[1],digits=report_precision),",",format(dci[2],digits=report_precision),")"),
            " ",
            "!jCohens f = ",
            paste0(format(d/2,digits=report_precision),"+/-",format(dse/2,digits=report_precision)),
            paste0("CI: (",format(dci[1]/2,digits=report_precision),",",format(dci[2]/2,digits=report_precision),")"),
            " "
          )
          outputText<-list(outputText=output,nc=4,nr=length(output)/4)
          
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
