
# This file is a generated template, your changes will not be overwritten

BrawLMClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "BrawLMClass",
    inherit = BrawLMBase,
    private = list(
        .run = function() {

          getGlobals()
          
          if (is.null(self$options$IV) || is.null(self$options$DV)) {
            self$results$reportPlot$setState(NULL)
            return()
          }
          defaults<-getDefaults()
          dataFull<-checkData(self$data)
          tableOutput<-self$results$reportTable$state
          
          DV<-getVariable(self$options$DV,dataFull)    
          DVdata<-DV$data
          participants<-cbind(1:length(DVdata))
          
          IVs<-c()
          for (i in 1:length(self$options$IV)) {
            IVs<-cbind(IVs,getVariable(self$options$IV[i],dataFull))
            if (IVs[,i]$type=="Categorical")
              dataL<-as.factor(IVs[,i]$data)
            else
              dataL<-IVs[,i]$data
            if (i==1) IVdata<-data.frame(dataL)
            else IVdata<-data.frame(IVdata,dataL)
          }

          result<-generalAnalysis(data.frame(participants,DVdata,IVdata))
          
          if (self$options$whichR=="Full") {
            IV<-makeVar(name="Model",type="Interval")
            IV$data<-result$lmRaw$fitted.values
            IV$mu<-mean(IV$data)
            IV$sd<-sd(IV$data)
            IV2<-NULL
            sample<-prepareSample(IV,IV2,DV)
            sample$ivplot<-sample$iv
            sample$dvplot<-sample$dv
            defaults$design$sN<-length(sample$dv)
            
            resultFull<-analyseSample(IV,IV2,DV,
                                      defaults$effect,
                                      defaults$design,
                                      defaults$evidence,
                                  sample)
            resultFull$ivplot<-result$lmRaw$fitted.values
            resultFull$dvplot<-result$lmRaw$fitted.values+result$lmRaw$residuals
            DV$mu<-mean(resultFull$dvplot)
            DV$sd<-sd(resultFull$dvplot)
            DV$type<-"Interval"
            outputGraph<-graphSample(IV,IV2,DV,defaults$effect,defaults$design,defaults$evidence,resultFull)
          } else {
            outputGraph<-plotGLM(DV,IVs,result,self$options$whichR)
          }
          self$results$graphPlot$setState(outputGraph)
          outputText<-reportGLM(DV,IVs,result,self$options$inferWhich)
          self$results$reportPlot$setState(outputText)
          
          
          tableOutput<-rbind(list(AIC=result$AIC,
                                              Rsqr=result$r.full,
                                              model=paste(self$options$IV,collapse="+")
                                          ),
                             tableOutput
          )
          
          ne<-nrow(tableOutput)
          if (ne>15) {
            use1<-which.min(tableOutput$AIC[15:ne])
            use<-c(1:14,use1)
          } else {
            use<-1:ne
          }
          
          for (i in use) {
            self$results$reportTable$setRow(rowNo=i,
                                            values=tableOutput[i,]
            )
          }
          self$results$reportTable$setState(tableOutput)

        },
        
        .plotGraph=function(image, ...) {
          
          outputGraph <- image$state
          if (is.null(outputGraph)) {
            plot<-plotBlankTheme
          } else {
            plot<-outputGraph
          }
          print(plot)
          TRUE
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
