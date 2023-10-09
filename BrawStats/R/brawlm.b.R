
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
          dataFull<-self$data
          
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

          outputText<-reportGLM(DV,IVs,result,self$options$inferWhich)
          self$results$reportPlot$setState(outputText)
          outputGraph<-plotGLM(DV,IVs,result,self$options$whichR)
          self$results$graphPlot$setState(outputGraph)
          

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
