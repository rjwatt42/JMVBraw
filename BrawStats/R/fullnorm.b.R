
# This file is a generated template, your changes will not be overwritten

BrawStatsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "BrawStatsClass",
    inherit = BrawStatsBase,
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
          result$ivplot<-IV$plot
          result$dvplot<-DV$plot
          result$IVs<-IV
          result$DVs<-DV
          
          switch(self$options$show,
                 "Sample"={
                   outputText<-reportSample(IV,IV2,DV,defaults$design,result)
                   outputGraph<-graphSample(IV,IV2,DV,defaults$effect,defaults$design,defaults$evidence,result)
                 },
                 "Describe"={
                   outputText<-reportDescription(IV,IV2,DV,defaults$evidence,result)
                   outputGraph<-graphDescription(IV,IV2,DV,defaults$effect,defaults$design,defaults$evidence,result)
                 },
                 "Infer"={
                   outputText<-reportInference(IV,IV2,DV,defaults$effect,defaults$evidence,result)
                   outputGraph<-graphInference(IV,IV2,DV,defaults$effect,defaults$design,defaults$evidence,result)
                 }
          )
          self$results$reportPlot$setState(outputText)
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
