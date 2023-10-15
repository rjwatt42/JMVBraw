
# https://docs.jamovi.org/_pages/api_overview.html

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
          dataFull<-checkData(self$data)

          DV<-getVariable(self$options$DV,dataFull)
          IV<-getVariable(self$options$IV[1],dataFull)
          if (length(self$options$IV)>1) {
            IV2<-getVariable(self$options$IV[2],dataFull)
          } else {
            IV2<-NULL
          }
          
          defaults$evidence$rInteractionOn<-self$options$doInteraction
          
          sample<-prepareSample(IV,IV2,DV)
          result<-analyseSample(IV,IV2,DV,
                                defaults$effect,
                                defaults$design,
                                defaults$evidence,
                                sample)
          # data<-dataFull[[self$options$IV[1]]]
          # outputText<-list(outputText=sort(levels(data))[1],nc=1,nr=1)
          # self$results$reportPlot$setState(outputText)
          
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
                   outputGraph<-graphInference(IV,IV2,DV,defaults$effect,defaults$design,defaults$evidence,result,self$options$inferWhich)
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
