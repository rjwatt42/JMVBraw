
# This file is a generated template, your changes will not be overwritten

BrawAnClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawAnClass",
  inherit = BrawAnBase,
  private = list(
    .run = function() {
      BrawOpts()
      
      if (is.null(self$options$IV) || is.null(self$options$DV)) {
        self$results$reportPlot$setState(NULL)
        return()
      }
      
      if (!is.element(self$options$IV,names(self$data)) || !is.element(self$options$DV,names(self$data))) {
        self$results$reportPlot$setState(NULL)
        return()
      }

      defaults<-getDefaults()
      effect<-defaults$effect
      design<-defaults$design
      evidence<-defaults$evidence
      evidence$rInteractionOn<-self$options$doInteraction
      
      dataFull<-self$data
      
      DV<-getVariable(self$options$DV,dataFull)
      IV<-getVariable(self$options$IV[1],dataFull)
      if (length(self$options$IV)>1) {
        IV2<-getVariable(self$options$IV[2],dataFull)
      } else {
        IV2<-NULL
      }
      
      sample<-prepareSample(IV,IV2,DV)
      design$sN<-length(sample$dv)
      
      result<-analyseSample(IV,IV2,DV,
                            effect,
                            design,
                            evidence,
                            sample)
      
      switch(self$options$show,
             "Sample"={
               outputText<-reportSample(IV,IV2,DV,design,result)
               outputGraph<-graphSample(IV,IV2,DV,effect,design,evidence,result)
             },
             "Describe"={
               outputText<-reportDescription(IV,IV2,DV,evidence,result)
               outputGraph<-graphDescription(IV,IV2,DV,effect,design,evidence,result)
             },
             "Infer"={
               outputText<-reportInference(IV,IV2,DV,effect,evidence,result)
               outputGraph<-graphInference(IV,IV2,DV,effect,design,evidence,result,self$options$inferWhich)
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
