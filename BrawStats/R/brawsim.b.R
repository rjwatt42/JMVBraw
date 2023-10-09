
# This file is a generated template, your changes will not be overwritten

BrawSimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawSimClass",
  inherit = BrawSimBase,
  private = list(
    .run = function() {
      
      a<-self$options$makeValues
      b<-self$options$copyValues
      getGlobals()
      
      # if (!a) {
      #   self$results$reportPlot$setState(NULL)
      # } else {
      
      if (a) {
        defaults<-getDefaults()
        effect<-defaults$effect
        design<-defaults$design
        evidence<-defaults$evidence
        
        DV<-makeVar(self$options$DVname,self$options$DVtype,mu=60,sd=15)
        IV<-makeVar(self$options$IVname,self$options$IVtype,mu=100,sd=15)
        if (self$options$IV2on) {
          IV2<-makeVar(self$options$IV2name,self$options$type,ncats=2,cases="Yes,No")
        } else {
          IV2<-NULL
        }
        
        effect$rIV<-self$options$EffectSize1
        if (!is.null(IV2)) {
          effect$rIV2<-self$options$EffectSize2
          effect$rIVIV2<-self$options$EffectSize3
          effect$rIVIV2DV<-self$options$EffectSize12
        }
        
        design$sN<-self$options$SampleSize
        design$sMethod<-self$options$SampleMethod
        design$sDependence<-self$options$Dependence
        design$sOutliers<-self$options$Outliers
        
        evidence$rInteractionOn<-self$options$doInteraction
        
        sample<-makeSample(IV,IV2,DV,effect,design)
        result<-analyseSample(IV,IV2,DV,
                              effect,design,evidence,
                              sample)
        
        
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
      }
      
      if (b) {
        clipData<-data.frame(iv=sample$iv,iv=sample$iv2,dv=sample$dv)
        write_clip(clipData,allow_non_interactive = TRUE,col.names=FALSE)
      }
      
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
