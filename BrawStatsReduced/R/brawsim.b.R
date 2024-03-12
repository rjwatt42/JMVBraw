
# This file is a generated template, your changes will not be overwritten

BrawSimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawSimClass",
  inherit = BrawSimBase,
  private = list(
    .run = function() {
      # debug information
      # self$results$debug$setVisible(TRUE)
      # self$results$debug$setContent(c(self$options$showExploreBtn,is.null(dataStore$exploreResult)))
      
      # initialization code
      if (is.null(braw.env$dataStore)) {
        # set up global variables
        BrawOpts(fontScale = 1.35)
        dataStore<-list(sample=NULL,
                        expectedResult=NULL,
                        exploreResult=NULL,
                        iteration=NULL,
                        savedVariables=NULL
        )
        braw.env$dataStore<-dataStore
        statusStore<-list(lastOutput="Hypothesis",
                          showHypothesis="Hypothesis",
                          showSample="Sample",
                          showInfer="Basic",
                          showMultiple="Basic",
                          showExplore="r"
        )
        braw.env$statusStore<-statusStore
      } 
      
      # get the stored data
      dataStore<-braw.env$dataStore
      statusStore<-braw.env$statusStore
      h1<-statusStore$lastOutput
      
      # get some flags for later
      
      showHypothesisOut<-self$options$showHypothesis
      
      makeSampleNow<-self$options$makeSampleBtn
      showSampleOut<-self$options$showSample
      showInfer<-self$options$showInfer
      if (showInfer=="2D") {
        dimensionInfer<-"2D"
        showInfer<-"Basic"
      } else dimensionInfer<-"1D"
      
      makeMultipleNow<-self$options$makeMultipleBtn
      showMultipleOut<-self$options$showMultiple
      if (showMultipleOut=="2D") {
        dimensionMultiple<-"2D"
        showMultipleOut<-"Basic"
      } else dimensionMultiple<-"1D"
      whichShowMultipleOut<-self$options$whichShowMultiple

      outputNow<-statusStore$lastOutput
      if (self$options$showHypothesisBtn) outputNow<-"Hypothesis"
      if (self$options$showSampleBtn) outputNow<-"Sample"
      if (self$options$showMultipleBtn) outputNow<-"Multiple"

      if (self$options$showMultiple != statusStore$showMultiple) outputNow<-"Multiple"
      if (self$options$showInfer != statusStore$showInfer) outputNow<-"Sample"
      if (self$options$showSample != statusStore$showSample) outputNow<-"Sample"
      if (self$options$showHypothesis != statusStore$showHypothesis) outputNow<-"Hypothesis"
      
      if (self$options$showSampleBtn && is.null(dataStore$sample)) makeSampleNow<-TRUE
      if (self$options$showMultipleBtn && is.null(dataStore$expectedResult)) makeMultipleNow<-TRUE

      # make all the standard things we need
      locals<-list(hypothesis=NULL,design=NULL,evidence=NULL,
                   sample=NULL,analysis=NULL,explore=NULL)
      
      DV<-makeVariable(self$options$DVname,self$options$DVtype,
                       mu=self$options$DVmu,sd=self$options$DVsd,skew=self$options$DVskew,kurtosis=self$options$DVkurt,
                       ncats=self$options$DVncats,proportions=self$options$DVprops,
                       nlevs=self$options$DVnlevs,iqr=self$options$DViqr)
      IV<-makeVariable(self$options$IVname,self$options$IVtype,
                       mu=self$options$IVmu,sd=self$options$IVsd,skew=self$options$IVskew,kurtosis=self$options$IVkurt,
                       ncats=self$options$IVncats,proportions=self$options$IVprops,
                       nlevs=self$options$IVnlevs,iqr=self$options$IViqr)
      if (self$options$IV2on) {
        IV2<-makeVariable(self$options$IV2name,self$options$IV2type,
                          mu=self$options$IV2mu,sd=self$options$IV2sd,skew=self$options$IV2skew,kurtosis=self$options$IV2kurt,
                          ncats=self$options$IV2ncats,proportions=self$options$IV2props,
                          nlevs=self$options$IV2nlevs,iqr=self$options$IV2iqr)
      } else {
        IV2<-NULL
      }
      
      effect<-makeEffect(rIV=self$options$EffectSize1,
                         rIV2=self$options$EffectSize2,
                         rIVIV2<-self$options$EffectSize3,
                         rIVIV2DV<-self$options$EffectSize12,
                         world=makeWorld(worldOn=self$options$WorldOn,
                                         populationPDF=self$options$WorldPDF,
                                         populationRZ = self$options$WorldRZ,
                                         populationPDFk = self$options$Worldk,
                                         populationNullp = self$options$WorldNullP
                         )
      )
      
      locals$hypothesis<-makeHypothesis(IV,IV2,DV,effect)
      
      locals$design<-makeDesign(sN=self$options$SampleSize,
                                sMethod=makeSampling(self$options$SampleMethod),
                                sIV1Use=self$options$SampleUsage1,
                                sIV2Use=self$options$SampleUsage2,
                                sDependence=self$options$Dependence,
                                sOutliers=self$options$Outliers,
                                sCheating=self$options$Cheating,sCheatingAttempts=self$options$CheatingAttempts)
      
      locals$evidence<-makeEvidence(Welch=self$options$Welch=="yes",Transform=self$options$Transform)
      
      locals$sample<-dataStore$sample
      locals$analysis<-dataStore$analysis
      locals$expectedResult<-dataStore$expectedResult

      # did we ask for a new sample?
      if (makeSampleNow) {
        # make a sample
        locals$sample<-makeSample(locals$hypothesis,locals$design)
        dataStore$sample<-locals$sample
        # if we haven't got a sample, then do nothing more
        if (!length(locals$sample$iv)>0) {
          return()
        }
        outputNow<-"Sample"
      }
      
      # did we ask for new multiples?
      if (makeMultipleNow) {
        numberSamples<-self$options$numberSamples
        appendMultiple<-self$options$appendMultiple=="yes"
        if (!appendMultiple) locals$expectedResult<-NULL
        locals$expectedResult<-makeExpected(nsims=numberSamples,expectedResult=locals$expectedResult,
                                            doingNull=self$options$multipleDoingNull=="yes",
                                            hypothesis=locals$hypothesis,design=locals$design,evidence=makeEvidence())
        dataStore$expectedResult<-locals$expectedResult
        outputNow<-"Multiple"
      }

      # are we showing the sample?
      outputText<-NULL
      outputGraph<-NULL
      if (!is.null(outputNow)) {
        switch(outputNow,
               "Hypothesis"={
                 switch(showHypothesisOut,
                        "Hypothesis"=outputGraph<-showHypothesis(locals$hypothesis,doWorld=TRUE),
                        "Design"=    outputGraph<-showDesign(locals$design),
                        "Population"=outputGraph<-showPopulation(locals$hypothesis),
                        "Prediction"=outputGraph<-showPrediction(locals$hypothesis,locals$design)
                 )
               },
               "Sample"={
                 switch(showSampleOut,
                        "Sample"={
                          outputText<-reportSample(locals$sample)
                          outputGraph<-showSample(locals$sample)
                        },
                        "Describe"={
                          locals$analysis<-makeAnalysis(locals$sample,locals$evidence)
                          outputText<-reportDescription(locals$analysis)
                          outputGraph<-showDescription(locals$analysis)
                        },
                        "Infer"={
                          locals$analysis<-makeAnalysis(locals$sample,locals$evidence)
                          outputText<-reportInference(locals$analysis)
                          outputGraph<-showInference(locals$analysis,showType=showInfer,dimension=dimensionInfer)
                        }
                 )
               },
               "Multiple"={
                 outputText<-reportExpected(locals$expectedResult,showType=showMultipleOut)
                 outputGraph<-showExpected(locals$expectedResult,showType=showMultipleOut,dimension=dimensionMultiple,effectType=whichShowMultipleOut)
               }
        )
      }
      statusStore$lastOutput<-outputNow
      
      # end of actions      
      statusStore$showHypothesis<-self$options$showHypothesis
      statusStore$showSample<-self$options$showSample
      statusStore$showInfer<-self$options$showInfer
      statusStore$showMultiple<-self$options$showMultiple

      # save everything for the next round      
      braw.env$dataStore<-dataStore
      braw.env$statusStore<-statusStore
      
      
      # main results graphs/reports
      if (!is.null(outputText))      self$results$reportPlot$setState(outputText)
      if (!is.null(outputGraph))     self$results$graphPlot$setState(outputGraph)
    },
    
    .plotGraph=function(image, ...) {
      outputGraph <- image$state
      if (!is.null(outputGraph)) {
        print(outputGraph)
        TRUE
      } else {
        FALSE
      }
    },
    
    .plotReport=function(image, ...) {
      outputReport <- image$state
      if (!is.null(outputReport)) {
        print(outputReport)
        TRUE
      } else  {
        FALSE
      }
    }
  )
)
