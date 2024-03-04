
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
        dimension<-"2D"
        showInfer<-"Basic"
      } else dimension<-"1D"
      
      makeMultipleNow<-self$options$makeMultipleBtn
      showMultipleOut<-self$options$showMultiple
      if (showMultipleOut=="2D") {
        dimension<-"2D"
        showMultipleOut<-"Basic"
      } else dimension<-"1D"
      whichShowMultipleOut<-self$options$whichShowMultiple
      
      makeExploreNow<-self$options$makeExploreBtn
      typeExplore<-self$options$typeExplore
      showExploreOut<-self$options$showExplore
      whichShowExploreOut<-self$options$whichShowExplore
      
      makeCopyNow<-self$options$makeCopyBtn
      doClipboard<-self$options$sendClipboard
      doJamovi<-self$options$sendJamovi
      makeCopyNow<-makeCopyNow && (doClipboard || doJamovi)
      
      outputNow<-statusStore$lastOutput
      if (self$options$showHypothesisBtn) outputNow<-"Hypothesis"
      if (self$options$showSampleBtn) outputNow<-"Sample"
      if (self$options$showMultipleBtn) outputNow<-"Multiple"
      if (self$options$showExploreBtn) outputNow<-"Explore"
      
      if (self$options$showExplore != statusStore$showExplore) outputNow<-"Explore"
      if (self$options$showMultiple != statusStore$showMultiple) outputNow<-"Multiple"
      if (self$options$showInfer != statusStore$showInfer) outputNow<-"Sample"
      if (self$options$showSample != statusStore$showSample) outputNow<-"Sample"
      if (self$options$showHypothesis != statusStore$showHypothesis) outputNow<-"Hypothesis"
      
      if (self$options$showSampleBtn && is.null(dataStore$sample)) makeSampleNow<-TRUE
      if (self$options$showMultipleBtn && is.null(dataStore$expectedResult)) makeMultipleNow<-TRUE
      if (self$options$showExploreBtn && is.null(dataStore$exploreResult)) makeExploreNow<-TRUE
      
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
      
      locals$evidence<-makeEvidence(Welch=self$options$Welch,Transform=self$options$Transform)
      
      locals$sample<-dataStore$sample
      locals$analysis<-dataStore$analysis
      locals$expectedResult<-dataStore$expectedResult
      locals$exploreResult<-dataStore$exploreResult
      
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
      
      # did we ask for new explore?
      if (makeExploreNow) {
        numberExplores<-self$options$numberExplores
        appendExplore<-self$options$appendExplore=="yes"
        if (!appendExplore) locals$exploreResult<-NULL
        locals$exploreResult<-makeExplore(nsims=numberExplores,exploreResult=locals$exploreResult,exploreType=typeExplore,
                                          exploreNPoints=self$options$exploreNPoints,
                                          doingNull=self$options$exploreDoingNull=="yes",
                                          max_n=self$options$exploreMaxN,xlog=self$options$exploreNscale,
                                          hypothesis=locals$hypothesis,design=locals$design,evidence=makeEvidence())
        dataStore$exploreResult<-locals$exploreResult
        outputNow<-"Explore"
      }
      h2<-outputNow
      # prepare the variables for being stored 
      
      if (makeCopyNow) {
        nv0<-length(dataStore$savedVariables)
        
        #  convert Categorical values to strings
        if (is.element(IV$type,c("Categorical","Ordinal"))) {
          locals$sample$iv<-as.character(locals$sample$iv)
        }
        if (!is.null(IV2)) {
          if (is.element(locals$IV2$type,c("Categorical","Ordinal"))) {
            locals$sample$iv2<-as.character(locals$sample$iv2)
          }
        }
        if (is.element(DV$type,c("Categorical","Ordinal"))) {
          locals$sample$dv<-as.character(locals$sample$dv)
        }
        
        # get names for the new variables 
        if (is.null(dataStore$iteration) || self$options$appendValues=="no")
          iteration<-0
        else   iteration<-dataStore$iteration+1
        
        if (iteration<1) suffix<-""
        else             suffix<-paste0("_",iteration)
        
        # make the newVariables list
        if (is.null(IV2)) {
          newVariables<-data.frame(locals$sample$iv,locals$sample$dv)
          names(newVariables)<-paste0(c(IV$name,DV$name),suffix)
        } else {
          newVariables<-data.frame(locals$sample$iv,locals$sample$iv2,locals$sample$dv)
          names(newVariables)<-paste0(c(IV$name,IV2$name,DV$name),suffix)
        }
        nv1<-length(newVariables)
        
        # are we copying to clipboard?  
        if (doClipboard) {
          write_clip(newVariables,allow_non_interactive = TRUE,col.names=FALSE)
        }
        
        if (doJamovi) {
          # merge with old variables (if appending)
          if (is.null(dataStore$savedVariables) || self$options$appendValues=="no")
            savedVariables<-newVariables
          else {
            oldNames<-names(dataStore$savedVariables)
            savedVariables<-cbind(dataStore$savedVariables,newVariables)
            names(savedVariables)<-c(oldNames,names(newVariables))
          }
          
          nvars<-length(savedVariables)
          keys<-1:nvars
          measureTypes<-sapply(savedVariables,function(x) { if (is.character(x)) "nominal" else "continuous"})
          
          self$results$sendJamovi$set(keys=keys,names(savedVariables),
                                      descriptions=rep("simulated",nvars),
                                      measureTypes=measureTypes
          )
          for (i in keys) {
            self$results$sendJamovi$setValues(index=i,savedVariables[[i]])
          }
          # update the dataStore
          dataStore$savedVariables<-savedVariables
          dataStore$iteration<-iteration
        }
      }
      
      
      # are we showing the sample?
      if (!is.null(outputNow)) {
        outputText<-NULL
        outputGraph<-NULL
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
                          outputGraph<-showInference(locals$analysis,showType=showInfer,dimension=dimension)
                        }
                 )
               },
               "Multiple"={
                 outputText<-reportExpected(locals$expectedResult,showType=showMultipleOut,effectType=whichShowMultipleOut)
                 outputGraph<-showExpected(locals$expectedResult,showType=showMultipleOut,dimension=dimension,effectType=whichShowMultipleOut)
               },
               "Explore"={
                 outputText<-reportExplore(locals$exploreResult,showType=showExploreOut,effectType=whichShowExploreOut)
                 outputGraph<-showExplore(locals$exploreResult,showType=showExploreOut,effectType=whichShowExploreOut)
               }
        )
        
        # main results graphs/reports
        if (!is.null(outputText))      self$results$reportPlot$setState(outputText)
        if (!is.null(outputGraph))     self$results$graphPlot$setState(outputGraph)
      }
      statusStore$lastOutput<-outputNow
      
      # end of actions      
      statusStore$showHypothesis<-self$options$showHypothesis
      statusStore$showSample<-self$options$showSample
      statusStore$showInfer<-self$options$showInfer
      statusStore$showMultiple<-self$options$showMultiple
      statusStore$showExplore<-self$options$showExplore
      
      # save everything for the next round      
      braw.env$dataStore<-dataStore
      braw.env$statusStore<-statusStore
      
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
