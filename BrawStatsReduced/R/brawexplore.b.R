
# This file is a generated template, your changes will not be overwritten

BrawExploreClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawExploreClass",
  inherit = BrawExploreBase,
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
      
      makeExploreNow<-self$options$makeExploreBtn
      typeExplore<-self$options$typeExplore
      showExploreOut<-self$options$showExplore
      whichShowExploreOut<-self$options$whichShowExplore
      
      outputNow<-statusStore$lastOutput
      if (self$options$showHypothesisBtn) outputNow<-"Hypothesis"
      if (self$options$showExploreBtn) outputNow<-"Explore"
      
      if (self$options$showExplore != statusStore$showExplore) outputNow<-"Explore"
      if (self$options$showHypothesis != statusStore$showHypothesis) outputNow<-"Hypothesis"
      
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
                                sCheating=self$options$Cheating,sCheatingAttempts=self$options$CheatingAttempts
                                )
      
      locals$evidence<-makeEvidence(Welch=self$options$Welch=="yes",
                                    Transform=self$options$Transform,
                                    shortHand=self$options$shorthand=="yes"
                                    )
      
      locals$exploreResult<-dataStore$exploreResult
      
      # did we ask for new explore?
      if (makeExploreNow) {
        numberExplores<-self$options$numberExplores
        appendExplore<-self$options$appendExplore=="yes"
        if (!appendExplore) locals$exploreResult<-NULL
        locals$exploreResult<-makeExplore(nsims=numberExplores,exploreResult=locals$exploreResult,exploreType=typeExplore,
                                          exploreNPoints=self$options$exploreNPoints,
                                          doingNull=self$options$exploreDoingNull=="yes",
                                          # max_n=self$options$exploreMaxN,xlog=self$options$exploreNscale,
                                          hypothesis=locals$hypothesis,design=locals$design,evidence=locals$evidence)
        dataStore$exploreResult<-locals$exploreResult
        outputNow<-"Explore"
      }

      # are we showing the sample?
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
               "Explore"={
                 outputGraph<-showExplore(locals$exploreResult,showType=showExploreOut,effectType=whichShowExploreOut)
               }
        )
      }
      statusStore$lastOutput<-outputNow
      
      # end of actions      
      statusStore$showHypothesis<-self$options$showHypothesis
      statusStore$showExplore<-self$options$showExplore
      
      # save everything for the next round      
      braw.env$dataStore<-dataStore
      braw.env$statusStore<-statusStore
      
      # main results graphs/reports
      if (!is.null(outputGraph))   {
        self$results$graphPlot$setState(outputGraph)
      }
    },
    
    .plotGraph=function(image, ...) {
      outputGraph <- image$state
      if (!is.null(outputGraph)) {
        y2<-y1
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
