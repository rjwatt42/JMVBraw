
# This file is a generated template, your changes will not be overwritten

BrawSimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawSimClass",
  inherit = BrawSimBase,
  private = list(
    .run = function() {
      
      makeSample<-self$options$makeValues
      doCopy<-self$options$copyValues
      doSend<-self$options$sendValues
      
      getGlobals()
      # we haven't done anything yet
      # if (!a && is.null(self$results$tableStore$state)) {
      #   # self$results$reportPlot$setState(NULL)
      #   return()
      # }
      
      # make all the standard things we need
      defaults<-getDefaults()
      
      # variables first
      DV<-makeVar(self$options$DVname,self$options$DVtype,
                  mu=self$options$DVmu,sd=self$options$DVsd,skew=self$options$DVskew,kurtosis=self$options$DVkurt,
                  ncats=self$options$DVncats,proportions=self$options$DVprops,
                  nlevs=self$options$DVnlevs,iqr=self$options$DViqr)
      IV<-makeVar(self$options$IVname,self$options$IVtype,
                  mu=self$options$IVmu,sd=self$options$IVsd,skew=self$options$IVskew,kurtosis=self$options$IVkurt,
                  ncats=self$options$IVncats,proportions=self$options$IVprops,
                  nlevs=self$options$IVnlevs,iqr=self$options$IViqr)
      if (self$options$IV2on) {
        IV2<-makeVar(self$options$IV2name,self$options$IV2type,
                     mu=self$options$IV2mu,sd=self$options$IV2sd,skew=self$options$IV2skew,kurtosis=self$options$IV2kurt,
                     ncats=self$options$IV2ncats,proportions=self$options$IV2props,
                     nlevs=self$options$IV2nlevs,iqr=self$options$IV2iqr)
      } else {
        IV2<-NULL
      }
      
      effect<-defaults$effect
      effect$rIV<-self$options$EffectSize1
      if (!is.null(IV2)) {
        effect$rIV2<-self$options$EffectSize2
        effect$rIVIV2<-self$options$EffectSize3
        effect$rIVIV2DV<-self$options$EffectSize12
      }
      
      design<-defaults$design
      design$sN<-self$options$SampleSize
      design$sMethod<-self$options$SampleMethod
      design$sDependence<-self$options$Dependence
      design$sOutliers<-self$options$Outliers
      
      evidence<-defaults$evidence
      evidence$rInteractionOn<-self$options$doInteraction
      
      sample<-list(iv=c())
      # did we ask for it?
      if (makeSample) {
        # make a sample
        sample<-makeSample(IV,IV2,DV,effect,design)
        sampleChanged<-TRUE
      } else {
        sampleChanged<-FALSE
        # we get the old data 
        dataStore<-self$results$tableStore$state
        # have we got an old one
        if (!is.null(dataStore)) {
          sample$participant<-dataStore$participant
          sample$iv<-dataStore$iv
          sample$iv2<-dataStore$iv2
          sample$dv<-dataStore$dv
          sample$ivplot<-dataStore$ivplot
          sample$iv2plot<-dataStore$iv2plot
          sample$dvplot<-dataStore$dv
        }
      }
      # self$results$debug$setContent(paste0("g = ",length(CatCatcols)))
      # self$results$debug$setVisible(TRUE)
      
      if (length(sample$iv)>0) {
        
        result<-analyseSample(IV,IV2,DV,
                              effect,design,evidence,
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
        
        # main results graphs/reports
        self$results$reportPlot$setState(outputText)
        if (length(outputGraph)==1) {
          self$results$graphPlot$setState(outputGraph[[1]])
          self$results$graphPlot$setVisible(TRUE)
          self$results$graphPlot1$setVisible(FALSE)
          self$results$graphPlot2$setVisible(FALSE)
          self$results$graphPlot3$setVisible(FALSE)
        } 
        if (length(outputGraph)==2) {
          self$results$graphPlot1$setState(outputGraph[[1]])
          self$results$graphPlot2$setState(outputGraph[[2]])
          self$results$graphPlot1$setVisible(TRUE)
          self$results$graphPlot2$setVisible(TRUE)
          self$results$graphPlot$setVisible(FALSE)
          self$results$graphPlot3$setVisible(FALSE)
        } 
        if (length(outputGraph)==3) {
          self$results$graphPlot1$setState(outputGraph[[1]])
          self$results$graphPlot2$setState(outputGraph[[2]])
          self$results$graphPlot3$setState(outputGraph[[3]])
          self$results$graphPlot1$setVisible(TRUE)
          self$results$graphPlot2$setVisible(TRUE)
          self$results$graphPlot3$setVisible(TRUE)
          self$results$graphPlot$setVisible(FALSE)
        }
        
        # save the data in the table as well
        if (is.null(IV2)) {
          dataStore<-cbind(sample$participant,sample$iv,sample$iv*0,sample$dv,sample$ivplot,sample$ivplot*0,sample$dvplot)
        } else {
          dataStore<-cbind(sample$participant,sample$iv,sample$iv2,sample$dv,sample$ivplot,sample$iv2plot,sample$dvplot)
        }
        for (i in 1) {
          data<-list(participant=dataStore[i,1],
                     iv=dataStore[i,2],iv2=dataStore[i,3],dv=dataStore[i,4],
                     ivplot=dataStore[i,5],iv2plot=dataStore[i,6],dvplot=dataStore[i,7])
          self$results$tableStore$setRow(rowNo=i,values=data)
        }
        self$results$tableStore$setState(sample)
        self$results$tableStore$setVisible(TRUE)
      }
      
      if (is.element(IV$type,c("Categorical","Ordinal"))) {
        sample$iv<-as.character(sample$iv)
      }
      if (!is.null(IV2)) {
        if (is.element(IV2$type,c("Categorical","Ordinal"))) {
          sample$iv2<-as.character(sample$iv2)
        }
      }
      if (is.element(DV$type,c("Categorical","Ordinal"))) {
        sample$dv<-as.character(sample$dv)
      }
      
      if (is.null(IV2)) {
        dataStore<-data.frame(sample$iv,sample$dv)
        names(dataStore)<-c(IV$name,DV$name)
      } else {
        dataStore<-data.frame(sample$iv,sample$iv2,sample$dv)
        names(dataStore)<-c(IV$name,IV2$name,DV$name)
      }
      # prefix<-as.character(ceiling(runif(1)*100))
      # titles<-sapply(titles,function(x){paste0(prefix,x)})
      
      if (doCopy && length(sample$iv)>0) {
        write_clip(dataStore,allow_non_interactive = TRUE,col.names=FALSE)
      }
      
      nvars<-length(dataStore)
      keys<-1:nvars
      measureTypes<-sapply(dataStore,function(x) { if (is.character(x)) "nominal" else "continuous"})
      if (doSend && length(sample$iv)>0) {
        self$results$sendValues$set(keys=keys,titles=names(dataStore),
                                    descriptions=rep("simulated",nvars),
                                    measureTypes=measureTypes
        )
        for (i in keys)
          self$results$sendValues$setValues(index=i,dataStore[[i]])
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
