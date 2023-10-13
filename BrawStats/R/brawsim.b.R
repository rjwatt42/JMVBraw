
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
      
      dataStore<-self$results$tableStore$state
      
      sample<-list(iv=c())
      # did we ask for it?
      if (makeSample) {
        # make a sample
        sample<-makeSample(IV,IV2,DV,effect,design)
        dataStore$sample<-sample
      } else {
        # we get the old data 
        oldSample<-dataStore$sample
        # have we got an old one
        if (!is.null(oldSample)) {
          sample$participant<-oldSample$participant
          sample$iv<-oldSample$iv
          sample$iv2<-oldSample$iv2
          sample$dv<-oldSample$dv
          sample$ivplot<-oldSample$ivplot
          sample$iv2plot<-oldSample$iv2plot
          sample$dvplot<-oldSample$dv
        }
      }
      # self$results$debug$setContent(paste0("g = ",length(CatCatcols)))
      # self$results$debug$setVisible(TRUE)
      
      # if we haven't got a sample, then do nothing more
      if (length(sample$iv)>0) {
        # analyse the sample
        result<-analyseSample(IV,IV2,DV,
                              effect,design,evidence,
                              sample)
        
        # show the results
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
          tableData<-cbind(sample$participant,sample$iv,sample$iv*0,sample$dv,sample$ivplot,sample$ivplot*0,sample$dvplot)
        } else {
          tableData<-cbind(sample$participant,sample$iv,sample$iv2,sample$dv,sample$ivplot,sample$iv2plot,sample$dvplot)
        }
        for (i in 1) {
          data<-list(participant=tableData[i,1],
                     iv=tableData[i,2],iv2=tableData[i,3],dv=tableData[i,4],
                     ivplot=tableData[i,5],iv2plot=tableData[i,6],dvplot=tableData[i,7])
          self$results$tableStore$setRow(rowNo=i,values=data)
        }
      
      if (is.null(dataStore$iteration)) {
        iteration<-1
        suffix<-""
      }
      else {
        iteration<-dataStore$iteration+1
        suffix<-paste0("-",iteration)
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
        savedVariable<-data.frame(sample$iv,sample$dv)
        names(savedVariable)<-paste0(c(IV$name,DV$name),suffix)
      } else {
        savedVariable<-data.frame(sample$iv,sample$iv2,sample$dv)
        names(savedVariable)<-paste0(c(IV$name,IV2$name,DV$name),suffix)
      }
      if (is.null(dataStore$savedVariables))
        savedVariables<-savedVariable
      else
        savedVariables<-cbind(dataStore$savedVariables,savedVariable)
      
      if (doCopy) {
        write_clip(savedVariable,allow_non_interactive = TRUE,col.names=FALSE)
      }
      
      if (doSend) {
        
        nvars<-length(savedVariables)
        keys<-1:nvars
        measureTypes<-sapply(savedVariables,function(x) { if (is.character(x)) "nominal" else "continuous"})
        
        self$results$sendValues$set(keys=keys,names(savedVariables),
                                    descriptions=rep("simulated",nvars),
                                    measureTypes=measureTypes
        )
        for (i in keys)
          self$results$sendValues$setValues(index=i,savedVariables[[i]])
      }
      
      self$results$tableStore$setState(list(sample=dataStore$sample,savedVariables=savedVariables,iteration=iteration))
      self$results$tableStore$setVisible(FALSE)
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
