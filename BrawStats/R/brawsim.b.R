
# This file is a generated template, your changes will not be overwritten

BrawSimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "BrawSimClass",
  inherit = BrawSimBase,
  private = list(
    .run = function() {
      
# set up global variables
      getGlobals()
      
# get some flags for later
      makeSample<-self$options$makeValues
      doCopy<-self$options$copyValues
      doSend<-self$options$sendValues
      show<-self$options$show

      doShow<-(show!="None")
      doStore<-(doCopy || doSend)
      
      # get the stored data
      dataStore<-self$results$tableStore$state
      
      # debug information
      # self$results$debug$setContent(TRUE)
      # self$results$debug$setVisible(TRUE)
      
# make all the standard things we need
      locals<-getDefaults()
      
      # variables first
      locals$DV<-makeVar(self$options$DVname,self$options$DVtype,
                  mu=self$options$DVmu,sd=self$options$DVsd,skew=self$options$DVskew,kurtosis=self$options$DVkurt,
                  ncats=self$options$DVncats,proportions=self$options$DVprops,
                  nlevs=self$options$DVnlevs,iqr=self$options$DViqr)
      locals$IV<-makeVar(self$options$IVname,self$options$IVtype,
                  mu=self$options$IVmu,sd=self$options$IVsd,skew=self$options$IVskew,kurtosis=self$options$IVkurt,
                  ncats=self$options$IVncats,proportions=self$options$IVprops,
                  nlevs=self$options$IVnlevs,iqr=self$options$IViqr)
      if (self$options$IV2on) {
        locals$IV2<-makeVar(self$options$IV2name,self$options$IV2type,
                     mu=self$options$IV2mu,sd=self$options$IV2sd,skew=self$options$IV2skew,kurtosis=self$options$IV2kurt,
                     ncats=self$options$IV2ncats,proportions=self$options$IV2props,
                     nlevs=self$options$IV2nlevs,iqr=self$options$IV2iqr)
      } else {
        locals$IV2<-NULL
      }
      
      locals$effect$rIV<-self$options$EffectSize1
      if (!is.null(locals$IV2)) {
        locals$effect$rIV2<-self$options$EffectSize2
        locals$effect$rIVIV2<-self$options$EffectSize3
        locals$effect$rIVIV2DV<-self$options$EffectSize12
      }
      
      locals$design$sN<-self$options$SampleSize
      locals$design$sMethod<-self$options$SampleMethod
      locals$design$sDependence<-self$options$Dependence
      locals$design$sOutliers<-self$options$Outliers
      
      locals$evidence$rInteractionOn<-self$options$doInteraction

      locals$sample<-dataStore$sample
      
# did we ask for a new sample?
      if (makeSample) {
        # make a sample
        locals$sample<-makeSample(locals$IV,locals$IV2,locals$DV,locals$effect,locals$design)
        # store it 
        dataStore$sample<-locals$sample
        
        # and list the first line of the data in the table
        # we don't make this visible, except for testing
        if (is.null(locals$IV2)) {
          tableData<-cbind(participant=locals$sample$participant[1],iv=locals$sample$iv[1],iv2=0,dv=locals$sample$dv[1])
        } else {
          tableData<-cbind(participant=locals$sample$participant[1],iv=locals$sample$iv[1],iv2=locals$sample$iv2[1],dv=locals$sample$dv[1])
        }
        self$results$tableStore$setRow(rowNo=1,values=tableData)
      }

      # if we haven't got a sample, then do nothing more
      if (!length(locals$sample$iv)>0) {
        return()
      }

# prepare the variables for being stored 
      if (doStore) {
        #  convert Categorical values to strings
        if (is.element(locals$IV$type,c("Categorical","Ordinal"))) {
          locals$sample$iv<-as.character(locals$sample$iv)
        }
        if (!is.null(locals$IV2)) {
          if (is.element(locals$IV2$type,c("Categorical","Ordinal"))) {
            locals$sample$iv2<-as.character(locals$sample$iv2)
          }
        }
        if (is.element(locals$DV$type,c("Categorical","Ordinal"))) {
          locals$sample$dv<-as.character(locals$sample$dv)
        }
        
        # get names for the new variables 
        if (is.null(dataStore$iteration) || !self$options$appendValues)
          iteration<-0
        else   iteration<-dataStore$iteration+1
        
        if (iteration<1) suffix<-""
        else             suffix<-paste0("_",iteration)
        
        # make the newVariables list
        if (is.null(locals$IV2)) {
          newVariables<-list(locals$sample$iv,locals$sample$dv)
          names(newVariables)<-paste0(c(locals$IV$name,locals$DV$name),suffix)
        } else {
          newVariables<-list(locals$sample$iv,locals$sample$iv2,locals$sample$dv)
          names(newVariables)<-paste0(c(locals$IV$name,locals$IV2$name,locals$DV$name),suffix)
        }
        
        # merge with old variables (if appending)
        if (is.null(dataStore$savedVariables) || !self$options$appendValues)
          savedVariables<-newVariables
        else {
          oldNames<-names(dataStore$savedVariables)
          savedVariables<-cbind(dataStore$savedVariables,newVariables)
          names(savedVariables)<-c(oldNames,names(newVariables))
        }
        
# are we copying to clipboard?      
        if (doCopy) {
          write_clip(savedVariables,allow_non_interactive = TRUE,col.names=FALSE)
        }
        
# are we sending to Jamovi?      
        if (doSend) {
          nvars<-length(savedVariables)
          keys<-1:nvars
          measureTypes<-sapply(savedVariables,function(x) { if (is.character(x)) "nominal" else "continuous"})
          
          self$results$sendValues$set(keys=keys,names(savedVariables),
                                      descriptions=rep("simulated",nvars),
                                      measureTypes=measureTypes
          )
          for (i in keys) {
            self$results$sendValues$setValues(index=i,savedVariables[[i]])
          }
        }
      
        # update the dataStore
        dataStore$savedVariables<-savedVariables
        dataStore$iteration<-iteration
      }

            
# save everything for the next round      
      self$results$tableStore$setState(dataStore)
      self$results$tableStore$setVisible(FALSE)
      
      # are we showing the sample?
      # if so, we'll need an analysis
      if (doShow) {
        # analyze the sample
        locals$result<-analyseSample(locals$IV,locals$IV2,locals$DV,
                                     locals$effect,locals$design,locals$evidence,
                                     locals$sample)
        
        # prepare the output results
        switch(show,
               "Sample"={
                 outputText<-reportSample(locals$IV,locals$IV2,locals$DV,locals$design,locals$result)
                 outputGraph<-graphSample(locals$IV,locals$IV2,locals$DV,locals$effect,locals$design,locals$evidence,locals$result)
               },
               "Describe"={
                 outputText<-reportDescription(locals$IV,locals$IV2,locals$DV,locals$evidence,locals$result)
                 outputGraph<-graphDescription(locals$IV,locals$IV2,locals$DV,locals$effect,locals$design,locals$evidence,locals$result)
               },
               "Infer"={
                 outputText<-reportInference(locals$IV,locals$IV2,locals$DV,locals$effect,locals$evidence,locals$result)
                 outputGraph<-graphInference(locals$IV,locals$IV2,locals$DV,locals$effect,locals$design,locals$evidence,locals$result,"2D")
               }
        )
        
        # main results graphs/reports
        self$results$reportPlot$setState(outputText)
        self$results$reportPlot$setVisible(TRUE)

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
      } # end of showing sample
# end of actions      
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
      
      outputText <- image$state
      if (!is.null(outputText)) {
        plot<-reportPlot(outputText$outputText,outputText$nc,outputText$nr)        
        print(plot)
        TRUE
      } else  {
        FALSE
      }
    }
  )
)
