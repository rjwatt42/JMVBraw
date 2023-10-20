
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
      
      makeSample<-TRUE

      doKeep<-TRUE
      doShow<-(show!="None")
      
      if (!doShow) {
        self$results$graphPlot$setVisible(FALSE)
        self$results$graphPlot1$setVisible(FALSE)
        self$results$graphPlot2$setVisible(FALSE)
        self$results$graphPlot3$setVisible(FALSE)
        self$results$reportPlot$setVisible(FALSE)
      }
      
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
      
      # get the stored data
      dataStore<-self$results$tableStore$state
      
      # get the sample
      locals$sample<-dataStore$sample
      
      # did we ask for a new sample?
      if (makeSample) {
        # make a sample
        locals$sample<-makeSample(locals$IV,locals$IV2,locals$DV,locals$effect,locals$design)
        dataStore$sample<-locals$sample
      }

      # if we haven't got a sample, then do nothing more
      # otherwise proceed and analyze the sample
      if (length(locals$sample$iv)>0) {
        # analyse the sample
        locals$result<-analyseSample(locals$IV,locals$IV2,locals$DV,
                              locals$effect,locals$design,locals$evidence,
                              locals$sample)
        
        # get the results
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
        # self$results$debug$setContent(paste0("class graph = ",class(outputGraph)))
        # self$results$debug$setVisible(TRUE)
        
        if (doShow) {
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
        }
        
        # list the first line of the data in the table as well
        if (is.null(locals$IV2)) {
          tableData<-cbind(participant=locals$sample$participant[1],iv=locals$sample$iv[1],iv2=0,dv=locals$sample$dv[1])
        } else {
          tableData<-cbind(participant=locals$sample$participant[1],iv=locals$sample$iv[1],iv2=locals$sample$iv2[1],dv=locals$sample$dv[1])
        }
        self$results$tableStore$setRow(rowNo=1,values=tableData)

        if (doKeep) {
          # prepare the variables for being stored 
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
          
          if (is.null(dataStore$iteration))  iteration<-0
            else                             iteration<-dataStore$iteration
          
          if (self$options$appendValues) iteration<-iteration+1
          if (iteration<=1) suffix<-""
          else              suffix<-paste0("-",iteration)
          
          if (is.null(locals$IV2)) {
            savedVariable<-list(locals$sample$iv,locals$sample$dv)
            names(savedVariable)<-paste0(c(locals$IV$name,locals$DV$name),suffix)
          } else {
            savedVariable<-list(locals$sample$iv,locals$sample$iv2,locals$sample$dv)
            names(savedVariable)<-paste0(c(locals$IV$name,locals$IV2$name,locals$DV$name),suffix)
          }
          
          if (is.null(dataStore$savedVariables) || !self$options$appendValues)
            savedVariables<-savedVariable
          else
            savedVariables<-cbind(dataStore$savedVariables,savedVariable)
          
          if (doCopy) {
            write_clip(savedVariables,allow_non_interactive = TRUE,col.names=FALSE)
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
          dataStore$savedVariables<-savedVariables
          dataStore$iteration<-iteration
        }
      }
      self$results$tableStore$setState(dataStore)
      self$results$tableStore$setVisible(FALSE)
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
