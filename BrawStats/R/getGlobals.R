getGlobals<-function() {
  ################################
  # ui design
  
  fontScale=1.0
  char3D=1.3
  
  GraphsOnly<-FALSE
  
  maincoloursBL<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF",graphBack="#999999")
  maincoloursBW<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF",graphBack="#999999")
  maincolours<<-maincoloursBL
  
  plotcolours<<-list(maineffectES="#FFCC00",covariationES="#FF1100",interactionES="#0011FF",
                    sampleC="#FFCC00",descriptionC="#FF9955",
                    descriptionC1="#FF5533",descriptionC2="#CCBB33",
                    infer_sigC="#11CC00",infer_nsigC="#FF4400",infer_none="#AAAAAA",
                    infer_sigNonNull="#11CC00",infer_isigNonNull="#881100",infer_nsNonNull="#881100",infer_nsdNonNull="#CCCCCC",
                    infer_sigNull="#118800",infer_isigNull="#FF4400",infer_nsNull="#FF4400",infer_nsdNull="#CCCCCC",
                    psig="#FFAA00",alpha="#44FF22",one="#FF4422",
                    fdr="#227700",fmr="#BB5555")
  
  shapes<<-list(data=21,study=22,parameter=21,meta=24)
  
  report_precision<<-3
  graph_precision<<-2
  labelSize<<-4
  nchars<<-80
  
  # graph themes
  mainTheme=theme(panel.background = element_rect(fill=maincolours$graphBack, colour="black"),
                  panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                  plot.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC))
  SMplotTheme=theme(plot.title=element_text(size=14,face="bold"),axis.title=element_text(size=16,face="bold"),
                    axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
  LGplotTheme=theme(plot.title=element_text(size=21,face="bold"),axis.title=element_text(size=24,face="bold"),
                    axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))
  
 # top right bottom left
  plotTheme<<-mainTheme+SMplotTheme+theme(plot.margin=margin(1.0,1.5,0.5,0.5,"cm"))
  reportTheme<<-mainTheme+SMplotTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))
  pplotTheme<<-mainTheme+SMplotTheme+theme(plot.margin=margin(0.15,0.8,0,0.25,"cm"))
  
  plotBlankTheme<<-theme(panel.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                       panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                       plot.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                       axis.title=element_text(size=16,face="bold")
  )
  
  gridTheme<<-theme(plot.margin=margin(0,0,0,0,"cm"))
  
  #########################
  # display choices
  
  z_range<<-1.5
  r_range<<-0.975
  w_range<<-c(0.05,0.99)
  w_range<<-c(0.05,1)
  fullRange<<-3
  nNpoints<<-51
  worldNPoints<<-51
  varNPoints<<-101
  
  reportGroupMeans<<-TRUE
  allScatter<<-"all"
  showMedians<<-FALSE
  minN<<-10
  maxRandN<<-5 # times mean sample size
  
  plotDescriptionCols<<-c()
  CatCatcols<<-c()
  
  points_threshold<<-50 # for displaying expected results
  wPlotScale<<-"log10"
  pPlotScale<<-"log10"
  nPlotScale<<-"log10"
  RZ<<-"r"
  
  STMethod<<-"NHST"
  alphaSig<<-0.05

  useSignificanceCols<<-TRUE
  showInteractionOnly<<-TRUE
  
  switches<<-list(doLikelihoodInfer=FALSE)
  
}
