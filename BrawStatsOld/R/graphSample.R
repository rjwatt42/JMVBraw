graphSample<-function(IV,IV2,DV,effect,design,evidence,result) {
  
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  switch (no_ivs,{
    # g<-g+annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
    g<-drawSample(IV,DV,effect,result)
  },
  { 
    effect1<-effect
    effect2<-effect
    effect2$rIV<-effect2$rIV2
    effect3<-effect
    effect3$rIV<-effect3$rIVIV2
    
    result1<-result
    result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot,nval=result$nval)
    result3<-list(IVs=result$IVs, DVs=result$IV2s, rIV=result$rIVIV2, ivplot=result$ivplot,dvplot=result$iv2plot,nval=result$nval)
    
    g<-g+
      annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect1,result1)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
      annotation_custom(grob=ggplotGrob(drawSample(IV2,DV,effect2,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)+
      annotation_custom(grob=ggplotGrob(drawSample(IV,IV2,effect3,result3)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
  }
  )
  g
}