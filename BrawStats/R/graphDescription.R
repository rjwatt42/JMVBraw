graphDescription<-function(IV,IV2,DV,effect,design,evidence,result) {
  
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  switch (no_ivs, 
          {
            g<-drawDescription(IV,NULL,DV,effect,design,result)
          },
          { 
            if (evidence$rInteractionOn==FALSE){
              effect2<-effect
              effect2$rIV<-effect2$rIV2
              
              result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
              
              g<-g+
                annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
                annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
            }
            else{
              if (showInteractionOnly){
                if (DV$type=="Categorical") {
                  if (IV2$type=="Interval") {
                    effect1<-effect
                    result1<-result
                    use<-result1$iv2<median(result$iv2)
                    result1$iv<-result$iv[use]
                    result1$dv<-result$dv[use]
                    result1$IVs$vals<-result$iv[use]
                    result1$DVs$vals<-result$dv[use]
                    
                    effect2<-effect
                    result2<-result
                    result2$iv<-result$iv[!use]
                    result2$dv<-result$dv[!use]
                    result2$IVs$vals<-result$iv[!use]
                    result2$DVs$vals<-result$dv[!use]
                    
                    g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,">",format(median(result$iv2),digits=3)))),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                    g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect2,design,result2)+gridTheme+ggtitle(paste0(IV2$name,"<",format(median(result$iv2),digits=3)))),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                  } else {
                    switch (IV2$ncats,
                            {},
                            {xmin<-c(0.5,5.5)
                            xmax<-c(4.5,9.5)
                            ymin<-c(0,0)
                            ymax<-c(5,5)},
                            {xmin<-c(0.5,5.5,3)
                            xmax<-c(4.5,9.5,7)
                            ymin<-c(0,0,5)
                            ymax<-c(4.25,4.25,9.25)},
                            {xmin<-c(0.5,5.5,0.5,5.5)
                            xmax<-c(4.5,9.5,4.5,9.5)
                            ymin<-c(0,0,5,5)
                            ymax<-c(4.25,4.25,9.25,9.25)},
                            {}
                    )
                    for (i in 1:IV2$ncats) {
                      effect1<-effect
                      result1<-result
                      use<-result1$iv2<-as.numeric(result$iv2)==i
                      result1$iv<-result$iv[use]
                      result1$dv<-result$dv[use]
                      result1$IVs$vals<-result$iv[use]
                      result1$DVs$vals<-result$dv[use]
                      
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,"==",IV2$cases[i]))),xmin=xmin[i],xmax=xmax[i],ymin=ymin[i],ymax=ymax[i])
                    }
                  }
                  # effect2<-effect
                  # result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
                } else {
                  g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
                }
              } else{
                effect2<-effect
                effect2$rIV<-effect2$rIV2
                
                result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, iv=result$iv, dv=result$dv, ivplot=result$iv2plot,dvplot=result$dvplot)
                
                g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
              }
            }
          }
  )
  return(g)
}