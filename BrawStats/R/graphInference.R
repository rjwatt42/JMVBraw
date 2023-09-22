graphInference<-function(IV,IV2,DV,effect,design,evidence,result,graphType="2D") {

  # g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
  #   scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  # 
  switch (graphType,
          "EffectSize"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"r")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          },
          "Power"= {
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"w")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"nw")
          },
          "2D"= {
            g1<-draw2Inference(IV,IV2,DV,effect,design,evidence,result,"r","p")
            return(g1)
          },
          "log(lrs)"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrs)")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          },
          "log(lrd)"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrd)")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          }
  )
  # g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4.5,ymin=0,ymax=10)
  # g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5,xmax=10,ymin=0,ymax=10)
  g<-g1
  return(g)
}