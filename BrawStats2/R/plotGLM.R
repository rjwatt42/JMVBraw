plotGLM<-function(DV,IVs,result,whichR) {
  
  switch(whichR,
         "Direct"={
           r<-result$r.direct
           p<-result$p.direct
         },
         "Unique"={
           r<-result$r.unique
           p<-result$p.unique
         },
         "Total"={
           r<-result$r.total
           p<-result$p.total
         }
  )
  
  fontSize<-labelSize*1.1
  g<-ggplot()
  g<-g+geom_label(data=data.frame(x=0,y=0,label=DV$name),aes(x=x,y=y,label=label),
                  size=fontSize,fontface="bold",
                  label.size=0.25,label.padding=unit(0.5,"lines"))
  xStart<-4+nchar(DV$name)/2*(fontSize/14)
  arrowLength<-4-1
  yRange<-max(sum(r>0),sum(r<0))/2
  
  if (any(r>0)) {
    use<-which(r>0)
    useOrder<-order(r[use])
    use<-use[useOrder]
    y<-seq(0.65,100,1.3)
    if (y[length(use)]>3*2.5) y<-y-(3*2.5-y[length(use)])
    # y<-seq(1,-1,length.out=length(use))*yRange
    for (i in 1:length(use)) {
      if (p[use[i]]<alphaSig) {
        col<-"#00BB00"
        colArrow<-"#009900"
        fill<-"white"
      } else {
        col<-"black"
        fill<-"darkgrey"
        colArrow<-"darkgrey"
      }
      g<-g+geom_label(data=data.frame(x=-xStart,y=y[i],label=IVs[,use[i]]$name),aes(x=x,y=y,label=label),
                      size=fontSize,fontface="bold",col=col,fill=fill,hjust=1,
                      label.size=0.25,label.padding=unit(0.5,"lines"))
      direction<-atan2(y[i]*0.9,arrowLength+0.25)
      pts<-drawArrow(start=c(-(xStart-0.5),y[i]),arrowLength/cos(direction),direction=direction*180/pi,width=r[use[i]]/2,ends="last")
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),color=NA,fill=colArrow, lwd=0.5)
    }
  }
  
  if (any(r<0)) {
    use<-which(r<0)
    useOrder<-order(-r[use])
    use<-use[useOrder]
    y<-seq(-0.65,-100,-1.3)
    if (y[length(use)]< -3*2.5) y<-y+(-3*2.5-y[length(use)])
    # y<-seq(1,-1,length.out=length(use))*yRange
    for (i in 1:length(use)) {
      if (p[use[i]]<alphaSig) {
        col<-"red"
        colArrow<-"#990000"
        fill<-"white"
      } else {
        col<-"black"
        fill<-"darkgrey"
        colArrow<-"darkgrey"
      }
      g<-g+geom_label(data=data.frame(x=xStart,y=y[i],label=IVs[,use[i]]$name),aes(x=x,y=y,label=label),
                      size=fontSize,fontface="bold",col=col,fill=fill,hjust=0,
                      label.size=0.25,label.padding=unit(0.5,"lines"))
      direction<-atan2(y[i]*0.9,arrowLength+0.25)
      pts<-drawArrow(start=c(xStart-0.5,y[i]),arrowLength/cos(direction),direction=180-direction*180/pi,width=abs(r[use[i]]/2),ends="last")
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),color=NA,fill=colArrow, lwd=0.5)
    }
  }
  
  g<-g+coord_fixed(1,xlim = c(-1,1)*5*2.5, ylim = c(-1,1)*3*2.5)
  g<-g+labs(x="  ",y="  ")+reportTheme+theme(legend.position = "none")
  g<-g+theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             panel.background = element_rect(fill=graphcolours$graphC, colour=graphcolours$graphC)
  )
  
  return(g)

}