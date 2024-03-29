
trimanalysis<-function(analysis) {
  
  use<-(!is.na(analysis$rIV))
  
  analysis$rIV=analysis$rIV[use]
  analysis$pIV=analysis$pIV[use]
  analysis$rpIV=analysis$rpIV[use]
  analysis$raIV=analysis$raIV[use]
  analysis$roIV=analysis$roIV[use]
  analysis$poIV=analysis$poIV[use]
  analysis$nval=analysis$nval[use]
  analysis$df1=analysis$df1[use]
  
  if (!is.null(analysis$hypothesis$IV2)) {
    analysis$rIV2=analysis$rIV2[use]
    analysis$pIV2=analysis$pIV2[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$r$direct=matrix(analysis$r$direct[use,],nrow=sum(use))
    analysis$r$unique=matrix(analysis$r$unique[use,],nrow=sum(use))
    analysis$r$total=matrix(analysis$r$total[use,],nrow=sum(use))
    analysis$p$direct=matrix(analysis$p$direct[use,],nrow=sum(use))
    analysis$p$unique=matrix(analysis$p$unique[use,],nrow=sum(use))
    analysis$p$total=matrix(analysis$p$total[use,],nrow=sum(use))
  }
  
  analysis
}

plotInference<-function(analysis,otheranalysis=NULL,disp="r",orientation="vert",effectType="direct",showTheory=TRUE,g=NULL){
  if (length(disp)==2) {
    return(plot2Inference(analysis,disp[1],disp[2]))
  } 
  analysis<-trimanalysis(analysis)
  
  switch (disp,
          "r"= {g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "rp"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ro"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ra"= {g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ci1"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ci2"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},

          "p"= {g<-p_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "po"= {g<-p_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          
          "log(lrs)"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          "log(lrd)"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          
          "w"= {g<-w_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          "wp"={g<-w_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          
          "wn"={g<-n_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          "n"= {g<-n_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          
          "e1"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,g=g)},
          "e2"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,g=g)},
          "e1a"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,g=g)},
          "e2a"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,g=g)},
          "e1b"={g<-e1_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,g=g)},
          "e2b"={g<-e2_plot(analysis,disp,otheranalysis,orientation=orientation,showTheory=showTheory,g=g)}
  )
  return(g)
}


plot2Inference<-function(analysis,disp1,disp2,metaPlot=FALSE){
    
  r<-analysis$hypothesis$rIV
  if (!is.null(analysis$hypothesis$IV2)){
    r<-c(r,analysis$hypothesis$rIV2,analysis$hypothesis$rIVIV2DV)
  }
  
  pvals<-analysis$pIV
  rvals<-analysis$rIV
  nvals<-analysis$nval
  df1vals<-analysis$df1

  xaxis<-showAxis(disp1,analysis$hypothesis$effect)
  yaxis<-showAxis(disp2,analysis$hypothesis$effect)
  switch (disp1,
          "r"={
            d1<-analysis$rIV
            if (braw.env$RZ=="z") d1<-atanh(d1)
          },
          "p"={
            d1<-analysis$pIV
            if (braw.env$pPlotScale=="log10") d1<-log10(d1)
            },
          "rp"={
            d1<-analysis$rpIV
            if (braw.env$RZ=="z") d1<-atanh(d1)
          },
          "ro"={
            d1<-analysis$roIV
            if (braw.env$RZ=="z") d1<-atanh(d1)
          },
          "po"={
            d1<-analysis$poIV
            if (braw.env$pPlotScale=="log10") d1<-log10(d1)
          },
          "n"={
            d1<-analysis$nval
            if (braw.env$nPlotScale=="log10") d1<-log10(d1)
          },
          "log(lrs)"=d1<-res2llr(analysis,"sLLR"),
          "log(lrd)"=d1<-res2llr(analysis,"dLLR"),
          "w"={
            d1<-rn2w(analysis$rIV,analysis$nval)
            if (braw.env$wPlotScale=="log10") d1<-log10(d1)
          },
          "wp"={
            d1<-rn2w(analysis$rp,analysis$nval)
            if (braw.env$wPlotScale=="log10") d1<-log10(d1)
          },
          "wn"={
            d1<-rw2n(analysis$rIV,0.8,analysis$design$Replication$ReplTails)
            if (braw.env$wPlotScale=="log10") d1<-log10(d1)
          }
  )
  switch (disp2,
          "r"={
            d2<-analysis$rIV
            if (braw.env$RZ=="z") d2<-atanh(d2)
          },
          "p"={
            d2<-analysis$pIV
            if (braw.env$pPlotScale=="log10") d2<-log10(d2)
          },
          "rp"={
            d2<-analysis$rpIV
            if (braw.env$RZ=="z") d2<-atanh(d2)
          },
          "ro"={
            d2<-analysis$roIV
            if (braw.env$RZ=="z") d2<-atanh(d2)
          },
          "po"={
            d2<-analysis$poIV
            if (braw.env$pPlotScale=="log10") d2<-log10(d2)
          },
          "n"={
            d2<-analysis$nval
            if (braw.env$nPlotScale=="log10") d2<-log10(d2)
          },
          "log(lrs)"=d2<-res2llr(analysis,"sLLR"),
          "log(lrd)"=d2<-res2llr(analysis,"dLLR"),
          "w"={
            d2<-rn2w(analysis$rIV,analysis$nval)
            if (braw.env$wPlotScale=="log10") d2<-log10(d2)
          },
          "wp"={
            d2<-rn2w(analysis$rp,analysis$nval)
            if (braw.env$wPlotScale=="log10") d2<-log10(d2)
          },
          "wn"={
            d2<-rw2n(analysis$rIV,0.8,analysis$design$Replication$ReplTails)
            if (braw.env$wPlotScale=="log10") d2<-log10(d2)
          }
  )
  
  pts<-data.frame(x=d1,y=d2)
  braw.env$plotArea<-c(0,0,1,1)
  g<-ggplot()+coord_cartesian(xlim = c(0,1), ylim = c(0, 1))+braw.env$blankTheme
  g<-startPlot(xaxis$lim,yaxis$lim,box="both",top=FALSE,g=g)
  g<-g+xAxisTicks(NULL,logScale=xaxis$logScale)
  g<-g+xAxisLabel(xaxis$label)
  g<-g+yAxisTicks(NULL,logScale=yaxis$logScale)
  g<-g+yAxisLabel(yaxis$label)
  
  if (disp1=="r" && disp2=="p") {
    rs<-seq(-braw.env$r_range,braw.env$r_range,length.out=51)
    ps<-r2p(rs,analysis$nval[1])
    if (braw.env$pPlotScale=="log10")  ps<-log10(ps)
    g<-g+dataLine(data=data.frame(x=rs,y=ps),col="white")
  }

  dotSize=min(8,max(3.5,sqrt(400/length(d1))))
  dotSize<-dotSize<-(braw.env$plotTheme$axis.title$size)/3

  if (!metaPlot && braw.env$useSignificanceCols){
    c1=braw.env$plotColours$infer_sigC
    c2=braw.env$plotColours$infer_nsigC
  } else {
    c1=braw.env$plotColours$descriptionC
    c2=braw.env$plotColours$descriptionC
  }
  if (length(d1)<200) {
    use<-!isSignificant(braw.env$STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
    pts1=pts[use,]
    g<-g+dataPoint(data=pts1,shape=braw.env$plotShapes$study, colour = "black", fill = c2, size = dotSize)
    pts2=pts[!use,]
    g<-g+dataPoint(data=pts2,shape=braw.env$plotShapes$study, colour = "black", fill = c1, size = dotSize)
  } else {
      use<-!isSignificant(braw.env$STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
      pts1=pts[use,]
      g<-g+dataPoint(data=pts1,shape=braw.env$plotShapes$study, colour = c2, fill = c2, size = dotSize/4)
      pts2=pts[!use,]
      g<-g+dataPoint(data=pts2,shape=braw.env$plotShapes$study, colour = c1, fill = c1, size = dotSize/4)
  }
  return(g)
}
