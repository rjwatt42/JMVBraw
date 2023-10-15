prepareSample<-function(IV,IV2,DV) {
  
  iv<-IV$data
  dv<-DV$data
  if (!is.null(IV2))
    iv2<-IV2$data
  else 
    iv2<-iv*0
  keep<-rowSums(is.na(cbind(iv,iv2,dv)))>0
  iv<-iv[keep]
  iv2<-iv2[keep]
  dv<-dv[keep]
  
  switch(IV$type,
         "Interval"={
           IVs<-list(mu=mean(iv),sd=sd(iv),name=IV$name,type=IV$type,vals=iv)
         },
         "Ordinal"={
           IVs<-list(mu=IV$median, sd=IV$iqr/2, name=IV$name,type=IV$type,nlevs=IV$nlevs,median=IV$median,iqr=IV$iqr,discrete=IV$discrete,vals=iv)
         },
         "Categorical"={
           iv<-as.factor(iv)
           IVs<-list(mu=0, sd=1, name=IV$name,type=IV$type,ncats=IV$ncats,cases=IV$cases,proportions=IV$proportions,vals=iv)
         }
  )
  ivplot<-IV$plot

  
  if (!is.null(IV2)) {
    iv2<-IV2$data
    switch(IV2$type,
           "Interval"={
             IV2s<-list(name=IV2$name,type=IV2$type,mu=mean(iv2),sd=sd(iv2),vals=iv2)
           },
           "Ordinal"={
             IV2s<-list(mu=IV2$median, sd=IV2$iqr/2, name=IV2$name,type=IV2$type,nlevs=IV2$nlevs,median=IV2$median,iqr=IV2$iqr,discrete=IV2$discrete,vals=iv2)
           },
           "Categorical"={
             iv2<-as.factor(iv2)
             IV2s<-list(name=IV2$name,type=IV2$type,mu=0, sd=1, ncats=IV2$ncats,cases=IV2$cases,proportions=IV2$proportions,vals=iv2)
           }
    )
    iv2plot<-IV2$plot
  } else{
    iv2<-NULL
    IV2s<-NULL
    iv2plot<-NULL
  }
  
  switch(DV$type,
         "Interval"={
           DVs<-list(mu=mean(dv),sd=sd(dv),name=DV$name,type=DV$type,vals=dv)
         },
         "Ordinal"={
           DVs<-list(mu=DV$median, sd=DV$iqr/2, name=DV$name,type=DV$type,nlevs=DV$nlevs,median=DV$median,iqr=DV$iqr,discrete=DV$discrete,vals=dv)
         },
         "Categorical"={
           dv<-as.factor(dv)
           DVs<-list(mu=0, sd=1, name=DV$name,type=DV$type,ncats=DV$ncats,cases=DV$cases,proportions=DV$proportions,vals=dv)
         }
  )
  dvplot<-DV$plot
  
    list(participant=1:length(IV$data),iv=iv,iv2=iv2,dv=dv,
         ivplot=ivplot,iv2plot=iv2plot,dvplot=dvplot,
         IVs=IVs,IV2s=IV2s,DVs=DVs)

}