getDefaults<-function() {
  IV<-list(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2",proportions="1,1")
  IV2<-list(name="none",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2",proportions="1,1")
  DV<-list(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2",proportions="1,1")
  
  effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,
               world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=0.2,populationRZ="r",populationNullp=0,worldAbs=FALSE)
  )
  
  nulleffect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,
                   world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=0.0,populationRZ="r",populationNullp=0)
  )
  
  design<-list(sN=42, sNRand=FALSE,sNRandK=2, 
               sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
               sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
               sDependence=0, sOutliers=0, sClustering=0,
               sCheating=FALSE,sCheatingLimit="Budget",sCheatingAmount=5,sCheatingBudget=1000,
               sBudgetOn=FALSE,sNBudget=1000,
               sReplicationOn=FALSE,sReplPowerOn=TRUE,sReplPower=0.8,sReplTails=2,sReplType="Fixed",
               sReplSigOnly="No",sReplRepeats=1,sReplKeep="last",sReplBudget=1000,
               sReplCorrection="None",
               sReplVarAlpha=FALSE,sReplAlpha=2,
               sN_Strata=5, sR_Strata=2,
               sNClu_Cluster=5,     sRClu_Cluster=0.7,
               sNClu_Convenience=1, sRClu_Convenience=0.7, sNCont_Convenience=5, sRCont_Convenience=0.7, sRSpread_Convenience=0.5,
               sNClu_Snowball=2,   sRClu_Snowball=0.7,   sNCont_Snowball=2,    sRCont_Snowball=0.7,    sRSpread_Snowball=0.1
  )    
  
  evidence<-list(rInteractionOn=TRUE,
                 rInteractionOnly=TRUE,
                 showType="EffectSize",
                 showTheory=FALSE,
                 sigOnly=FALSE,
                 shortHand=FALSE,
                 ssqType="Type3",
                 llr=list(e1=c(),e2=0),
                 evidenceCaseOrder="Alphabetic",
                 allScatter="all",
                 Welch=FALSE,
                 dataType="Raw",
                 analysisType="Anova",
                 pScale="log10",wScale="linear",nScale="log10",
                 usePrior="world",
                 prior=list(worldOn=FALSE,populationPDF="",
                            populationPDFk=0,populationRZ="r",
                            populationNullp=0)
  )
  
  list(IV=IV,IV2=IV2,DV=DV,effect=effect,nulleffect=nulleffect,design=design,evidence=evidence)
}