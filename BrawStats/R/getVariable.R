getVariable<-function(varname,dataFull) {

  Name<-varname
  data<-dataFull[[varname]]
  Type<-varType(data)
  
  ncats<-0
  nlevs<-0
  cases<-c()
  mu<-0
  sd1<-0
  proportions<-""
  ordProportions<-c()
  if (Type=="Categorical") {
    cases<-levels(data)
    ncats<-length(cases)
    proportions<-"1,1"
    plot<-as.numeric(data)
    
    xp<-plot
    for (i in 1:ncats) {
      use1=(xp==i)
      plot[use1]<-i-1+rnorm(sum(use1))*mean(use1)*0.3
    }
  }
  if (Type=="Ordinal") {
    nlevs<-length(levels(data))
    data<-as.numeric(data)
    plot<-as.numeric(data)
    mu<-mean(data)
    sd1<-sd(data)
    ordProportions<-paste(rep("1",nlevs),collapse=",")
  } 
  if (Type=="Interval") {
    mu<-mean(data)
    sd1<-sd(data)
    plot<-as.numeric(data)
  } 

  var<-makeVar(name=Name,type=Type,
       ncats=ncats,cases=cases,proportions=proportions,
       nlevs=nlevs,ordProportions=ordProportions,
       mu=mu,sd=sd1)
  
  var$plot<-plot
  var$data<-data
  return(var)

  }