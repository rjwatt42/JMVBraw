convertOutputText<-function(outputText) {
  output<-""
  ml<-max(sapply(outputText$outputText,nchar))
  ml<-min(ml,20)
  k=0
  for (i in 1:outputText$nr) {
    output1<-""
    for (j in 1:outputText$nc) {
      k<-k+1
      t<-outputText$outputText[k]
      t<-sub("\b","",t)
      if (grepl("!j",t)) {just<-"right"} else {just<-"left"}
      t<-sub("!j","",t)
      t<-sub("\r","",t)
      t<-sub("!g","",t)
      switch (just,
              "left"={t<-formatC(t,width=-(ml+1))},
              "right"={t<-paste0(formatC(t,width=(ml+1-4)),"    ")}
              )
      output1<-paste0(output1,t)
    }
    output<-paste(output,output1,sep="\n")
  }
  output
}