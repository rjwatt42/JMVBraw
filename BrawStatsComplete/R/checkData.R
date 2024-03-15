checkData<-function(dataFull,require="All") {
  
  if (require=="All") {
    keep<-TRUE
    for (i in 1:length(dataFull)) {
      keep<-keep & !is.na(dataFull[[i]])
    }
  } else {
    keep<-FALSE
    for (i in 1:length(dataFull)) {
      keep<-keep | !is.na(dataFull[[i]])
    }
  }
  dataFull<-dataFull[keep,]
  return(dataFull)
}