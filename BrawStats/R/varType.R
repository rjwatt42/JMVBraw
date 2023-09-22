varType<-function(data) {
  if (is.numeric(data)) {
    "Interval"
  } else {
    l<-as.numeric(levels(data))
    if (!is.na(l)) {
      "Ordinal"  
    } else {
      "Categorical"
    }
  }
  
}