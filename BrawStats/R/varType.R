varType<-function(data) {
  if (is.numeric(data)) {
    "Interval"
  } else {
    l<-is.numeric(levels(data))
    if (all(l)) {
      "Ordinal"  
    } else {
      "Categorical"
    }
  }
  
}