varType<-function(data) {
  if (is.numeric(data)) {
    "Interval"
  } else {
    l<-grepl("[^0-9.-]+",levels(data))
    if (all(!l)) {
      "Ordinal"  
    } else {
      "Categorical"
    }
  }
  
}