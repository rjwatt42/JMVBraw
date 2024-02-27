brawFormat<-function(numbers,digits=braw.env$report_precision) {
  
  pad<-function(x) if(x>=0) paste0(" ",x) else x
  if (is.numeric(numbers)) {
    if (all(numbers==round(numbers))) {
      r<-sprintf(numbers,fmt="%d")
    } else {
      r<-sprintf(numbers,fmt=paste0("%0.",digits,"f"))
    }
  } else {
    r<-numbers
  }
  r<-unname(sapply(r,pad))
  r
}
