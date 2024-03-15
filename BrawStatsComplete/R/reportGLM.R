makeText<-function(r,p) {
  if (p<alphaSig) {
    if (r>0) {
      paste0("\b!g",brawFormat(r,digits=3))
    } else {
      paste0("\b!r",brawFormat(r,digits=3))
    }
  } else {
    brawFormat(r,digits=3)
  }
}

reportGLM<-function(DV,IVs,result,p_or_r) {
  
  nc<-4

  outputText<-c(
    paste0("\b",p_or_r," values"," (","DV = ",DV$name,")"),"","","",
    " ","","","",
    "\b!jEffect    ","\bDirect","\bUnique","\bTotal"
    )
  switch (p_or_r,
          "r"={
            for (i in 1:length(result$r.direct)) {
              outputText<-c(outputText,
                            paste0("\b!j",IVs[,i]$name,"    "),
                            makeText(result$r.direct[i],result$p.direct[i]),
                            makeText(result$r.unique[i],result$p.unique[i]),
                            makeText(result$r.total[i],result$p.total[i])
              )
            }
          },
          "p"={
            for (i in 1:length(result$r.direct)) {
              outputText<-c(outputText,
                            paste0("\b!j",IVs[,i]$name,"    "),
                            makeText(result$p.direct[i],result$p.direct[i]),
                            makeText(result$p.unique[i],result$p.unique[i]),
                            makeText(result$p.total[i],result$p.total[i])
              )
            }
          })
  
  if (p_or_r=="r") {
    outputText<-c(outputText,
                  "\b!jFull model    ",paste0("\b",brawFormat(result$r.full,digits=3)),
                  paste0("\b",brawFormat(result$r.full.unique,digits=3)),"",
                  " ","","","",
                  " ","","","",
                  "!j\bAIC:",paste0("\b",brawFormat(result$AIC,digits=3)),"",""
    )
  }
  list(outputText=outputText,nc=nc,nr=length(outputText)/nc)
}
