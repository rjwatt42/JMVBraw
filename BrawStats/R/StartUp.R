files<-setdiff(dir("R","*.R"),"StartUp.R")
fAvoid<-dir(".","example[0-9]*.R")
files<-setdiff(files,fAvoid)

for (fi in files) source(paste0("R/",fi))

library(ggplot2)
library(grDevices)

library(mnormt)      # pmnorm for logistic
library(lme4)        # lmer (mixed models)
library(car)         # Anova type 3 correct
library(pracma)      # for meshgrid & fmincon


