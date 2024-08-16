
# load libraries ----------------------------------------------------------

library(icesTAF)
library(icesAdvice)
library(tidyverse)
library(reshape)
library(ss3diags)
# retro's directories
mkdir("output/retro")

retro_esc<-paste0(getwd(),"/model/retro/")
retro_out<-paste0(getwd(),"/output/retro/")
esc<-list.files(retro_esc)

for(i in 1:length(esc)){
  retro.dir  <- paste0(retro_esc,esc[i])

retroModels<-SSgetoutput(dirvec=file.path(retro_esc,esc[i],paste("retro",0:-5,sep="")))
retroSummary <- SSsummarize(retroModels)
# Save output objects -----------------------------------------------------

save(retroModels,retroSummary,
     file=paste0(retro_out,"retrospective_",esc[i],".RData"))
}


# End of script -----------------------------------------------------------

rm(list=ls())