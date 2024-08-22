
# Script information ------------------------------------------------------
# This script automates the process of summarizing and saving retrospective
# analysis results for SS3 model scenarios. It first loads the necessary 
# libraries and sets up directories for storing output. For each scenario in 
# the model/retro directory, the script retrieves the retrospective 
# model outputs for a series of years (0 to -5) using `SSgetoutput`, 
# then summarizes these results with `SSsummarize`.
# The summarized results and model outputs are saved as `.RData` 
# files in the `output/retro` directory, ensuring that all retrospective analyses 
# are properly stored for each scenario. 
# The script concludes by clearing the workspace.

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