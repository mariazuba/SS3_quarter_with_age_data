
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
# delete  folder named "retro" 



retro_esc<-paste0(getwd(),"/model/retro/")
retro_out<-paste0(getwd(),"/output/retro/")
list.files(retro_esc)
esc<-"S0"

system(paste("rm -r", shQuote(retro_out)))
mkdir(retro_out)

  retro.dir  <- paste0(retro_esc,esc)

retroModels<-SSgetoutput(dirvec=file.path(retro_esc,esc,paste("retro",0:-5,sep="")))
retroSummary <- SSsummarize(retroModels)
# Save output objects -----------------------------------------------------

save(retroModels,retroSummary,
     file=paste0(retro_out,"retrospective_",esc,".RData"))



# End of script -----------------------------------------------------------

rm(list=ls())