## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)

mkdir("output/run")

# Script information ------------------------------------------------------


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)

# Setup and read in files -----------------------------------------------------

run_esc<-paste0(getwd(),"/model/run/")

esc<-list.files(run_esc)

for(i in 1:length(esc)){
run.dir  <- paste0(run_esc,esc[i])
output <- r4ss::SS_output(dir = run.dir,forecast=FALSE)
summary <- read.table(paste0(run.dir,"/ss_summary.sso"),header=F,sep="",na="NA",fill=T)

mkdir(paste0("output/run/",esc[i]))
run_out<<-paste0("output/run/",esc[i])

r4ss::SS_plots(replist = output, dir = run_out,
               printfolder = "plots",showpost = FALSE)

# Write .RData ----
save(output,  summary,         
     file=paste0(run_out,"/output.RData"))
}