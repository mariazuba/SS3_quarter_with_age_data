# Script information ------------------------------------------------------

# run retrospective analysis 
rm(list=ls())
# Load packages -----------------------------------------------------------
library(r4ss)
library(icesTAF)
library(lubridate)


old_wd <- getwd()
# Run retrospective analysis ----------------------------------------------
# copy files to the retro directory --

run_esc<-paste0(getwd(),"/model/run/") 
retro_esc<-paste0(getwd(),"/model/retro/")

esc<-list.files(run_esc)

for(i in 1:length(esc)){

run.dir<-paste0(run_esc,esc[i])
retro.dir <- paste0(retro_esc,esc[i])

mkdir(retro.dir)
copy_SS_inputs(dir.old = run.dir, 
               dir.new =  retro.dir,
               copy_exe = FALSE,
               verbose = FALSE)

cp("boot/software/ss3", retro.dir )
wd <- retro.dir 
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
retro(dir = wd, oldsubdir = "", newsubdir = "", 
      years = 0:-5,exe = "ss3")
}

# End of script -----------------------------------------------------------
setwd(old_wd)


