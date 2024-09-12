## Run analysis, write model results

## Before:
## After:

# Script information ------------------------------------------------------
# This script automates the execution of SS3 model scenarios. It sets up the 
# necessary directories, copies the required data and software for each scenario
# (such as "S0"), and runs the SS3 model within the specified directory. 
# After running the model, the script performs a commit and push to a Git repository, 
# ensuring that all model results are properly versioned in the current branch.
# The process includes logging the execution time and managing the 
# version control of the `model/run` directory, making sure that the results
# from each scenario are accurately tracked and stored. 
rm(list=ls())
run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 
# libraries ------------------------------------------------------
library(r4ss) 
library(icesTAF)

# directorios ----
old_wd <- getwd()

mkdir("model/run")

path_esc<-"model/run"

path.data<-"boot/data/run" 
list.files(path.data)

# Scenarios ----
#'*------------------------------------------------------------------------------------------*

path   <- file.path(path.data, esc) 
model  <- file.path(path_esc)
cp(path, model)
cp("boot/software/ss3", paste0("model/run/",esc))
wd <- paste0(getwd(),"/model/run/",esc)
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)

#'*------------------------------------------------------------------------------------------*




setwd(old_wd)
rm(list=ls())





