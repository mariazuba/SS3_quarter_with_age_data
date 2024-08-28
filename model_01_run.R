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
### S0 ----
s0_path   <- file.path(path.data, "S0") 
s0_model  <- file.path(path_esc)
cp(s0_path, s0_model)
cp("boot/software/ss3", "model/run/S0")
wd <- paste0(getwd(),"/model/run/S0")
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)

#'*------------------------------------------------------------------------------------------*
### S1 ----
# Specify the source and destination folders
# model_S0 <- "model/run/S0"
# model_S1 <- "model/run/S1"
# # Create the destination folder if it doesn't exist
# dir.create(model_S1, recursive = TRUE, showWarnings = FALSE)
# # List of files you want to copy
# files_to_copy <- c("starter.ss", "control.SS", "data.SS", "forecast.ss", "ss3","wtatage.ss")
# # Copy the files to the destination folder
# sapply(files_to_copy, function(file) {
#   file.copy(file.path(model_S0, file), model_S1, overwrite = TRUE)
# })
# 
# #### modify the model
# inputs <- r4ss::SS_read(dir = model_S1)
# 
# 
# wd <- paste0(getwd(),"/model/run/S0")
# system(wd)
# system(paste0("chmod 755 ",wd,"/ss3"))
# r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)



setwd(old_wd)
rm(list=ls())





