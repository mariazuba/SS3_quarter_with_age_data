## Run analysis, write model results

## Before:
## After:
# libraries
library(r4ss) 
library(reshape2)
library(readxl)
library(openxlsx)
library(dplyr)
library(kableExtra)
library(devtools)
library(ss3diags)
library(icesTAF)

old_wd <- getwd()

mkdir("model/run")

path_esc<-"model/run"


# directorios ----
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
#setwd(wd)
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)
setwd(old_wd)

