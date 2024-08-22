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
#'*To avoid making changes directly to the main repository, it is recommended to*
#'*either comment out the section that performs the commit and push, or switch to* 
#'*a different branch before running the script. This will help ensure proper*
#'*version control of the generated files without impacting the main branch.*

# libraries ------------------------------------------------------
library(r4ss) 
library(reshape2)
library(readxl)
library(openxlsx)
library(dplyr)
library(kableExtra)
library(devtools)
library(ss3diags)
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
#setwd(wd)
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)
setwd(old_wd)


# Se hace commit y push de los cambios cada vez que se ejecuta el modelo
run_esc<-paste0(getwd(),"/model/run/")
esc<-list.files(run_esc)

for(i in 1:length(esc)){
  run_mod <- paste0("model/run/",esc[i])
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_mod))
  system2("git", args = c("add","model_01_run.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualizados model/run/", esc[i]," ", fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
  system2("git", args = c("push"))
}