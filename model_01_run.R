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