## Extract results of interest, write TAF output tables

## Before:
## After:

# Script information ------------------------------------------------------
# This script automates the extraction of key results and the generation of
# TAF output tables from SS3 model runs. For each model scenario in the
# `model/run` directory, the script loads the results, generates plots,
# and saves them in the `output/run` directory, while removing any previously 
# generated plot subdirectories. The results and summaries are stored 
# in `.RData` files. Finally, the script commits and pushes the generated 
# files to a Git repository, ensuring that the processed results are properly 
# versioned in the current branch of the repository. *To avoid making changes 
# directly to the main repository, it is recommended to either comment out 
# the section that performs the commit and push, or switch to a different 
# branch before running the script. This will help ensure proper version 
# control of the generated files without impacting the main branch.*

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

# datos para reporte
R0 <- output$estimated_non_dev_parameters["SR_LN(R0)", "Value"]



mkdir(paste0("output/run/",esc[i]))
run_out<-paste0("output/run/",esc[i])

# Definir la ruta al directorio que quieres borrar
dir_to_remove <- paste0("output/run/",esc[i],"/plots")

# Ejecutar el comando para borrar la carpeta
system(paste("rm -r", shQuote(dir_to_remove)))

r4ss::SS_plots(replist = output, dir = run_out,
               printfolder = "plots",showpost = FALSE)
# Write .RData ----
save(output,  summary, R0,        
     file=paste0(run_out,"/output.RData"))
}

# Se hace commit y push de los cambios 
for(i in 1:length(esc)){
  run_out <- paste0("output/run/",esc[i])
# Agregar todos los archivos en la carpeta específica al área de preparación
system2("git", args = c("add",run_out))
system2("git", args = c("add","output_01_run.R"))
# Realizar el commit con un mensaje descriptivo
fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
commit_message <- paste0("Actualizados output/run/", esc[i]," ",fecha_hora)
# Usar shQuote para manejar correctamente los espacios en el mensaje de commit
commit_message_quoted <- shQuote(commit_message)
# Ejecutar el comando git commit
system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
# (Opcional) Subir los cambios al repositorio remoto
system2("git", args = c("push"))
}
