# Script information ------------------------------------------------------
# The script automates the generation of `DATA.bib` and `SOFTWARE.bib` files,
# which contain metadata about the data and software used for an SS3 model, 
# based on files in the `boot/initial` directory. It employs the `draft.data()` 
# and `draft.software()` functions to create these files, and organizes the data
# and software into the `boot/data` and `boot/software` directories using `taf.bootstrap()`. 
# Finally, it commits and pushes these changes to the Git repository, 
# ensuring proper version control of the generated files. 
#'*To avoid making changes directly to the main repository,*
#'* it is recommended to either comment out the section that performs the commit and push, *
#'* or switch to a different branch before running the script. *
#'* This will help ensure proper version control of the generated files without impacting the main branch.*

library("icesTAF")
library("r4ss")
#taf.skeleton()
# -boot
#   - initial
#      - data
# -data.R
# -model.R
# -output.R
# -report.R

# crea DATA.bib
# SS3 scenarios
 draft.data(
   originator = "WKBANSP",
   year = 2024,
   title = "SS3 data format",
  period = "1989-2023",file=TRUE)
taf.bootstrap() # se obtiene los datos de la carpeta boot/data

# crea SOFTWARE.bib
r4ss::get_ss3_exe(dir = "boot/initial/software", version = "v3.30.22.1")
draft.software('boot/initial/software/ss3',file=TRUE)
taf.bootstrap() # se obtiene los datos de la carpeta boot/data, 



# Se hace commit y push de los cambios cada vez que se ejecuta el modelo
  run_boot <- "boot/"
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_boot))
  system2("git", args = c("add","boot_01_run.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualiza boot",fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
  system2("git", args = c("push"))

