# Definir la ruta al directorio del repositorio
repo_dir <- getwd()

# Definir la ruta a la carpeta específica
folder_path <-  "output/run/S0"
# Agregar todos los archivos en la carpeta específica al área de preparación
system2("git", args = c("add", folder_path))
# Realizar el commit con un mensaje descriptivo
commit_message <- "Actualizados outputs escenario S0 output/run/S0"
# Usar shQuote para manejar correctamente los espacios en el mensaje de commit
commit_message_quoted <- shQuote(commit_message)
# Ejecutar el comando git commit
system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
# (Opcional) Subir los cambios al repositorio remoto
system2("git", args = c("push"))
