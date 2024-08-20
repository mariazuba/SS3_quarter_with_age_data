# función que elimina archivos


deleteFiles<-function(dir,carpetas_principales){
dir.origin<-getwd()
# Lista de archivos a eliminar
archivos_a_mantener <- c("control.SS", "data.SS", "forecast.ss","starter.ss","wtatage.ss")
# Iterar sobre cada carpeta principal
for (i in 1:length(carpetas_principales)) {
  # Construir la ruta completa de la carpeta
  ruta_carpeta <- file.path(dir, carpetas_principales[i])
  
  # Cambiar el directorio de trabajo a la carpeta actual
  setwd(ruta_carpeta)
  
  # Obtener lista de archivos y carpetas en la carpeta
  elementos <- list.files()
  
  # Filtrar elementos a eliminar
  elementos_a_eliminar <- setdiff(elementos, archivos_a_mantener)
  
  # Eliminar elementos
  for (elemento in elementos_a_eliminar) {
    if (dir.exists(elemento)) {
      unlink(elemento, recursive = TRUE)  # Eliminar carpetas recursivamente
      cat("Carpeta eliminada:", file.path(ruta_carpeta, elemento), "\n")
    } else {
      file.remove(elemento)  # Eliminar archivos
      cat("Archivo eliminado:", file.path(ruta_carpeta, elemento), "\n")
    }
  }
  setwd(dir.origin)
}
}



dir<-"model/run/"
#dir<-"model/retro/S0"
# Lista de carpetas
carpetas_principales  <- c("S0","S0.1","S0.2","S0.3","S0.4","S0.5","S1.0",
                           "S1.1","S2.0","S2.1","S2.2","S3.0","S4.0","S4.1","S5.0",
                           "S5.1","S5.2")
#carpetas_principales  <- "S0"
#carpetas_principales  <- c("retro0","retro-1","retro-2","retro-3","retro-4","retro-5")

deleteFiles(dir,carpetas_principales)
