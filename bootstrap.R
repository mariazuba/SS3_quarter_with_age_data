

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


