# Script information ------------------------------------------------------

# run TAF analysis for Anchovy in ICES Subdivision 9a South.

#Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024


# Load packages -----------------------------------------------------------

library(icesTAF)
library(rmarkdown)

# clean the TAF directories (all except bootstrap/initial):
#clean()

# Run the TAF analysis ----------------------------------------------------

# run step by step

# sourceTAF("bootstrap")
# sourceTAF("data")
# sourceTAF("model")
# sourceTAF("output") 
# sourceTAF("report")

sourceTAF("bootstrap")
sourceTAF("data")
sourceTAF("model_01_run")
sourceTAF("output_01_run")
sourceTAF("report_01_run")

# Compilar reporte.Rmd a reporte.html
render("Report_SS3_quarter_with_age_data.Rmd", 
       output_file = "Report_SS3_quarter_with_age_data.docx")

sourceTAF("script_git")

#sourceAll()


# End of script -----------------------------------------------------------



