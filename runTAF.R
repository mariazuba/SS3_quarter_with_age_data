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

# S0
#'*Selectivity*
# S1 Sel logistic fixed all data
#   S1.1: S1 + Sel block Ecocadiz
#   S1.2: S1 + Sel ramdon-walk fishery
#   S1.3: S1 + Sel_semiparametric fishery
#'*Remuve survey data*
# S2: S1 + Sin BOCADEVA
# S3: S1 + Sin PELAGO
# S4: S1 + Sin ECOCADIZ
# S5:  S1 + ECOCADIZ desde el 2010  composiciones de edad
#   S5.1: S5 +   ECOCADIZ desde el 2010  composiciones de edad y biomasas
#'
#'*Catchability q=1*
# S6:  S1 + q=1 PELAGO
# S7:  S1 + q=1 ECOCADIZ
# S8:  S1 + q=1 BOCADEVA
# S9:  S1 + q=1 ECOCADIZ-RECLUTAS

#'*Catchability SDextra surveys*
# S10: S1 + SDextra PELAGO,ECOCADIZ,BOCADEVA, ECOCADIZ-RECLUTAS

#'*Catchability Q_power*
# s11: s1 + Q_power_PELAGO
# s12: s1 + Q_power_ECOCADIZ
# s13: s1 + Q_power_BOCADEVA
# s14: s1 + Q_power_ECOCADIZ-RECLUTAS
# s15: s1 + Q_power_PELAGO_ECOCADIZ_BOCADEVA_ECOCADIZ-RECLUTAS

#'*Catchability Q_power and SDextra surveys*
# s16: s15 + S10 Q_power and SDextra PELAGO_ECOCADIZ_BOCADEVA_ECOCADIZ-RECLUTAS



boot<-"boot/initial/data/run/" 
esc<-"S5.1"
write(esc, file = paste0(boot,"Esc.txt"))
sourceTAF("bootstrap")
sourceTAF("data")
sourceTAF("model_01_run")
sourceTAF("output_01_run")
sourceTAF("report_01_run")


# sourceTAF("model_02_retro")
# sourceTAF("output_02_retro")
# sourceTAF("report_02_retro")

# Compilar reporte.Rmd a reporte.html
 render("Report_SS3_quarter_with_age_data.Rmd", 
        output_file = paste0("Report_SS3_quarter_with_age_data_",esc,".docx"))

sourceTAF("script_git")

#sourceAll()


# End of script -----------------------------------------------------------



