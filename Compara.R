# Comparación de escenarios


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ss3diags)


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

#'*Catchability Q_power*
# s11: s1 + Q_power_PELAGO
# s12: s1 + Q_power_ECOCADIZ
# s13: s1 + Q_power_BOCADEVA
# s14: s1 + Q_power_ECOCADIZ-RECLUTAS
# s15: s1 + Q_power_PELAGO_ECOCADIZ_BOCADEVA_ECOCADIZ-RECLUTAS

#'*Catchability Q_power and SDextra surveys*
# s16: s15 + S10 Q_power and SDextra PELAGO_ECOCADIZ_BOCADEVA_ECOCADIZ-RECLUTAS

#'*Relación stock-recluta Beverton-Holt*
# s17: s1 + relación stock-recluta Beverton-Holt h=0.6
# s18: s1 + relación stock-recluta Beverton-Holt h=0.7
# s19: s1 + relación stock-recluta Beverton-Holt h=0.8
# s20: s1 + relación stock-recluta Beverton-Holt h=0.9

#'*Corrección por sesgo desvios reclutamiento*

#'*initial* *probar otros valores!!!*
# s21: s1 + se incluye captura inicial 1988 trimestral

#'*Spawning and settlement*
# s22: s1 + spawn month  1, settlement month 7 (age 0)
# s23: s1 + spawn month  1, settlement month 10 (age 0)
# s24: s1 + spawn month 7, settlement month7 (age 0)
# s25: s1 + spawn month  7, settlement month 10 (age 0)
# s26: s1 + spawn month 7, settlement month 1 (age 1) # no converge
# s27: s1 + spawn month  4, settlement month 10 (age 0)

#'*M estimado*
# s28: s1 + M estimado fijo para todas las edades

#'*Catchability SDextra surveys*
# S10: S1 + SDextra Estimate extra standard error for an index to be added to the input standard deviation of the survey variability

#'*CV changes*
# s29: s1 + CV estimados para Bocadeva
# s30: s1 + CV estimados externamente con loess
# s10 
# s31: s30 + s10


boot<-"boot/initial/data/run/" 
esc<-"S30"

write(esc, file = paste0(boot,"Esc.txt"))
sourceTAF("bootstrap")
sourceTAF("data")
sourceTAF("model_01_run")
sourceTAF("output_01_run")
sourceTAF("report_01_run")


# sourceTAF("model_02_retro")
# sourceTAF("output_02_retro")
# sourceTAF("report_02_retro")





ls_esc<-list.files(boot)
esc<-ls_esc[ls_esc!= c("Esc.txt","S26")]

#Se extrae S26 porque no converge

replist<-list()
qvalues<-list()
Sel.values<-list()
diag<-list()
for(i in 1:length(esc)){
Esc<-esc[i]
load(paste0("output/run/",Esc,"/output.RData"))
replist[[Esc]]<-output

#Catchability
lnq_base <- output$estimated_non_dev_parameters[grep("^LnQ_base", row.names(output$estimated_non_dev_parameters)), ]
qvalues[[Esc]] <- data.frame(par=row.names(lnq_base),value=lnq_base$Value,std=lnq_base$Parm_StDev)
#Selectivity
Age_Sel <- output$estimated_non_dev_parameters[grep("^Age", row.names(output$estimated_non_dev_parameters)), ]
Sel.values[[Esc]] <- data.frame(par=row.names(Age_Sel),value=Age_Sel$Value,std=Age_Sel$Parm_StDev)
#Run test
run_cpue<-SSplotRunstest(output,subplots = "cpue")
run_age<-SSplotRunstest(output,subplots = "age")
#RMSE index and age
jaba_cpue<-SSplotJABBAres(output,subplots = "cpue")
jaba_age<-SSplotJABBAres(output,subplots = "age")




diag[[Esc]]<-data.frame(ESC=Esc,
                        convergency=output$maximum_gradient_component,
                        Total_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"],
                        Age_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Age_comp"],
                        RMSE_index=jaba_cpue$RMSE.perc[jaba_cpue$indices == "Combined"],
                        RMSE_age=jaba_age$RMSE.perc[jaba_age$indices == "Combined"])



}


dir.create("report/run/comparison")

diagsSS<-plyr::ldply(diag,data.frame)


save(diagsSS,file=paste0("report/run/comparison","/report.RData"))

mod.sum <- SSsummarize(replist)


SSplotComparisons(mod.sum, subplots=c(13),
                  legendlabels = esc,
                  pheight=4.5,png=TRUE,plotdir="report/run/comparison",legendloc='topleft')





