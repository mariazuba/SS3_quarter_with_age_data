# Comparación de escenarios


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ss3diags)

boot<-"boot/initial/data/run/" 
ls_esc<-list.files(boot)
esc<-ls_esc[ls_esc != "Esc.txt"]

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
                        Totallike=output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"],
                        RMSE_index=jaba_cpue$RMSE.perc[jaba_cpue$indices == "Combined"],
                        RMSE_age=jaba_age$RMSE.perc[jaba_age$indices == "Combined"])
}



mod.sum <- SSsummarize(replist)

dir.create("report/run/comparison")

SSplotComparisons(mod.sum, subplots=c(13),
                  legendlabels = esc,
                  pheight=4.5,png=TRUE,plotdir="report/run/comparison",legendloc='topleft')





