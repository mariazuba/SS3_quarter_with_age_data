# Comparación de escenarios


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)

boot<-"boot/initial/data/run/" 
ls_esc<-list.files(boot)
esc<-ls_esc[ls_esc != "Esc.txt"]

replist<-list()
qvalues<-list()
for(i in 1:length(esc)){
Esc<-esc[i]
load(paste0("output/run/",Esc,"/output.RData"))
replist[[Esc]]<-output
lnq_base <- output$estimated_non_dev_parameters[grep("^LnQ_base", row.names(output$estimated_non_dev_parameters)), ]
qvalues[[Esc]] <- data.frame(par=row.names(lnq_base),value=lnq_base$Value)
}

mod.sum <- SSsummarize(replist)

dir.create("report/run/comparison")

SSplotComparisons(mod.sum, subplots=c(13),
                  legendlabels = esc,
                  pheight=4.5,png=TRUE,plotdir="report/run/comparison",legendloc='topleft')


convergency<-output$maximum_gradient_component
like<-output$likelihoods_used

run_cpue<-SSplotRunstest(output,subplots = "cpue")
jaba_cpue<-SSplotJABBAres(output,subplots = "cpue")
run_age<-SSplotRunstest(output,subplots = "age")
jaba_age<-SSplotJABBAres(output,subplots = "age")



