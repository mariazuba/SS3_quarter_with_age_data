## Prepare plots and tables for report

## Before:
## After:
rm(list=ls())


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ss3diags)


# working directory
wd <- getwd()
retro_out<-paste0(getwd(),"/output/retro/")
retro_ls<-list.files(retro_out)
esc<-sub(".RData", "", sub("retrospective_", "", retro_ls))

for(i in 1:length(esc)){
  retro_rep<-paste0(getwd(),"/report/retro/",esc[i])
  mkdir(retro_rep)
# Load data ---------------------------------------------------------------
load(paste0(retro_out,"retrospective_",esc[i],".RData"))

# figure ----
png(file.path(paste0(retro_rep,"/Retro.png")),
    width=7,height=7,res=300,units='in')

sspar(mfrow=c(2,1),plot.cex=0.8)

rb = SSplotRetro(retroSummary,
                 add=T,
                 forecast = T,
                 legend = T,
                 verbose=F)

rf = SSplotRetro(retroSummary,
                 add=T,
                 subplots="F",
                 forecast = T,
                 legend = T,
                 legendloc = "topleft",
                 legendcex = 0.8,
                 verbose=F)
dev.off()

#table ----
ssb.retro<-SShcbias(retroSummary, quant="SSB",verbose=F)
f.retro<-SShcbias(retroSummary, quant="F",verbose=F)

rho_retro<-rbind(ssb.retro,f.retro)
ft1<-rho_retro %>%
  mutate(across(c(Rho,ForcastRho), round, 2)) %>%  
  flextable()

# Guarda la flextable como un archivo de imagen PNG
save_as_image(ft1, path = paste0(retro_rep,"/table_rho.png"))
save(ft1, file=paste0(retro_rep,"/table_rho.RData"))
save(ssb.retro,f.retro, file=paste0(retro_rep,"/rho.RData"))
}

setwd(wd)