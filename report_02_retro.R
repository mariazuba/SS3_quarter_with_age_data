## Prepare plots and tables for report

## Before:
## After:

# Script information ------------------------------------------------------
# This script automates the generation of plots and tables for retrospective
# analysis reports of SS3 model scenarios. It first sets up the working 
# directory and loads necessary packages. For each scenario with retrospective 
# analysis results stored in the output/retro directory, the script creates a 
# corresponding report directory in report/retro. It then loads the retrospective 
# data, generates plots showing the retrospective patterns in spawning stock 
# biomass (SSB) and fishing mortality (F), and saves these plots as PNG files. 
# Additionally, the script calculates the retrospective bias metrics 
# (Rho and ForecastRho), organizes these metrics into a table using flextable,
# and saves the table as both a PNG image and an .RData file. The final output 
# for each scenario is stored in its respective report/retro directory. 


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
    width=5,height=5,res=300,units='in')

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


rho_ssb<-ssb.retro %>% filter(peel=="Combined") %>% select(Rho) %>% as.numeric()
rho_f<-f.retro %>% filter(peel=="Combined") %>% select(Rho) %>% as.numeric()


rho_retro<-rbind(ssb.retro,f.retro)
ft1<-rho_retro %>%
  mutate(across(c(Rho,ForcastRho), round, 2)) %>%  
  flextable()

# Guarda la flextable como un archivo de imagen PNG
save_as_image(ft1, path = paste0(retro_rep,"/table_rho.png"))
save(ft1, file=paste0(retro_rep,"/table_rho.RData"))
save(rho_ssb,rho_f, file=paste0(retro_rep,"/rho.RData"))
}

setwd(wd)