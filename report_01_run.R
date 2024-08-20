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
library(flextable)


# working directory
wd <- getwd()
# Load data ---------------------------------------------------------------

# input data
inputs<-"data/run"
outputs<-"output/run"
esc<-list.files(inputs)
#i=1
for(i in 1:length(esc)){
  
  load(paste0("output/run/",esc[i],"/output.RData"))
  load(paste0("data/run/",esc[i],"/inputs.RData")) 
  
  mkdir(paste0("report/run/",esc[i]))
  path<-paste0("report/run/",esc[i])

# Figures --------------------------------------------
## Temporal coverage of input data ----
  png(file.path(paste0(path,"/input_data.png")),width=9,height=5,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.8)
  SSplotData(output, subplots = 2,cex.main = 0.8,cex = 1,margins = c(2.1, 2.1, 1.1, 8.1))
  dev.off()

## Growth curve, length-weight relationship and maturity ----
  png(file.path(paste0(path,"/Biology.png")),width=10,height=8,res=300,units='in')
  sspar(mfrow = c(2, 2), plot.cex = 0.8)
  SSplotBiology(output, subplot = c(1,5,6),seas=4,mainTitle = FALSE)
  dev.off()


## wt at age ----
  inputs$wtatage[inputs$wtatage==0]<-NA
  watage_mid<-inputs$wtatage %>% filter(Fleet==-1) %>% 
    select(c(Yr,Seas,`0`,`1`,`2`,`3`)) %>% melt(id.vars=c("Yr","Seas"))
  
  fig7<-watage_mid %>% ggplot(aes(x=Yr,y=value,colour=variable)) +
    geom_point() + geom_line()+
    facet_wrap(.~Seas,ncol=2,as.table = TRUE, strip.position = "top")+
    labs(x="Year",y="Weight mean (Kg)")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('Weight at age by quarters')+
    theme(plot.title = element_text(size =12),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99")) + 
    theme(legend.position = 'top') 
  ggsave(file.path(paste0(path,"/Weight_by_quarters.png")), fig7,  width=5, height=6)
  
  
## Fit data: Abundance indices ----
  png(file.path(paste0(path,"/Indices_fit.png")),width=6,height=7,res=300,units='in')
  sspar(mfrow = c(4, 2), plot.cex = 0.6)
  SSplotIndices(output, subplots = c(2,3),mainTitle = T)
  dev.off()


## Fit data: Length composition (aggregated) ----
  png(file.path(paste0(path,"/Age_fit_agg.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(21),kind = "AGE",maxrows = 2,maxcols = 2,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()


## Fit data: Length composition by source data ----
### *FLEET by quarters* ----
  png(file.path(paste0(path,"/Age_fit_Seine.png")),width=10,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 1,maxrows = 12,maxcols =12,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()


### *PELAGO spring survey* ----
  png(file.path(paste0(path,"/Age_fit_Pelago.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 3,maxrows = 6,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

### *ECOCADIZ summer survey* ----
  png(file.path(paste0(path,"/Age_fit_Ecocadiz.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 2,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

### *ECOCADIZ-RECLUTAS fall survey* ----

  png(file.path(paste0(path,"/Age_fit_EcocadizRecl.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 5,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

## Residuals length composition by source data

### *FLEET by quarters* ----
  png(file.path(paste0(path,"/Age_residuals_Seine.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 1,maxrows = 12,maxcols = 5,
              cexZ1 = 1.5,yupper=5,
              cohortlines=T, showsampsize = F,showeffN = F)
  dev.off()

### *PELAGO spring survey* ----
  png(file.path(paste0(path,"/Age_residuals_Pelago.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets =3,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

### *ECOCADIZ summer survey* ----
  png(file.path(paste0(path,"/Age_residuals_Ecocadiz.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 2,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

### *ECOCADIZ-RECLUTAS fall survey* ----
  png(file.path(paste0(path,"/Age_residuals_EcocadizRecl.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 5,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

## Run test indices ----
  png(file.path(paste0(path,"/Runtest_residuals_indices.png")),width=7,height=7,res=300,units='in')
  sspar(mfrow = c(3, 2), plot.cex = 0.8)
  SSplotRunstest(output,subplots = "cpue", add = TRUE, legendcex = 0.8,verbose = F)
  SSplotJABBAres(output,subplots = "cpue", add = TRUE, legendcex = 0.8,verbose = F)
  dev.off()

## Run test length ----
  png(file.path(paste0(path,"/Runtest_residuals_age.png")),width=7,height=7,res=300,units='in')
  sspar(mfrow = c(3, 2), plot.cex = 0.8)
  SSplotRunstest(output,subplots = "age", add = TRUE, legendcex = 0.8,verbose = F)
  SSplotJABBAres(output,subplots = "age", add = TRUE, legendcex = 0.8,verbose = F)
  dev.off()

## Selectivity ----
  png(file.path(paste0(path,"/Selectividad.png")),width=6,height=5,res=300,units='in')
  SSplotSelex(output,subplots =1)
  dev.off()


# tablas ----



indices <- inputs$dat$CPUE%>% 
  pivot_wider(
    names_from = "index",  # Esta columna (index) se convertirá en nombres de columnas
    values_from = c("obs", "se_log","seas")  # Estas columnas llenarán las nuevas columnas
  )

nsamp <- inputs$dat$agecomp %>%
  filter(FltSvy >= 2) %>% 
  select(Yr, FltSvy, Nsamp) %>% 
  pivot_wider(names_from = FltSvy, values_from = Nsamp) %>% 
  rename_with(~c("year", "nm_2", "nm_3", "nm_5"), .cols = c(Yr, `2`, `3`, `5`)) %>% mutate(nm_4=NA)

combined_df <- left_join(indices, nsamp, by = "year")

params<-output$estimated_non_dev_parameters%>%
  rownames_to_column(var = "Parameter")

params_est<-params %>% select(c(Parameter,Value,Phase,Min,Max,Init,Status,Parm_StDev,Gradient))

natM<-inputs$ctl$natM

maturity<-inputs$wtatage %>% filter(Fleet==-2) %>% select(`0`,`1`,`2`,`3`)

convergency<-output$maximum_gradient_component
like<-output$likelihoods_used

run_cpue<-SSplotRunstest(output,subplots = "cpue")
jaba_cpue<-SSplotJABBAres(output,subplots = "cpue")
run_age<-SSplotRunstest(output,subplots = "age")
jaba_age<-SSplotJABBAres(output,subplots = "age")

#diagnostico
#Convergencia Likehood RMSE_indices RMSE_tallas Rho ForcastRho

diags<-data.frame(convergency=convergency,
                  Totallike=like$values[1],
                  RMSE_index=jaba_cpue$RMSE.perc[5],
                  RMSE_age=jaba_age$RMSE.perc[5])

## table index by surveys ----
ft1<-indices %>%
  select(year,obs_2,obs_3,obs_4,obs_5) %>%
  arrange(year) %>%  # Ordenar por la columna 'year'
  mutate(across(c(obs_2,obs_3,obs_4,obs_5), \(x) round(x, 0))) %>%  # Redondear todas las columnas seleccionadas
  flextable()  # Crear la flextable

ft1<-set_header_labels(ft1, 
                         year="year",
                         obs_2="PELAGO",
                         obs_3="ECOCADIZ",
                         obs_4="BOCADEVA",
                         obs_5="ECOCADIZ-RECLUTAS")

ft1 <- add_header_row(ft1, 
                      values = c("", "Acoustic Biomass (ton) by surveys"),
                      colwidths = c(1, 4))

ft1 <- colformat_double(ft1, digits=1, na_str = "")
ft1 <- colformat_num(ft1,big.mark = "", na_str = "")
ft1 <- align(ft1,part = "header", align = "center") 
ft1 <- autofit(ft1)
ft1

# table data weighting by surveys ----
ft2<-combined_df %>%
  select(year,seas_2,se_log_2,nm_2,seas_3,se_log_3,nm_3,seas_4,se_log_4,nm_4,seas_5,se_log_5,nm_5) %>%
  arrange(year)%>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%  # Redondear solo columnas numéricas
  flextable()

ft2<-set_header_labels(ft2,
                       year="year",
                       seas_2="month",
                       se_log_2="cv",
                       nm_2="nm",
                       seas_3="month",
                       se_log_3="cv",
                       nm_3="nm",
                       seas_4="month",
                       se_log_4="cv",
                       nm_4="nm",
                       seas_5="month",
                       se_log_5="cv",
                       nm_5="nm")

ft2 <- add_header_row(ft2, 
                      values = c("", "PELAGO","ECOCADIZ","BOCADEVA","ECOCADIZ-RECLUTAS"),
                      colwidths = c(1,3,3,3,3))

ft2 <- colformat_double(ft2, digits=1, na_str = "")
ft2 <- colformat_num(ft2,big.mark = "", na_str = "")
ft2 <- align(ft2,part = "header", align = "center") 
ft2 <- autofit(ft2)
ft2

ft3<-params_est %>%  # Redondear solo columnas numéricas
  flextable()
ft3

ft4<-natM %>% flextable()
ft4



ft5 <- maturity[1, , drop = FALSE] %>% 
  setNames(c("Age_0", "Age_1", "Age_2", "Age_3")) %>% 
  flextable()
ft5

ft6<-diags%>% 
  flextable()
ft6

ft7<-run_cpue%>%
  flextable()
ft7

ft8<-jaba_cpue %>% flextable()
ft8

ft9<-run_age%>%
  flextable()
ft9

ft10<-jaba_age %>% flextable()
ft10

save_as_image(ft1, path = paste0(path,"/tb_index.png"))
save_as_image(ft2, path = paste0(path,"/tb_cv_nm.png"))
save_as_image(ft3, path = paste0(path,"/tb_params_est.png"))
save_as_image(ft4, path = paste0(path,"/tb_natM.png"))
save_as_image(ft5, path = paste0(path,"/tb_maturity.png"))
save_as_image(ft6, path = paste0(path,"/tb_diagnostic.png"))

save_as_image(ft7, path = paste0(path,"/tb_run_cpue.png"))
save_as_image(ft8, path = paste0(path,"/tb_jabba_cpue.png"))

save_as_image(ft9, path = paste0(path,"/tb_run_age.png"))
save_as_image(ft10, path = paste0(path,"/tb_jabba_age.png"))

save(ft1,ft2,ft3, file=paste0(path,"/tables_run.RData"))
}

