## Prepare plots and tables for report

## Before:
## After:

# Script information ------------------------------------------------------
# This script automates the preparation of plots and tables for a report based 
# on the results of SS3 model runs. For each model scenario, the script loads 
# input and output data, generates a series of plots including temporal data 
# coverage, growth curves, catches, age compositions, residuals, and more. 
# Additionally, the script creates summary tables using `flextable` for estimated 
# parameters, time series, and other key diagnostics. The generated plots and 
# tables are saved in the `report/run` directory corresponding to each scenario.
# Finally, the script performs a commit and push of the generated files to the
# Git repository, ensuring that all report elements are properly versioned. 
# *To avoid making changes directly to the main repository, it is recommended to
# either comment out the section that performs the commit and push, or switch to 
# a different branch before running the script. This will help ensure proper 
# version control of the generated files without impacting the main branch.*

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
  png(file.path(paste0(path,"/fig_input_data.png")),width=9,height=5,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.8)
  SSplotData(output, subplots = 2,cex.main = 0.8,cex = 1,margins = c(2.1, 2.1, 1.1, 8.1))
  dev.off()

## Growth curve, length-weight relationship and maturity ----
  png(file.path(paste0(path,"/fig_biology.png")),width=10,height=8,res=300,units='in')
  sspar(mfrow = c(2, 2), plot.cex = 0.8)
  SSplotBiology(output, subplot = c(1,5,6),seas=4,mainTitle = FALSE)
  dev.off()


## wt at age ----
  inputs$wtatage[inputs$wtatage==0]<-NA
  watage_mid<-inputs$wtatage %>% filter(Fleet==-1) %>% 
    select(c(Yr,Seas,`0`,`1`,`2`,`3`)) %>% melt(id.vars=c("Yr","Seas"))
  
  fig7<-watage_mid %>% ggplot(aes(x=Yr,y=value,colour=variable)) +
    geom_point() + geom_line()+
    facet_wrap(.~Seas,ncol=2,as.table = TRUE, strip.position = "top",
               labeller = labeller(Seas = c("1" = "Q1", 
                                            "2" = "Q2",
                                            "3" = "Q3", 
                                            "4" = "Q4")))+
    labs(x="Year",y="Weight mean (Kg)")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('')+
    theme(plot.title = element_text(size =5),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99"),
          legend.title = element_text(size = 6, face = "bold"), 
          legend.text = element_text(size = 6)) + 
    theme(legend.position = 'top') 
  ggsave(file.path(paste0(path,"/fig_weight_by_quarters.png")), fig7,  width=5, height=5)
  
## Catches by $uarters
catches<-  inputs$dat$catch%>% filter(year>-999) %>% select(c(year,seas,catch)) 
fig1b<- ggplot(catches, aes(x = year, y = catch,fill=factor(seas))) +
  geom_bar(stat = "identity") +
  labs(x = "Year",y = "Catches (ton)",title = "",fill = "Quarters" ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "top")

ggsave(file.path(paste0(path,"/fig_catches.png")), fig1b,  width=8, height=5)

  
## Fit data: Abundance indices ----
  png(file.path(paste0(path,"/fig_indices_fit.png")),width=6,height=7,res=300,units='in')
  sspar(mfrow = c(4, 2), plot.cex = 0.6)
  SSplotIndices(output, subplots = c(2,3),mainTitle = T)
  dev.off()

## age composition ----
inputs$dat$agecomp[ inputs$dat$agecomp==0]<-NA
agecomp<-  inputs$dat$agecomp  %>% filter(FltSvy==1) %>% 
           select(c(Yr,Seas,`a0`,`a1`,`a2`,`a3`)) %>% 
           melt(id.vars=c("Yr","Seas")) %>% 
           mutate(variable = factor(variable, levels = c("a0","a1", "a2", "a3"),
                           labels = c("0","1", "2", "3")))

figxx<- agecomp %>% ggplot(aes(x=Yr,y=value,fill=variable)) +
  geom_bar(stat = "identity") + 
    facet_wrap(.~Seas,ncol=2,as.table = TRUE, strip.position = "top",
               labeller = labeller(Seas = c("3" = "Q1", 
                                            "6" = "Q2",
                                            "9" = "Q3", 
                                            "12" = "Q4")))+
    labs(x="Year",y="Proportion",fill="Age")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('')+
    theme(plot.title = element_text(size =12),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99")) + 
    theme(legend.position = 'top') 
ggsave(file.path(paste0(path,"/fig_agecomp_by_quartersSeine.png")), figxx,  width=7, height=5)
  
# Age composition surveys ----

agecompSurvey <-  inputs$dat$agecomp  %>% filter(FltSvy>=2) %>% 
  select(c(Yr,Seas,FltSvy,`a0`,`a1`,`a2`,`a3`)) %>% 
  melt(id.vars=c("Yr","Seas","FltSvy")) %>% 
  mutate(variable = factor(variable, 
                           levels = c("a0","a1", "a2", "a3"),
                           labels = c("0","1", "2", "3")))

figx1<- agecompSurvey %>% ggplot(aes(x=Yr,y=value,fill=variable)) +
  geom_bar(stat = "identity") + 
  facet_wrap(.~FltSvy,ncol=1,as.table = TRUE, strip.position = "top",
             labeller = labeller(FltSvy = c("2" = "PELAGO", 
                                          "3" = "ECOCADIZ",
                                          "5" = "ECOCADIZ-RECLUTAS"))) +
  labs(x="Year",y="Proportion",fill="Age")+
  scale_color_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray80")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99")) + 
  theme(legend.position = 'top') 
ggsave(file.path(paste0(path,"/fig_agecomp_by_quartersSurveys.png")), figx1,  width=5, height=5)

  
## Fit data: Length composition (aggregated) ----
  png(file.path(paste0(path,"/fig_age_fit_agg.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(21),kind = "AGE",maxrows = 2,maxcols = 2,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()


## Fit data: Length composition by source data ----
### *FLEET by quarters* ----
  png(file.path(paste0(path,"/fig_age_fit_Seine.png")),width=10,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 1,maxrows = 12,maxcols =12,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()


### *PELAGO spring survey* ----
  png(file.path(paste0(path,"/fig_age_fit_Pelago.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 2,maxrows = 6,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

### *ECOCADIZ summer survey* ----
  png(file.path(paste0(path,"/fig_age_fit_Ecocadiz.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 3,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

### *ECOCADIZ-RECLUTAS fall survey* ----

  png(file.path(paste0(path,"/fig_age_fit_EcocadizRecl.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 5,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

## Residuals length composition by source data

### *FLEET by quarters* ----
  png(file.path(paste0(path,"/fig_age_residuals_Seine.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 1,maxrows = 12,maxcols = 5,
              cexZ1 = 1.5,yupper=5,
              cohortlines=T, showsampsize = F,showeffN = F)
  dev.off()

### *PELAGO spring survey* ----
  png(file.path(paste0(path,"/fig_age_residuals_Pelago.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets =3,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

### *ECOCADIZ summer survey* ----
  png(file.path(paste0(path,"/fig_age_residuals_Ecocadiz.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 2,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

### *ECOCADIZ-RECLUTAS fall survey* ----
  png(file.path(paste0(path,"/fig_age_residuals_EcocadizRecl.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 5,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

## Run test indices ----
  png(file.path(paste0(path,"/fig_runtest_residuals_indices.png")),width=7,height=7,res=300,units='in')
  sspar(mfrow = c(3, 2), plot.cex = 0.8)
  SSplotRunstest(output,subplots = "cpue", add = TRUE, legendcex = 0.8,verbose = F)
  SSplotJABBAres(output,subplots = "cpue", add = TRUE, legendcex = 0.8,verbose = F)
  dev.off()

## Run test length ----
  png(file.path(paste0(path,"/fig_runtest_residuals_age.png")),width=7,height=7,res=300,units='in')
  sspar(mfrow = c(3, 2), plot.cex = 0.8)
  SSplotRunstest(output,subplots = "age", add = TRUE, legendcex = 0.8,verbose = F)
  SSplotJABBAres(output,subplots = "age", add = TRUE, legendcex = 0.8,verbose = F)
  dev.off()

## Selectivity ----
  png(file.path(paste0(path,"/fig_age_selectivity.png")),width=6,height=5,res=300,units='in')
  SSplotSelex(output,subplots =1)
  dev.off()
  
## Stock-Recluta ----
  png(file.path(paste0(path,"/fig_stock-recluta.png")),width=4,height=4,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.6)
  SSplotSpawnrecruit(output,subplots =2,pwidth = 4,pheight = 4,legendloc ="bottomright")
  dev.off()

##  Recruitment devs
  png(file.path(paste0(path,"/fig_Recdevs.png")),width=5,height=5,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.8)
  SSplotRecdevs(output,subplots = 2,pwidth = 5,pheight = 5)
  dev.off()
  
  #SSplotSPR(output)
  #SSplotCatch(output)
  #SSplotNumbers(output)
  
  ## time series ----
  stdreptlist<-data.frame(output$derived_quants[,1:3])
  head(stdreptlist)
  head(summary)
  
  # Define the range of years to include
  start_year <- 1989
  end_year <- 2023
  
  # Define a function to process data
  process_data <- function(data, pattern, value_col, stddev_col = NULL) {
    # Ensure the required column exists
    if (!"Label" %in% colnames(data)) {
      stop("The 'Label' column is missing in the data frame.")
    }
    
    filtered_data <- data %>%
      filter(grepl(pattern, Label)) %>%
      mutate(year = as.numeric(sub(paste0(pattern, "_"), "", Label))) %>%
      filter(!is.na(year) & year >= start_year & year <= end_year) %>%
      select(year, Value, StdDev)
    
    # Remove StdDev column if not needed
    if (is.null(stddev_col)) {
      filtered_data <- filtered_data %>% select(-StdDev)
    }
    
    return(filtered_data)
  }
  
  # Process 'summary' data
  process_summary_data <- function(data, pattern, value_col) {
    if (!"V1" %in% colnames(data)) {
      stop("The 'V1' column is missing in the summary data frame.")
    }
    
    filtered_data <- data %>%
      filter(grepl(pattern, V1)) %>%
      mutate(year = as.numeric(sub(paste0(pattern, "_"), "", V1))) %>%
      filter(!is.na(year) & year >= start_year & year <= end_year) %>%
      select(year, value_col)
    
    return(filtered_data)
  }
  
  # Apply the function to each dataset
  ssb <- process_data(stdreptlist, "SSB", "Value", "StdDev")
  ssb$type<-"SSB"
  recr <- process_data(stdreptlist, "Recr", "Value", "StdDev")
  recr$type<-"Rt"
  ft <- process_data(stdreptlist, "F", "Value", "StdDev")
  ft$type<-"Ft"
  bt <- process_summary_data(summary, "TotBio", "V2") %>% mutate(StdDev=NA)
  colnames(bt)<-c("year","Value","StdDev")
  bt$type<-"Bt"
  
  catch <- process_summary_data(summary, "TotCatch", "V2") %>% mutate(StdDev=NA)
  colnames(catch)<-c("year","Value","StdDev")
  catch$type<-"Catch"
  
  data<-rbind(ssb,recr,ft,bt)
  data <- data %>%
    mutate(
      Value = as.numeric(Value),
      StdDev = as.numeric(StdDev)) %>%
    mutate(
      lower = case_when(
        is.na(StdDev) ~ 0,
        TRUE ~ Value - 1.96 * StdDev),
      upper = case_when(
        is.na(StdDev) ~ 0,
        TRUE ~ Value + 1.96 * StdDev))
  
  
fig1a<- ggplot(data, aes(x = year, y = Value)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
        facet_wrap(.~type,scales = "free")+
        labs(x = "Year",y = "Value",title = "") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),legend.position = "top")
ggsave(file.path(paste0(path,"/fig_time_series.png")), fig1a,  width=8, height=5)
  
  
# tablas ----
tb_catch <- inputs$dat$catch%>% filter(year>-999) %>% 
            select(c(year,seas,catch)) %>% 
            pivot_wider(
            names_from = "seas", 
            values_from = c("catch"))

indices <- inputs$dat$CPUE%>% 
           pivot_wider(
           names_from = "index",  
           values_from = c("obs", "se_log","seas"))

nsamp <- inputs$dat$agecomp %>%
         filter(FltSvy >= 2) %>% 
         select(Yr, FltSvy, Nsamp) %>% 
         pivot_wider(
           names_from = FltSvy, 
           values_from = Nsamp) %>% 
         rename_with(~c("year", "nm_2", "nm_3", "nm_5"),
                     .cols = c(Yr, `2`, `3`, `5`)) %>%
         mutate(nm_4=NA)

combined_df <- left_join(indices, nsamp, by = "year")

SR_input<-inputs$ctl$SR_parms

params <- output$estimated_non_dev_parameters%>%
          rownames_to_column(var = "Parameter") %>% 
          mutate(cut=c("Recruitment",
                      rep("Catchability",4),
                      rep("Selectivity",4))) 

params_est <- params %>% 
              select(c(cut,Parameter,Value,Phase,Min,Max,Init,Status,Parm_StDev,Gradient))

natM <- inputs$ctl$natM

maturity <- inputs$wtatage %>% 
            filter(Fleet==-2) %>% 
            select(`0`,`1`,`2`,`3`)

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


timeseries<-data %>% select(c(year,Value,type)) %>% 
pivot_wider(
  names_from = "type",  # Esta columna (index) se convertirá en nombres de columnas
  values_from = c("Value")  # Estas columnas llenarán las nuevas columnas
)


#'*=================================================================*
# Flextables ----
## table index by surveys 
indices<-indices %>%
  select(year,obs_2,obs_3,obs_4,obs_5) %>%
  arrange(year) %>%  # Ordenar por la columna 'year'
  mutate(across(c(obs_2,obs_3,obs_4,obs_5), \(x) round(x, 0))) 

ft1<-indices%>%  # Redondear todas las columnas seleccionadas
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
ft1 <- fontsize(ft1, size = 9, part = "body")
ft1 <- autofit(ft1)
ft1


#'*CV, NM and timing survey*
ft2<-combined_df %>%
  select(year,
         seas_2,se_log_2,nm_2,
         seas_3,se_log_3,nm_3,
         seas_4,se_log_4,nm_4,
         seas_5,se_log_5,nm_5) %>%
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

#'* estimates parameters: tb_params_est*
ft3<-params_est %>%flextable()
ft3<-merge_at(ft3,i=2:5, j=1)
ft3<-merge_at(ft3,i=6:9, j=1)
ft3<-set_header_labels(ft3,cut="")
ft3

#'*tb_natM*
ft4<-natM %>% flextable()
ft4


#'*tb_maturity*
ft5 <- maturity[1, , drop = FALSE] %>% 
  setNames(c("Age_0", "Age_1", "Age_2", "Age_3")) %>% 
  flextable()
ft5

#'*diagnostic table*
ft6<-diags%>% 
  flextable()
ft6

#'*index residuals: tb_run_cpue*
ft7<-run_cpue%>%
  flextable()
ft7

#'*aggregate index residuals: tb_jabba_cpue*
ft8<-jaba_cpue %>% flextable()
ft8

#'*age residuals: tb_run_age*
ft9<-run_age%>%
  flextable()
ft9

#'*aggregate age residuals: tb_jabba_age*
ft10<-jaba_age %>% flextable()
ft10

#'*time series*
ft11<-timeseries%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))%>% flextable()
ft11 <- colformat_double(ft11, digits=1, na_str = "")
ft11 <- colformat_num(ft11,big.mark = "", na_str = "")
ft11 <- align(ft11,part = "header", align = "center") 
ft11 <- autofit(ft11)
ft11

#'*catches*
ft12<-tb_catch %>%
  arrange(year)%>%
  mutate(across(where(is.numeric), ~round(.x, 0)))%>% 
  setNames(c("Year","Q1", "Q2", "Q3", "Q4")) %>% 
  mutate(Total=Q1+Q2+Q3+Q4)%>% 
  flextable()
ft12 <- add_header_row(ft12, 
                      values = c("", "Catches (ton)"),
                      colwidths = c(1, 5))
ft12 <- colformat_double(ft12, digits=1, na_str = "")
ft12 <- colformat_num(ft12,big.mark = "", na_str = "")
ft12 <- align(ft12,part = "header", align = "center") 
ft12 <- fontsize(ft12, size = 8, part = "body")
ft12 <- autofit(ft12)
ft12

#'*=================================================================*
# save tables
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

save_as_image(ft11, path = paste0(path,"/tb_timeseries.png"))
save_as_image(ft12, path = paste0(path,"/tb_catches.png"))
#'*=================================================================*
# save Rdata tables
save(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,file=paste0(path,"/tables_run.RData"))
}


# Se hace commit y push de los cambios 
for(i in 1:length(esc)){
  run_rep <- paste0("report/run/",esc[i])
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_rep))
  system2("git", args = c("add","report_01_run.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualizados report/run/", esc[i]," ",fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
  system2("git", args = c("push"))
}