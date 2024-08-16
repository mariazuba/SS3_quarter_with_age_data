## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(r4ss)
library(openxlsx)
library(readxl)

run_esc<-"boot/data/run/" 
esc<-list.files(run_esc)

for(i in 1:length(esc)){
run.dir  <- paste0(run_esc,esc[i])
inputs <- r4ss::SS_read(dir = run.dir)

mkdir(paste0("data/run/",esc[i]))
data_esc<<-paste0("data/run/",esc[i])

index <- inputs$dat$CPUE

agecompSeine<-inputs$dat$agecomp %>% filter(FltSvy==1)
agecompPelago<-inputs$dat$agecomp %>% filter(FltSvy==2)
agecompEcocadiz<-inputs$dat$agecomp %>% filter(FltSvy==3)
agecompEcoReclutas<-inputs$dat$agecomp %>% filter(FltSvy==5)

fecundity<-inputs$wtatage %>% filter(Fleet==-2)
watage_init<-inputs$wtatage %>% filter(Fleet==0)
watage_mid<-inputs$wtatage %>% filter(Fleet==-1)
watageSeine<-inputs$wtatage %>% filter(Fleet==1)
watagePelago<-inputs$wtatage %>% filter(Fleet==2)
watageEcocadiz<-inputs$wtatage %>% filter(Fleet==3)
watageBocadeva<-inputs$wtatage %>% filter(Fleet==4)
watageEcoReclutas<-inputs$wtatage %>% filter(Fleet==5)

save(inputs,     
     file=paste0(data_esc,"/inputs.RData"))

#'*-------------------------------------------------------------*
wb <- createWorkbook()
addWorksheet(wb, "Seine")
writeData(wb, sheet = "Seine", x = agecompSeine)
addWorksheet(wb, "Pelago")
writeData(wb, sheet = "Pelago", x = agecompPelago)
addWorksheet(wb, "Ecocadiz")
writeData(wb, sheet = "Ecocadiz", x = agecompEcocadiz)
addWorksheet(wb, "EcoReclutas")
writeData(wb, sheet = "EcoReclutas", x = agecompEcoReclutas)

saveWorkbook(wb, paste0(data_esc,"/agecomposition.xlsx"),overwrite = TRUE)
#'*-------------------------------------------------------------*
wb <- createWorkbook()
addWorksheet(wb, "fecundity")
writeData(wb, sheet = "fecundity", x = fecundity)
addWorksheet(wb, "watage_init")
writeData(wb, sheet = "watage_init", x = watage_init)
addWorksheet(wb, "watage_mid")
writeData(wb, sheet = "watage_mid", x = watage_mid)
addWorksheet(wb, "watageSeine")
writeData(wb, sheet = "watageSeine", x = watageSeine)
addWorksheet(wb, "watagePelago")
writeData(wb, sheet = "watagePelago", x = watagePelago)
addWorksheet(wb, "watageEcocadiz")
writeData(wb, sheet = "watageEcocadiz", x = watageEcocadiz)
addWorksheet(wb, "watageBocadeva")
writeData(wb, sheet = "watageBocadeva", x = watageBocadeva)
addWorksheet(wb, "watageEcoReclutas")
writeData(wb, sheet = "watageEcoReclutas", x = watageBocadeva)
saveWorkbook(wb, paste0(data_esc,"/Watage.xlsx"),overwrite = TRUE)
#'*-------------------------------------------------------------*
wb <- createWorkbook()
addWorksheet(wb, "Index")
writeData(wb, sheet = "Index", x = index)

saveWorkbook(wb, paste0(data_esc,"/Index.xlsx"),overwrite = TRUE)
#'*-------------------------------------------------------------*
}


