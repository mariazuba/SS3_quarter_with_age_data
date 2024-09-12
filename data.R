# Script information ------------------------------------------------------

# Read assessment data (after the bootstrap procedure) and write TAF data tables

# Before running the script in folder ./bootstrap/initial/data we have: 
#         forecast.ss 
#         control.SS
#         data.SS
#         starter.ss
#         wtatage.ss
#         and other files needed later
# After running the script in folder ./data we have:
#         inputData.RData with r4ss input object and TAF .csv data tables:

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024/08/30
rm(list=ls())

# Load libraries ----------------------------------------------------------

library(icesTAF)
library(r4ss)

# Working directory and folders -------------------------------------------

# check working directory
getwd()

# directory with input files
run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 
data_esc<-paste0("data/run/",esc)

# create data folder using the function mkdir in icesTAF
mkdir(data_esc)
#dir.create(data_esc)

run.dir  <- paste0(run_esc,esc)
dat <- r4ss::SS_readdat(file = paste0(run.dir,"/data.SS"),verbose = TRUE)
wtatage <-r4ss::SS_readwtatage(file = paste0(run.dir,"/wtatage.ss"),verbose = TRUE)
#ctl <-r4ss::SS_readctl(file = paste0(run.dir,"/control.SS"),datlist =paste0(run.dir,"/data.SS"),verbose = FALSE)
#----------------------------------------------------------

# Prepare TAF tables ------------------------------------------------------

# natural maturity table
age<-c("a0","a1","a2","a3")
M<-c(2.97,	1.33,	1.33,	1.33)
natmort <- data.frame(rbind(M)) #ctl$natM
names(natmort)<-age

# catch in tonnes
catch <- subset(dat$catch, year>=(dat$styr), c('year','seas','catch'))
# total biomass in the acoustic survey Pelago
btotal_idx_pelago <- subset(dat$CPUE,index==2,c("year",'seas',"obs"))
# total biomass in the acoustic survey Ecocadiz
btotal_idx_ecocadiz <- subset(dat$CPUE,index==3,c("year",'seas',"obs"))
# total biomass in the DEPM survey Bocadeva
btotal_idx_bocadeva <- subset(dat$CPUE,index==4,c("year",'seas',"obs"))
# total biomass in the acoustic survey EcocadizReclutas
btotal_idx_ecocadizRec <- subset(dat$CPUE,index==5,c("year",'seas',"obs"))


# historical numbers at age in the catch table
catage    <- subset(dat$agecomp,FltSvy==1 & Yr>=(dat$styr) & Yr<(dat$endyr),c("Yr","Seas","a0","a1","a2","a3"))
# numbers at age in the acoustic survey Pelago
natage_idx_pelago <- subset(dat$agecomp,FltSvy==2 & Yr%in% dat$styr:dat$endyr,c("Yr","Seas","a0","a1","a2","a3"))
# numbers at age in the acoustic survey Ecocadiz
natage_idx_ecocadiz <- subset(dat$agecomp,FltSvy==3 & Yr%in% dat$styr:dat$endyr,c("Yr","Seas","a0","a1","a2","a3"))
# numbers at age in the acoustic survey EcocadizReclutas
natage_idx_ecocadizRec <- subset(dat$agecomp,FltSvy==5 & Yr%in% dat$styr:dat$endyr,c("Yr","Seas","a0","a1","a2","a3"))


# weight at age in the catch
waca <- subset(wtatage, Fleet=="1" & Yr %in% dat$styr:dat$endyr,c("Yr","Seas","0","1","2","3"))
# weight at age in the stock  mid season
west <- subset(wtatage, Fleet=="-1" & Yr %in% dat$styr:dat$endyr,c("Yr","Seas","0","1","2","3"))

# fecundity
fecundity <- subset(wtatage, Fleet=="-2" & Yr %in% dat$styr:dat$endyr,c("Yr","Seas","0","1","2","3"))

# maturity
maturity <- fecundity/west
maturity$'0'[maturity$'0'=="NaN"]<-0

# Write TAF tables in data folder -----------------------------------------

write.taf(list(natmort=natmort, 
               catage = catage, catch = catch, 
               btotal_idx_pelago = btotal_idx_pelago, natage_idx_pelago=natage_idx_pelago,
               btotal_idx_ecocadiz = btotal_idx_ecocadiz,natage_idx_ecocadiz=natage_idx_ecocadiz,
               btotal_idx_bocadeva = btotal_idx_bocadeva, 
               btotal_idx_ecocadizRec = btotal_idx_ecocadizRec,natage_idx_ecocadizRec=natage_idx_ecocadizRec,
               waca = waca, west = west, fecundity = fecundity, maturity = maturity),dir=data_esc)


# Save data in RData file  -----------------------------------------
save.image(paste0(data_esc,"/inputData.RData"))

# Script info -------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

rm(list=ls())
