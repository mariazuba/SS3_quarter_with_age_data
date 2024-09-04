
run_esc<-paste0(getwd(),"/model/run/")

esc<-list.files(run_esc)


  i=1
run.dir  <- paste0(run_esc,esc[i])
output <- r4ss::SS_output(dir = run.dir,forecast=FALSE)
summary <- read.table(paste0(run.dir,"/ss_summary.sso"),header=F,sep="",na="NA",fill=T)
library(r4ss)
# 
# # LOAD output and wtatage
waa <- SS_readwtatage(paste0(run.dir,"/wtatage.ss_new"))

# CHECK reported SSB 1991 value = 4300

output$timeseries[10,1:8]
output$derived_quants["SSB_1989",]

# GET numbers at age 1991 S1

num_y1s1 <-  output$natage[
  output$natage$Yr == 1989 &
    output$natage$Seas == 2 &
    output$natage$"Beg/Mid" == "B",
  paste(0:output$accuage)]

# GET Mat*Fecund 1991

mwaa_y1s1 <- unlist(waa[waa$Yr == 1989 & waa$Fleet == -2, -c(1:6)])

# COMPARE SSB to 4300
sum(num_y1s1 * mwaa_y1s1)

# calculo de catchability
output$cpue %>% select(c("Yr","Fleet_name","Vuln_bio","Obs","Exp","Calc_Q"))

