args <- commandArgs(trailingOnly = TRUE)
delay_no<-as.numeric(args[1])
scenario_no <-as.numeric(args[2])
extended_followup<-as.numeric(args[3])
early_to_late_rate<-as.numeric(args[4])
screen_arm_delay<-as.numeric(args[5])

#delay_no=4
#scenario_no=2
#extended_followup=0
#early_to_late_rate=2
#screen_arm_delay<-1

library(ggplot2)
library(reshape2)
library(dplyr)



the_delay=case_when(delay_no == 1 ~ 0,
                    delay_no==2 ~ 2,
                    delay_no==3 ~ 10,
                    delay_no==4 ~ 30,
                    delay_no==5 ~ 60,
                    delay_no==6 ~ 90)

summary_list=list()


dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/delay_",delay_no)
for(i in 1:200){
  thename=paste0("summary_delay_", extended_followup, "_", early_to_late_rate,"_", screen_arm_delay, "_", i,".Rdata")
  try({load(file.path(dirname,thename))})
  summary_list[[i]]=out  

}

all_control_followup=do.call(rbind,lapply(summary_list,"[[","control_cancers_followup"))

all_screen=do.call(rbind,lapply(summary_list,"[[","summary_screen_cancers"))
all_at_risk=do.call(rbind,lapply(summary_list,"[[","summary_screen_at_risk"))
all_interval=do.call(rbind,lapply(summary_list,"[[","summary_interval_cancers"))

savename=paste0("combine_summary_delay_", extended_followup, "_", early_to_late_rate, "_", screen_arm_delay, ".Rdata")

save(all_control_followup,all_screen,all_at_risk, all_interval,the_delay, file=file.path(dirname, savename))




