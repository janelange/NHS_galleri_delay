library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

#args <- commandArgs(trailingOnly = TRUE)

#scenario_no <-as.numeric(args[1])
#extended_followup<-as.numeric(args[2])
#early_to_late_rate<-as.numeric(args[3])
#screen_arm_delay<-as.numeric(args[4])

#scenario_no=3
#extended_followup=1
#early_to_late_rate=4
#delay_no=1
#screen_arm_delay=1

all_list=list()

for(early_to_late_rate in c(0)){
  for(extended_followup in 0:1){
    for(scenario_no in 1:5){
      for(screen_arm_delay in 0:1){  
        num_delays=6
        counts_list<-list()
        
        for(delay_no in 1:num_delays){
          
          dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/delay_",delay_no)
          filename=paste0("combine_summary_delay_", extended_followup, "_", early_to_late_rate, "_", screen_arm_delay, ".Rdata")
          
          load(file.path(dirname,filename))
          
          late_control<-all_control_followup %>% filter(stage=="2")%>%group_by(seed, followup)%>%
            summarise(tot_late_control=sum(clinical))%>%arrange(seed,followup)%>%filter(as.numeric(followup)<=3)
          
          late_screen<-all_screen %>% filter(stage=="2")%>%group_by(seed,interval_no)%>%
            summarise(tot_late_screen=sum(screen))%>%
            mutate(followup = factor(str_extract(interval_no, "\\d+"),levels = seq(1:10)))%>%
            arrange(seed,followup)%>%filter(as.numeric(followup)<=3)
          
          late_interval<-all_interval %>% filter(stage=="2")%>%group_by(seed,interval_no)%>%
            summarise(tot_late_interval=sum(clinical))%>%
            mutate(followup = factor(str_extract(interval_no, "\\d+"),levels = seq(1:10)))%>%
            arrange(seed,followup)%>%filter(as.numeric(followup)<=3)
          
          early_control<-all_control_followup %>% filter(stage=="1")%>%group_by(seed, followup)%>%
            summarise(tot_early_control=sum(clinical))%>%arrange(seed,followup)%>%
            arrange(seed,followup)%>%filter(as.numeric(followup)<=3)
          
          early_screen<-all_screen %>% filter(stage=="1")%>%group_by(seed,interval_no)%>%
            summarise(tot_early_screen=sum(screen))%>%
            mutate(followup = factor(str_extract(interval_no, "\\d+"),levels = seq(1:10)))%>%
            arrange(seed,followup)%>%filter(as.numeric(followup)<=3)
          
          early_interval<-all_interval %>% filter(stage=="1")%>%group_by(seed,interval_no)%>%
            summarise(tot_early_interval=sum(clinical))%>%
            mutate(followup = factor(str_extract(interval_no, "\\d+"),levels = seq(1:10)))%>%
            arrange(seed,followup)%>%filter(as.numeric(followup)<=3)
          
          
          counts_list[[delay_no]]<-data.frame(late_control,late_screen,late_interval,early_control,early_screen,early_interval,delay=the_delay,extended_followup=extended_followup,
                                              early_to_late_rate=early_to_late_rate,scenario_no=scenario_no,screen_arm_delay=screen_arm_delay)
        }
        
        counts=do.call(rbind,counts_list)%>%mutate(followup=as.numeric(as.character(followup)))
        
        counts_summary <- subset(counts,followup<=3) %>% group_by(seed,delay) %>% summarise(tot_late_control=sum(tot_late_control),
                                                                                            tot_late_screen=sum(tot_late_screen), 
                                                                                            tot_late_interval=sum(tot_late_interval),
                                                                                            tot_early_screen=sum(tot_early_screen),
                                                                                            tot_early_interval=sum(tot_early_interval),
                                                                                            tot_early_control=sum(tot_early_control))
        
        
        counts_summary_table=counts_summary
        
        
        
        
        
        counts_summary <- counts_summary %>% group_by(delay) %>% summarise(mean_late_control=mean(tot_late_control),
                                                                           se_late_control=sd(tot_late_control)/sqrt(200),
                                                                           mean_early_control=mean(tot_early_control),
                                                                           se_early_control=sd(tot_early_control)/sqrt(200),
                                                                           mean_late_screen=mean(tot_late_screen), 
                                                                           se_late_screen=sd(tot_late_screen)/sqrt(200),
                                                                           mean_late_screen_arm=mean(tot_late_screen+tot_late_interval),
                                                                           se_late_screen_arm=sd(tot_late_screen+tot_late_interval)/sqrt(200),
                                                                           mean_late_interval=mean(tot_late_interval),
                                                                           se_late_interval=sd(tot_late_control)/sqrt(200),
                                                                           mean_reduction=mean(tot_late_control-(tot_late_screen+tot_late_interval)),
                                                                           se_reduction=sd(tot_late_control-(tot_late_screen+tot_late_interval))/sqrt(200),
                                                                           mean_ratio=mean((tot_late_screen+tot_late_interval)/(tot_late_control)),
                                                                           se_ratio=sd((tot_late_screen+tot_late_interval)/(tot_late_control))/sqrt(200),
                                                                           mean_rel_reduction=mean((tot_late_control-(tot_late_screen+tot_late_interval))/tot_late_control),
                                                                           se_rel_reduction=sd((tot_late_control-(tot_late_screen+tot_late_interval))/tot_late_control)/sqrt(200),
                                                                           mean_early_screen=mean(tot_early_screen), 
                                                                           se_early_screen=sd(tot_early_screen)/sqrt(200),
                                                                           mean_early_screen_arm=mean(tot_early_screen+tot_early_interval),
                                                                           se_early_screen_arm=sd(tot_early_screen+tot_early_interval)/sqrt(200),
                                                                           mean_early_interval=mean(tot_early_interval),
                                                                           se_early_interval=sd(tot_early_control)/sqrt(200)
        )
        
        
        
        all_list=c(all_list,counts_list)
        
        save(counts_list,file=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no, "/delay_summary_", extended_followup,"_", early_to_late_rate, "_", screen_arm_delay, ".Rdata"))
      }
    }
  }
}
combined_data=do.call(rbind,all_list)
save(combined_data,file=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/all_delay_summary_cal.Rdata"))