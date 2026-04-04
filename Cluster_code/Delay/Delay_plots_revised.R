library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
#Read in the combined data
num_delays=6
scenario_no=1

counts_list<-list()

for(delay_no in 1:6){
  dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/delay_",delay_no)
  load(file.path(dirname,"combine_summary_delay.Rdata"))
  
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
  
  
  counts_list[[delay_no]]<-data.frame(late_control,late_screen,late_interval,early_control,early_screen,early_interval,delay=the_delay)
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


counts_long <- counts_summary %>%
  pivot_longer(
    cols = -delay,
    names_to = c(".value", "variable"),
    names_pattern = "(mean|se)_(.*)"
)

out_abs<-ggplot(data=subset(counts_long,!variable%in%c("rel_reduction","ratio")),aes(x=delay,y=mean))+
  geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se))+facet_wrap(~variable)+theme_minimal()

out_reduction<-ggplot(data=subset(counts_long,variable%in%c("rel_reduction","ratio")),aes(x=delay,y=mean))+
  geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se))+facet_wrap(~variable)+theme_minimal()

save(counts_list,file=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/delay_summary.Rdata"))

