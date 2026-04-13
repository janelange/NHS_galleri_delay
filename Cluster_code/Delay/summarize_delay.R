#process data for Marta's trial simulations
library(dplyr)
library(Epi)
library(cmprsk)
library(reshape2)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])
scenario_no <-as.numeric(args[2])
delay_no <-as.numeric(args[3])
extended_followup<-as.numeric(args[4])
early_to_late_rate<-as.numeric(args[5])


#i=1
#scenario_no=1
#delay_no=6
#extended_followup=0
#early_to_late_rate=4

the_delay=case_when(delay_no == 1 ~ 0,
                              delay_no==2 ~ .1,
                              delay_no==3 ~ .2,
                              delay_no==4 ~ .3,
                              delay_no==5 ~.5,
                              delay_no==6 ~.6)
                              

###################################################################################################################
#Load data file
dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/delay_",delay_no)

load(file.path(dirname,paste0("delay_", extended_followup, "_", early_to_late_rate,"_", i,".Rdata")))

###################################################################################################################
screen_info<-expand.grid(sex=c("Male","Female"),cancer_site=c("Anus",  "Bladder",  "Colorectal", "Esophagus",
                                                              "Headandneck", "Liver" , "Lung","Lymphoma",
                                                              "Ovary",  "Pancreas", "Gastric"),stage=c(1,2),interval_no=c("Year1","Year2","Year3","Year4","Year5","Year6","Year7","Year8","Year9","Year10"),seed=i,scenario_no=scenario_no)
control_info_age=expand.grid(sex=c("Male","Female"),cancer_site=c("Anus",  "Bladder",  "Colorectal", "Esophagus",
                                                              "Headandneck", "Liver" , "Lung","Lymphoma",
                                                              "Ovary",  "Pancreas", "Gastric"),stage=c(1,2),
                         age=factor(seq(50,81)),seed=i,scenario_no=scenario_no)
control_info_followup=expand.grid(sex=c("Male","Female"),cancer_site=c("Anus",  "Bladder",  "Colorectal", "Esophagus",
                                                                  "Headandneck", "Liver" , "Lung","Lymphoma",
                                                                  "Ovary",  "Pancreas", "Gastric"),stage=c(1,2),
                             followup=factor(seq(1,10)),seed=i,scenario_no=scenario_no)

###################################################################################################################
#SCREEN ARM
screen<-subset(results,arm=="S")%>%mutate(int1_age=start_age,int2_age=start_age+1,int3_age=start_age+2,int4_age=start_age+3,int5_age=start_age+4,
                                          int6_age=start_age+5,int7_age=start_age+6,int8_age=start_age+7,int9_age=start_age+8,int10_age=start_age+9)

#Get the number at risk at each screen
summary_screen_at_risk<- screen %>% group_by(sex) %>% summarise(Year1=sum(I(diagnosis_age_screen_scenario>=int1_age)),
                                                                Year2=sum(I(diagnosis_age_screen_scenario>=int2_age)),
                                                                Year3=sum(I(diagnosis_age_screen_scenario>=int3_age)),
                                                                Year4=sum(I(diagnosis_age_screen_scenario>=int4_age)),
                                                                Year5=sum(I(diagnosis_age_screen_scenario>=int5_age)),
                                                                Year6=sum(I(diagnosis_age_screen_scenario>=int6_age)),
                                                                Year7=sum(I(diagnosis_age_screen_scenario>=int7_age)),
                                                                Year8=sum(I(diagnosis_age_screen_scenario>=int8_age)),
                                                                Year9=sum(I(diagnosis_age_screen_scenario>=int9_age)),
                                                                Year10=sum(I(diagnosis_age_screen_scenario>=int10_age)))%>%
                          mutate(seed=i,scenario_no=scenario_no)

screen <- screen %>%
  mutate(
    dx_cat = case_when(
      diagnosis_age_screen_scenario >= start_age     & diagnosis_age_screen_scenario < start_age + 1  ~ "Year1",
      diagnosis_age_screen_scenario >= start_age + 1 & diagnosis_age_screen_scenario < start_age + 2  ~ "Year2",
      diagnosis_age_screen_scenario >= start_age + 2 & diagnosis_age_screen_scenario < start_age + 3  ~ "Year3",
      diagnosis_age_screen_scenario >= start_age + 3 & diagnosis_age_screen_scenario < start_age + 4  ~ "Year4",
      diagnosis_age_screen_scenario >= start_age + 4 & diagnosis_age_screen_scenario < start_age + 5  ~ "Year5",
      diagnosis_age_screen_scenario >= start_age + 5 & diagnosis_age_screen_scenario < start_age + 6  ~ "Year6",
      diagnosis_age_screen_scenario >= start_age + 6 & diagnosis_age_screen_scenario < start_age + 7  ~ "Year7",
      diagnosis_age_screen_scenario >= start_age + 7 & diagnosis_age_screen_scenario < start_age + 8  ~ "Year8",
      diagnosis_age_screen_scenario >= start_age + 8 & diagnosis_age_screen_scenario < start_age + 9  ~ "Year9",
      diagnosis_age_screen_scenario >= start_age + 9 & diagnosis_age_screen_scenario < start_age + 10 ~ "Year10",
      TRUE ~ "other"
    )
  )


#get the number of clinical cancers that occurred between screens
summary_interval_cancers<- filter(screen,!is.na(dx_cat)) %>% group_by(sex,interval_no=dx_cat,cancer_site, 
                                                                          stage=diagnosis_event_stage_screen_scenario) %>%
  summarise(clinical=sum(diagnosis_event_screen_scenario=="clin_cancer_diagnosis"),.groups = 'drop')%>%
  filter(stage!=3&!interval_no%in%c("other"))%>%
  mutate(seed=i,scenario_no=scenario_no)

summary_interval_cancers<-screen_info%>%full_join(summary_interval_cancers)
summary_interval_cancers[is.na(summary_interval_cancers)]<-0

#get the number of screen-detected cancers
summary_screen_cancers<- filter(screen,!is.na(dx_cat)) %>% 
    group_by(sex,interval_no=dx_cat,cancer_site, stage=diagnosis_event_stage_screen_scenario) %>% 
  summarise(screen=sum(diagnosis_event_screen_scenario=="screen_cancer_diagnosis"))

summary_screen_cancers<-screen_info%>%full_join(summary_screen_cancers)%>%
  filter(stage!=3&!interval_no%in%c("other"))%>%
  mutate(seed=i,scenario_no=scenario_no)
summary_screen_cancers[is.na(summary_screen_cancers)]<-0


###############################################################################
#Get the clinical cancers by start age and year of follow-up in the CONTROL ARM
###############################################################################


control<-subset(results,arm=="C") 
#get the counts of cancers by Followup
control <-control %>% mutate(followup=cut(clin_dx_age-start_age,breaks=seq(0,10),right=F,labels=seq(1,10)),seed=i,scenario_no=scenario_no)
control_cancers<- control %>% group_by(sex,followup, cancer_site,stage= clin_dx_event_stage,seed,scenario_no) %>% 
  summarise(clinical=sum(clin_dx_event=="clin_cancer_diagnosis"))%>%
  filter(stage!=3)

control_cancers_followup=control_cancers

###############################################################################

#Save summary data file


save_file_name=file.path(dirname,paste0("summary_delay_", extended_followup, "_", early_to_late_rate,"_", i,".Rdata"))
out=list(control_cancers_followup=control_cancers_followup,
         summary_screen_at_risk=summary_screen_at_risk,
         summary_screen_cancers=summary_screen_cancers,
         summary_interval_cancers=summary_interval_cancers,
          delay=the_delay,
         extended_followup=extended_followup,
         early_to_late_rate=early_to_late_rate)
save(out,file=save_file_name)





