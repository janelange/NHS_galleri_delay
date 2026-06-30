#process data for Marta's trial simulations
library(dplyr)
library(Epi)
library(cmprsk)
library(reshape2)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])
scenario_no <-as.numeric(args[2])

#i=1
#scenario_no=3

###################################################################################################################
#Load data file
dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no)
load(file.path(dirname,paste0("Marta_sim_results_", i,".Rdata")))


###################################################################################################################
screen_info<-expand.grid(sex=c("Male","Female"),cancer_site=c("Anus",  "Bladder",  "Colorectal", "Esophagus",
                                                              "Headandneck", "Liver" , "Lung","Lymphoma",
                                                              "Ovary",  "Pancreas", "Gastric"),stage=c(1,2),screen_no=c("Screen1","Screen2","Screen3","Post_screen3"),seed=i,scenario_no=scenario_no)
control_info=expand.grid(sex=c("Male","Female"),cancer_site=c("Anus",  "Bladder",  "Colorectal", "Esophagus",
                                                              "Headandneck", "Liver" , "Lung","Lymphoma",
                                                              "Ovary",  "Pancreas", "Gastric"),stage=c(1,2),
                         age=factor(seq(50,81)),seed=i,scenario_no=scenario_no)

###################################################################################################################
#SCREEN ARM
screen<-subset(results,arm=="S")%>%mutate(screen1_age=start_age,screen2_age=start_age+1,screen3_age=start_age+2)

#Get the number at risk at each screen
summary_screen_at_risk<- screen %>% group_by(sex) %>% summarise(Screen1=sum(I(diagnosis_age_screen_scenario>=screen1_age)),
                                                                Screen2=sum(I(diagnosis_age_screen_scenario>=screen2_age)),
                                                                Screen3=sum(I(diagnosis_age_screen_scenario>=screen3_age)),
                                                                Year3=sum(I(diagnosis_age_screen_scenario>=screen3_age+1)))%>%
                          mutate(seed=i,scenario_no=scenario_no)

screen <-screen %>% mutate(dx_cat=case_when(diagnosis_age_screen_scenario>=start_age&diagnosis_age_screen_scenario<start_age+1~"Screen1",
                                                diagnosis_age_screen_scenario+1>=start_age&diagnosis_age_screen_scenario<start_age+2~"Screen2",
                                                diagnosis_age_screen_scenario+2>=start_age&diagnosis_age_screen_scenario<start_age+3~"Screen3",
                                                .default = "other"))

#get the number of clinical cancers that occurred between screens
summary_interval_cancers<- filter(screen,!is.na(dx_cat)) %>% group_by(sex,screen_no=dx_cat,cancer_site, 
                                                                          stage=diagnosis_event_stage_screen_scenario) %>%
  summarise(clinical=sum(diagnosis_event_screen_scenario=="clin_cancer_diagnosis"),.groups = 'drop')%>%
  filter(stage!=3&!screen_no%in%c("other"))%>%
  mutate(seed=i,scenario_no=scenario_no)

summary_interval_cancers<-screen_info%>%full_join(summary_interval_cancers)
summary_interval_cancers[is.na(summary_interval_cancers)]<-0

#get the number of screen-detected cancers
summary_screen_cancers<- filter(screen,!is.na(dx_cat)) %>% 
    group_by(sex,screen_no=dx_cat,cancer_site, stage=diagnosis_event_stage_screen_scenario) %>% 
  summarise(screen=sum(diagnosis_event_screen_scenario=="screen_cancer_diagnosis"))
summary_screen_cancers<-screen_info%>%full_join(summary_screen_cancers)%>%
  filter(stage!=3&!screen_no%in%c("Post_screen3","other"))%>%
  mutate(seed=i,scenario_no=scenario_no)
summary_screen_cancers[is.na(summary_screen_cancers)]<-0


###############################################################################
#Get the clinical cancers by start age and year of follow-up in the control arm
###############################################################################
#CONTROL ARM
#get the person years
control<-subset(results,arm=="C")  %>% mutate(entry=0,event=as.numeric(I(clin_dx_event=="clin_cancer_diagnosis")))
Lcoh=Lexis(entry=list(Time=control$start_age),
           exit=list(Time=control$clin_dx_age),
           entry.status = control$entry,
           exit.status = control$event,
           id=control$ID,
           data=control,
           merge = TRUE,
           notes = TRUE,
           tol = .Machine$double.eps^0.5,
           keep.dropped = FALSE)

lex=splitLexis(Lcoh,breaks=seq(50,82),time.scale = "Time",tol=.0000000001)
control_PY=aggregate(list(dur=dur(lex),
                          clin_cancer_early=I(status(lex,"exit")==1&lex$clin_dx_event_stage==1),
                          clin_cancer_late=I(status(lex,"exit")==1&lex$clin_dx_event_stage==2)),
                     list(followup=timeBand(lex,"Time","right"),
                          start_age=lex$start_age, sex=lex$sex),FUN="sum")%>%
  mutate(age=followup-1)%>%
  filter(followup<=start_age+4)%>%
  group_by(age,sex)%>%summarise(PY=sum(dur),clin_cancer_early=sum(clin_cancer_early), clin_cancer_late=sum(clin_cancer_late))%>%
  mutate(age=factor(age))

#get the counts of cancers 
control <-control %>% mutate(age=cut(clin_dx_age,breaks=seq(50,82),right=F,labels=seq(50,81)),seed=i,scenario_no=scenario_no)
control_cancers<- control %>% group_by(sex,age,cancer_site, stage= clin_dx_event_stage,seed,scenario_no) %>% 
  summarise(clinical=sum(clin_dx_event=="clin_cancer_diagnosis"))%>%
  filter(stage!=3)

control_cancers=control_info%>%full_join(control_cancers)%>%
  right_join(control_PY, by=c("age","sex"))%>%
  select(!c("clin_cancer_early","clin_cancer_late"))
control_cancers[is.na(control_cancers)]<-0


##################################################################
#reduction in late stage disease
###################################################################

Late_no_screen=timepoints(cuminc(ftime=(results$clin_dx_age-results$start_age),fstatus=results$clin_dx_event_stage,cencode=3),c(0,.999999,1.999999,2.999999,3.999999))$est[2,]

Late_screen=timepoints(cuminc(ftime=(results$diagnosis_age_screen_scenario-results$start_age),fstatus=results$diagnosis_event_stage_screen_scenario,cencode=3),c(0,.999999,1.999999,2.999999,3.999999))$est[2,]

late_reduction=data.frame(time=as.numeric(names(Late_screen)),rel_reduction=(Late_no_screen-Late_screen)/Late_no_screen,
                         abs_reduction=Late_no_screen-Late_screen)%>%melt(id.var="time")%>%rename(type=variable,reduction=value)






#Save summary data file
save_file_name=file.path(dirname,paste0("summary_", i,".Rdata"))
out=list(control_cancers=control_cancers,
         summary_screen_at_risk=summary_screen_at_risk,
         summary_screen_cancers=summary_screen_cancers,
         summary_interval_cancers=summary_interval_cancers,
         late_reduction=late_reduction)
save(out,file=save_file_name)





