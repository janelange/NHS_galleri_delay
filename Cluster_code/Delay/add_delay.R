library(dplyr)

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
#early_to_late_rate=1

the_delay=case_when(delay_no == 1 ~ 0,
                    delay_no==2 ~ .1,
                    delay_no==3 ~ .2,
                    delay_no==4 ~ .3,
                    delay_no==5 ~.4,
                    delay_no==6 ~.5)
###################################################################################################################
#Load data file
delay_dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/delay_",delay_no)
orig_dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no)
load(file.path(orig_dirname,paste0("Marta_sim_results_", i,".Rdata")))

#Get stage after delay
get_delay_stage<-function(current_stage,early_to_late_rate,delay){
  if(is.na(current_stage)){
    return(current_stage)
  }
  else{
    if(current_stage=="Late"){
      return(current_stage)
    }else{
    #  browser()
      late_prob=1-exp(-early_to_late_rate * delay)
      out=sample(c("Early","Late"),size=1,prob=c(1-late_prob,late_prob))
      return(out)
    }
  }
}

advance_dx_time_stage<-function(the_data,delay,early_to_late_rate, extended_followup=0){
  #update stage at clinical diagnosis
  new_stage=unlist(lapply(the_data$clinical_diagnosis_stage,FUN="get_delay_stage",delay=delay, early_to_late_rate=early_to_late_rate))
  
  #update time at clinical diagnosis and recalculate all variables
  the_data<-the_data %>% mutate( clinical_diagnosis_time = clinical_diagnosis_time + delay * (extended_followup == 0),
                                 clinical_diagnosis_stage = new_stage)%>%
    mutate(clin_dx_age = pmin(other_cause_death_time,clinical_diagnosis_time,end_time,na.rm = T),
           clin_dx_event = case_when(
             clin_dx_age == other_cause_death_time ~ "other_cause_death",
             clin_dx_age == end_time ~ "censor",
             clin_dx_age == clinical_diagnosis_time ~ "clin_cancer_diagnosis",
             .default = NA
           ),
           clin_dx_event_stage = case_when(clin_dx_event == "clin_cancer_diagnosis" & clinical_diagnosis_stage == "Early"~1,
                                           clin_dx_event == "clin_cancer_diagnosis" & clinical_diagnosis_stage == "Late"~2,
                                           .default = 3),
          
           screen_dx_age = pmin(other_cause_death_time,screen_diagnosis_time,end_time,na.rm = T),
           screen_dx_event = case_when(
             screen_dx_age == other_cause_death_time ~ "other_cause_death",
             screen_dx_age == end_time ~ "censor",
             screen_dx_age == screen_diagnosis_time ~ "screen_cancer_diagnosis",
             .default = NA
           ),
           screen_dx_event_stage = case_when(screen_dx_event == "screen_cancer_diagnosis" & screen_diagnosis_stage == "Early"~1,
                                             screen_dx_event == "screen_cancer_diagnosis" & screen_diagnosis_stage == "Late"~2,
                                             .default = 3),
           death_age_no_screen=pmin(other_cause_death_time,cancer_death_time_no_screen,end_time,na.rm = T),
           death_age_screen=pmin(other_cause_death_time,cancer_death_time_screen,end_time,na.rm = T),
           death_event_no_screen=case_when(
             death_age_no_screen == other_cause_death_time ~ "other_cause_death",
             death_age_no_screen == end_time ~ "censor",
             death_age_no_screen == cancer_death_time_no_screen ~ "cancer_death",
             .default = NA
           ),
           death_event_screen=case_when(
             death_age_screen == other_cause_death_time ~ "other_cause_death",
             death_age_screen == end_time ~ "censor",
             death_age_screen == cancer_death_time_screen ~ "cancer_death",
             .default = NA
           ),
           diagnosis_age_screen_scenario=pmin(clin_dx_age,screen_dx_age,na.rm=T),
           diagnosis_event_screen_scenario=ifelse(screen_dx_age<=clin_dx_age, screen_dx_event,
                                                  clin_dx_event),
           diagnosis_event_stage_screen_scenario=case_when(screen_dx_event == "screen_cancer_diagnosis" & screen_diagnosis_stage == "Early"~1,
                                                           screen_dx_event == "screen_cancer_diagnosis" & screen_diagnosis_stage == "Late" ~2,
                                                           (screen_dx_event!="screen_cancer_diagnosis" & clin_dx_event=="clin_cancer_diagnosis") & clinical_diagnosis_stage=="Early"~1,
                                                           (screen_dx_event !="screen_cancer_diagnosis" & clin_dx_event=="clin_cancer_diagnosis") & clinical_diagnosis_stage=="Late"~2,
                                                           .default = 3),
           life_years_diff=death_age_screen-death_age_no_screen,
           overdiagnosis=ifelse(screen_dx_event=="screen_cancer_diagnosis"&clin_dx_event=="other_cause_death",1,0))%>%
    mutate(cancer_death_time=ifelse(arm=="S",cancer_death_time_screen,cancer_death_time_no_screen),
           age_dx=ifelse(arm=="S",clin_dx_age,diagnosis_age_screen_scenario),
           dx_event=ifelse(arm=="S",clin_dx_event, diagnosis_event_screen_scenario),
           stage_dx=ifelse(arm=="S",clin_dx_event_stage,diagnosis_event_stage_screen_scenario),
           age_death=ifelse(arm=="S",death_age_no_screen,death_age_screen),
           death_event=ifelse(arm=="S",death_event_no_screen,death_event_screen))
  
  return(the_data)
  
}

results=advance_dx_time_stage(the_data=results,delay=the_delay,early_to_late_rate=early_to_late_rate,extended_followup=extended_followup)

#results1=advance_dx_time_stage(the_data=results,delay=the_delay,early_to_late_rate=1,extended_followup=extended_followup)
#results4=advance_dx_time_stage(the_data=results,delay=the_delay,early_to_late_rate=4,extended_followup=extended_followup)


#Save summary data file
save_file_name=file.path(delay_dirname,paste0("delay_", extended_followup, "_", early_to_late_rate,"_", i,".Rdata"))
save(results,file=save_file_name)





