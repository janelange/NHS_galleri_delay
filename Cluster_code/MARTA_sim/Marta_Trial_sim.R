library(MCEDsim)
library(dplyr)
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])
scenario_no <-as.numeric(args[2])


#Load the other-cause mortality tables
data("cdc_hmd_data")
#Load the prefitted natural history models
data("combined_fits_NHS")
#Load the prefitted cause-specific survival models
data("parametric_surv_fits")

#Specify inputs
#Cancer sites in the MCED test
cancer_sites_vec=c("Anus",  "Bladder",  "Colorectal", "Esophagus",
                   "Headandneck", "Liver" , "Lung", "Lymphoma",
                   "Ovary",  "Pancreas", "Gastric")


#        Assumption     OMST   LMST   Relative early stage sensitivity
# Fast-fast-optimistic    1      0.5          100
# Slow-fast-conservative  2      0.5          50
# Slow-slow-conservative  2       1           50

if(scenario_no == 1) {
  OMST <- 1; LMST <- 0.5; screen_interval <- 1; sens_factor<-1
}  else if(scenario_no == 2) {
  OMST <- 2; LMST <- 0.5; screen_interval <- 1; sens_factor<-.5
} else if(scenario_no == 3) {
  OMST <- 2; LMST <- 1; screen_interval <- 1; sens_factor<-.5
} else {
  stop("Invalid scenario")
}

OMST_vec=rep(OMST,times=11)

LMST_vec=rep(LMST,times=11)

# Test sensitivities
early_sens=sens_factor*c(0.5,
0.18,
0.67,
0.48,
0.33,
0.72,
0.94,
0.4,
0.46,
0.6,
0.62)

late_sens=c(1,
0.83,
0.92,
0.97,
0.94,
0.93,
1,
0.93,
0.66,
0.9,
0.88)
                        
#dataframe with all of the test performance inputs
test_performance_dataframe = data.frame(early_sens, late_sens,cancer_site = cancer_sites_vec)
input_table=read.csv("/home/groups/CEDAR/MCED_sim/Data/NHS_data/Input_table_NHS_galleri.csv")%>%filter(!is.na(age_entry))

#input_table$counts=10

set.seed(i*scenario_no*1e6) 
results <- sim_MCED_trial(input_table,
                          cancer_sites=cancer_sites_vec,
                          trial_duration=3,
                          num_screens=3,
                          screen_interval=1,
                          LMST_vec=LMST_vec,
                          OMST_vec=OMST_vec,
                          test_performance_dataframe=test_performance_dataframe,
                          MCED_specificity=.995,
                          all_rates_male=all_rates_male,
                          all_rates_female=all_rates_female,
                          all_meta_data_female=all_meta_data_female,
                          all_meta_data_male=all_meta_data_male,
                          cdc_data=all_cause_cdc,
                          hmd_data=hmd_data,
                          MCED_cdc=MCED_cdc,
                          surv_param_table=param_table)
#########################################
#save results to a file with index given by the argument 
save(results, file = paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no,"/Marta_sim_results_", i, ".Rdata"))








