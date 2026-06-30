#Generate simulated trial data similar to NHS Galleri.
#We use natural history models based on US SEER incidence data.
#Cancer sites do not include lymphoma. 
library(MCEDsim)

data("cdc_hmd_data")
data("combined_fits")
data("parametric_surv_fits")

cdc_data_test=all_cause_cdc
hmd_data_test=hmd_data
MCED_cdc_test=MCED_cdc
all_rates_male_test=all_rates_male
all_rates_female_test = all_rates_female
all_meta_data_female_test = all_meta_data_female
all_meta_data_male_test = all_meta_data_male

cancer_sites_vec_test=c("Anus", "Bladder" ,  "Colorectal" , "Esophagus" , "Gastric", "Headandneck", "Liver" , "Lung", "Pancreas", "Ovary")

OMST_vec_test=rep(2,times=14)
LMST_vec_test=rep(.5,times=14)
starting_age_test=63
num_screens_test = 3
screen_interval_test=1
num_males_test = 35000
num_females_test = 35000

MCED_specificity=.995
#end time=100

early_sens= c(0.5, 0.18, 0.67, 0.48, 0.33, 0.72, 0.94, 0.40, 0.62, 0.60)
late_sens=c(1, 0.83, 0.92, 0.97, 0.94, 0.93, 1, 0.93, 0.88, 0.90)

test_performance_dataframe_test = data.frame(early_sens, late_sens,cancer_site = cancer_sites_vec_test)


#########################################
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

set.seed(i) 

screen <- sim_multiple_individuals_MCED_parallel_universe(cancer_sites = cancer_sites_vec_test,
                                            LMST_vec = LMST_vec_test,
                                            OMST_vec = OMST_vec_test,
                                            test_performance_dataframe = test_performance_dataframe_test,
                                            starting_age = starting_age_test,
                                            ending_age=500,
                                            num_screens = num_screens_test,
                                            screen_interval = screen_interval_test,
                                            num_males = num_males_test,
                                            num_females = num_females_test,
                                            all_rates_male = all_rates_male_test,
                                            all_rates_female = all_rates_female_test,
                                            all_meta_data_female = all_meta_data_female_test,
                                            all_meta_data_male = all_meta_data_male_test,
                                            cdc_data = cdc_data_test,
                                            hmd_data = hmd_data_test,
                                            MCED_cdc = MCED_cdc_test,
                                            surv_param_table=param_table,
                                            MCED_specificity = MCED_specificity)

set.seed(i*1e7) 
control <- sim_multiple_individuals_MCED_parallel_universe(cancer_sites = cancer_sites_vec_test,
                                                          LMST_vec = LMST_vec_test,
                                                          OMST_vec = OMST_vec_test,
                                                          test_performance_dataframe = test_performance_dataframe_test,
                                                          starting_age = starting_age_test,
                                                          ending_age=500,
                                                          num_screens = num_screens_test,
                                                          screen_interval = screen_interval_test,
                                                          num_males = num_males_test,
                                                          num_females = num_females_test,
                                                          all_rates_male = all_rates_male_test,
                                                          all_rates_female = all_rates_female_test,
                                                          all_meta_data_female = all_meta_data_female_test,
                                                          all_meta_data_male = all_meta_data_male_test,
                                                          cdc_data = cdc_data_test,
                                                          hmd_data = hmd_data_test,
                                                          MCED_cdc = MCED_cdc_test,
                                                          surv_param_table=param_table,
                                                          MCED_specificity = MCED_specificity)

#save results to a file with index given by the argument 
save(screen,control, file = paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/MARTA_sim_results_", i, ".Rdata"))

#saveRDS(results, file = "/home/groups/CEDAR/MCED_sim/Output/lung_sim_results.rds")








