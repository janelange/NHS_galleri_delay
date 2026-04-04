#!/bin/bash
#SBATCH --job-name=MARTA_sim_results
#SBATCH --output=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.out
#SBATCH --error=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.err
#SBATCH --time=36:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=2
#SBATCH --array=1-200
#SBATCH --partition=batch
#SBATCH --account=cedar

# Load R module
module load r/4.5.0

for delay in {1,2,3,4,5,6}
do

#make output directories
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_1/delay_$delay
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_2/delay_$delay
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_3/delay_$delay

#Run script that adds delay for each of the scenarios
#No extended follwoup
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} 1 $delay 0
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} 2 $delay 0
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} 3 $delay 0

#Extended followup
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} 1 $delay 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} 2 $delay 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} 3 $delay 1

# Run R script for each of the scenarios
#No extended follwoup
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} 1 $delay 0
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} 2 $delay 0
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} 3 $delay 0

#Extended followup
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} 1 $delay 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} 2 $delay 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} 3 $delay 1

done
