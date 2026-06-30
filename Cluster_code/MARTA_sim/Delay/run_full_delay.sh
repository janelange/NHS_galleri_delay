#!/bin/bash
#SBATCH --job-name=MARTA_sim_results
#SBATCH --output=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.out
#SBATCH --error=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.err
#SBATCH --time=36:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=2
#SBATCH --array=1-200
#SBATCH --partition=batch
#SBATCH --account=cedar

# Load R module
module load r/4.5.0

for delay in {1,2,3,4,5,6}
do

for scenario in {1,2,3,4,5}
do

# for rate in {0,1,2,4}
for rate in {0,1,2,4}
do

for extended_followup in {0,1}
do

for screen_arm_delay in {0,1}
do   
#make output directories
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_1/delay_$delay
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_2/delay_$delay
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_3/delay_$delay
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_4/delay_$delay
mkdir -p /home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_5/delay_$delay

#Run script that adds delay for each of the scenarios
#Use _calibrated version for NHS calibrated models
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay_calibrated.R ${SLURM_ARRAY_TASK_ID} $scenario $delay $extended_followup $rate $screen_arm_delay
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/add_delay.R ${SLURM_ARRAY_TASK_ID} $scenario $delay $extended_followup $rate $screen_arm_delay

#Run script that summarizes data for each of the scenarios
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/summarize_delay.R ${SLURM_ARRAY_TASK_ID} $scenario $delay $extended_followup $rate $screen_arm_delay

done
done
done
done
done

