#!/bin/bash
#SBATCH --job-name=MARTA_sim_results
#SBATCH --output=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.out
#SBATCH --error=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.err
#SBATCH --time=36:00:00
#SBATCH --mem=600G
#SBATCH --cpus-per-task=2
#SBATCH --array=1-6
#SBATCH --partition=batch
#SBATCH --account=cedar

# Load R module
module load r/4.5.0



#for scenario in {1,2,3}
for scenario in {1,2,3,4,5}
do

for rate in {0,1,2,4}
do

for extended_followup in {0,1}
do

for screen_arm_delay in {0,1}
do 
# Run R script for each of the scenarios
#No extended followup, rate=1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} $scenario $extended_followup $rate $screen_arm_delay

done
done
done
done


