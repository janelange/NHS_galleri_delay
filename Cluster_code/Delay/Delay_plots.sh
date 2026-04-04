#!/bin/bash
#SBATCH --job-name=MARTA_sim_results
#SBATCH --output=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.out
#SBATCH --error=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.err
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=2
#SBATCH --array=0-1
#SBATCH --partition=batch
#SBATCH --account=cedar

# Load R module
module load r/4.5.0

# Run R script for each of the scenarios
#No extended followup
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/Delay_plots.R ${SLURM_ARRAY_TASK_ID} 1 
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/Delay_plots.R ${SLURM_ARRAY_TASK_ID} 2 
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/Delay_plots.R ${SLURM_ARRAY_TASK_ID} 3 

