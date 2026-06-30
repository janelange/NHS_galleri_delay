#!/bin/bash
#SBATCH --job-name=Calibrated_sim_results
#SBATCH --output=/home/groups/CEDAR/MCED_sim/Logs/Calibrated_sim_test_%A_%a.out
#SBATCH --error=/home/groups/CEDAR/MCED_sim/Logs/Calbrated_sim_test_%A_%a.err
#SBATCH --time=36:00:00
#SBATCH --mem=30G
#SBATCH --cpus-per-task=2
#SBATCH --array=1-200
#SBATCH --partition=batch
#SBATCH --account=cedar

# Load R module
module load r/4.5.0


# Run R script for each of the scenarios
#Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Calibrated_Trial_sim.R ${SLURM_ARRAY_TASK_ID} 4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Calibrated_Trial_sim.R ${SLURM_ARRAY_TASK_ID} 5

#chmod -R 777 /home/groups/CEDAR/MCED_sim/Output
