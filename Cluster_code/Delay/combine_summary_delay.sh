#!/bin/bash
#SBATCH --job-name=MARTA_sim_results
#SBATCH --output=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.out
#SBATCH --error=/home/groups/CEDAR/MCED_sim/Logs/MARTA_sim_test_%A_%a.err
#SBATCH --time=36:00:00
#SBATCH --mem=128G
#SBATCH --cpus-per-task=2
#SBATCH --array=1-6
#SBATCH --partition=batch
#SBATCH --account=cedar

# Load R module
module load r/4.5.0

# Run R script for each of the scenarios
#No extended followup, rate=1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 1 0 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 2 0 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 3 0 1

#No extended followup, rate=2
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 1 0 2
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 2 0 2
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 3 0 2

#No extended followup, rate=4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 1 0 4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 2 0 4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 3 0 4


#Extended followup, rate=1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 1 1 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 2 1 1
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 3 1 1

#Extended followup, rate=2
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 1 1 2
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 2 1 2
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 3 1 2

#Extended followup, rate=4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 1 1 4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 2 1 4
Rscript /home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.R ${SLURM_ARRAY_TASK_ID} 3 1 4
