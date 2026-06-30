#!/bin/bash

set -euo pipefail

RUN_SCRIPT="/home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/run_full_delay.sh"
COMBINE_SCRIPT="/home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/combine_summary_delay.sh"
PLOT_SCRIPT="/home/groups/CEDAR/MCED_sim/Scripts/MARTA_sim/Delay/Delay_plots.sh"

# Submit the array job
jid=$(sbatch "$RUN_SCRIPT" | awk '{print $4}')
echo "Submitted run_full_delay.sh as job $jid"

# Submit the combine job, dependent on successful completion
cid=$(sbatch --dependency=afterok:$jid "$COMBINE_SCRIPT" | awk '{print $4}')
echo "Submitted combine_summary_delay.sh as job $cid (afterok:$jid)"

pid=$(sbatch --dependency=afterok:$cid "$PLOT_SCRIPT" | awk '{print $4}')
echo "Submitted Delay_plots.sh as job $pid (afterok:$cid)"
