#!/bin/bash -l
# HMM fit (02_HMM_model_fitting_server.R) on partition long (max. 2 days).
# The fit takes about a week, so it runs as a chain of jobs: each job continues
# from results/mod_checkpoint.rds, and jobs after the finished fit stop at once.
# Submit from the project directory (the one with jrssa/, rawdata/, results/):
#
#   jid=$(sbatch --parsable jrssa/run_lido_job.sh)
#   for i in 1 2 3 4 5; do jid=$(sbatch --parsable --dependency=afterany:$jid jrssa/run_lido_job.sh); done
#
#SBATCH --job-name=hmmotion_hmm
#SBATCH --partition=long
#SBATCH --time=2-00:00:00
#SBATCH --mem=200G
#SBATCH --cpus-per-task=1
#SBATCH --output=/work/mroumich/hmmotion_hmm_%j.out

# project root = directory sbatch was called from (needs .here or BDB-2025.Rproj)
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gcc141-base

Rscript jrssa/02_HMM_model_fitting_server.R
