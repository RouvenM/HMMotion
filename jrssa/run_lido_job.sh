#!/bin/bash -l
# HMM fit (02_HMM_model_fitting_server.R): about 2 days of setup (tape) before
# the first iteration plus about 5 days of optimisation, so it runs as one job on
# partition ultralong. Submit from the project directory (the one with jrssa/,
# rawdata/, results/):
#
#   sbatch jrssa/run_lido_job.sh
#
# If it ever hits the time limit, submitting it again continues from
# results/mod_checkpoint.rds (after repeating the setup).
#SBATCH --job-name=hmmotion_hmm
#SBATCH --partition=ultralong
#SBATCH --time=14-00:00:00
#SBATCH --mem=200G
#SBATCH --cpus-per-task=1
#SBATCH --output=/work/mroumich/hmmotion_hmm_%j.out

# project root = directory sbatch was called from (needs .here or BDB-2025.Rproj)
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gcc141-base

Rscript jrssa/02_HMM_model_fitting_server.R
