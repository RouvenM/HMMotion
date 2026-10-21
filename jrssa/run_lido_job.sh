#!/bin/bash -l
# HMM fit (02_HMM_model_fitting_server.R). Submit with: sbatch jrssa/run_lido_job.sh
#SBATCH --job-name=hmmotion_hmm
#SBATCH --partition=ultralong
#SBATCH --time=10-00:00:00
#SBATCH --mem=200G
#SBATCH --cpus-per-task=1
#SBATCH --output=/work/mroumich/hmmotion_hmm_%j.out

# Project root on LiDO: must contain BDB-2025.Rproj, jrssa/, rawdata/ and results/
PROJECT_DIR=/work/mroumich/HMMotion_Claude

cd "$PROJECT_DIR"
module load R/4.4.2-gcc141-base

Rscript jrssa/02_HMM_model_fitting_server.R
