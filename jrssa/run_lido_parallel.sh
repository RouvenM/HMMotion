#!/bin/bash -l
# Coverage models (04_zvm_cv_runs.R). Submit with: sbatch jrssa/run_lido_parallel.sh
#SBATCH --job-name=hmmotion_zvm
#SBATCH --partition=long
#SBATCH --time=2-00:00:00
#SBATCH --mem=30G
#SBATCH --cpus-per-task=10            # = number of repetitions run in parallel (nsim = 10)
#SBATCH --output=/work/mroumich/hmmotion_zvm_%j.out

# Project root on LiDO: must contain BDB-2025.Rproj, jrssa/, rawdata/ and results/
PROJECT_DIR=/work/mroumich/HMMotion_Claude

cd "$PROJECT_DIR"
module load R/4.4.2-gcc141-base

Rscript jrssa/04_zvm_cv_runs.R
