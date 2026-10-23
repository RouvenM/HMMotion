#!/bin/bash -l
# Coverage models (04_zvm_cv_runs.R). Submit from the project directory with:
#   sbatch jrssa/run_lido_parallel.sh
#SBATCH --job-name=hmmotion_zvm
#SBATCH --partition=long
#SBATCH --time=2-00:00:00
#SBATCH --mem=30G
#SBATCH --cpus-per-task=10            # = number of repetitions run in parallel (nsim = 10)
#SBATCH --output=/work/mroumich/hmmotion_zvm_%j.out

# project root = directory sbatch was called from (needs .here or BDB-2025.Rproj)
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gcc141-base

Rscript jrssa/04_zvm_cv_runs.R
