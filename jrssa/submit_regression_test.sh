#!/bin/bash
# Comparison of the HMM setup time with the run of Dec 2025 (week 1: complete
# fit in about 2 hours, 20.6 GB). Each job fits all plays of week 1 and stops
# after the first likelihood evaluation:
#   C1  old data (Dec 2025, rawdata_old/), alpha = 1, no warm start  = as then
#   C2  new data,                          alpha = 1, no warm start  -> C1 vs C2: data
#   C3  new data,                          alpha = 4, warm start     -> C2 vs C3: alpha, warm start
# rawdata_old/ must contain final_preprocessed_week_1_lag4.csv from Dec 2025.
# Run from the project directory:  bash jrssa/submit_regression_test.sh
# Results: grep -H "first evaluation" /work/mroumich/hmmotion_hmm_*.out
#          sacct -u mroumich --name=hmm_C1,hmm_C2,hmm_C3 --format=JobName,Elapsed,MaxRSS,State

common="HMMOTION_TEST=1,HMMOTION_TEST_N=9999,HMMOTION_WEEKS=1,HMMOTION_STOP_AFTER_FIRST=1"
opts="-p long --time=2-00:00:00 --mem=40G"

sbatch $opts --job-name=hmm_C1 \
  --export=ALL,$common,HMMOTION_DATA_DIR=rawdata_old,HMMOTION_ALPHA=1,HMMOTION_NO_WARMSTART=1,HMMOTION_RESULTS=results/test_C1 \
  jrssa/run_lido_job.sh
sbatch $opts --job-name=hmm_C2 \
  --export=ALL,$common,HMMOTION_ALPHA=1,HMMOTION_NO_WARMSTART=1,HMMOTION_RESULTS=results/test_C2 \
  jrssa/run_lido_job.sh
sbatch $opts --job-name=hmm_C3 \
  --export=ALL,$common,HMMOTION_RESULTS=results/test_C3 \
  jrssa/run_lido_job.sh
