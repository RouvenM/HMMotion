#!/bin/bash
# Scaling test of the HMM setup: fits the model on random subsets of plays and
# stops right after the first likelihood evaluation, which is the slow setup
# step. The times for several sizes show how the setup grows with the number of
# plays and give an estimate for the full data (4344 plays).
# Run from the project directory:  bash jrssa/submit_scaling_test.sh
# Results: grep -h "Test mode\|first evaluation" /work/mroumich/hmmotion_hmm_*.out

for n in 250 500 1000 2000; do
  mem=$(( n * 50 / 1000 + 10 ))G
  sbatch -p long --time=2-00:00:00 --mem=$mem --job-name=hmm_scale_$n \
    --export=ALL,HMMOTION_TEST=1,HMMOTION_TEST_N=$n,HMMOTION_STOP_AFTER_FIRST=1,HMMOTION_RESULTS=results/scale_$n \
    jrssa/run_lido_job.sh
done
