######################################################################################
#### Settings shared by the pipeline scripts
######################################################################################

# Defender reaction lag in frames (10 Hz) between attacker and defender
# positions, chosen by AIC in 02a_lag_and_delta_selection.R: 5 frames = 0.5 s
HMM_LAG <- 5

# HMM input file of a week written by 01
hmm_file <- function(week){
  here::here("rawdata", paste0("final_preprocessed_week_", week, "_lag", HMM_LAG, ".csv"))
}
