HMMotion pipeline for the JRSS-A resubmission
=============================================

All paths are resolved with here() relative to the project root (the folder
containing BDB-2025.Rproj). Expected layout:

  rawdata/   tracking_week_1..9.csv, plays.csv, players.csv, games.csv
  results/   created by you (git-ignored); all fitted models and outputs go here

Run the scripts in this order:

1. 01_preprocessing.R
   Preprocessing for the HMM and the supervised models. Writes per week to rawdata/:
   final_preprocessed_week_<w>_lag4.csv (HMM input), week<w>_y_ord_od.rds
   (pre-motion features), week_<w>_dist_adder.rds (naive post-motion features).

2. 02_HMM_model_fitting_server.R  (run on LiDO, see run_lido_job.sh)
   Final non-homogeneous HMM with distance covariate and team/position/play
   random effects. Writes results/mod_final_full.rds.
   Warm start: copy an earlier fit to results/mod_start.rds to start from its
   fixed-effect estimates (fewer iterations).
   (02b_HMM_model_fitting_exploratory.R: earlier homogeneous / team+position models.)

3. 03_create_data_HMM.R
   Decodes the HMM, builds the HMM features and joins all feature sets.
   Writes results/preprocessed_HMM_data_full.rds.

4. 04_zvm_cv_runs.R  (run on LiDO, see run_lido_parallel.sh)
   Coverage models (elastic net, XGBoost): 10 repetitions of 5-fold cross-fitting
   -> results/zvm_crossfit_runs.rds; leave-one-week-out CV -> results/zvm_lowo_runs.rds.

5. 05_zvm_team_analyses.R
   Leave-one-team-out comparison of the pre-motion and HMM post-motion models.
   Writes results/teams_zvm.rds.

zvm_models.R is the shared model-fitting function sourced by 04 and 05.
fig_HMM_expl.R draws the HMM illustration (figure HMM_expl).

Changes relative to the versions used for the AoAS submission
-------------------------------------------------------------
- 01: the start frame (first line_set) is now determined per game and play.
  Before, grouping by playId alone pooled plays with the same playId from
  different games of a week (6% of plays), so their start frame could come
  from another game: about half of them kept huddle frames, the others may
  have lost frames. The first grouping (player x playId) was already unique.
- 01: the deepest non-CB defenders are now removed by depth to the line of
  scrimmage. Before, the smallest centred x was used, which only picks the
  deepest player for plays going left; for plays going right it removed the
  defender closest to the line and kept deep safeties.
- 02: random-effect names now follow the factor levels used for indexing.
  Before, names came from unique() (order of appearance), so position and team
  effects were mislabelled and 44 play effects were attached to the wrong play.
- 04: train/test splits are seeded; 5-fold cross-fitting as described in the
  paper; leave-one-week-out uses the real week and the right function; the
  full run_models() reported glmnet instead of XGBoost for "af_ex_hmm".
- 04 / zvm_models.R, run time: 10 instead of 50 repetitions, XGBoost grid
  eta {0.05, 0.1, 0.3} x depth {1, 3, 5} instead of 6 x 4 values, one thread per
  XGBoost fit and as many workers as SLURM allocated (detectCores() returned
  all cores of the node, oversubscribing it).
- 05: reads the final data and excludes the play random effect ("beta") from
  the pre-motion model.
- zvm_models.R: random-forest branch referenced an undefined object.
