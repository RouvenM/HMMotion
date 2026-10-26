HMMotion pipeline for the JRSS-A resubmission
=============================================

All paths are resolved with here() relative to the project root (the folder
containing BDB-2025.Rproj). Expected layout:

  rawdata/   tracking_week_1..9.csv, plays.csv, players.csv, games.csv
  results/   created by you (git-ignored); all fitted models and outputs go here

Run the scripts in this order:

1. 01_preprocessing.R
   Preprocessing for the HMM and the supervised models. Writes per week to rawdata/:
   final_preprocessed_week_<w>_lag<HMM_LAG>.csv (HMM input), week<w>_y_ord_od.rds
   (pre-motion features), week_<w>_dist_adder.rds (naive post-motion features).

1a. 02a_lag_and_delta_selection.R  (local, before 02)
   Homogeneous HMM on 1000 random plays (all plays: options(hmmotion.lag_n = NA)):
   AIC over lags 0-6 (alpha = 1), then profile likelihood over
   alpha in {0.25, 0.5, 1, 2, 4, 8, 16} for the initial
   distribution and how much the posterior HMM features change with alpha.
   Writes results/lag_delta_selection.rds. Set the chosen alpha as alpha_delta
   at the top of 02. The lag is set once in settings.R (HMM_LAG, read by 01, 02,
   02a and 03); changing it requires rerunning 01. Shortcut from existing lag-4
   files: 01b_shift_lag4_to_lag5.R (shifts player*_y by one row per track, drops
   the first row of each track; otherwise identical to a rerun of 01).
   Results on the corrected data: lags 4 and 5 (0.4-0.5 s) are close and their
   order depends on the subsample (1000 plays: lag 5 best, lag 4 +93; 500 plays:
   lag 4 best, lag 5 +30); lags <= 3 and 6 are clearly worse. HMM_LAG = 4.
   alpha (1000 plays, grid 0.25-16): profile likelihood maximal at alpha = 4
   (alpha 2 and 8: +50, alpha 1: +356 in negative log-likelihood), so
   alpha_delta = 4 in 02 (AoAS: 1). Posterior HMM features hardly depend on
   alpha (correlation with alpha = 1: >= 0.988 at alpha = 4, >= 0.96 at 16).

2. 02_HMM_model_fitting_server.R  (run on LiDO, see run_lido_job.sh)
   Final non-homogeneous HMM with distance covariate and team/position/play
   random effects. Writes results/mod_final_full.rds.
   Warm start: copy an earlier fit to results/mod_start.rds to start from its
   fixed-effect estimates (fewer iterations).
   The fit takes about 7-8 days (about 2 days of setup before the first
   iteration), so it runs as one 14-day job on partition ultralong
   (run_lido_job.sh). nlminb runs uninterrupted; the best parameters so far are
   saved to results/mod_checkpoint.rds every 30 minutes, and a resubmitted job
   continues from there. When done it writes results/HMM_DONE.txt.
   Environment variables for a second run in parallel: HMMOTION_RESULTS=<folder>
   (results folder of the run, default results) and HMMOTION_MATMUL_PLAIN=1
   (TapeConfig(matmul = 'plain'): same likelihood, about 28% faster first
   evaluation in a local test with 200 plays, more memory).
   (02b_HMM_model_fitting_exploratory.R: earlier homogeneous / team+position models.)

3. 03_create_data_HMM.R
   Decodes the HMM, builds the HMM features and joins all feature sets.
   Writes results/preprocessed_HMM_data_full.rds.
   HMM features are posterior expectations (hmm_posterior_features.R): expected
   number of switches (sum, average), expected number of switching defenders
   (nr_player_changes) and entropy of the time-averaged state probabilities
   (average_ent). Also writes results/hmm_features_tracks.rds (per defender)
   and results/hmm_features_argmax.rds (AoAS argmax version, robustness check).

4. 04_zvm_cv_runs.R  (run on LiDO, see run_lido_parallel.sh)
   Coverage models (elastic net, XGBoost): 10 repetitions of 5-fold cross-fitting
   -> results/zvm_crossfit_runs.rds; leave-one-week-out CV -> results/zvm_lowo_runs.rds.
   Quick test without the HMM: options(hmmotion.test = TRUE) uses 500 random plays
   with noise as HMM features, 2 repetitions, a small grid and 2 test weeks
   (results/test/).

5. 05_zvm_team_analyses.R
   Leave-one-team-out comparison of the pre-motion and HMM post-motion models.
   Writes results/teams_zvm.rds, team_tab.rds, team_diffs.pdf, team_table.pdf.

6. 06_gcm_tests.R
   GCM conditional independence tests of the HMM features (package comets),
   Z = pre-motion + naive post-motion features:
   - partial: omnibus Y _||_ X | Z and each Y _||_ X_j | (Z, X_-j)
     -> results/gcm_partial.rds
   - marginal: each Y _||_ X_j | Z, with unnormalised statistic and sd
     -> results/gcm_marginal.rds
   fig_gcm.R draws both panels of the GCM figure from these files.

zvm_models.R is the shared model-fitting function sourced by 04 and 05.
fig_HMM_expl.R draws the HMM illustration (figure HMM_expl).

Note on cross-fitting: the AoAS figure came from cross_fitting_zvm_cluster.R
(sequential, 50 repetitions, XGBoost grid over eta x depth x colsample_bytree
x gamma = 120 combinations). 04 replaces it with run_models_crossfit(): same
5-fold scheme and feature sets, parallel, 10 repetitions, and a grid over eta
and depth only (as described in the paper).

Changes relative to the versions used for the AoAS submission
-------------------------------------------------------------
- 01: the start frame (first line_set) is now determined per game and play.
  Before, grouping by playId alone pooled plays with the same playId from
  different games of a week (about 36% of plays share a playId with another
  game of the same week), so their start frame could come from another game:
  some kept huddle frames, others lost frames or were dropped entirely.
  Together with the defender fix, M grows from 3963 to 4344 plays.
  The first grouping (player x playId) was already unique.
- 01: the deepest non-CB defenders are now removed by depth to the line of
  scrimmage. Before, the smallest centred x was used, which only picks the
  deepest player for plays going left; for plays going right it removed the
  defender closest to the line and kept deep safeties.
  Check after rerun (kept defenders, left vs right): deeper than 10 yd
  1.2% vs 1.0% (before 1.3% vs 20.4%), FS+SS 10.1% vs 9.8% (before 9.9% vs 37.5%).
- Lag selection rerun on the corrected data (02a); lag 4 kept, set in settings.R.
- 02: positions DB and LB (almost no observations) are merged into CB and ILB
  for the position random effect; alpha of the initial distribution is set once
  (alpha_delta) and chosen with 02a.
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
- 04, 05: drop all week columns (03 creates week.x and week.y); before, they
  entered the coverage models as features.
- 05: reads the final data and excludes the play random effect ("beta") from
  the pre-motion model.
- zvm_models.R: random-forest branch referenced an undefined object.
- 03: HMM features are now posterior expectations from a forward-backward pass
  instead of counts of the argmax state sequence, so they carry the
  uncertainty of the decoded assignments. A self-check compares the local
  state probabilities with LaMa's stateprobs_g().
- 06: the paper described the marginal tests (right panel) with the partial
  hypothesis Y _||_ X_j | (Z, X_-j); the code conditions on Z only there.
