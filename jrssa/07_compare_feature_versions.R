######################################################################################
#### Why do the HMM features help less than in the AoAS version?
#### Compares three versions of the HMM features on the same questions:
####   A  AoAS data and features  (results/preprocessed_HMM_data_full_AoAS.rds,
####      i.e. the old preprocessed_HMM_data_full_final.rds, if available)
####   B  new data, argmax features   (old feature definition)
####   C  new data, posterior features (current pipeline)
#### For each version:
####   1. AUC of each HMM feature alone for man vs zone
####   2. repeated 5-fold cross-fitting with an elastic net (alpha = 0.5):
####      naive post-motion features vs naive post-motion + HMM features,
####      paired difference in log-loss and AUC
#### Fast (glmnet only, a few minutes); run locally.
######################################################################################

library(dplyr)
library(stringr)
library(glmnet)
library(pROC)
library(here)

hmm_vars <- c("beta", "average", "sum", "nr_player_changes", "average_ent")

quarter_seconds <- function(timestr){
  st <- str_split(timestr, ":")
  sapply(st, function(s) as.numeric(s[1]) * 60 + as.numeric(s[2]))
}

games <- read.csv(here("rawdata", "games.csv"))
plays <- read.csv(here("rawdata", "plays.csv")) |>
  mutate(gameplayId = as.numeric(paste0(gameId, playId)))

# same model data as in 04, without week and team columns
prep <- function(full_data){
  full_data |>
    left_join(games |> select(gameId, week), by = "gameId") |>
    left_join(plays |> select(gameplayId, possessionTeam), by = "gameplayId") |>
    filter(pff_manZone != "Other") |>
    mutate(half_seconds_remaining = if_else(quarter %in% c(1, 3),
                                            quarter_seconds(gameClock) + 15 * 60,
                                            quarter_seconds(gameClock)),
           y = as.numeric(pff_manZone == "Man")) |>
    select(-gameClock, -starts_with("nflId"), -starts_with("week"), -possessionTeam,
           -pff_manZone, -any_of(c("gameId", "playId", "pff_passCoverage")),
           -starts_with("dist_fb"), -starts_with("def_o_qb_diff"))
}

versions <- list()
aoas_file <- here("results", "preprocessed_HMM_data_full_AoAS.rds")
if(file.exists(aoas_file)) versions$A_aoas <- prep(readRDS(aoas_file))

new_full <- readRDS(here("results", "preprocessed_HMM_data_full.rds"))
argmax <- readRDS(here("results", "hmm_features_argmax.rds")) |>
  ungroup() |>
  mutate(gameplayId = as.numeric(paste0(gameId, playId))) |>
  select(gameplayId, average, sum, nr_player_changes, average_ent)
versions$B_new_argmax <- prep(new_full |>
                                select(-average, -sum, -nr_player_changes, -average_ent) |>
                                inner_join(argmax, by = "gameplayId"))
versions$C_new_posterior <- prep(new_full)

# 1. AUC of each HMM feature alone (direction-free: max(AUC, 1 - AUC))
auc_tab <- bind_rows(lapply(names(versions), \(v){
  d <- versions[[v]]
  vars <- intersect(hmm_vars, names(d))
  data.frame(version = v, n = nrow(d), feature = vars,
             auc = sapply(vars, \(x){
               a <- as.numeric(auc(roc(d$y, d[[x]], quiet = TRUE)))
               max(a, 1 - a)
             }))
}))
print(auc_tab, row.names = FALSE)

# 2. repeated cross-fitting: naive post vs naive post + HMM (elastic net)
crossfit_glmnet <- function(d, vars, seed, K = 5){
  set.seed(seed)
  folds <- sample(rep(1:K, length.out = nrow(d)))
  x <- as.matrix(d[, vars])
  p <- numeric(nrow(d))
  for(k in 1:K){
    fit <- cv.glmnet(x[folds != k, ], d$y[folds != k], family = "binomial", alpha = 0.5)
    p[folds == k] <- predict(fit, x[folds == k, , drop = FALSE], s = "lambda.min", type = "response")
  }
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  c(logloss = -mean(d$y * log(p) + (1 - d$y) * log(1 - p)),
    auc = as.numeric(auc(roc(d$y, p, quiet = TRUE))))
}

cf_tab <- bind_rows(lapply(names(versions), \(v){
  d <- versions[[v]] |> select(where(is.numeric)) |> filter(if_all(everything(), is.finite))
  hmm <- intersect(hmm_vars, names(d))
  naive <- setdiff(names(d), c("y", "gameplayId", hmm))
  bind_rows(lapply(1:5, \(s){
    a <- crossfit_glmnet(d, naive, seed = s)
    b <- crossfit_glmnet(d, c(naive, hmm), seed = s)
    data.frame(version = v, rep = s,
               d_logloss = b["logloss"] - a["logloss"],   # < 0: HMM features help
               d_auc = b["auc"] - a["auc"])               # > 0: HMM features help
  }))
}))
cf_tab |>
  group_by(version) |>
  summarise(d_logloss_mean = mean(d_logloss), d_logloss_min = min(d_logloss),
            d_logloss_max = max(d_logloss), d_auc_mean = mean(d_auc), .groups = "drop") |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print()
