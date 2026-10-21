######################################################################################
######################################################################################
#### GCM tests
######################################################################################
######################################################################################

library(dplyr)
library(stringr)
library(here)
library(comets)
library(xgboost)
library(coin)

# Two kinds of tests, Z = pre-motion + naive post-motion features:
# 1. "partial": omnibus Y _||_ X | Z for all four HMM features X, and for each
#    feature Y _||_ X_j | (Z, X_-j), i.e. given the other HMM features too
#    -> results/gcm_partial.rds (left panel of the GCM figure)
# 2. "marginal": for each feature Y _||_ X_j | Z, other HMM features excluded;
#    with the unnormalised statistic and its sd for the partially linear
#    logistic model interpretation -> results/gcm_marginal.rds (right panel)


test_zvm_features <- function(model_data,
                              test = c("gcm","pcm"),
                              test_vars,
                              excl_vars = NULL,
                              ...){
  dta <- model_data |>
    select(-gameId,-playId,
           -gameplayId,-pff_passCoverage,-c({{excl_vars}}))
  
  comet(as.formula(paste0("pff_manZone_num ~ ", paste0(test_vars,collapse = " + ")," | . -",paste0(test_vars,collapse = " - "))),
        data = dta, test = test,...)
} 


full_data <- readRDS(here("results", "preprocessed_HMM_data_full.rds"))
#full_data <- readRDS("~/zvm_models/preprocessed_HMM_data_full_final.rds")

######################
### clock manipulations


quarter_seconds <- function(timestr){
  st <- str_split(timestr,":")
  sapply(st,function(s){
    as.numeric(s[1])*60+as.numeric(s[2])
  })
}

games = read.csv(here("rawdata", "games.csv"))
plays = read.csv(here("rawdata", "plays.csv")) |>
  mutate(gameplayId = as.numeric(paste0(gameId, playId)))

fm_add <- full_data |>
  left_join(games |> select(gameId,week),by = "gameId") |>
  left_join(plays |> select(gameplayId,possessionTeam))

model_data_full <- fm_add |>
  filter(pff_manZone != "Other") |>
  mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
                                          quarter_seconds(gameClock)+15*60,
                                          quarter_seconds(gameClock))) |>
  select(-gameClock,-starts_with("nflId")) |>
  mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
  select(-pff_manZone) 

ffs <- model_data_full |>
  #select(-starts_with("week"),-beta,-possessionTeam)
  select(-starts_with("week"),-possessionTeam)

#ffs <- model_data_full 


####################
#### 1. partial tests: each HMM feature given Z and the other HMM features
####    (small models, i.e. exclude orientation and dist_fb)
####################

set.seed(123)

gcm_o_s <- test_zvm_features(ffs,
                             test = "gcm",
                             test_vars = c("sum","average_ent","nr_player_changes","beta"),
                             excl_vars = c("average",
                                           names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                           names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                             reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                             args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                             args_XonZ = list(early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)))


gcm_ae_s <- test_zvm_features(ffs,
                            test = "gcm",
                            test_vars = "average_ent",
                            excl_vars = c("average",
                                          names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                          names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                            reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                            args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            args_XonZ = list(early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            type = "scalar")


gcm_s_s <- test_zvm_features(ffs,
                             test = "gcm",
                             test_vars = "sum",
                             excl_vars = c("average",
                                           names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                           names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]
                             ),
                             reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                             args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                             args_XonZ = list(early_stopping_rounds = 15,
                                              etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                             type = "scalar")


gcm_nc_s <- test_zvm_features(ffs,
                              test = "gcm",
                              test_vars = "nr_player_changes",
                              excl_vars = c("average",
                                            names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                            names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]
                              ),
                              reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                              args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              args_XonZ = list(early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              type = "scalar")

gcm_b_s <- test_zvm_features(ffs,
                              test = "gcm",
                              test_vars = "beta",
                              excl_vars = c("average",
                                            names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                            names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]
                              ),
                              reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                              args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              args_XonZ = list(early_stopping_rounds = 15,
                                               etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                              type = "scalar")

GCM_unscaled <- sapply(list(gcm_ae_s,gcm_nc_s,gcm_s_s,gcm_b_s), \(x) sum(x$rY*x$rX))
sds <- sapply(list(gcm_ae_s,gcm_nc_s,gcm_s_s,gcm_b_s), \(x) { 
  tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
  sqrt(variance(tst))
})

# pvals_tbl_s <- data.frame(predictor = c("average entropy","# player changes","sum changes","omnibus"),
#                           p.value = c(gcm_ae_s$p.value,gcm_nc_s$p.value,gcm_s_s$p.value,gcm_o_s$p.value),
#                           tstat = c(gcm_ae_s$statistic,gcm_nc_s$statistic,gcm_s_s$statistic,gcm_o_s$statistic))
pvals_tbl_s_2 <- data.frame(predictor = c("average entropy","# player changes","sum changes","beta","omnibus"),
                            p.value = c(gcm_ae_s$p.value,gcm_nc_s$p.value,gcm_s_s$p.value,gcm_b_s$p.value,gcm_o_s$p.value),
                            tstat = c(gcm_ae_s$statistic,gcm_nc_s$statistic,gcm_s_s$statistic,gcm_b_s$statistic,gcm_o_s$statistic),
                             GCSM_unscaled = c(GCM_unscaled,NA),
                             sds = c(sds,NA))

#saveRDS(pvals_tbl_s,"pvals_hmm_features_s_020525.rds") ### s stands for small models (w/o or and dist_fb) but all hmm features in each model!
saveRDS(pvals_tbl_s_2, here("results", "gcm_partial.rds")) ### s stands for small models (w/o or and dist_fb) but all hmm features in each model!
  

####################
#### 2. marginal tests: each HMM feature given Z only (other HMM features excluded)
####################

set.seed(123)

gcm_ae_s1 <- test_zvm_features(ffs,
                            test = "gcm",
                            test_vars = "average_ent",
                            excl_vars = c("average","nr_player_changes","sum","beta",
                                          names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                          names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                            reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                            args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            args_XonZ = list(early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            type = "scalar")

gcm_nc_s1 <- test_zvm_features(ffs,
                            test = "gcm",
                            test_vars = "nr_player_changes",
                            excl_vars = c("average","average_ent","sum","beta",
                                          names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                          names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                            reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                            args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            args_XonZ = list(early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            type = "scalar")


gcm_s_s1 <- test_zvm_features(ffs,
                            test = "gcm",
                            test_vars = "sum",
                            excl_vars = c("average","nr_player_changes","average_ent","beta",
                                          names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                          names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                            reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                            args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            args_XonZ = list(early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            type = "scalar")

gcm_b_s1 <- test_zvm_features(ffs,
                            test = "gcm",
                            test_vars = "beta",
                            excl_vars = c("average","nr_player_changes","average_ent","sum",
                                          names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                          names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                            reg_YonZ = "tuned_xgb",reg_XonZ = "tuned_xgb",
                            args_YonZ = list(metrics = list("logloss"),objective = "binary:logistic",early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            args_XonZ = list(early_stopping_rounds = 15,
                                             etas = c(0.01,0.1,0.5,1),nrounds = c(10,50,100)),
                            type = "scalar")

GCM_unscaled <- sapply(list(gcm_ae_s1,gcm_nc_s1,gcm_s_s1,gcm_b_s1), \(x) sum(x$rY*x$rX))
sds <- sapply(list(gcm_ae_s1,gcm_nc_s1,gcm_s_s1,gcm_b_s1), \(x) { 
  tst <- independence_test(x$rY ~ x$rX, teststat = "scalar")
  sqrt(variance(tst))
})

# pvals_tbl_s1 <- data.frame(predictor = c("average entropy","# player changes","sum changes"),
#                           p.value = c(gcm_ae_s1$p.value,gcm_nc_s1$p.value,gcm_s_s1$p.value),
#                           tstat = c(gcm_ae_s1$statistic,gcm_nc_s1$statistic,gcm_s_s1$statistic))
pvals_tbl_s1_2 <- data.frame(predictor = c("average entropy","# player changes","sum changes","beta"),
                          p.value = c(gcm_ae_s1$p.value,gcm_nc_s1$p.value,gcm_s_s1$p.value,gcm_b_s1$p.value),
                          tstat = c(gcm_ae_s1$statistic,gcm_nc_s1$statistic,gcm_s_s1$statistic,gcm_b_s1$statistic),
                          GCM_unscaled,
                          sds)

#saveRDS(pvals_tbl_s1,"pvals_hmm_features_s1_020525.rds") ### s1 stands for small models (w/o or and dist_fb) and one hmm variable only in the model
#saveRDS(pvals_tbl_s1_2,"pvals_hmm_features_s1_100725.rds") ### s1 stands for small models (w/o or and dist_fb) and one hmm variable only in the model
saveRDS(pvals_tbl_s1_2, here("results", "gcm_marginal.rds")) ### s1 stands for small models (w/o or and dist_fb) and one hmm variable only in the model
