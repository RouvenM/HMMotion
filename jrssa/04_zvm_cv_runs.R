######################################################################################
######################################################################################
#### zone vs. man cross valid new
######################################################################################
######################################################################################


# Libraries ---------------------------------------------------------------

#library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(pROC)
library(glmnet)
library(glmnetUtils)
library(ranger)
library(here)
library(xgboost)
library(stringr)
source(here("jrssa", "zvm_models.R"))
# Load data ---------------------------------------------------------------

# Quick test: options(hmmotion.test = TRUE) runs the whole script on 500 random
# plays (options(hmmotion.test_n04 = ...)) with noise in place of the HMM
# features, 2 repetitions, a small XGBoost grid and 2 test weeks, and writes to
# results/test/. It checks the pipeline (cross-fitting, LOWO, parallel workers,
# saving) without the HMM fit.
TEST <- isTRUE(getOption("hmmotion.test", FALSE))
res_dir <- if(TEST) here("results", "test") else here("results")
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)
test_grid <- if(TEST) expand_grid(eta = 0.3, max_depth = c(1, 3)) else NULL

if(TEST){
  set.seed(1)
  pre <- bind_rows(lapply(1:9, \(w) readRDS(here("rawdata", paste0("week", w, "_y_ord_od.rds")))))
  post <- bind_rows(lapply(1:9, \(w) readRDS(here("rawdata", paste0("week_", w, "_dist_adder.rds")))))
  full_data <- pre |> left_join(post, by = "gameplayId") |> drop_na()
  full_data <- full_data[sample(nrow(full_data), min(nrow(full_data), getOption("hmmotion.test_n04", 500))), ]
  full_data <- full_data |> 
    mutate(beta = rnorm(n()), average = rexp(n()), sum = rexp(n()),
           nr_player_changes = runif(n(), 0, 5), average_ent = runif(n()))
}else{
  full_data <- readRDS(file.path(res_dir, "preprocessed_HMM_data_full.rds"))
}

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

# keep the real week for leave-one-week-out CV, but not as a feature
play_week <- model_data_full$week

# 03 adds week columns from both feature sets (week.x, week.y): drop all
model_data_full <- model_data_full |>
  select(-starts_with("week"),-possessionTeam)

# Modelling function ---------------------------------------------------------------

run_models <- function(seedu, model_data_full){
  
  n <- nrow(model_data_full)
  set.seed(seedu)
  sample_idx <- sample(n, size = 0.85*n)
  model_data <- model_data_full[sample_idx,]  
  test_data <- model_data_full[-sample_idx,]
  
  
  # GLMNET -------------------------------------------------
  
  glmnet_full <- zvm_models(model_data,test_data,
                            ml_type = "glmnet",
                            #trace.it = TRUE,
                            seed = seedu)
  
  glmnet_full2 <- zvm_models(model_data,test_data,
                             ml_type = "glmnet",
                             excl_vars = c("beta", "average","sum","nr_player_changes","average_ent"),
                             #trace.it = TRUE,
                             seed = seedu)
  
  
  glmnet_af_npo <- zvm_models(model_data,test_data,
                              ml_type = "glmnet",
                              excl_vars = c(
                                max_x_o,max_y_o,tot_dist_o,
                                max_x_d,max_y_d,tot_dist_d),
                              #trace.it = TRUE,
                              seed = seedu)
  
  glmnet_af_npo2 <- zvm_models(model_data,test_data,
                               ml_type = "glmnet",
                               excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                             max_x_o,max_y_o,tot_dist_o,
                                             max_x_d,max_y_d,tot_dist_d),
                               #trace.it = TRUE,
                               seed = seedu)
  
  glmnet_xy <- zvm_models(model_data,test_data,
                          ml_type = "glmnet",
                          excl_vars = c(
                            names(model_data)[grep("dist_fb",names(model_data))],
                            names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                          #trace.it = TRUE,
                          seed = seedu)
  
  glmnet_xy2 <- zvm_models(model_data,test_data,
                           ml_type = "glmnet",
                           excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                         names(model_data)[grep("dist_fb",names(model_data))],
                                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                           #trace.it = TRUE,
                           seed = seedu)
  
  glmnet_xy_npo <- zvm_models(model_data,test_data,
                              ml_type = "glmnet",
                              excl_vars = c(
                                names(model_data)[grep("dist_fb",names(model_data))],
                                names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                max_x_o,max_y_o,tot_dist_o,
                                max_x_d,max_y_d,tot_dist_d),
                              #trace.it = TRUE,
                              seed = seedu)
  
  glmnet_xy_npo2 <- zvm_models(model_data,test_data,
                               ml_type = "glmnet",
                               excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                             names(model_data)[grep("dist_fb",names(model_data))],
                                             names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                             max_x_o,max_y_o,tot_dist_o,
                                             max_x_d,max_y_d,tot_dist_d),
                               #trace.it = TRUE,
                               seed = seedu)
  
  # XGBOOST -------------------------------------------------
  
  xgb_full <- zvm_models(model_data,test_data,
                         ml_type = "xgboost",
                         seed = seedu,
                         verbose = FALSE)
  
  xgb_full2 <- zvm_models(model_data,test_data,
                          ml_type = "xgboost",
                          excl_vars = c("beta", "average","sum","nr_player_changes","average_ent"),
                          seed = seedu,
                          verbose = FALSE)
  
  
  xgb_af_npo <- zvm_models(model_data,test_data,
                           ml_type = "xgboost",
                           excl_vars = c(
                             max_x_o,max_y_o,tot_dist_o,
                             max_x_d,max_y_d,tot_dist_d),
                           seed = seedu,
                           verbose = FALSE)
  
  xgb_af_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  
  xgb_xy <- zvm_models(model_data,test_data,
                       ml_type = "xgboost",
                       excl_vars = c(
                         names(model_data)[grep("dist_fb",names(model_data))],
                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                       seed = seedu,
                       verbose = FALSE)
  
  xgb_xy2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                      names(model_data)[grep("dist_fb",names(model_data))],
                                      names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                        seed = seedu,
                        verbose = FALSE)
  
  
  xgb_xy_npo <- zvm_models(model_data,test_data,
                           ml_type = "xgboost",
                           excl_vars = c(
                             names(model_data)[grep("dist_fb",names(model_data))],
                             names(model_data)[grep("def_o_qb_diff",names(model_data))],
                             max_x_o,max_y_o,tot_dist_o,
                             max_x_d,max_y_d,tot_dist_d),
                           seed = seedu,
                           verbose = FALSE)
  
  xgb_xy_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                          names(model_data)[grep("dist_fb",names(model_data))],
                                          names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  
  # Result Tables -------------------------------------------------
  
  glm_all_mods <- list(glmnet_full,glmnet_full2,
                       glmnet_af_npo,glmnet_af_npo2, 
                       glmnet_xy,glmnet_xy2, 
                       glmnet_xy_npo,glmnet_xy_npo2)
  names(glm_all_mods) <- paste0("glm_",c("af","af_ex_hmm",
                                         "af_npo","af_npo_ex_hmm",
                                         "xy_f","xy_f_ex_hmm",
                                         "xy_npo","xy_npo_ex_hmm"))
  
  glm_accs <- sapply(glm_all_mods,"[[",2)
  glm_aucs <- sapply(glm_all_mods,"[[",4)
  glm_lls <- sapply(glm_all_mods,"[[",5)
  
  xgb_all_mods <- list(xgb_full,xgb_full2,
                       xgb_af_npo,xgb_af_npo2, 
                       xgb_xy,xgb_xy2, 
                       xgb_xy_npo,xgb_xy_npo2)
  names(xgb_all_mods) <- paste0("xgb_",c("af","af_ex_hmm",
                                         "af_npo","af_npo_ex_hmm",
                                         "xy_f","xy_f_ex_hmm",
                                         "xy_npo","xy_npo_ex_hmm"))
  
  xgb_accs <- sapply(xgb_all_mods,"[[",2)
  xgb_aucs <- sapply(xgb_all_mods,"[[",4)
  xgb_lls <- sapply(xgb_all_mods,"[[",5)
  
  
  ########
  
  
  full_accs <- as.data.frame(rbind(glm_accs,xgb_accs))
  full_accs$mod <- c("glmnet","xgboost")
  names(full_accs) <- c("af","af_ex_hmm",
                        "af_npo","af_npo_ex_hmm",
                        "xy_f","xy_f_ex_hmm",
                        "xy_npo","xy_npo_ex_hmm","mod")
  
  full_aucs <- as.data.frame(rbind(glm_aucs,xgb_aucs))
  full_aucs$mod <- c("glmnet","xgboost")
  names(full_aucs) <- c("af","af_ex_hmm",
                        "af_npo","af_npo_ex_hmm",
                        "xy_f","xy_f_ex_hmm",
                        "xy_npo","xy_npo_ex_hmm","mod")
  
  full_lls <- as.data.frame(rbind(glm_lls,xgb_lls))
  full_lls$mod <- c("glmnet","xgboost")
  names(full_lls) <- c("af","af_ex_hmm",
                       "af_npo","af_npo_ex_hmm",
                       "xy_f","xy_f_ex_hmm",
                       "xy_npo","xy_npo_ex_hmm","mod")
  
  return(list(accuracy = full_accs,
              AUC = full_aucs,
              logloss = full_lls))
  
}

run_models_small <- function(seedu, model_data_full){
  
  library(tidyr)
  library(dplyr)
  library(pROC)
  library(glmnet)
  library(glmnetUtils)
  library(ranger)
  library(here)
  library(xgboost)
  library(stringr)
  
  n <- nrow(model_data_full)
  set.seed(seedu)
  sample_idx <- sample(n, size = 0.85*n)
  model_data <- model_data_full[sample_idx,]  
  test_data <- model_data_full[-sample_idx,]
  
  
  # GLMNET -------------------------------------------------
  

  # glmnet_xy <- zvm_models(model_data,test_data,
  #                         ml_type = "glmnet",
  #                         excl_vars = c(
  #                           names(model_data)[grep("dist_fb",names(model_data))],
  #                           names(model_data)[grep("def_o_qb_diff",names(model_data))]),
  #                         #trace.it = TRUE,
  #                         seed = seedu)
  # 
  # glmnet_xy2 <- zvm_models(model_data,test_data,
  #                          ml_type = "glmnet",
  #                          excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
  #                                        names(model_data)[grep("dist_fb",names(model_data))],
  #                                        names(model_data)[grep("def_o_qb_diff",names(model_data))]),
  #                          #trace.it = TRUE,
  #                          seed = seedu)
  # 
  # 
  # glmnet_xy_npo2 <- zvm_models(model_data,test_data,
  #                              ml_type = "glmnet",
  #                              excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
  #                                            names(model_data)[grep("dist_fb",names(model_data))],
  #                                            names(model_data)[grep("def_o_qb_diff",names(model_data))],
  #                                            max_x_o,max_y_o,tot_dist_o,
  #                                            max_x_d,max_y_d,tot_dist_d),
  #                              #trace.it = TRUE,
  #                              seed = seedu)
  # 
  # XGBOOST -------------------------------------------------

  
  xgb_xy <- zvm_models(model_data,test_data,
                       ml_type = "xgboost",
                       excl_vars = c(
                         names(model_data)[grep("dist_fb",names(model_data))],
                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                       seed = seedu,
                       verbose = FALSE)

  xgb_xy2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                      names(model_data)[grep("dist_fb",names(model_data))],
                                      names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                        seed = seedu,
                        verbose = FALSE)



  xgb_xy_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                          names(model_data)[grep("dist_fb",names(model_data))],
                                          names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  # 
  # Result Tables -------------------------------------------------
  
  # glm_all_mods <- list(glmnet_xy,glmnet_xy2,glmnet_xy_npo2)
  # names(glm_all_mods) <- paste0("glm_",c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm"))
  # 
  # glm_accs <- sapply(glm_all_mods,"[[",2)
  # glm_aucs <- sapply(glm_all_mods,"[[",4)
  # glm_lls <- sapply(glm_all_mods,"[[",5)

  xgb_all_mods <- list(xgb_xy,xgb_xy2,xgb_xy_npo2)
  names(xgb_all_mods) <- paste0("xgb_",c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm"))

  xgb_accs <- sapply(xgb_all_mods,"[[",2)
  xgb_aucs <- sapply(xgb_all_mods,"[[",4)
  xgb_lls <- sapply(xgb_all_mods,"[[",5)
  
  
  ########
  
  
  # full_accs <- as.data.frame(rbind(glm_accs,xgb_accs))
  # full_accs$mod <- c("glmnet","xgboost")
  # names(full_accs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  # full_aucs <- as.data.frame(rbind(glm_aucs,xgb_aucs))
  # full_aucs$mod <- c("glmnet","xgboost")
  # names(full_aucs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  # full_lls <- as.data.frame(rbind(glm_lls,xgb_lls))
  # full_lls$mod <- c("glmnet","xgboost")
  # names(full_lls) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # full_accs <- as.data.frame(rbind(glm_accs))
  # full_accs$mod <- c("glmnet")
  # names(full_accs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  # full_aucs <- as.data.frame(rbind(glm_aucs))
  # full_aucs$mod <- c("glmnet")
  # names(full_aucs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  # full_lls <- as.data.frame(rbind(glm_lls))
  # full_lls$mod <- c("glmnet")
  # names(full_lls) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  full_accs <- as.data.frame(rbind(xgb_accs))
  full_accs$mod <- c("xgboost")
  names(full_accs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")

  full_aucs <- as.data.frame(rbind(xgb_aucs))
  full_aucs$mod <- c("xgboost")
  names(full_aucs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")

  full_lls <- as.data.frame(rbind(xgb_lls))
  full_lls$mod <- c("xgboost")
  names(full_lls) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  return(list(accuracy = full_accs,
              AUC = full_aucs,
              logloss = full_lls))
  
}

run_models_small_lowo <- function(seedu, model_data_full_train, model_data_full_test, param_grid = NULL){
  
  library(tidyr)
  library(dplyr)
  library(pROC)
  library(glmnet)
  library(glmnetUtils)
  library(ranger)
  library(here)
  library(xgboost)
  library(stringr)
  
#  sample_idx <- sample(n, size = 1*n)
  model_data <- model_data_full_train#[sample_idx,]  
  test_data <- model_data_full_test#[-sample_idx,]
  
  
  # GLMNET -------------------------------------------------
  
  
  # glmnet_xy <- zvm_models(model_data,test_data,
  #                         ml_type = "glmnet",
  #                         excl_vars = c(
  #                           names(model_data)[grep("dist_fb",names(model_data))],
  #                           names(model_data)[grep("def_o_qb_diff",names(model_data))]),
  #                         #trace.it = TRUE,
  #                         seed = seedu)
  # 
  # glmnet_xy2 <- zvm_models(model_data,test_data,
  #                          ml_type = "glmnet",
  #                          excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
  #                                        names(model_data)[grep("dist_fb",names(model_data))],
  #                                        names(model_data)[grep("def_o_qb_diff",names(model_data))]),
  #                          #trace.it = TRUE,
  #                          seed = seedu)
  # 
  # 
  # glmnet_xy_npo2 <- zvm_models(model_data,test_data,
  #                              ml_type = "glmnet",
  #                              excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
  #                                            names(model_data)[grep("dist_fb",names(model_data))],
  #                                            names(model_data)[grep("def_o_qb_diff",names(model_data))],
  #                                            max_x_o,max_y_o,tot_dist_o,
  #                                            max_x_d,max_y_d,tot_dist_d),
  #                              #trace.it = TRUE,
  #                              seed = seedu)
  
  # XGBOOST -------------------------------------------------
  
  
  xgb_xy <- zvm_models(model_data,test_data,
                       ml_type = "xgboost",
                       param_grid = param_grid,
                       excl_vars = c(
                         names(model_data)[grep("dist_fb",names(model_data))],
                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                       seed = seedu,
                       verbose = FALSE)
  
  xgb_xy2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        param_grid = param_grid,
                        excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                      names(model_data)[grep("dist_fb",names(model_data))],
                                      names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                        seed = seedu,
                        verbose = FALSE)
  
  
  
  xgb_xy_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            param_grid = param_grid,
                            excl_vars = c("beta", "average","sum","nr_player_changes","average_ent",
                                          names(model_data)[grep("dist_fb",names(model_data))],
                                          names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  
  # Result Tables -------------------------------------------------
  
  # glm_all_mods <- list(glmnet_xy,glmnet_xy2,glmnet_xy_npo2)
  # names(glm_all_mods) <- paste0("glm_",c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm"))
  # 
  # glm_accs <- sapply(glm_all_mods,"[[",2)
  # glm_aucs <- sapply(glm_all_mods,"[[",4)
  # glm_lls <- sapply(glm_all_mods,"[[",5)
  
  xgb_all_mods <- list(xgb_xy,xgb_xy2,xgb_xy_npo2)
  names(xgb_all_mods) <- paste0("xgb_",c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm"))
  
  xgb_accs <- sapply(xgb_all_mods,"[[",2)
  xgb_aucs <- sapply(xgb_all_mods,"[[",4)
  xgb_lls <- sapply(xgb_all_mods,"[[",5)
  
  
  ########
  
  
  # full_accs <- as.data.frame(rbind(glm_accs,xgb_accs))
  # full_accs$mod <- c("glmnet","xgboost")
  # names(full_accs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  # full_aucs <- as.data.frame(rbind(glm_aucs,xgb_aucs))
  # full_aucs$mod <- c("glmnet","xgboost")
  # names(full_aucs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  # full_lls <- as.data.frame(rbind(glm_lls,xgb_lls))
  # full_lls$mod <- c("glmnet","xgboost")
  # names(full_lls) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  # 
  full_accs <- as.data.frame(rbind(xgb_accs))
  full_accs$mod <- c("xgboost")
  names(full_accs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  full_aucs <- as.data.frame(rbind(xgb_aucs))
  full_aucs$mod <- c("xgboost")
  names(full_aucs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  full_lls <- as.data.frame(rbind(xgb_lls))
  full_lls$mod <- c("xgboost")
  names(full_lls) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  return(list(accuracy = full_accs,
              AUC = full_aucs,
              logloss = full_lls))
  
}

# K-fold cross-fitting (as described in the paper): every play gets an
# out-of-sample prediction from a model trained on the other K-1 folds;
# hyperparameters are tuned by the inner CV inside zvm_models().
run_models_crossfit <- function(seedu, model_data_full, K = 5, param_grid = NULL){
  
  # load packages on the parallel workers, zvm_models() needs all of them
  library(dplyr)
  library(tidyr)
  library(pROC)
  library(glmnet)
  library(glmnetUtils)
  library(ranger)
  library(xgboost)
  
  excl_base <- c(names(model_data_full)[grep("dist_fb",names(model_data_full))],
                 names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))])
  hmm_vars <- c("beta","average","sum","nr_player_changes","average_ent")
  naive_vars <- c("max_x_o","max_y_o","tot_dist_o","max_x_d","max_y_d","tot_dist_d")
  feature_sets <- list(xy_f = excl_base,                                  # HMM post
                       xy_f_ex_hmm = c(excl_base, hmm_vars),              # naive post
                       xy_npo_ex_hmm = c(excl_base, hmm_vars, naive_vars)) # pre
  
  set.seed(seedu)
  folds <- sample(rep(1:K, length.out = nrow(model_data_full)))
  y <- model_data_full$pff_manZone_num
  
  preds <- list()
  for(ml in c("glmnet","xgboost")){
    for(fs in names(feature_sets)){
      p <- numeric(nrow(model_data_full))
      for(k in 1:K){
        args <- list(model_data = model_data_full[folds != k,],
                     test_data = model_data_full[folds == k,],
                     ml_type = ml,
                     excl_vars = feature_sets[[fs]],
                     seed = seedu,
                     return_preds = TRUE)
        if(ml == "xgboost"){
          args$verbose <- FALSE
          args$param_grid <- param_grid
        }
        p[folds == k] <- c(do.call(zvm_models, args)$preds)
      }
      preds[[ml]][[fs]] <- p
    }
  }
  
  metric_tab <- function(f){
    tab <- as.data.frame(do.call(rbind, lapply(preds, \(pm) sapply(pm, f))))
    tab$mod <- names(preds)
    tab
  }
  
  return(list(accuracy = metric_tab(\(p) mean((p > 0.5) == y)),
              AUC = metric_tab(\(p) as.numeric(auc(roc(y, p, quiet = TRUE)))),
              logloss = metric_tab(\(p) -mean(y*log(p)+(1-y)*log(1-p))),
              preds = preds,
              folds = folds))
}

# nsim = 50
# seeds <- round(seq(1,1000,length.out = nsim))
# 
# results_list <- list()
# for(i in 1:nsim){
#   seedu = seeds[i]
#   cat("\n\n")
#   cat("Iteration ",i)
#   cat("\n Seed: ",seedu)
#   cat("\n\n")
#   
#   results_list[[i]] <- run_models_small(seedu)
# }

library(parallel)

# Each repetition yields out-of-sample predictions for all plays, so 10
# repetitions of 5-fold cross-fitting (50 fits per model) suffice
nsim <- if(TEST) 2 else 10
seeds <- round(seq(1, 1000, length.out = nsim))

# Cluster mit Anzahl der Kerne erstellen
# use the cores SLURM allocated (detectCores() returns all cores of the node)
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", detectCores() - 1))
if(TEST) n_cores <- min(n_cores, 2)
cl <- makeCluster(n_cores)

# Optional: benötigte Funktionen/Objekte exportieren
clusterExport(cl, c("run_models_crossfit", "zvm_models", "seeds", "model_data_full", "test_grid"))

results_list <- parLapply(
  cl,
  seeds,
  function(seedu) {
    cat("\n\nIteration ", which(seeds == seedu))
    cat("\nSeed: ", seedu, "\n\n")
    run_models_crossfit(seedu, model_data_full, param_grid = test_grid)
  }
)

stopCluster(cl)   # Cluster wieder schließen

#saveRDS(results_list,"data/zvm_cv_runs_240925_full_n14.rds")
#saveRDS(results_list, here("results", "zvm_cv_runs_full_only_xgb.rds"))
saveRDS(results_list, file.path(res_dir, "zvm_crossfit_runs.rds"))



# LOWO CV -----------------------------------------------------------------
# Uses the real week of each play (play_week), not row blocks
weeks <- sort(unique(play_week))
if(TEST) weeks <- weeks[1:2]

# Cluster erstellen
cl <- makeCluster(n_cores)

# Notwendige Objekte/Funktionen exportieren
clusterExport(cl, c("run_models_small_lowo", "zvm_models", "model_data_full", "play_week", "weeks", "test_grid"))

results_list <- parLapply(
  cl,
  weeks,
  function(test_week) {
    cat("\n\nLeave-one-week-out CV")
    cat("\nTest week:", test_week, "\n\n")
    
    # Trainingsdaten = alle Wochen außer der Testwoche
    train_data <- model_data_full[play_week != test_week, ]
    
    # Testdaten = nur die aktuelle Woche
    test_data  <- model_data_full[play_week == test_week, ]
    
    # Modell auf Trainingsdaten fitten und auf Testdaten evaluieren
    run_models_small_lowo(seedu = 123,
                          model_data_full_train = train_data,
                          model_data_full_test  = test_data,
                          param_grid = test_grid)
  }
)

stopCluster(cl)

saveRDS(results_list, file.path(res_dir, "zvm_lowo_runs.rds"))


# Results ---------------------------------------------------------------

results_list <- readRDS(file.path(res_dir, "zvm_crossfit_runs.rds"))

#### tables and averages

accs <- do.call(rbind,lapply(results_list,"[[",1))
accs_glmnet <- accs |> filter(mod == "glmnet") |> select(-mod)
accs_xgb <- accs |> filter(mod == "xgboost") |> select(-mod)

#apply(accs_glmnet,2,mean)
colMeans(accs_glmnet)
colMeans(accs_xgb)


aucs <- do.call(rbind,lapply(results_list,"[[",2))
aucs_glmnet <- aucs |> filter(mod == "glmnet") |> select(-mod)
aucs_xgb <- aucs |> filter(mod == "xgboost") |> select(-mod)

#apply(aucs_glmnet,2,mean)
colMeans(aucs_glmnet)
colMeans(aucs_xgb)

lls <- do.call(rbind,lapply(results_list,"[[",3))
lls_glmnet <- lls |> filter(mod == "glmnet") |> select(-mod)
lls_xgb <- lls |> filter(mod == "xgboost") |> select(-mod)

colMeans(lls_glmnet)
colMeans(lls_xgb)


#### plots

col_vals <- c("#38761d","yellowgreen","gray50")
names(col_vals) <- c("HMM post","naive post","pre")

accsl <- accs |> pivot_longer(-mod)

accs_bdb <- accsl |>
  filter(name %in% c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm")) |>
  mutate(display_name = ifelse(name == "xy_f","HMM post",
                               ifelse(name == "xy_f_ex_hmm","naive post","pre")))
accs_bdb$display_name = factor(accs_bdb$display_name, levels = c("pre","naive post","HMM post"))

accs_bdb |>
  ggplot(aes(x=reorder(display_name, value, FUN=median),y =value,fill = display_name)) +
  geom_boxplot()+
  scale_fill_manual(values = rev(col_vals))+
  facet_wrap(~mod) +
  labs(title = "Accuracys of models (50 different seeds)",
       x = "Model",
       y = "Accuracy",
       fill = "Model")


aucsl <- aucs |> pivot_longer(-mod)

aucs_bdb <- aucsl |>
  filter(name %in% c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm")) |>
  mutate(display_name = ifelse(name == "xy_f","HMM post",
                               ifelse(name == "xy_f_ex_hmm","naive post","pre")))
aucs_bdb$display_name = factor(aucs_bdb$display_name, levels = c("pre","naive post","HMM post"))


aucsl |>
  ggplot(aes(x=name,y =value,fill = name)) +
  geom_boxplot()+
  scale_fill_manual(values = col_vals)+
  facet_wrap(~mod) +
  labs(title = "AUCs of models (50 different seeds)")


aucs_bdb |>
  ggplot(aes(x=reorder(display_name, value, FUN=median),y =value,fill = reorder(display_name, value, FUN=median))) +
  geom_boxplot()+
  scale_fill_manual(values = rev(col_vals))+
  facet_wrap(~mod) +
  labs(title = "AUCs of models (50 different seeds)",
       x = "Model",
       y = "AUC",
       fill = "Model")

# aucs_app |>
#   ggplot(aes(x=dp_fac,y =value,fill = dp_fac)) +
#   geom_boxplot()+
#   scale_fill_manual(values = col_vals)+
#   facet_wrap(~mod) +
#   labs(title = "AUCs of models (50 different seeds)",
#        x = "Model",
#        y = "AUC",
#        fill = "Model")


llsl <- lls |> pivot_longer(-mod)



lls_bdb <- llsl |>
  filter(name %in% c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm")) |>
  mutate(display_name = ifelse(name == "xy_f","HMM post",
                               ifelse(name == "xy_f_ex_hmm","naive post","pre")))

lls_bdb |>
  ggplot(aes(x=reorder(display_name, -value, FUN=median),y =-value,fill = reorder(display_name, -value, FUN=median))) +
  geom_boxplot()+
  scale_fill_manual(values = rev(col_vals))+
  facet_wrap(~mod) +
  labs(title = "Negative loglosses of models (50 different seeds)",
       x = "Model",
       y = "Negative logloss (log-likelihood)",
       fill = "Model")

