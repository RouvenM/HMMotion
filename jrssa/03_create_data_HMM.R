######################################################################################
######################################################################################
#### HMMotion: data creation
######################################################################################
######################################################################################

# Load data ---------------------------------------------------------------
library(LaMa)
library(tidyverse)
library(dplyr)
library(vroom)
library(stringr)
library(here)
plays = read.csv(here("rawdata","plays.csv"))

data <- vroom(here("rawdata", "final_preprocessed_week_1_lag4.csv"),
              col_types = list(
                uniId = col_character(),
                game_play_id = col_character()
              ))
data$week = 1
for (i in 2:9) {
  data <- rbind(data, vroom(here("rawdata", paste0("final_preprocessed_week_",i,
                                                   "_lag4.csv")),
                            col_types = list(
                              uniId = col_character(),
                              game_play_id = col_character()
                            )) |> mutate(week = i))
}

# premotion_data <- readRDS("data/w12_y_ord_5od.rds")
# premotion_data <- rbind(premotion_data,readRDS("data/w34_y_ord_5od.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w56_y_ord_5od.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w78_y_ord_5od.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w9_y_ord_5od.rds"))
premotion_data <- readRDS(here("rawdata", "week1_y_ord_od.rds")) |> mutate(week = 1)
for (i in 2:9) {
  premotion_data <- rbind(premotion_data, 
                          readRDS(here("rawdata", 
                                       paste0("week",i,"_y_ord_od.rds"))) |> mutate(week = i))
}


### naive distances
postmotion_adder <- readRDS(here("rawdata", "week_1_dist_adder.rds")) |> mutate(week = 1)
for (i in 2:9) {
  postmotion_adder <- rbind(postmotion_adder, readRDS(here("rawdata", 
                                                       paste0("week_",i,"_dist_adder.rds"))) |> mutate(week = i))
}
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/second_dist_adder.rds"))
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/w56_dist_adder.rds"))
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/w78_dist_adder.rds"))
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/w9_dist_adder.rds"))

### HMM data
#load("stateprobs_with_ids.rds") ### needs load function because probably not save via saveRDS!
#load("HMM_features_lag_4_per_play.rds") ### needs load function because probably not save via saveRDS!
#load("HMM_features_lag_4.rds") ### needs load function because probably not save via saveRDS!
mod = readRDS(here("results", "mod_final_full.rds"))
#mod = readRDS("mod_final_full.rds")

probs = stateprobs_g(mod = mod)

n_att = 5
colnames(probs) = paste0("attacker_", 1:n_att)
probs = cbind(ID = data$uniId, probs)

probs = (split(as.data.frame(probs), probs[,1]))[as.character(unique(probs[,1]))]
probs = lapply(probs, as.matrix)

# Function to calculate entropy
calculate_entropy <- function(Z) {
  Z <- Z[Z > 0]  # Remove zero values
  entropy <- -sum(Z * log(Z))  # Calculate entropy
  return(entropy)
}

analyze_data <- function(df) {
  # Convert columns 2 to 6 to numeric values
  numeric_values <- apply(df[, which(str_detect(colnames(df),"attacker"))], 2, function(col) as.numeric(as.character(col)))
  
  # Handle missing values (NA) if conversion fails
  if (anyNA(numeric_values)) {
    warning("There are NA values after the conversion. Please check!")
  }
  #
  # if(nrow(numeric_values) > 3){
  # numeric_values = numeric_values[-c(1:2),] # Delete the first 5 rows that can occur due to incorrect assignments
  # }
  
  sd = mean(apply(numeric_values, 2, sd))
  # Determine the maximum value per row
  max_values <- apply(numeric_values, 1, which.max)
  
  # Calculate the number of changes in the maximum value
  changes <- sum(c(NA, diff(max_values)) != 0, na.rm = TRUE)
  
  # Calculate Zn(j, k): The proportion of time points where each attacker was covered
  Zn <- table(factor(max_values, levels = 1:5)) / nrow(df)
  
  # Calculate the defensive entropy
  entropy <- calculate_entropy(Zn)
  
  # Return results as a list
  data.frame(sd = sd, num_changes = changes, entropy = entropy)
}

results_df <- do.call(rbind, lapply(probs, analyze_data))
results_df$gameId = str_sub(rownames(results_df), 1, 10)
results_df$playId = str_sub(rownames(results_df), 11, str_length(rownames(results_df))-5)

#save(results_df, file = "HMM_features_lag_4.rds")
#save(probs, file = "stateprobs_with_ids.rds")

res_df = results_df %>% group_by(gameId, playId) %>% 
  mutate(player_change = ifelse(num_changes > 0, 1, 0)) %>% 
  summarize(average = mean(num_changes),
            sum = sum(num_changes),
            nr_player_changes = sum(player_change), 
            average_sd = mean(sd),
            average_ent = mean(entropy)) |> 
  mutate(game_play_id = as.numeric(paste0(gameId, playId)))

random_effs = mod$beta_play |> as.data.frame()
random_effs = random_effs |> mutate(game_play_id = rownames(random_effs), 
                                    beta = `mod$beta_play`) |> select(-`mod$beta_play`)

res_df = merge(res_df, random_effs, by = c("game_play_id"))
res_df = merge(plays, res_df, by = c("gameId", "playId"))

# df_num = res_df |> select(sum, nr_player_changes, average_ent, beta)
# s <- sapply(df_num, summary)
# library(knitr)
# library(kableExtra)
# 
# tab <- data.frame(
#   Statistic = rownames(s),
#   s,
#   row.names = NULL
# )
# 
# tab[-1] <- lapply(tab[-1], function(x) round(as.numeric(x), 3))
# 
# kable(
#   tab,
#   format = "latex",
#   booktabs = TRUE,
#   align = "lcccc",
#   caption = "Summary statistics"
# ) |>
#   kable_styling(latex_options = "hold_position")

res_df <- res_df |> 
  mutate(gameplayId = as.numeric(paste0(gameId, playId)))

full_data <- premotion_data |> #select(-nflId_football) |>
  left_join(postmotion_adder,by = "gameplayId") |>
  left_join(res_df |> ungroup() |> select(gameplayId,beta,average,sum,nr_player_changes,ends_with("_ent")),by = "gameplayId") |>
  drop_na()


saveRDS(full_data, here("results", "preprocessed_HMM_data_full.rds"))
