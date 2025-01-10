
#### BDB 2025 - Coverage model ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
library(LaMa)
# Load the ggplot2 library
library(ggplot2)
# With real data ----------------------------------------------------------

pre_process <- function(tracking_data){
  
  tracking_data$uniId2 = paste0(tracking_data$gameId, tracking_data$playId)
  
  unid2_motion = tracking_data %>% filter(event == "man_in_motion") %>% pull(uniId2)
  
  tracking = tracking_data %>% filter(uniId2 %in% unid2_motion)
  
  tracking_presnap1 = tracking %>% 
    mutate(uniId = paste0(gameId, playId, nflId)) %>% 
    mutate(uniId2 = paste0(gameId, playId)) %>% 
    mutate(y = y - 53.3/2) %>% # center y-coordinate
    mutate(x = x - 120/2) %>% 
    mutate(displayName_fac = factor(displayName, levels = unique(displayName))) %>% 
    mutate(playId_fac = factor(playId, levels = unique(playId))) %>% 
    group_by(displayName_fac, playId_fac) %>% #use group split to stay with the order
    group_split() %>% 
    # Take out the time where teams are in the huddle, i.e. before first line up
    map(~mutate(., initial_y = y[which(event == "line_set")[1]])) %>% 
    bind_rows() %>% 
    group_by(playId_fac) %>% 
    group_split() %>% 
    map(~filter(.,frameId >= frameId[which(event == "line_set")[1]])) %>% 
    bind_rows() %>% 
    filter(frameType == "BEFORE_SNAP") %>% # Filter out for data before the snap, i.e. the time of the motion
    # Connect with player and players df to receive information
    left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
    left_join(., plays %>% filter(offenseFormation != "JUMBO", 
                                  offenseFormation != "WILDCAT") %>% 
                dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
              by = c("playId", "gameId")) %>% 
    mutate(off_def = ifelse(club == possessionTeam, 1, 0))
  
  
  # Ensure that online 5 Oline are on the field and only one QB
  position_counts <- tracking_presnap1 %>%
    group_by(uniId2) %>% 
    mutate(Count_all = n()/length(unique(nflId))) %>% 
    ungroup() %>% 
    group_by(uniId2, position) %>% 
    summarise(Count = n(), .groups = "drop", Count_all = unique(Count_all)) %>%
    mutate(Count = Count/Count_all) %>% 
    filter(position %in% c("C", "G", "T")) %>% 
    group_by(uniId2) %>% summarise(sum_oline = sum(Count)) %>% 
    filter(sum_oline > 5) %>% 
    pull(uniId2)
  
  position_counts2 <- tracking_presnap1 %>%
    group_by(uniId2) %>% 
    mutate(Count_all = n()/length(unique(nflId))) %>% 
    ungroup() %>% 
    group_by(uniId2, position) %>% 
    summarise(Count = n(), .groups = "drop", Count_all = unique(Count_all)) %>%
    mutate(Count = Count/Count_all) %>% 
    filter(position == "QB", Count == 2) %>% 
    pull(uniId2)
  
  valid_play_ids2 = tracking_presnap1 %>% group_by(uniId2) %>% 
    summarise(Count = n(), .groups = "drop") %>% 
    mutate(invalid = ifelse(Count < 24, 1, 0)) %>% 
    filter(invalid != 1) %>% pull(uniId2)
  
  # 3. Keep only relevant playids
  tracking_presnap1 <- tracking_presnap1 %>%
    filter(!(uniId2 %in% position_counts)) %>% 
    filter(!(uniId2 %in% position_counts2)) %>% 
    filter(uniId2 %in% valid_play_ids2)
  
  # Positional adjustment
  # Filter out the ball, offensive and defensive line and the QB
  tracking_presnap1 = tracking_presnap1 %>% 
    filter(!(position %in% c("T", "G", "C", NA, "QB", "NT", "DT", "DE")))
  
  # extract offensive players
  off_players <- tracking_presnap1 %>%
    filter(off_def == 1) %>%
    group_by(gameId, playId) %>%
    mutate(player_order = rank(initial_y, ties.method = "first")) %>%
    arrange(gameId, playId, player_order) %>%
    ungroup() %>% 
    select(-player_order) %>% 
    group_by(gameId, playId, time) %>%
    mutate(off_num = row_number()) %>% 
    # arrange(initial_y) %>% 
    ungroup()
  
  off_data = off_players %>% 
    select(gameId, playId, time, off_num, y, x) %>%
    pivot_wider(
      names_from = off_num,
      values_from = c(x, y),
      names_glue = "player{off_num}_{.value}" # name columns as player1_x, player1_y, etc.
    )
  
  # off_data <- tracking_presnap %>%
  #   filter(off_def == 1) %>%
  #   select(time, nflId, y) %>%
  #   pivot_wider(
  #     names_from = nflId,
  #     values_from = c(y),
  #     names_glue = "player{nflId}_{.value}"
  #   )
  
  # extract defensive data
  
  def_data <- tracking_presnap1 %>%
    group_by(gameId, playId) %>%
    mutate(player_order = rank(initial_y, ties.method = "first")) %>%
    arrange(gameId, playId, player_order) %>%
    select(-player_order)  %>% 
    filter(off_def == 0)
  
  # Daten zusammenführen
  data <- def_data %>%
    left_join(off_data, by = c("time", "playId", "gameId"))
  
  return(data)
}

n_att = 5

setwd("C:/Users/michels/sciebo/BDB 2025")
players = read.csv("players.csv")
plays = read.csv("plays.csv")
tracking_data = pre_process(read.csv("tracking_week_1.csv"))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_2.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_3.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_4.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_5.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_6.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_7.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_8.csv")))
tracking_data = bind_rows(tracking_data, pre_process(read.csv("tracking_week_9.csv")))

data = tracking_data

# Spiel 2022091110, playId 291 as primary example

plays = plays %>% 
  mutate(uniId2 = paste0(gameId, playId)) 


# Fit model ---------------------------------------------------------------
#data = data %>% filter(gameId == 2022091110) %>% filter(playId == 291)

## deterministic initial distribution

Deltas =   t(
  sapply((split(data, data$uniId))[as.character(unique(data$uniId))],
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + n_att + 1:n_att]))] = 1
           delta
         })
)

## deterministic initial distribution
# only defenders that are close to offensive players
Delta2 = t(
  sapply(split(data, data$uniId)[as.character(unique(data$uniId))],
         function(x){
           delta = numeric(n_att)
           off_indice = which(str_detect(names(x),"player1_y"))
           delta[which.min(abs(x$y[1] - x[1, off_indice[1]-1 + 1:n_att]))] = 
             min(abs(x$y[1] - x[1, off_indice[1]-1 + 1:n_att]))
           delta
         })
)# # only defenders that are close to offensive players

# weighted version
# Idea: always select the defender closest to the attacker first until
#library(clue)
# each attacker is assigned
Delta2 <- t(
  sapply(split(data, data$uniId)[as.character(unique(data$uniId))],
         function(x){
           delta <- numeric(n_att)  # Initialize the result vectors
           
           # Extract the relevant columns
           y_def <- x$y[1]  # Y-coordinate of the defender
           x_def <- x$x[1]  # X-coordinate of the defender
           
           # Indices of attackers (columns with "player" in the name)
           offender_indices_start <- which(str_detect(names(x), "player1_x"))
           offender_indices = offender_indices_start + 0:(n_att-1)
           # Calculate the weighted distances
           distances <- sapply(offender_indices, function(i) {
             y_off <- x[1,i+n_att]
             x_off <- x[1,i]
             
             # Weighted distance calculation
             wy <- 0.95  # Weight for the y-distance
             wx <- 0.05  # Weight for the x-distance
             sqrt(wy * (y_off - y_def)^2 + wx * (x_off - x_def)^2)
           }) %>% unlist()
           
           # Find the attacker with the minimum distance
           #closest_defender <- which.min(distances)
           delta =#[closest_defender] 
             distances#[closest_defender]
           delta
         })
)

rnames = rownames(Delta2)
Delta2 = as.data.frame(Delta2)
Delta2$uniId2 = str_sub(rownames(Delta2), 1, str_length(rownames(Delta2))-5)
Delta2$rnames = rnames

# Hungarian Algorithm (alternative)
# Delta3 <- #t(
#   lapply(split(Delta2, Delta2$uniId2)[as.character(unique(Delta2$uniId2))],
#          function(x){
#   # Number of defenders (rows) and attackers (columns)
#   n_defenders <- nrow(x)
#   n_attackers <- n_att
# 
#   # Add dummy columns with high costs to make the matrix square
#   if (n_defenders > n_attackers) {
#     dummy_cols <- matrix(10^6, nrow = n_defenders, ncol = n_defenders - n_attackers)
#     x <- cbind(x, dummy_cols)
#   }
#   
#   uniId2 = x$uniId2[1]
#   rnames = x$rnames[1]
#   x = x[,-c(6,7)] %>% as.matrix() 
#   assignment <- solve_LSAP(x)
# 
#   # Display the result
#   result <- data.frame(
#     rnames = rownames(x),  # Defenders (rows)
#     Attacker = paste0("player", assignment, "_y"),  # Assigned attackers (columns)
#     Cost = x[cbind(1:nrow(x), assignment)]  # Distance
#   )
# 
#   result = result[which(result$Cost < 10^5),]
# 
#   # One-hot encoding with model.matrix
#   one_hot <- model.matrix(~ Attacker - 1, result)
#   colnames(one_hot) = paste0("player", 1:5, "_y")
# 
#   # Add the one-hot columns to the original table
#   data_one_hot <- cbind(result, one_hot) %>% dplyr::select(-c("Attacker", "Cost"))
#          }
#   )
# 
# Delta3 = bind_rows(Delta3)
# 
# rownames(Delta3) = Delta3$rnames
# 


Delta3 <- #t(
  lapply(split(Delta2, Delta2$uniId2)[as.character(unique(Delta2$uniId2))],
         function(distance_matrix){
           # Number of attackers and defenders
           n_defenders <- nrow(distance_matrix)
           n_attackers <- n_att
           
           # Result list: attacker -> defender
           assignments <- rep(NA, n_attackers)
           
           # Copy the matrix to modify it
           matrix_copy <- distance_matrix[,1:n_att]
           
           # Repeat until all attackers are assigned
           for (i in 1:n_att) {
             # Find the global smallest distance
             min_index <- which(matrix_copy == min(matrix_copy, na.rm = TRUE), arr.ind = TRUE)[1,]
             
             # Row and column of the minimum value
             defender <- min_index[1]
             attacker <- min_index[2]
             
             # Save the assignment
             assignments[attacker] <- defender
             
             # Remove the row (defender) and column (attacker) from the matrix
             matrix_copy[defender, ] <- NA  # Defender is no longer available
             matrix_copy[, attacker] <- NA  # Attacker is assigned
           }
           
           assignments2 = assignments[1:n_att]
           
           # Create the binary matrix based on the original order
           binary_matrix <- matrix(0, nrow = n_defenders, ncol = n_att) %>% as.data.frame()
           
           # Set ones according to `row_indices`
           for (i in seq_along(assignments2)) {
             binary_matrix[assignments2[i], i] <- 1
           }
           
           rownames(binary_matrix) <- rownames(distance_matrix)
           colnames(binary_matrix) <- c(paste0("player", 1:5, "_y"))
           
           binary_matrix = binary_matrix[rowSums(binary_matrix[,1:5]) > 0, ]
           
           
           return(binary_matrix)
         }
  )

Delta3 = bind_rows(Delta3)
rnames_new = rownames(Delta3)

data = data %>% filter(uniId %in% rnames_new)

dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att]),
           ID = data$uniId,
           n_att = n_att, 
           Delta = as.matrix(Deltas))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1))

nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma)

  Mu = X
  REPORT(Mu)
  REPORT(alpha)
  
  allprobs = matrix(1, length(y_pos), n_att)
  ind = which(!is.na(y_pos))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos, Mu[,j], sigma)
  }
  
  -forward(Delta, Gamma, allprobs, trackID = ID)
}

# all off-diagonal probablities are the same
#map = list(eta = factor(rep(1, n_att * (n_att - 1))))

obj = MakeADFun(nll, par)#, map = map)
opt = nlminb(obj$par, obj$fn, obj$gr)

mod = obj$report()
(Delta = mod$delta)
(Gamma = mod$Gamma)
allprobs = mod$allprobs
trackID = mod$trackID
(alpha = mod$alpha)

probs = stateprobs(mod = mod)
colnames(probs) = paste0("attacker_", 1:n_att)
probs = cbind(ID = trackID, probs)

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
  numeric_values <- apply(df[, 2:6], 2, function(col) as.numeric(as.character(col)))
  
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

res_df = results_df %>% group_by(gameId, playId) %>% 
  mutate(player_change = ifelse(num_changes > 0, 1, 0)) %>% 
  summarize(average = mean(num_changes),
            sum = sum(num_changes),
            nr_player_changes = sum(player_change), 
            average_sd = mean(sd),
            average_ent = mean(entropy))

res_df = merge(plays, res_df, by = c("gameId", "playId"))

res_df = res_df %>% filter(pff_manZone != "Other")

# Visualize the frequency of categories by group
ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = nr_player_changes, fill = pff_manZone)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = pff_manZone)) + # ..prop.. calculates relative frequency
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Category", 
       y = "Relative Frequency") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = sum, fill = pff_manZone)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = pff_manZone)) + # ..prop.. calculates relative frequency
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Category", 
       y = "Relative Frequency") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = average_sd, fill = pff_manZone)) +
  geom_histogram(aes(y = ..density..), position = "dodge", alpha = 0.7, binwidth = 0.01) +
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Value of average_sd", 
       y = "Relative Frequency (Density)") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = average_ent, fill = pff_manZone)) +
  geom_histogram(aes(y = ..density..), position = "dodge", alpha = 0.7, binwidth = 0.01) +
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Value of average_ent", 
       y = "Relative Frequency (Density)") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

res_df$day = str_sub(res_df$gameId, 1, 8)
days = unique(res_df$day)
res_df = res_df %>% filter(day %in% days[7:11])

pl_data = res_df %>% filter(pff_manZone != "Other")
plot(pl_data$average_ent, as.factor(pl_data$pff_manZone))

par(mfrow = c(1,2))
boxplot(pl_data$average_ent[which(pl_data$pff_manZone != "Zone")], ylim = c(0,1))
boxplot(pl_data$average_ent[which(pl_data$pff_manZone == "Zone")], ylim = c(0,1))

mod_log = glm(as.factor(pff_manZone) ~ average_ent, data = pl_data, family = "binomial")
summary(mod_log)
