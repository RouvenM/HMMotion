
#### BDB 2025 - Coverage model ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
library(LaMa)

# With real data ----------------------------------------------------------

setwd("C:/Users/michels/sciebo/BDB 2025")
tracking_data = read.csv("tracking_week_1.csv")
players = read.csv("players.csv")
plays = read.csv("plays.csv")
players_play = read.csv("player_play.csv")

# Spiel 2022091110, playId 291 als Beispiel benutzen
# Marco Wilson geht mit Mecole Hardman die ganze Zeit in Man Coverage mit

tracking = tracking_data %>% 
  filter(gameId == unique(tracking_data$gameId)[1])

tracking = tracking_data %>% filter(gameId == 2022091110) %>% filter(playId == 291) 

#write.csv(tracking, file = "tracking_data_week_1_game_kcaz.csv")

rm(tracking_data)

# Ideen: 

# Sortieren der Defender nach der y-Achse (damit immer die Wahrscheinlichkeit,
# dass Spieler, die nah beieinander sind, eher die Angreifer übergeben, höher ist)

# Es gibt auch das Event man_in_motion, nach dem

# Filter out for data before the snap, i.e. the time of the motion
# Connect with player and players df to receive information
# Filter out the ball, offensive and defensive line and the QB
tracking_presnap1 = tracking %>% 
  mutate(y = y - 53.3/2) %>% # center y-coordinate
  #split(.,tracking$displayName) %>% 
  #map(~mutate(., initial_y = y[which(event == "line_set")[1]])) %>% 
  #bind_rows() %>% 
  filter(frameType == "BEFORE_SNAP") %>% 
  # Take out the time where teams are in the huddle, i.e. before first line up
  left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
  left_join(., plays %>% dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
            by = c("playId", "gameId")) %>% 
  mutate(off_def = ifelse(club == possessionTeam, 1, 0)) %>% 
  filter(frameId >= frameId[which(event == "line_set")[1]])
  
# Positional adjustment
tracking_presnap = tracking_presnap1 %>% 
  filter(!(position %in% c("T", "G", "C", NA, "QB", "NT", "DT", "DE")))

# Offensivspieler-Daten extrahieren
off_data <- tracking_presnap %>%
  filter(off_def == 1) %>%
  group_by(time) %>%
  mutate(off_num = row_number()) %>% # Nummeriere Offensivspieler innerhalb jeder Sekunde
  ungroup() %>%
  select(time, off_num, y) %>% #, x) %>%
  pivot_wider(
    names_from = off_num, # Verwende die generierte Nummer als Basis für die Spalten
    values_from = y, #c(x, y),
    names_glue = "player{off_num}_{.value}" # Benenne die Spalten als player1_x, player1_y, etc.
  )

# off_data <- tracking_presnap %>%
#   filter(off_def == 1) %>%
#   select(time, nflId, y) %>%
#   pivot_wider(
#     names_from = nflId,
#     values_from = c(y),
#     names_glue = "player{nflId}_{.value}"
#   )

# Defensivspieler-Daten extrahieren
def_data <- tracking_presnap %>%
  filter(off_def == 0)

# Daten zusammenführen
data <- def_data %>%
  left_join(off_data, by = "time")

# Take out off-coverage players (most likely safeties that do now follow players at the LOS)
# wenn IMMER hinter 10
#data = data %>% filter(abs(x - absoluteYardlineNumber) < 10)

# Fit model ---------------------------------------------------------------

## deterministic initial distribution
n_att = 5

Delta = t(
  sapply(split(data, data$nflId),
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))] = 1
           delta
         })
)

rnames_old = rownames(Delta)
# only defenders that are close to offensive players
Delta2 = t(
  sapply(split(data, data$nflId),
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))] = min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))
           delta
         })
)

# Funktion zum Anwenden der Regel pro Zeile
process_row <- function(row) {
  non_zero_indices <- which(row > 0)
  if (length(non_zero_indices) == 1) {
    result <- rep(0, length(row))
    result[non_zero_indices] <- row[non_zero_indices]
  } else if (length(non_zero_indices) > 1) {
    result <- rep(0, length(row))
    smallest_index <- non_zero_indices[which.min(row[non_zero_indices])]
    result[smallest_index] <- row[smallest_index]
  } else {
    result <- row
  }
  return(result)
}

# Anwenden auf den gesamten Data Frame
processed_df <- as.data.frame((apply(Delta2, 2, process_row)))

# Spaltennamen beibehalten
colnames(processed_df) <- colnames(Delta2)
row.names(processed_df) = rnames_old

Delta = processed_df[which(rowSums(processed_df)>0),]
rnames_new = rownames(Delta)

data = data %>% filter(nflId %in% rnames_new)

dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + 1:n_att]),
           ID = data$nflId,
           n_att = n_att, 
           Delta = as.matrix(Delta))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1),
           logitalpha = qlogis(0.95))

nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma)
  alpha = plogis(logitalpha)
  
  # assuming y variable is centered -> pulling to the middle
  # alpha * X + (1-alpha) * y_middle
  Mu = alpha * X
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
map = list(eta = factor(rep(1, n_att * (n_att - 1))))

obj = MakeADFun(nll, par, map = map)
opt = nlminb(obj$par, obj$fn, obj$gr)

mod = obj$report()
(Delta = mod$delta)
Gamma = mod$Gamma
allprobs = mod$allprobs
trackID = mod$trackID
(alpha = mod$alpha)

probs = stateprobs(mod = mod)
colnames(probs) = paste0("attacker_", 1:n_att)
probs = cbind(ID = trackID, probs)

probs = split(as.data.frame(probs), probs[,1])
probs = lapply(probs, as.matrix)

def = 5
probs[[def]][1,1]
start = 1
plot(probs[[def]][start,-1], type = "h", ylim = c(0,1))
for(t in (start + 1) : nrow(probs[[def]])){
  plot(probs[[def]][t,-1], type = "h", ylim = c(0,1), main = paste("Time:", t/10))
  Sys.sleep(0.1)
}

# which defender
def_ids = data %>% 
  filter(off_def == 0) %>% 
  pull(nflId) %>% 
  unique() 

players %>% filter(nflId %in% def_ids)

# which offenders
(att_ids = tracking_presnap %>% 
  filter(off_def == 1) %>% 
  pull(nflId) %>% 
  unique()) 

players %>% filter(nflId %in% att_ids)

plays %>% 
  filter(gameId == tracking$gameId[2]) %>% 
  mutate(gameClockNum = as.numeric(str_sub(gameClock, 1, 2)) * 60 + as.numeric(str_sub(gameClock, 4, 5))) %>% 
  arrange(desc(gameClockNum)) %>% 
  filter(pff_manZone == "Man") %>%
  View()
  select(playId)
  
  
# Some Ideas

# only defenders in the vicinity of the los (safeties out, done)
# only defenders that are not in the vicinity of the ball (i.e. ILB as DE raus)

