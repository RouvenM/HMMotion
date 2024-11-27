
#### BDB 2025 - Coverage model ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
library(LaMa)

# With real data ----------------------------------------------------------

# Version control
# coverage_model_real_data = baseline model
# coverage_model_real_data_v2 = 1) position ordered such that we can estimate different
#                                  transition probs (attention when using split)
#                               2) preprocessing prepared for a whole match 
#                                  (not only one play)

setwd("~/Sciebo/BDB 2025")
tracking_data = read.csv("tracking_week_1.csv")
players = read.csv("players.csv")
plays = read.csv("plays.csv")

# Spiel 2022091110, playId 291 als Beispiel benutzen
# Marco Wilson geht mit Mecole Hardman die ganze Zeit in Man Coverage mit

tracking = tracking_data %>% 
  filter(gameId == unique(tracking_data$gameId)[1])

tracking = tracking_data %>% filter(gameId == 2022091110) #%>% filter(playId == 291) 

#write.csv(tracking, file = "tracking_data_week_1_game_kcaz.csv")

rm(tracking_data)

# Ideen: 

# Es gibt auch das Event man_in_motion, nachdem wir starten können

tracking_presnap1 = tracking %>% 
  mutate(y = y - 53.3/2) %>% # center y-coordinate
  mutate(displayName_fac = factor(displayName, levels = unique(displayName))) %>% 
  mutate(playId_fac = factor(playId, levels = unique(playId))) %>% 
  group_by(displayName_fac, playId_fac) %>% #use group split to stay with the order
  group_split() %>% 
  map(~mutate(., initial_y = y[which(event == "line_set")[1]])) %>% 
  bind_rows() %>% 
  group_by(playId_fac) %>% 
  group_split() %>% 
  map(~filter(.,frameId >= frameId[which(event == "line_set")[1]])) %>% 
  bind_rows() %>% 
  # Take out the time where teams are in the huddle, i.e. before first line up
  filter(frameType == "BEFORE_SNAP") %>% # Filter out for data before the snap, i.e. the time of the motion
  # Connect with player and players df to receive information
  left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
  left_join(., plays %>% dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
            by = c("playId", "gameId")) %>% 
  mutate(off_def = ifelse(club == possessionTeam, 1, 0))

# Positional adjustment
# Filter out the ball, offensive and defensive line and the QB
tracking_presnap = tracking_presnap1 %>% 
  filter(!(position %in% c("T", "G", "C", NA, "QB", "NT", "DT", "DE")))

# Offensivspieler-Daten extrahieren
off_players <- tracking_presnap %>%
  filter(off_def == 1) %>%
  group_by(time) %>%
  mutate(off_num = row_number()) %>% # Nummeriere Offensivspieler innerhalb jeder Sekunde
  ungroup() %>%
  arrange(initial_y) 

off_data = off_players %>% 
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
  arrange(initial_y) %>% 
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

#new
playId = data$playId
playerId = data$nflId

dat = list(playId = data %>% group_by(playId) %>% group_split() %>% map(~pull(.,playId)),
           playerId = data %>% group_by(playId) %>% group_split() %>% map(~pull(.,nflId)),
           y_pos = data %>% group_by(playId) %>% group_split() %>% map(~pull(.,y)),
           X = data.frame(playId = data$playId, 
                            X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + 1:n_att])) %>% 
             group_by(playId) %>% group_split() %>% map(~select(.,-c(playId))),
           n_att = n_att,
           Deltas = lapply(data %>% group_by(playId) %>% group_split(), function(x) Delta_fun(x))
)

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
  loglik = rep(NA, length(Deltas))
  for (i in 1:length(Deltas)) {
  Mu = alpha * X[[i]]
  REPORT(Mu)
  REPORT(alpha)
  
  allprobs = matrix(1, length(y_pos[[i]]), n_att)
  ind = which(!is.na(y_pos[[i]]))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos[[i]], Mu[,j], sigma)
  }
  
  loglik[i] = -forward(Deltas[[i]], Gamma, allprobs, trackID = playerId[[i]])
  
  }
  return(sum(loglik))
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

# which defender
def_ids = def_data %>% 
  pull(nflId) %>% 
  unique() 

(players %>% filter(nflId %in% def_ids))[match(def_ids, players %>% 
                                                 filter(nflId %in% def_ids) %>% pull(nflId)), ]

# which offenders
(att_ids = off_players %>% 
    pull(nflId) %>% 
    unique()) 

(players %>% filter(nflId %in% att_ids))[match(att_ids, players %>% 
                                                 filter(nflId %in% att_ids) %>% pull(nflId)), ]

def = 1
probs[[def]][1,1]
start = 1
plot(probs[[def]][start,-1], type = "h", ylim = c(0,1))
for(t in (start + 1) : nrow(probs[[def]])){
  plot(probs[[def]][t,-1], type = "h", ylim = c(0,1), main = paste("Time:", t/10))
  Sys.sleep(0.1)
}


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

