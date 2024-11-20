
#### BDB 2025 - Coverage model ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)


## data looks like

n_def = 11
n_att = 6
n_obs = 100

X = matrix(NA, n_def * n_obs, n_att)
for(i in 1:n_att) X[,i] = rep(cumsum(rnorm(n_obs)), n_def)
colnames(X) = paste0("x_", 1:n_att)

y_pos = as.vector(sapply(1:n_def, function(i) 2*i + cumsum(rnorm(n_obs))))

data = data.frame(ID = rep(1:n_def, each = n_obs),
                  y_pos = y_pos)
data = cbind(data, X)


## likelihood function

# devtools::install_github("janoleko/LaMa")
library(LaMa)
           
nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma)
  alpha = plogis(logitalpha)
  
  # assuming y variable is centered -> pulling to the middle
  # alpha * X + (1-alpha) * y_middle
  #Mu = alpha * X
  Mu = X
  REPORT(Mu)
  
  allprobs = matrix(1, length(y_pos), n_att)
  ind = which(!is.na(y_pos))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos, Mu[,j], sigma)
  }
  
  -forward(Delta, Gamma, allprobs, trackID = ID)
}

## deterministic initial distribution
Delta = t(
  sapply(split(data, data$ID),
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y_pos[1] - x[1,-(1:2)]))] = 1
           delta
         })
)
  
## fit model
dat = list(y_obs = data$y_obs,
           X = data[,-(1:2)],
           ID = data$ID,
           n_att = n_att, 
           Delta = as.matrix(Delta))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1),
           logitalpha = qlogis(0.95))

obj = MakeADFun(nll, par)
opt = nlminb(obj$par, obj$fn, obj$gr)

mod = obj$report()
# Delta = mod$Delta
# Gamma = mod$Gamma
# allprobs = mod$allprobs
# trackID = mod$trackID

probs = stateprobs(mod = mod)
plot(probs[1,], type = "h")
for(t in 2:n_obs){
  plot(probs[t,], type = "h")
  Sys.sleep(0.1)
}



# With real data ----------------------------------------------------------

setwd("~/Sciebo/BDB 2025")
tracking_data = read.csv("tracking_week_1.csv")
players = read.csv("players.csv")
plays = read.csv("plays.csv")

tracking = tracking_data %>% 
  filter(gameId == tracking_data$gameId[1])

tracking_presnap = tracking %>% 
  filter(frameType == "BEFORE_SNAP") %>% 
  left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
  left_join(., plays %>% dplyr::select(playId, gameId, possessionTeam, pff_manZone), 
            by = c("playId", "gameId")) %>% 
  mutate(off_def = ifelse(club == possessionTeam, 1, 0)) %>% 
  filter(!(position %in% c("T", "G", "C", NA, "QB", "NT", "DT", "DE"))) %>% 
  mutate(y = y - 53.3/2) %>% # center y-coordinate
  filter(playId == 85)

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

# Defensivspieler-Daten extrahieren
def_data <- tracking_presnap %>%
  filter(off_def == 0)

# Daten zusammenführen
data <- def_data %>%
  left_join(off_data, by = "time")


# Fit model ---------------------------------------------------------------

## deterministic initial distribution
n_att = 5

Delta = t(
  sapply(split(data, data$nflId),
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y[1] - x[1, 22 + 1:n_att]))] = 1
           delta
         })
)

dat = list(y_pos = data$y,
           X = as.matrix(data[, 22 + 1:n_att]),
           ID = data$nflId,
           n_att = n_att, 
           Delta = as.matrix(Delta))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1),
           logitalpha = qlogis(0.95))

nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma); REPORT(sigma)
  alpha = plogis(logitalpha); REPORT(alpha)
  
  # assuming y variable is centered -> pulling to the middle
  # alpha * X + (1-alpha) * y_middle
  Mu = alpha * X
  REPORT(Mu)
  
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
Delta = mod$delta
Gamma = mod$Gamma
allprobs = mod$allprobs
trackID = mod$trackID
(alpha = mod$alpha)
(sigma = mod$sigma)

probs = stateprobs(mod = mod)
colnames(probs) = paste0("attacker_", 1:n_att)
probs = cbind(ID = trackID, probs)

probs = split(as.data.frame(probs), probs[,1])
probs = lapply(probs, as.matrix)

def = 1
probs[[def]][1,1]
start = 1
plot(probs[[def]][start,-1], type = "h", ylim = c(0,1))
for(t in (start + 1) : nrow(probs[[def]])){
  plot(probs[[def]][t,-1], type = "h", ylim = c(0,1), main = paste("Time:", t/10))
  Sys.sleep(0.1)
}

# which defender
def_ids = tracking_presnap %>% 
  filter(off_def == 0) %>% 
  pull(nflId) %>% 
  unique() 

players %>% filter(nflId %in% def_ids)

# which offenders
att_ids = tracking_presnap %>% 
  filter(off_def == 1) %>% 
  pull(nflId) %>% 
  unique() 

players %>% filter(nflId %in% att_ids)

plays %>% 
  filter(gameId == tracking_data$gameId[1]) %>% 
  filter(quarter == 1) %>% 
  mutate(gameClockNum = as.numeric(str_sub(gameClock, 1, 2)) * 60 + as.numeric(str_sub(gameClock, 4, 5))) %>% 
  arrange(desc(gameClockNum)) %>% 
  select(playId)
  
