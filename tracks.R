
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

setwd("C:/Users/michels/sciebo/BDB 2025")
tracking_data = read.csv("tracking_week_1.csv")
players = read.csv("players.csv")
plays = read.csv("plays.csv")

# Spiel 2022091110, playId 291 als Beispiel benutzen
# Marco Wilson geht mit Mecole Hardman die ganze Zeit in Man Coverage mit

tracking = tracking_data #%>% 
#filter(gameId %in% unique(tracking_data$gameId)[1:2]) 

tracking = tracking %>% filter(playId %in% unique(tracking$playId)[1:2])
#tracking = tracking_data %>% filter(gameId == 2022091110) #%>% filter(playId == 291) 

tracking_data$uniId2 = paste0(tracking_data$gameId, tracking_data$playId)

unid2_motion = tracking_data %>% filter(event == "man_in_motion") %>% pull(uniId2)

tracking = tracking_data %>% filter(uniId2 %in% unid2_motion)
#write.csv(tracking, file = "tracking_data_week_1_game_kcaz.csv")

#rm(tracking_data)

# Ideen: 

# Es gibt auch das Event man_in_motion, nachdem wir starten können

tracking_presnap1 = tracking %>% 
  mutate(uniId = paste0(gameId, playId, nflId)) %>% 
  mutate(uniId2 = paste0(gameId, playId)) %>% 
  mutate(y = y - 53.3/2) %>% # center y-coordinate
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
  left_join(., plays %>% filter(offenseFormation != "JUMBO") %>% 
              dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
            by = c("playId", "gameId")) %>% 
  mutate(off_def = ifelse(club == possessionTeam, 1, 0))

#tracking_presnap1 = tracking_presnap1 %>% filter(uniId == "2022091200185147904")


# 1. Zähle die Häufigkeiten von T und G pro PlayId
position_counts <- tracking_presnap1 %>%
  group_by(uniId2, position) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(position %in% c("T", "G", "QB", "C")) %>%
  pivot_wider(names_from = position, values_from = Count, values_fill = 0)

# 2. Filtere PlayIds mit gleichen Häufigkeiten von T und G
valid_play_ids <- position_counts %>%
  filter(T == G) %>%
  filter(QB == C) %>% 
  pull(uniId2)

valid_play_ids2 = tracking_presnap1 %>% group_by(uniId2) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  mutate(invalid = ifelse(Count < 24, 1, 0)) %>% 
  filter(invalid != 1) %>% pull(uniId2)

# 3. Behalte nur die relevanten PlayIds im ursprünglichen Datensatz
tracking_presnap1 <- tracking_presnap1 %>%
  filter(uniId2 %in% valid_play_ids) %>% 
  filter(uniId2 %in% valid_play_ids2)

# Positional adjustment
# Filter out the ball, offensive and defensive line and the QB
tracking_presnap = tracking_presnap1 %>% 
  filter(!(position %in% c("T", "G", "C", NA, "QB", "NT", "DT", "DE")))

# Offensivspieler-Daten extrahieren
off_players <- tracking_presnap %>%
  filter(off_def == 1) %>%
  group_by(gameId, playId) %>%
  mutate(player_order = rank(initial_y, ties.method = "first")) %>%
  arrange(gameId, playId, player_order) %>%
  ungroup() %>% 
  select(-player_order) %>% 
  group_by(gameId, playId, time) %>%
  mutate(off_num = row_number()) %>% # Nummeriere Offensivspieler innerhalb jeder Sekunde
  # arrange(initial_y) %>% 
  ungroup()

off_data = off_players %>% 
  select(gameId, playId, time, off_num, y) %>% #, x) %>%
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
  group_by(gameId, playId) %>%
  mutate(player_order = rank(initial_y, ties.method = "first")) %>%
  arrange(gameId, playId, player_order) %>%
  select(-player_order)  %>% 
  filter(off_def == 0)

# Daten zusammenführen
data <- def_data %>%
  left_join(off_data, by = c("time", "playId", "gameId"))

# Take out off-coverage players (most likely safeties that do now follow players at the LOS)
# wenn IMMER hinter 10
#data = data %>% filter(abs(x - absoluteYardlineNumber) < 10)

# Fit model ---------------------------------------------------------------

## deterministic initial distribution
n_att = 5

Delta_fun = function(data){
  t(
    sapply((split(data, data$nflId))[as.character(unique(data$nflId))],
           function(x){
             delta = numeric(n_att)
             delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))] = 1
             delta
           })
  )
}

# #new
# playId = data$playId
# playerId = data$nflId
# 
# dat = list(playId = data %>% group_by(playId) %>% group_split() %>% map(~pull(.,playId)),
#            playerId = data %>% group_by(playId) %>% group_split() %>% map(~pull(.,nflId)),
#            y_pos = data %>% group_by(playId) %>% group_split() %>% map(~pull(.,y)),
#            X = data.frame(playId = data$playId, 
#                           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + 1:n_att])) %>% 
#              group_by(playId) %>% group_split() %>% map(~select(.,-c(playId))) %>% lapply(as.matrix),
#            n_att = n_att,
#            Deltas = lapply(data %>% group_by(playId) %>% group_split(), function(x) Delta_fun(x))
# )
Deltas =   t(
  sapply((split(data, data$uniId))[as.character(unique(data$uniId))],
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))] = 1
           delta
         })
)

## deterministic initial distribution
# n_att = 5
# 
# Delta = t(
#   sapply(split(data, data$nflId),
#          function(x){
#            delta = numeric(n_att)
#            delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))] = 1
#            delta
#          })
# )
# 
#rnames_old = rownames(Delta)
# only defenders that are close to offensive players
Delta2 = t(
  sapply(split(data, data$uniId)[as.character(unique(data$uniId))],
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))] = min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + 1:n_att]))
           delta
         })
)# # only defenders that are close to offensive players
rnames = rownames(Delta2)
Delta2 = as.data.frame(Delta2)
Delta2$uniId2 = str_sub(rownames(Delta2), 1, str_length(rownames(Delta2))-5)
Delta2$rnames = rnames
# 
# # Funktion zum Anwenden der Regel pro Zeile
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

Delta3 <- # %>%
  split(Delta2, Delta2$uniId2)[as.character(unique(Delta2$uniId2))] #%>%


Delta3 = Delta3 %>%   map(~ {
  # Wende die Funktion auf die Spalten 1 bis 5 an
  transformed <- apply(.[1:5], 2, process_row)
  
  # Kombiniere die transformierten Spalten mit den unveränderten Spalten 6 und 7
  cbind(as.data.frame(transformed), .[, 6:7])
})


Delta4 <- lapply(Delta3, function(df) {
  df[rowSums(df[,1:5]) > 0, ]
}) %>% bind_rows() %>% select(-uniId2)

rnames_new = (Delta4$rnames)

Delta4 = Delta4 %>% select(-rnames)
Delta4[Delta4 > 0] = 1

data = data %>% filter(uniId %in% rnames_new)


dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + 1:n_att]),
           ID = data$uniId,
           n_att = n_att, 
           Delta = as.matrix(Delta4))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1),
           logitalpha = qlogis(0.95))

nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma)
  alpha = plogis(logitalpha)
  
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
#probs = apply(probs, 2, as.numeric)
#probs = as.matrix(data.frame(ID = probs[,1], apply(probs[, 2:6], 2, as.numeric)))

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

def = 999
def = "20220911136547877"
start = 1
plot(as.numeric(probs[[def]][start,-1]), type = "h", ylim = c(0,1))
for(t in (start + 1) : nrow(probs[[def]])){
  plot(as.numeric(probs[[def]][t,-1]), type = "h", ylim = c(0,1), main = paste("Time:", t/10))
  Sys.sleep(0.1)
}


analyze_data <- function(df) {
  # Spalten 2 bis 6 in numerische Werte umwandeln
  numeric_values <- apply(df[, 2:6], 2, function(col) as.numeric(as.character(col)))
  
  # Fehlende Werte (NA) behandeln, falls Umwandlung fehlschlägt
  if (anyNA(numeric_values)) {
    warning("Es gibt NA-Werte nach der Umwandlung. Bitte prüfen!")
  }
  
  # Den größten Wert pro Zeile bestimmen
  max_values <- apply(numeric_values, 1, which.max)
  
  # Anzahl der Änderungen des größten Wertes berechnen
  changes <- sum(c(NA, diff(max_values)) != 0, na.rm = TRUE)
  
  # Ergebnis als Liste zurückgeben
  #list(max_values = max_values, num_changes = changes)
  
  data.frame(#max_columns = paste(max_column_names, collapse = ", "),
             num_changes = changes)
}

#results <- lapply(probs, analyze_data)
results_df <- do.call(rbind, lapply(probs, analyze_data))
#results_df$uniId2 = str_sub(rownames(results_df), 1, str_length(rownames(results_df))-5)
results_df$gameId = str_sub(rownames(results_df), 1, 10)#str_length(rownames(results_df))-5)
results_df$playId = str_sub(rownames(results_df), 11, str_length(rownames(results_df))-5)

res_df = results_df %>% group_by(gameId, playId) %>% summarize(average = mean(num_changes),
                                                               sum = sum(num_changes))
res_df = merge(plays, res_df, by = c("gameId", "playId"))

# Laden der ggplot2-Bibliothek
library(ggplot2)

# Häufigkeit der Kategorien nach Gruppe visualisieren
ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = average, fill = pff_manZone)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = pff_manZone)) + # prop.. berechnet die relative Häufigkeit
  labs(title = "Relative Häufigkeit der Kategorien nach Gruppe", 
       x = "Kategorie", 
       y = "Relative Häufigkeit") +
  scale_y_continuous(labels = scales::percent) + # Prozentformat auf der y-Achse
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = sum, fill = pff_manZone)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = pff_manZone)) + # prop.. berechnet die relative Häufigkeit
  labs(title = "Relative Häufigkeit der Kategorien nach Gruppe", 
       x = "Kategorie", 
       y = "Relative Häufigkeit") +
  scale_y_continuous(labels = scales::percent) + # Prozentformat auf der y-Achse
  theme_minimal()


plays %>% 
  filter(gameId == tracking$gameId[2]) %>% 
  mutate(gameClockNum = as.numeric(str_sub(gameClock, 1, 2)) * 60 + as.numeric(str_sub(gameClock, 4, 5))) %>% 
  arrange(desc(gameClockNum)) %>% 
  filter(pff_manZone == "Man") %>%
  View()
select(playId)

save(probs, file = "probs.RData")

# Some Ideas

# only defenders in the vicinity of the los (safeties out, done)
# only defenders that are not in the vicinity of the ball (i.e. ILB as DE raus)

