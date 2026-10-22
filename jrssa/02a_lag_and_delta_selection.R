####################################################################
############# Lag and initial distribution (delta) selection #######
####################################################################
# Run after 01 and before 02. Uses the homogeneous HMM (banded t.p.m.,
# one parameter per distance |i-j|, Gaussian state-dependent distributions)
# as in the original lag selection, on a random subset of plays.
#
# 1. Lag l (defender reaction time): AIC for l = 0, ..., 6 with alpha = 1.
# 2. Initial distribution delta = softmax(-alpha * |y_def - y_att|) at the
#    selected lag: profile likelihood over alpha, and how much the posterior
#    HMM features (as in 03) change with alpha.
#
# All lags are fitted on the same rows: the files from 01 hold the attacker
# positions lagged by HMM_LAG frames (settings.R), so the attacker position at
# frame f is in the row of frame f + HMM_LAG. Dropping the first
# max(lags) - HMM_LAG and the last HMM_LAG rows of each track leaves every lag
# in lags available.
#
# Quick test: options(hmmotion.test = TRUE) uses 20 plays of week 1.
# Number of plays otherwise: options(hmmotion.lag_n = ...), default 1000;
# NA uses all plays.

library(dplyr)
library(vroom)
library(here)
source(here("jrssa", "settings.R"))
library(LaMa)
source(here("jrssa", "hmm_posterior_features.R"))

TEST <- isTRUE(getOption("hmmotion.test", FALSE))
res_dir <- if(TEST) here("results", "test") else here("results")
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)
weeks <- if(TEST) 1 else 1:9
n_plays <- if(TEST) getOption("hmmotion.test_n", 20) else getOption("hmmotion.lag_n", 1000)

n_att <- 5
lag_in_file <- HMM_LAG  # lag used when 01 wrote the files
lags <- 0:6
alphas <- c(0.25, 0.5, 1, 2, 4, 8, 16)
att_cols <- paste0("player", 1:n_att, "_y")

# Data --------------------------------------------------------------------

data <- bind_rows(lapply(weeks, \(w){
  vroom(hmm_file(w),
        col_select = all_of(c("uniId", "game_play_id", "frameId", "y", att_cols)),
        col_types = cols(uniId = "c", game_play_id = "c", .default = "d"),
        show_col_types = FALSE)
}))

set.seed(1)
plays <- unique(data$game_play_id)
if(!is.na(n_plays) && n_plays < length(plays)) plays <- sample(plays, n_plays)
data <- data |>
  filter(game_play_id %in% plays) |>
  mutate(track = match(uniId, unique(uniId))) |>
  arrange(track, frameId) |>
  group_by(track) |>
  mutate(pos = row_number(), m = n()) |>
  ungroup()

# frames must be consecutive within a track for the shift below
stopifnot(all(diff(data$frameId)[diff(data$track) == 0] == 1))

A <- as.matrix(data[, att_cols])  # attacker y, lagged by lag_in_file
first_pos <- max(1, max(lags) - lag_in_file + 1)
eval_rows <- which(data$pos >= first_pos & data$pos <= data$m - lag_in_file)

y <- data$y[eval_rows]
ID <- data$uniId[eval_rows]
first_rows <- !duplicated(ID)
cat("plays:", length(plays), "| tracks:", sum(first_rows), "| observations:", length(y), "\n")

# attacker y at lag l for the evaluation rows
X_lag <- function(l) A[eval_rows - l + lag_in_file, , drop = FALSE]

delta_softmax <- function(X, alpha){
  D <- abs(X[first_rows, , drop = FALSE] - y[first_rows])
  S <- exp(-alpha * D)
  S / rowSums(S)
}

# Homogeneous HMM ---------------------------------------------------------

# one transition parameter per distance |i - j| between attackers
band <- abs(as.vector(outer(1:n_att, 1:n_att, "-")))
map <- list(eta = factor(band[band != 0]))

fit_homogeneous <- function(X, Delta){
  dat <- list(y = y, X = X, ID = ID, Delta = Delta, n_att = n_att)
  par <- list(eta = rep(-4, n_att * (n_att - 1)), logsigma = log(1))
  nll <- function(par){
    getAll(par, dat)
    Gamma <- tpm(eta)
    sigma <- exp(logsigma)
    allprobs <- matrix(1, length(y), n_att)
    for(j in 1:n_att) allprobs[, j] <- dnorm(y, X[, j], sigma)
    -forward(Delta, Gamma, allprobs, trackID = ID)
  }
  obj <- MakeADFun(nll, par, map = map, silent = TRUE)
  opt <- nlminb(obj$par, obj$fn, obj$gr)
  list(mod = obj$report(), nll = opt$objective, k = length(opt$par),
       convergence = opt$convergence)
}

# 1. Lag ------------------------------------------------------------------

lag_tab <- do.call(rbind, lapply(lags, \(l){
  X <- X_lag(l)
  f <- fit_homogeneous(X, delta_softmax(X, alpha = 1))
  cat("lag", l, "nll", round(f$nll, 1), "\n")
  data.frame(lag = l, nll = f$nll, k = f$k, AIC = 2 * f$k + 2 * f$nll,
             convergence = f$convergence)
}))
lag_tab$dAIC <- lag_tab$AIC - min(lag_tab$AIC)
lag_sel <- lag_tab$lag[which.min(lag_tab$AIC)]
print(lag_tab)

# 2. Initial distribution -------------------------------------------------

X <- X_lag(lag_sel)
track_rows <- split(seq_along(y), factor(ID, levels = unique(ID)))
play_of_track <- data$game_play_id[eval_rows][first_rows]

alpha_fits <- lapply(alphas, \(a){
  f <- fit_homogeneous(X, delta_softmax(X, alpha = a))
  cat("alpha", a, "nll", round(f$nll, 1), "\n")
  # posterior features as in 03, aggregated per play
  post <- lapply(seq_along(track_rows), \(k){
    idx <- track_rows[[k]]
    hmm_track_posterior(f$mod$delta[k, ], f$mod$Gamma, f$mod$allprobs[idx, , drop = FALSE])
  })
  feats <- data.frame(play = play_of_track,
                      exp_switches = sapply(post, `[[`, "exp_switches"),
                      p_switch = sapply(post, `[[`, "p_switch"),
                      entropy = sapply(post, `[[`, "entropy")) |>
    group_by(play) |>
    summarise(sum = sum(exp_switches),
              nr_player_changes = sum(p_switch),
              average_ent = mean(entropy))
  list(nll = f$nll, feats = feats, mod = f$mod)
})
names(alpha_fits) <- alphas

ref <- alpha_fits[[as.character(1)]]$feats
alpha_tab <- do.call(rbind, lapply(seq_along(alphas), \(i){
  fe <- alpha_fits[[i]]$feats
  data.frame(alpha = alphas[i],
             nll = alpha_fits[[i]]$nll,
             mean_sum = mean(fe$sum),
             mean_nr_player_changes = mean(fe$nr_player_changes),
             mean_average_ent = mean(fe$average_ent),
             cor_sum = cor(fe$sum, ref$sum),
             cor_nr_player_changes = cor(fe$nr_player_changes, ref$nr_player_changes),
             cor_average_ent = cor(fe$average_ent, ref$average_ent))
}))
alpha_tab$dnll <- alpha_tab$nll - min(alpha_tab$nll)
print(alpha_tab)

saveRDS(list(lag_tab = lag_tab, lag_sel = lag_sel, alpha_tab = alpha_tab,
             plays = plays, n_obs = length(y)),
        file.path(res_dir, "lag_delta_selection.rds"))
