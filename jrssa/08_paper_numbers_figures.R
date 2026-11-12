######################################################################################
#### Numbers, tables and figures for the JRSS-A manuscript (paper_jrssa/main.tex)
#### Run after 02-06 (and optionally 07). Reads only saved results, apart from
#### week 1 of the HMM input for the example play. Run time: a few minutes
#### (07 adds a few more if run_07 = TRUE).
####
#### Output
####   results/paper/paper_numbers.txt   all numbers (also printed)
####   results/paper/*.pdf               figures, also copied to
####   paper_jrssa/figures/plots/        under the names used in main.tex:
####     gcm_test_all_plus_omnibus.pdf, gcm_teststat.pdf  (GCM, from 06)
####     team_diffs.pdf                                   (team differences, from 05's teams_zvm.rds)
####     re_def.pdf                                       (team random effects)
####     snapshot.pdf                                     (example play)
####     paired_diffs.pdf                                 (paired logloss differences)
####     all_losses_cross_fitting.pdf                     (boxplots of the loss metrics, as in AoAS)
####
#### Sections (each can be switched off below)
####   1  data: number of plays, man / zone
####   2  HMM: fixed effects with SE and CI, role effects, team effects (figure)
####   3  HMM features: summary table
####   4  example play (Marco Wilson): t.p.m. at the crossing, snapshot figure
####   5  coverage models: cross-fitting mean / sd, paired differences, LOWO
####   6  GCM figures
####   7  team table with confidence intervals, team figure
####   8  lag / alpha selection (02a)
####   9  posterior vs argmax features (07, optional)
######################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
source(here("jrssa", "settings.R"))

run_07 <- FALSE  # TRUE: also run 07_compare_feature_versions.R (few minutes)

res_dir <- here("results")
out_dir <- here("results", "paper")
fig_dir <- here("paper_jrssa", "figures", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# write figures to results/paper and copy them to the paper folder
save_fig <- function(plot, name, width, height){
  f <- file.path(out_dir, name)
  ggsave(f, plot, width = width, height = height)
  file.copy(f, file.path(fig_dir, name), overwrite = TRUE)
  cat("figure written:", name, "\n")
}

num_file <- file.path(out_dir, "paper_numbers.txt")
while(sink.number() > 0) sink()  # close sinks left open by an aborted earlier run
sink(num_file, split = TRUE)
cat("Numbers for paper_jrssa/main.tex, created", format(Sys.time()), "\n")
section <- function(s) cat("\n\n==================== ", s, " ====================\n", sep = "")

full_data <- readRDS(file.path(res_dir, "preprocessed_HMM_data_full.rds"))

# 1 Data --------------------------------------------------------------------
section("1 Data")
cat("plays in preprocessed_HMM_data_full.rds:", nrow(full_data), "\n")
print(table(full_data$pff_manZone, useNA = "ifany"))
cat("without 'Other' (used in the coverage models):",
    sum(full_data$pff_manZone %in% c("Man", "Zone")), "\n")
print(round(prop.table(table(full_data$pff_manZone[full_data$pff_manZone != "Other"])), 3))

# 2 HMM parameters ------------------------------------------------------------
section("2 HMM parameters")
mod <- readRDS(file.path(res_dir, "mod_final_full.rds"))
sdr <- mod$sdr

# estimates and SEs taken directly from the sdreport object (as summary.sdreport
# does), so the TMB summary method need not be loaded
fixed <- data.frame(est = sdr$par.fixed, se = sqrt(diag(sdr$cov.fixed)))
fixed$par <- make.unique(names(sdr$par.fixed))
print(fixed, digits = 4)

# standard deviations: estimate, delta-method SE and CI on the log scale
fixed_tab <- fixed |>
  mutate(name = sub("^log", "", par),
         is_log = grepl("^log", par),
         estimate = ifelse(is_log, exp(est), est),
         se_nat = ifelse(is_log, exp(est) * se, se),
         lo = ifelse(is_log, exp(est - 1.96 * se), est - 1.96 * se),
         hi = ifelse(is_log, exp(est + 1.96 * se), est + 1.96 * se)) |>
  select(name, estimate, se = se_nat, lo, hi)
cat("\nTable tab:fixed (natural scale, CI for SDs from the log scale):\n")
print(fixed_tab, digits = 4, row.names = FALSE)
cat("odds ratio per yard, exp(beta_dist):",
    round(exp(fixed$est[fixed$par == "beta_dist"]), 3), "\n")

# random effects with prediction SEs (in parameter order)
rand <- data.frame(est = sdr$par.random, se = sqrt(sdr$diag.cov.random),
                   block = names(sdr$par.random))
re_block <- function(b){
  r <- rand[rand$block == b, ]
  stopifnot(nrow(r) == length(mod[[b]]), max(abs(r$est - mod[[b]])) < 1e-4)
  r$level <- if(is.null(names(mod[[b]]))) paste0(b, seq_len(nrow(r))) else names(mod[[b]])
  r
}
re_pos <- re_block("beta_pos")
re_club <- re_block("beta_club")
cat("\nTable tab:position_effects (prediction SE):\n")
print(re_pos |> arrange(est) |> select(level, est, se), digits = 3, row.names = FALSE)
cat("\nTeam effects (sorted):\n")
print(re_club |> arrange(est) |> select(level, est, se), digits = 3, row.names = FALSE)

p_re_def <- re_club |>
  mutate(level = reorder(level, est)) |>
  ggplot(aes(x = est, y = level)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
  geom_linerange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), colour = "gray40") +
  geom_point(size = 2) +
  labs(x = "Predicted random effect (95% prediction interval)", y = NULL) +
  theme_classic(base_size = 12)
save_fig(p_re_def, "re_def.pdf", width = 6, height = 7)

# 3 HMM features ------------------------------------------------------------------
section("3 HMM features")
feat_vars <- c(sum = "Expected switches", nr_player_changes = "Expected # switching defenders",
               average_ent = "Mean entropy", beta = "RE/play")
feat_summary <- function(d){
  s <- sapply(names(feat_vars), \(v) summary(d[[v]])[1:6])
  colnames(s) <- feat_vars
  round(s, 2)
}
cat("Table tab:state_decod, all plays:\n")
print(feat_summary(full_data))
cat("\nwithout 'Other':\n")
print(feat_summary(full_data |> filter(pff_manZone != "Other")))
cat("\nby coverage (means):\n")
print(full_data |> group_by(pff_manZone) |>
        summarise(n = n(), across(all_of(names(feat_vars)), mean)), digits = 3)

# 4 Example play: Marco Wilson ------------------------------------------------
section("4 Example play")
ex_game <- "2022091110"; ex_play <- "291"; ex_def <- "Marco Wilson"
ex_gp <- paste0(ex_game, ex_play)

source(here("jrssa", "hmm_posterior_features.R"))
library(vroom)
# 02 binds the weeks in order 1..9, so the rows of week 1 are the first rows of
# the model (Gamma, allprobs)
w1 <- vroom(hmm_file(1), col_types = cols(uniId = "c", game_play_id = "c", gameId = "c",
                                          playId = "c", event = "c", .default = col_guess()),
            show_col_types = FALSE)
stopifnot(nrow(w1) <= dim(mod$Gamma)[3])
w1$row <- seq_len(nrow(w1))
ex <- w1 |> filter(game_play_id == ex_gp)
stopifnot(nrow(ex) > 0, ex_def %in% ex$displayName)

att_y <- paste0("player", 1:5, "_y"); att_x <- paste0("player", 1:5, "_x")
ex_tracks <- split(ex, factor(ex$uniId, levels = unique(ex$uniId)))
delta_rows <- if(is.null(rownames(mod$delta))) NULL else rownames(mod$delta)
probs_ex <- bind_rows(lapply(ex_tracks, \(tr){
  k <- if(is.null(delta_rows)) match(tr$uniId[1], unique(w1$uniId)) else match(tr$uniId[1], delta_rows)
  stopifnot(!is.na(k))
  pp <- hmm_track_posterior(mod$delta[k, ], mod$Gamma[, , tr$row, drop = FALSE],
                            mod$allprobs[tr$row, , drop = FALSE])
  # sanity check: allprobs must belong to this track (same defender y)
  stopifnot(max(abs(dnorm(tr$y, as.matrix(tr[, att_y]), mod$sigma) - mod$allprobs[tr$row, ])) < 1e-8)
  data.frame(uniId = tr$uniId[1], displayName = tr$displayName[1], frameId = tr$frameId,
             row = tr$row, setNames(as.data.frame(pp$probs), paste0("p", 1:5)))
}))

# names of the offensive players (player1..5 are ordered by initial y in 01):
# matched to the raw tracking data of week 1 if available
off_lab <- paste0("Off", 1:5)
raw_file <- here("rawdata", "tracking_week_1.csv")
if(file.exists(raw_file)){
  raw <- vroom(raw_file, col_types = cols(gameId = "c", playId = "c", .default = col_guess()),
               show_col_types = FALSE) |>
    filter(gameId == ex_game, playId == ex_play)
  players <- read.csv(here("rawdata", "players.csv"))
  # transformation raw -> preprocessed coordinates from the defenders
  dd <- ex |> select(nflId, frameId, x, y) |>
    inner_join(raw |> select(nflId, frameId, rx = x, ry = y), by = c("nflId", "frameId"))
  fx <- lm(x ~ rx, dd); fy <- lm(y ~ ry, dd)
  cat("raw -> preprocessed: x slope", round(coef(fx)[2], 3), "y slope", round(coef(fy)[2], 3),
      "| max residual", round(max(abs(c(resid(fx), resid(fy)))), 3), "\n")
  off_cand <- raw |>
    filter(club == ex$possessionTeam[1]) |>
    left_join(players |> select(nflId, position), by = "nflId") |>
    filter(!position %in% c("C", "G", "T", "QB")) |>
    mutate(tx = predict(fx, data.frame(rx = x)), ty = predict(fy, data.frame(ry = y)))
  # current (unlagged) x of the attackers per frame, as in the preprocessed file
  off_pre <- ex |> distinct(frameId, .keep_all = TRUE) |> select(frameId, all_of(att_x))
  cost <- sapply(1:5, \(j){
    sapply(split(off_cand, off_cand$displayName), \(o){
      m <- inner_join(o, off_pre, by = "frameId")
      mean(abs(m$tx - m[[att_x[j]]]))
    })
  })
  match_j <- apply(cost, 2, which.min)
  stopifnot(!anyDuplicated(match_j))
  off_names <- rownames(cost)[match_j]
  off_lab <- sapply(strsplit(off_names, " "), \(s) paste(substr(s, 1, 1), collapse = ""))
  cat("offensive players:", paste0(att_y, " = ", off_names, collapse = ", "), "\n")
} else cat("rawdata/tracking_week_1.csv not found: offensive players labelled Off1-Off5\n")

wil <- probs_ex |> filter(displayName == ex_def)
wil_rows <- ex |> filter(displayName == ex_def)
guarded <- which.max(colMeans(as.matrix(wil[, paste0("p", 1:5)])))
Y <- as.matrix(wil_rows[, att_y])
d_other <- abs(Y[, -guarded, drop = FALSE] - Y[, guarded])
crossing <- which(d_other == min(d_other), arr.ind = TRUE)[1, ]
other <- setdiff(1:5, guarded)[crossing[2]]
t_cross <- crossing[1]
cat(ex_def, "guards", off_lab[guarded], "; closest crossing with", off_lab[other],
    "at frame", wil_rows$frameId[t_cross], "(lagged lateral distance",
    round(min(d_other), 2), "yd)\n")
G <- mod$Gamma[, , wil_rows$row[t_cross]]
dimnames(G) <- list(off_lab, off_lab)
cat("t.p.m. at that frame:\n"); print(round(G, 3))
cat("LaTeX rows:\n")
for(i in 1:5) cat("      ", paste(sprintf("%.3f", G[i, ]), collapse = " & "), "\\\\\n")
cat("posterior P(guarding", off_lab[guarded], ") at that frame:",
    round(wil[[paste0("p", guarded)]][t_cross], 3), "\n")

# snapshot: 6 frames from line set to snap, incl. the crossing; lines between
# each defender and each offensive player with transparency = posterior probability
frames <- sort(unique(ex$frameId))
frames_off <- head(frames, -HMM_LAG)  # current attacker positions known (see below)
snap_frames <- sort(unique(c(quantile(frames_off, seq(0, 1, length.out = 6), type = 1),
                             wil_rows$frameId[t_cross])))
if(length(snap_frames) > 6) snap_frames <- snap_frames[-which.min(abs(snap_frames - median(snap_frames)))]
# current attacker y: the file holds y lagged by HMM_LAG, so y(t) is in row t + HMM_LAG
off_cur <- ex |>
  distinct(frameId, .keep_all = TRUE) |>
  arrange(frameId) |>
  mutate(across(all_of(att_y), \(v) lead(v, HMM_LAG))) |>
  select(frameId, all_of(att_x), all_of(att_y)) |>
  pivot_longer(-frameId, names_to = c("j", ".value"), names_pattern = "player(\\d)_(.)") |>
  mutate(j = as.integer(j), lab = off_lab[j])
def_cur <- ex |> select(frameId, uniId, displayName, x, y)
links <- probs_ex |>
  select(uniId, frameId, starts_with("p")) |>
  pivot_longer(starts_with("p"), names_to = "j", values_to = "prob") |>
  mutate(j = as.integer(sub("p", "", j))) |>
  left_join(def_cur, by = c("uniId", "frameId")) |>
  left_join(off_cur |> select(frameId, j, ox = x, oy = y), by = c("frameId", "j")) |>
  filter(frameId %in% snap_frames, prob > 0.05, !is.na(oy))
panel <- \(f) factor(paste0("t = ", (f - min(frames)) / 10, " s"),
                     levels = paste0("t = ", (snap_frames - min(frames)) / 10, " s"))
p_snap <- ggplot() +
  geom_segment(data = links |> mutate(panel = panel(frameId)),
               aes(x = x, y = y, xend = ox, yend = oy, alpha = prob,
                   colour = displayName == ex_def), linewidth = 0.8) +
  geom_point(data = def_cur |> filter(frameId %in% snap_frames) |> mutate(panel = panel(frameId)),
             aes(x, y), colour = "#97233F", size = 2.5) +
  geom_point(data = off_cur |> filter(frameId %in% snap_frames, !is.na(y)) |>
               mutate(panel = panel(frameId)),
             aes(x, y), colour = "#E31837", fill = "#FFB81C", shape = 21, size = 2.5) +
  geom_text(data = off_cur |> filter(frameId %in% snap_frames, !is.na(y)) |>
              mutate(panel = panel(frameId)),
            aes(x, y, label = lab), size = 2.5, nudge_x = -1.5) +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "gray50"), guide = "none") +
  scale_alpha(range = c(0.1, 1), limits = c(0, 1), name = "Posterior probability") +
  facet_wrap(~panel, ncol = 3) +
  coord_fixed() +
  labs(x = "x (yards)", y = "y (yards)") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
save_fig(p_snap, "snapshot.pdf", width = 9, height = 6)
cat("Note: the last", HMM_LAG, "frames before the snap are not shown, because the",
    "file holds the attacker positions lagged by", HMM_LAG, "frames.\n")

# 5 Coverage models -------------------------------------------------------------
section("5 Coverage models")
cf <- readRDS(file.path(res_dir, "zvm_crossfit_runs.rds"))
fs_lab <- c(xy_npo_ex_hmm = "pre", xy_f_ex_hmm = "naive", xy_f = "HMM")
cf_long <- bind_rows(lapply(seq_along(cf), \(r){
  bind_rows(lapply(c("accuracy", "AUC", "logloss"), \(m){
    cf[[r]][[m]] |>
      pivot_longer(-mod, names_to = "fs", values_to = "value") |>
      mutate(metric = m, rep = r)
  }))
})) |> mutate(fs = fs_lab[fs])
cat("Table tab:perf, mean (sd) over", length(cf), "repetitions:\n")
cf_long |>
  group_by(mod, fs, metric) |>
  summarise(txt = sprintf("%.3f (%.3f)", mean(value), sd(value)), .groups = "drop") |>
  pivot_wider(names_from = metric, values_from = txt) |>
  mutate(fs = factor(fs, levels = c("pre", "naive", "HMM"))) |>
  arrange(mod, fs) |>
  as.data.frame() |> print(row.names = FALSE)

paired <- cf_long |>
  pivot_wider(names_from = fs, values_from = value) |>
  mutate(`naive - pre` = naive - pre, `HMM - naive` = HMM - naive, `HMM - pre` = HMM - pre) |>
  pivot_longer(c(`naive - pre`, `HMM - naive`, `HMM - pre`), names_to = "comparison", values_to = "diff")
cat("\nTable tab:paired: mean [2.5%, 97.5% quantile], min, max, # repetitions improved:\n")
paired |>
  group_by(mod, metric, comparison) |>
  summarise(mean = mean(diff), q025 = quantile(diff, 0.025), q975 = quantile(diff, 0.975),
            min = min(diff), max = max(diff),
            improved = sum(if(first(metric) == "logloss") diff < 0 else diff > 0),
            .groups = "drop") |>
  mutate(across(mean:max, \(v) round(v, 4))) |>
  as.data.frame() |> print(row.names = FALSE)

# figure: paired logloss differences per repetition (own y-axis per panel, as the
# naive - pre effect is an order of magnitude larger than HMM - naive)
p_paired <- paired |>
  filter(metric == "logloss", comparison %in% c("naive - pre", "HMM - naive")) |>
  mutate(comparison = factor(comparison, levels = c("naive - pre", "HMM - naive"),
                             labels = c("Naive post-motion vs. pre-motion features",
                                        "HMM features vs. naive post-motion features")),
         mod = factor(mod, levels = c("glmnet", "xgboost"), labels = c("Elastic net", "XGBoost"))) |>
  ggplot(aes(x = mod, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkred") +
  geom_boxplot(width = 0.5, outlier.shape = NA, colour = "gray40") +
  geom_point(position = position_jitter(width = 0.08, height = 0, seed = 1), size = 1.8) +
  facet_wrap(~comparison, scales = "free_y") +
  labs(x = NULL, y = "Paired difference in logloss") +
  theme_bw(base_size = 12)
save_fig(p_paired, "paired_diffs.pdf", width = 8, height = 3.8)

# figure as in the AoAS version: boxplots of accuracy, AUC and negative logloss
# over the repetitions, per model class and feature set
n_rep <- length(cf)
loss_panel <- function(m, title, ylab, sign = 1){
  cf_long |>
    filter(metric == m) |>
    mutate(value = sign * value,
           fs = factor(fs, levels = c("pre", "naive", "HMM"))) |>
    ggplot(aes(x = fs, y = value, fill = fs)) +
    geom_boxplot() +
    facet_wrap(~mod) +
    scale_fill_manual(values = c(pre = "gray50", naive = "yellowgreen", HMM = "#38761d"),
                      name = "Model") +
    labs(title = paste0(title, " (", n_rep, " different seeds)"), x = "Model", y = ylab) +
    theme_bw(base_size = 11)
}
p_losses <- ggpubr::ggarrange(
  loss_panel("accuracy", "Accuracies", "Accuracy"),
  loss_panel("AUC", "AUCs", "AUC"),
  loss_panel("logloss", "Negative loglosses", "Negative logloss (log-likelihood)", sign = -1),
  ncol = 3, common.legend = TRUE)
# second row: paired differences HMM - naive per run, on their own scale
# (for the logloss the difference of the negative logloss, so that positive
# values indicate an improvement in all three panels)
diff_panel <- function(m, title, ylab, sign = 1){
  cf_long |>
    filter(metric == m, fs %in% c("naive", "HMM")) |>
    pivot_wider(names_from = fs, values_from = value) |>
    mutate(diff = sign * (HMM - naive)) |>
    ggplot(aes(x = mod, y = diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "darkred") +
    geom_boxplot(width = 0.5, outlier.shape = NA, fill = "#38761d", alpha = 0.3) +
    geom_point(position = position_jitter(width = 0.1, height = 0, seed = 1),
               size = 1, alpha = 0.6) +
    labs(title = title, x = "Model", y = ylab) +
    theme_bw(base_size = 11)
}
p_diffs <- ggpubr::ggarrange(
  diff_panel("accuracy", "Accuracy: HMM - naive", "Difference in accuracy"),
  diff_panel("AUC", "AUC: HMM - naive", "Difference in AUC"),
  diff_panel("logloss", "Negative logloss: HMM - naive", "Difference in negative logloss", sign = -1),
  ncol = 3)
p_losses <- ggpubr::ggarrange(p_losses, p_diffs, nrow = 2, heights = c(1.15, 1))
save_fig(p_losses, "all_losses_cross_fitting.pdf", width = 12, height = 9)

lowo <- readRDS(file.path(res_dir, "zvm_lowo_runs.rds"))
cat("\nLeave-one-week-out (rows = test weeks in order):\n")
for(m in c("accuracy", "AUC", "logloss")){
  tab <- bind_rows(lapply(lowo, \(r) r[[m]] |> select(where(is.numeric))))
  cat("\n", m, "\n"); print(round(tab, 4))
  cat("mean:\n"); print(round(colMeans(tab), 4))
  if("xy_f" %in% names(tab) && "xy_f_ex_hmm" %in% names(tab)){
    better <- if(m == "logloss") tab$xy_f < tab$xy_f_ex_hmm else tab$xy_f > tab$xy_f_ex_hmm
    cat("HMM better than naive in", sum(better), "of", nrow(tab), "weeks\n")
  }
  if("xy_npo_ex_hmm" %in% names(tab) && "xy_f_ex_hmm" %in% names(tab)){
    better <- if(m == "logloss") tab$xy_f_ex_hmm < tab$xy_npo_ex_hmm else tab$xy_f_ex_hmm > tab$xy_npo_ex_hmm
    cat("naive better than pre in", sum(better), "of", nrow(tab), "weeks\n")
  }
}

# 6 GCM ---------------------------------------------------------------------------
section("6 GCM")
gcm_p <- readRDS(file.path(res_dir, "gcm_partial.rds"))
gcm_m <- readRDS(file.path(res_dir, "gcm_marginal.rds"))
cat("partial:\n"); print(gcm_p)
cat("marginal:\n"); print(gcm_m)
cat("marginal, Bonferroni (4 tests):\n"); print(pmin(1, 4 * gcm_m$p.value))
source(here("jrssa", "fig_gcm.R"))  # creates p1 (partial) and ci1 (marginal)
save_fig(p1, "gcm_test_all_plus_omnibus.pdf", width = 5, height = 4.5)
save_fig(ci1, "gcm_teststat.pdf", width = 5, height = 4.5)

# 7 Teams -------------------------------------------------------------------------
section("7 Teams")
team_tab <- readRDS(file.path(res_dir, "team_tab.rds"))
wilson_ci <- function(k, n, z = 1.96){
  p <- k / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(centre - half, centre + half)
}
ci <- wilson_ci(team_tab$n_improv, team_tab$n)
team_out <- team_tab |>
  mutate(share = n_improv / n, lo = ci[, 1], hi = ci[, 2]) |>
  arrange(desc(share)) |>
  select(team, n, n_improv, share, lo, hi)
cat("Table tab:teams (Wilson 95% CI), all teams:\n")
print(team_out |> mutate(across(share:hi, \(v) round(v, 2))), row.names = FALSE)
cat("\nLaTeX rows (top 10):\n")
top <- head(team_out, 10)
for(i in seq_len(nrow(top))) cat(sprintf("%d & %s & %d & %d & %.2f [%.2f, %.2f] \\\\\n", i,
                                         top$team[i], top$n[i], top$n_improv[i], top$share[i],
                                         top$lo[i], top$hi[i]))
cat("overall share of improved plays:", round(sum(team_tab$n_improv) / sum(team_tab$n), 3), "\n")
# figure: difference in the predicted probability of the correct scheme
# (post-motion model incl. HMM features minus pre-motion model) per play, from 05
all_team_zvm <- readRDS(file.path(res_dir, "teams_zvm.rds"))
diff_tab <- bind_rows(lapply(names(all_team_zvm), \(team){
  x <- all_team_zvm[[team]]
  data.frame(team = team,
             diff = c(x$zone_table$post_zone_prob - x$zone_table$pre_zone_prob,
                      x$man_table$post_man_prob - x$man_table$pre_man_prob))
})) |>
  mutate(team = reorder(team, diff, FUN = median))
use_nfl <- requireNamespace("nflplotR", quietly = TRUE)
p_team <- ggplot(diff_tab, aes(x = team, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkred") +
  (if(use_nfl) geom_boxplot(aes(fill = team, colour = team), outlier.size = 0.6, outlier.alpha = 0.4)
   else geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.4, fill = "gray85", colour = "gray30")) +
  labs(x = NULL, y = "Difference in predicted probability\nof the correct scheme") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")
p_team <- if(use_nfl){
  p_team +
    nflplotR::scale_fill_nfl(alpha = 0.7) +
    nflplotR::scale_color_nfl(type = "secondary") +
    theme(axis.text.x = nflplotR::element_nfl_logo(size = 0.75))
} else p_team + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
save_fig(p_team, "team_diffs.pdf", width = 10, height = 4.5)

# 8 Lag and alpha ----------------------------------------------------------------
section("8 Lag and alpha (02a)")
ld_file <- file.path(res_dir, "lag_delta_selection.rds")
if(file.exists(ld_file)){
  ld <- readRDS(ld_file)
  cat("file from", format(file.mtime(ld_file)), "| observations:", ld$n_obs,
      "| plays:", length(ld$plays), "\n")
  cat("alpha profile computed at lag:",
      if(is.null(ld$lag_alpha)) "unknown (file written before this was saved; rerun 02a to be sure)"
      else ld$lag_alpha, "\n")
  print(ld$lag_tab, digits = 6)
  print(ld$alpha_tab, digits = 4)
} else cat("results/lag_delta_selection.rds not found\n")

sink()

# 9 Posterior vs argmax (07) --------------------------------------------------------
if(run_07){
  sink(num_file, split = TRUE, append = TRUE)
  section("9 Posterior vs argmax features (07)")
  source(here("jrssa", "07_compare_feature_versions.R"), echo = FALSE, print.eval = TRUE)
  sink()
}

cat("\nAll numbers in", num_file, "\n")
