######################################################################################
#### GCM test result plots (sourced by 08_paper_numbers_figures.R, or run alone)
#### p1:  partial tests, -log10(p) (omnibus and each feature given Z and the
####      other HMM features)
#### ci1: marginal tests, normalised test statistic of each feature given Z
######################################################################################

library(dplyr)
library(ggplot2)
library(ggpubr)
library(here)

# feature names as in the paper
gcm_labels <- c("sum changes" = "Expected switches",
                "# player changes" = "Expected # switching\ndefenders",
                "average entropy" = "Mean entropy",
                "beta" = "RE/play",
                "omnibus" = "Omnibus")
relabel <- function(x){
  stopifnot(all(x %in% names(gcm_labels)))
  factor(gcm_labels[x], levels = gcm_labels[c("omnibus", "sum changes", "# player changes",
                                              "average entropy", "beta")])
}

# left panel: partial tests
sig_df <- readRDS(here("results", "gcm_partial.rds")) |>
  mutate(predictor = relabel(predictor))

p1 <- sig_df |>
  ggplot(aes(x = predictor, y = -log10(p.value))) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "darkred") +
  geom_segment(aes(xend = predictor, y = 0, yend = -log10(p.value)), linewidth = 1,
               colour = "gray40") +
  geom_point(aes(colour = p.value <= 0.05), size = 3.5) +
  # after the discrete layers, otherwise the x scale becomes continuous
  geom_vline(xintercept = 1.5, colour = "gray60", linetype = 3) +
  scale_colour_manual(values = c(`TRUE` = "#38761d", `FALSE` = "gray50"), guide = "none") +
  labs(x = NULL, y = expression(-log[10](p))) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# right panel: marginal tests, normalised statistic; dashed lines: two-sided 5%
# critical values, dotted lines: Bonferroni-corrected for four tests
sig_df2 <- readRDS(here("results", "gcm_marginal.rds")) |>
  mutate(predictor = droplevels(relabel(predictor)))
z_crit <- qnorm(0.975)
z_bonf <- qnorm(1 - 0.025 / nrow(sig_df2))

ci1 <- sig_df2 |>
  ggplot(aes(x = tstat, y = predictor)) +
  geom_vline(xintercept = 0, colour = "gray40") +
  geom_vline(xintercept = c(-z_crit, z_crit), linetype = "dashed", colour = "darkred") +
  geom_vline(xintercept = c(-z_bonf, z_bonf), linetype = "dotted", colour = "darkred") +
  geom_segment(aes(x = 0, xend = tstat, yend = predictor), linewidth = 1, colour = "gray40") +
  geom_point(size = 3.5, colour = "#38761d") +
  scale_y_discrete(limits = rev) +
  labs(x = "Normalised GCM test statistic", y = NULL) +
  theme_classic(base_size = 12)

ggarrange(p1, ci1, ncol = 2)
