## -----------------------------------------------------------------------------
## Author: Francisco Garre-Frutos
## -----------------------------------------------------------------------------

# Loading packages----
source("scripts/functions.R")
source("scripts/load_data.R")

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(dplyr, # Data wrangling
       tidyr,
       metafor # Meta-analysis
)

ensure_project_dirs()

# Effect size: standardized mean change (SMCC) between IOR (Cued - Uncued, ms)
# for high- and low-value targets at the long SOA. Negative values indicate a
# smaller IOR for high-value targets.

# Present experiments ----
# First two no-reward blocks of each experiment
summarise_ior <- function(cells) {
  s <- ior_scores(cells, "RT")
  tibble(
    m1 = mean(s$IOR_High), m2 = mean(s$IOR_Low),
    sd1 = sd(s$IOR_High), sd2 = sd(s$IOR_Low),
    ni = nrow(s), ri = cor(s$IOR_High, s$IOR_Low)
  )
}

e1 <- summarise_ior(cell_means(raw1, 9:10))
e2 <- summarise_ior(cell_means(raw2, 9:10))

# Li et al. (2021) ----
# Discrimination task (N = 20), RT means of their Table 1 (cued, uncued): high
# value 499, 492; low value 518, 502. The SD of IOR is derived from the main
# effect of Cue, F(1, 19) = 19.91 (SD of the cueing effect averaged over the four
# Task x Reward cells; localization task: 362, 348; 362, 349), times 2 for a
# single cell. r = .10 is assumed.
N_li <- 20
cue_effect_li <- mean(c(518, 499, 362, 362)) - mean(c(502, 492, 349, 348))
sd_cell_li <- 2 * cue_effect_li / sqrt(19.91) * sqrt(N_li)

li <- tibble(m1 = 499 - 492, m2 = 518 - 502, sd1 = sd_cell_li, sd2 = sd_cell_li, ni = N_li, ri = .10)

# Wang et al. (2022) ----
# IOR (cued - uncued) in the near depth plane, rewarded vs. unrewarded blocks
# (Experiment 1: N = 30; Experiment 2: N = 28). The SD of the difference is
# derived from their paired t tests, t(29) = 3.01 and t(27) = 2.67.
sd_diff_from_t <- function(m1, m2, t, n) abs(m1 - m2) / t * sqrt(n)

# SMCC only depends on the SD of the difference (sd1 = sd2 = sd_diff / sqrt(2), r = 0)
from_sd_diff <- function(m1, m2, sd_diff, n) {
  tibble(m1 = m1, m2 = m2, sd1 = sd_diff / sqrt(2), sd2 = sd_diff / sqrt(2), ni = n, ri = 0)
}

wang_e1 <- from_sd_diff(29, 35, sd_diff_from_t(29, 35, 3.01, 30), 30)
wang_e2 <- from_sd_diff(26, 35, sd_diff_from_t(26, 35, 2.67, 28), 28)

# Models ----
meta_data <- bind_rows(
  mutate(e1, paper = "Garre-Frutos & Lupiáñez", study = "Present study, Experiment 1"),
  mutate(e2, paper = "Garre-Frutos & Lupiáñez", study = "Present study, Experiment 2"),
  mutate(li, paper = "Li et al. (2021)", study = "Li et al. (2021), Discrimination task"),
  mutate(wang_e1, paper = "Wang et al. (2022)", study = "Wang et al. (2022), Experiment 1"),
  mutate(wang_e2, paper = "Wang et al. (2022)", study = "Wang et al. (2022), Experiment 2")
) %>%
  mutate(es_id = row_number()) %>%
  escalc("SMCC", m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, ni = ni, ri = ri, data = ., slab = study)

# Random-effects model
meta_re <- rma(yi, vi, data = meta_data, method = "REML")
summary(meta_re)

# Multilevel model (effect sizes nested within studies)
meta_ml <- rma.mv(yi, vi, random = ~ 1 | paper / es_id, data = meta_data, method = "REML")
summary(meta_ml)

# Publication bias (k = 5, so very low power)
egger_test <- regtest(meta_re) # Funnel plot asymmetry
rank_test <- ranktest(meta_re) # Rank correlation
meta_tf <- trimfill(meta_re) # Trim-and-fill
egger_test
rank_test
meta_tf

publication_bias <- tibble(
  egger_z = egger_test$zval, egger_p = egger_test$pval,
  rank_tau = rank_test$tau, rank_p = rank_test$pval,
  tf_k0 = meta_tf$k0, tf_estimate = meta_tf$b[1],
  tf_lower = meta_tf$ci.lb, tf_upper = meta_tf$ci.ub
)
publication_bias

# Sample size for 80% power (paired t test) for the estimate and for the CI
# bound closest to zero
n_required <- tibble(
  effect = c("estimate", "CI bound closest to zero"),
  d = abs(c(meta_re$b[1], meta_re$ci.ub)),
  n = ceiling(vapply(d, function(x) power.t.test(delta = x, sd = 1, power = .80, type = "one.sample")$n, numeric(1)))
)
n_required

# Forest plot ----
png("output/figures/fig-meta.png", width = 4000, height = 2300, units = "px", res = 600)
par(mar = c(4, .5, 0, 2))
forest(
  meta_re,
  cex = 0.7,
  header = c("Study", "SMC [95% CI]"),
  xlab = "Standardized mean change (IOR high - IOR low)",
  mlab = "Random-effects model",
  shade = TRUE
)
dev.off()

save_analysis_objects(
  "meta_analysis",
  meta_data = meta_data,
  meta_re = meta_re,
  meta_ml = meta_ml,
  publication_bias = publication_bias,
  n_required = n_required
)
