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
       afex, # ANOVA
       emmeans, # Contrasts
       BayesFactor, # Bayes factors
       ggplot2, # Plotting
       patchwork
)

ensure_project_dirs()

# Earnings and RT thresholds ----
earnings_e2 <- raw2 %>%
  filter(Phase == "Reward") %>%
  group_by(ID) %>%
  summarise(points = last(points), .groups = "drop") %>%
  mutate(euros = earnings(points, 246.4))

summary(earnings_e2$euros)

# Individual threshold: mean + 1 SD of the RTs in the calibration block. Correct
# responses slower than the threshold lost the points of the trial, as errors did.
threshold_summary <- raw2 %>%
  filter(Phase == "Reward") %>%
  group_by(ID) %>%
  summarise(
    threshold = first(threshold),
    lost = mean(slow == 1 | correct == 0), # Trials in which points were lost
    slow = mean(slow), # Responses slower than the threshold
    .groups = "drop"
  ) %>%
  summarise(
    M = mean(threshold), SD = sd(threshold),
    min = min(threshold), max = max(threshold),
    slow = mean(slow) * 100, lost = mean(lost) * 100
  )
threshold_summary

# Reward phase (blocks 2-8) ----
cells_R <- cell_means(raw2, 2:8)

# Preregistered SOA x Value x Cueing ANOVA
aov_R_rt <- rm_anova(cells_R, "RT", c("SOA", "Value", "Cueing"))
aov_R_rt

cue_R <- cueing_effects(aov_R_rt) # Facilitation and IOR
cue_R

# Effect of value at each SOA (SOA x Value interaction)
value_by_soa_R <- as.data.frame(summary(pairs(pairs(emmeans(aov_R_rt, ~ Value | SOA)), by = NULL), infer = TRUE))
value_by_soa_R

aov_R_err <- rm_anova(cells_R, "Error", c("SOA", "Value", "Cueing"))
aov_R_err
err_R_soa <- as.data.frame(emmeans(aov_R_err, ~ SOA))
err_R_value <- as.data.frame(emmeans(aov_R_err, ~ Value))

# Value x Cueing at the long SOA and Bayes factors
aov_R_long <- rm_anova(filter(cells_R, SOA == "Long"), "RT", c("Value", "Cueing"))
aov_R_long

bf_R_long <- bf_contrast(ior_scores(cells_R, "RT")$IOR_diff)
bf_R_3way <- bf_contrast(interaction_scores(cells_R, "RT")$three_way)
bf_R_long
bf_R_3way

# Preregistered: effect of value across the reward blocks
cells_Rb <- raw2 %>%
  filter(Block %in% 2:8, rt >= 200, rt <= 1200, correct == 1) %>%
  group_by(ID, Block, Value) %>%
  summarise(RT = mean(rt), .groups = "drop") %>%
  mutate(Block = factor(Block))

aov_Rb <- rm_anova(cells_Rb, "RT", c("Block", "Value"))
aov_Rb # No Block x Value interaction

# Exploratory: last reward block (as in Experiment 1)
cells_R8 <- cell_means(raw2, 8)
aov_R8_long <- rm_anova(filter(cells_R8, SOA == "Long"), "RT", c("Value", "Cueing"))
bf_R8_long <- bf_contrast(ior_scores(cells_R8, "RT")$IOR_diff)
aov_R8_long
bf_R8_long

# No-reward phase (blocks 9-12) ----
cells_NR <- cell_means(raw2, 9:12)

# Preregistered SOA x Value x Cueing ANOVA
aov_NR_rt <- rm_anova(cells_NR, "RT", c("SOA", "Value", "Cueing"))
aov_NR_rt

cue_NR <- cueing_effects(aov_NR_rt)
cue_NR

aov_NR_err <- rm_anova(cells_NR, "Error", c("SOA", "Value", "Cueing"))
aov_NR_err

bf_NR_3way <- bf_contrast(interaction_scores(cells_NR, "RT")$three_way)
bf_NR_3way

# Exploratory: first two no-reward blocks (as in Experiment 1) ----
cells_NR2 <- cell_means(raw2, 9:10)

aov_NR2_rt <- rm_anova(cells_NR2, "RT", c("SOA", "Value", "Cueing"))
aov_NR2_rt

cue_NR2 <- cueing_effects(aov_NR2_rt)
cue_NR2

aov_NR2_err <- rm_anova(cells_NR2, "Error", c("SOA", "Value", "Cueing"))
aov_NR2_err

aov_NR2_long <- rm_anova(filter(cells_NR2, SOA == "Long"), "RT", c("Value", "Cueing"))
aov_NR2_long
ior_by_value_NR2 <- cueing_effects(aov_NR2_long, by = "Value", sides = c(High = "<", Low = "<"))
ior_by_value_NR2

bf_NR2_3way <- bf_contrast(interaction_scores(cells_NR2, "RT")$three_way)
bf_NR2_3way

# Awareness ----
awareness_e2 <- awareness2 %>%
  summarise(n = n(), Q2_correct = mean(Q2, na.rm = TRUE))
awareness_e2

# Not preregistered: recognition accuracy against .50 and correct answers to the
# explicit question against 1/9
recognition_test_e2 <- t.test(awareness2$recognition_acc, mu = .5)
rule_test_e2 <- binom.test(sum(awareness2$Q2 == 1, na.rm = TRUE), sum(!is.na(awareness2$Q2)), p = 1 / 9)
chance_e2 <- list(
  recognition = data.frame(
    M = mean(awareness2$recognition_acc), SD = sd(awareness2$recognition_acc),
    t = unname(recognition_test_e2$statistic), df = unname(recognition_test_e2$parameter),
    p = recognition_test_e2$p.value
  ),
  rule = data.frame(
    k = unname(rule_test_e2$statistic), n = unname(rule_test_e2$parameter),
    p = rule_test_e2$p.value
  )
)
chance_e2

# Preregistered: recognition accuracy and the effect of value
aware_cor_e2 <- awareness_correlation(awareness2, cells_R)
aware_cor_e2 # Not significant

# Figure ----
fig_e2 <- plot_two_panels(
  plot_cells(cells_R, limits = c(310, 400)),
  plot_cells(cells_NR2, limits = c(310, 400))
)
fig_e2

ggsave("output/figures/fig-3.png", fig_e2, height = 9, width = 20, dpi = 600, units = "cm")

save_analysis_objects(
  "analysis_e2",
  exclusions = exclusions2,
  earnings = earnings_e2,
  threshold_summary = threshold_summary,
  aov_R_rt = aov_R_rt,
  cue_R = cue_R,
  value_by_soa_R = value_by_soa_R,
  aov_R_err = aov_R_err,
  err_R_soa = err_R_soa,
  err_R_value = err_R_value,
  aov_R_long = aov_R_long,
  bf_R_long = bf_R_long,
  bf_R_3way = bf_R_3way,
  aov_Rb = aov_Rb,
  aov_R8_long = aov_R8_long,
  bf_R8_long = bf_R8_long,
  aov_NR_rt = aov_NR_rt,
  cue_NR = cue_NR,
  aov_NR_err = aov_NR_err,
  bf_NR_3way = bf_NR_3way,
  aov_NR2_rt = aov_NR2_rt,
  cue_NR2 = cue_NR2,
  aov_NR2_err = aov_NR2_err,
  aov_NR2_long = aov_NR2_long,
  ior_by_value_NR2 = ior_by_value_NR2,
  bf_NR2_3way = bf_NR2_3way,
  awareness = awareness_e2,
  chance = chance_e2,
  aware_cor = aware_cor_e2
)
