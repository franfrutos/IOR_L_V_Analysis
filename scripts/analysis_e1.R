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
       effectsize, # Cohen's f
       ggplot2, # Plotting
       patchwork
)

ensure_project_dirs()

# Earnings ----
earnings_e1 <- raw1 %>%
  filter(Phase == "Reward") %>%
  group_by(ID) %>%
  summarise(points = last(points), .groups = "drop") %>%
  mutate(euros = earnings(points, 281.6))

summary(earnings_e1$euros)

# Reward phase (blocks 1-8) ----
cells_R <- cell_means(raw1, 1:8)

# Preregistered SOA x Value x Cueing ANOVA
aov_R_rt <- rm_anova(cells_R, "RT", c("SOA", "Value", "Cueing"))
aov_R_rt

cue_R <- cueing_effects(aov_R_rt) # Facilitation and IOR
cue_R

aov_R_err <- rm_anova(cells_R, "Error", c("SOA", "Value", "Cueing"))
aov_R_err # No significant effects

# Value x Cueing at the long SOA and Bayes factors
aov_R_long <- rm_anova(filter(cells_R, SOA == "Long"), "RT", c("Value", "Cueing"))
aov_R_long

bf_R_long <- bf_contrast(ior_scores(cells_R, "RT")$IOR_diff)
bf_R_3way <- bf_contrast(interaction_scores(cells_R, "RT")$three_way)
bf_R_long
bf_R_3way

# Preregistered: effect of value across the reward blocks
cells_Rb <- raw1 %>%
  filter(Block %in% 1:8, rt >= 200, rt <= 1200, correct == 1) %>%
  group_by(ID, Block, Value) %>%
  summarise(RT = mean(rt), .groups = "drop") %>%
  mutate(Block = factor(Block))

aov_Rb <- rm_anova(cells_Rb, "RT", c("Block", "Value"))
aov_Rb

value_by_block <- as.data.frame(summary(pairs(emmeans(aov_Rb, ~ Value | Block)), by = NULL, adjust = "holm", infer = TRUE))
value_by_block # Only significant in the last block

# Preregistered: main analysis in the last reward block
cells_R8 <- cell_means(raw1, 8)
aov_R8_rt <- rm_anova(cells_R8, "RT", c("SOA", "Value", "Cueing"))
aov_R8_rt

value_by_cueing_R8 <- as.data.frame(summary(pairs(emmeans(aov_R8_rt, ~ Value | Cueing)), by = NULL, adjust = "holm", infer = TRUE))
value_by_cueing_R8

aov_R8_long <- rm_anova(filter(cells_R8, SOA == "Long"), "RT", c("Value", "Cueing"))
bf_R8_long <- bf_contrast(ior_scores(cells_R8, "RT")$IOR_diff)
bf_R8_long

# No-reward phase (blocks 9-10) ----
cells_NR <- cell_means(raw1, 9:10)

# Preregistered SOA x Value x Cueing ANOVA
aov_NR_rt <- rm_anova(cells_NR, "RT", c("SOA", "Value", "Cueing"))
aov_NR_rt # Three-way interaction not significant

cue_NR <- cueing_effects(aov_NR_rt)
cue_NR

bf_NR_3way <- bf_contrast(interaction_scores(cells_NR, "RT")$three_way)
bf_NR_3way

aov_NR_err <- rm_anova(cells_NR, "Error", c("SOA", "Value", "Cueing"))
aov_NR_err # Significant three-way interaction, so IES is analyzed (preregistered)

# IES
aov_NR_ies <- rm_anova(cells_NR, "IES", c("SOA", "Value", "Cueing"))
aov_NR_ies
cue_NR_ies <- cueing_effects(aov_NR_ies)

# Follow-up analyses at each SOA and IOR for each target value
aov_NR_long <- rm_anova(filter(cells_NR, SOA == "Long"), "RT", c("Value", "Cueing"))
aov_NR_short <- rm_anova(filter(cells_NR, SOA == "Short"), "RT", c("Value", "Cueing"))
ior_by_value_NR <- cueing_effects(aov_NR_long, by = "Value", sides = c(High = "<", Low = "<"))
aov_NR_long
aov_NR_short
ior_by_value_NR

aov_NR_long_ies <- rm_anova(filter(cells_NR, SOA == "Long"), "IES", c("Value", "Cueing"))
aov_NR_short_ies <- rm_anova(filter(cells_NR, SOA == "Short"), "IES", c("Value", "Cueing"))
ior_by_value_NR_ies <- cueing_effects(aov_NR_long_ies, by = "Value", sides = c(High = "<", Low = "<"))
ior_diff_NR_ies <- ior_difference(cells_NR, "IES")
ior_by_value_NR_ies
ior_diff_NR_ies

# Cohen's f of the three-way interaction (sensitivity power analysis)
f_NR_rt <- F_to_f(
  extract_afex_term(aov_NR_rt, "SOA:Value:Cueing")$f_value, 1, 39,
  ci = .95, alternative = "two.sided"
)
f_NR_ies <- F_to_f(
  extract_afex_term(aov_NR_ies, "SOA:Value:Cueing")$f_value, 1, 39,
  ci = .95, alternative = "two.sided"
)
f_NR_rt
f_NR_ies

# Awareness ----
awareness_e1 <- awareness1 %>%
  summarise(n = n(), Q2_correct = mean(Q2, na.rm = TRUE))
awareness_e1

# Not preregistered: recognition accuracy against .50 and correct answers to the
# explicit question against 1/9
recognition_test_e1 <- t.test(awareness1$recognition_acc, mu = .5)
rule_test_e1 <- binom.test(sum(awareness1$Q2 == 1, na.rm = TRUE), sum(!is.na(awareness1$Q2)), p = 1 / 9)
chance_e1 <- list(
  recognition = data.frame(
    M = mean(awareness1$recognition_acc), SD = sd(awareness1$recognition_acc),
    t = unname(recognition_test_e1$statistic), df = unname(recognition_test_e1$parameter),
    p = recognition_test_e1$p.value
  ),
  rule = data.frame(
    k = unname(rule_test_e1$statistic), n = unname(rule_test_e1$parameter),
    p = rule_test_e1$p.value
  )
)
chance_e1

# Preregistered: recognition accuracy and the effect of value
aware_cor_e1 <- awareness_correlation(awareness1, cells_R)
aware_cor_e1 # Not significant

# Figure ----
fig_e1 <- plot_two_panels(
  plot_cells(cells_R, limits = c(330, 420)),
  plot_cells(cells_NR, limits = c(330, 420))
)
fig_e1

ggsave("output/figures/fig-2.png", fig_e1, height = 9, width = 20, dpi = 600, units = "cm")

save_analysis_objects(
  "analysis_e1",
  exclusions = exclusions1,
  earnings = earnings_e1,
  aov_R_rt = aov_R_rt,
  cue_R = cue_R,
  aov_R_err = aov_R_err,
  aov_R_long = aov_R_long,
  bf_R_long = bf_R_long,
  bf_R_3way = bf_R_3way,
  aov_Rb = aov_Rb,
  value_by_block = value_by_block,
  aov_R8_rt = aov_R8_rt,
  value_by_cueing_R8 = value_by_cueing_R8,
  aov_R8_long = aov_R8_long,
  bf_R8_long = bf_R8_long,
  aov_NR_rt = aov_NR_rt,
  cue_NR = cue_NR,
  bf_NR_3way = bf_NR_3way,
  aov_NR_err = aov_NR_err,
  aov_NR_ies = aov_NR_ies,
  cue_NR_ies = cue_NR_ies,
  aov_NR_long = aov_NR_long,
  aov_NR_short = aov_NR_short,
  ior_by_value_NR = ior_by_value_NR,
  aov_NR_long_ies = aov_NR_long_ies,
  aov_NR_short_ies = aov_NR_short_ies,
  ior_by_value_NR_ies = ior_by_value_NR_ies,
  ior_diff_NR_ies = ior_diff_NR_ies,
  f_NR_rt = f_NR_rt,
  f_NR_ies = f_NR_ies,
  awareness = awareness_e1,
  chance = chance_e1,
  aware_cor = aware_cor_e1
)
