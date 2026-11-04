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
       lme4, # GLMM and LMM
       car, # ANOVA-like tables of the mixed models
       marginaleffects, # Model predictions and contrasts
       ggplot2, # Plotting
       patchwork
)

ensure_project_dirs()

# Participant codes are only unique within each experiment
raw <- bind_rows(raw1, filter(raw2, Phase != "Calibration")) %>%
  mutate(ID = factor(paste0(ifelse(Experiment == "Experiment 1", "e1_", "e2_"), ID)))

d_e1 <- filter(raw, Experiment == "Experiment 1")
d_e2 <- filter(raw, Experiment == "Experiment 2")
by_cells <- c("Experiment", "SOA", "Value", "Cueing")
within <- c("SOA", "Value", "Cueing")

# Predicted proportion of errors of a logistic mixed model
predicted_errors <- function(model, spec) {
  as.data.frame(emmeans(model, spec, type = "response")) %>%
    transmute(
      level = .data[[all.vars(spec)[1]]],
      emmean = 1 - prob, lower.CL = 1 - asymp.UCL, upper.CL = 1 - asymp.LCL
    )
}

# Reward phase of both experiments ----
cells_R <- bind_rows(
  cell_means(d_e1, 1:8, by = by_cells),
  cell_means(d_e2, 2:8, by = by_cells)
)

aov_R_rt <- rm_anova(cells_R, "RT", within, between = "Experiment")
aov_R_rt

cue_R <- cueing_effects(aov_R_rt)
cue_R

rt_by_experiment_R <- as.data.frame(emmeans(aov_R_rt, ~ Experiment))
rt_by_value_R <- as.data.frame(emmeans(aov_R_rt, ~ Value))

# Experiment x SOA x Value: effect of value (High - Low) at the long minus the
# short SOA in each experiment
value_by_soa_R <- as.data.frame(summary(
  pairs(pairs(emmeans(aov_R_rt, ~ Value | SOA * Experiment)), by = "Experiment"),
  infer = TRUE
))
rt_by_experiment_R
rt_by_value_R
value_by_soa_R

aov_R_long <- rm_anova(filter(cells_R, SOA == "Long"), "RT", c("Value", "Cueing"), between = "Experiment")
aov_R_long

bf_R_3way <- bf_contrast(interaction_scores(cells_R, "RT")$three_way)
bf_R_long <- bf_contrast(ior_scores(cells_R, "RT")$IOR_diff)
bf_R_3way
bf_R_long # BF01 > 3

# Accuracy: logistic mixed model on the proportion of correct responses in each
# cell, weighted by the number of trials (same fit as the trial-level model)
d_glm_R <- raw %>%
  filter(Phase == "Reward") %>%
  summarise(ACC = mean(correct), n = n(), .by = c(ID, Experiment, SOA, Value, Cueing)) %>%
  mutate(across(c(SOA, Value, Cueing, Experiment), factor))
contrasts(d_glm_R$SOA) <- contr.sum(2) / 2
contrasts(d_glm_R$Value) <- contr.sum(2) / 2
contrasts(d_glm_R$Cueing) <- contr.sum(2) / 2
contrasts(d_glm_R$Experiment) <- contr.sum(2) / 2

glmm_R_acc_max <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA * Value * Cueing | ID),
  data = d_glm_R,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit

glmm_R_acc_2way <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + ((SOA + Value + Cueing)^2 | ID),
  data = d_glm_R,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit

glmm_R_acc_main <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA + Value + Cueing | ID),
  data = d_glm_R,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit (Value has the smallest variance)

glmm_R_acc <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA + Cueing | ID),
  data = d_glm_R,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Maximal feasible model
summary(glmm_R_acc)

acc_R_anova <- Anova(glmm_R_acc, type = 3) # ANOVA-like table
acc_R_anova

# No effect involving Value or Cueing, so the RTs are not affected by a
# speed-accuracy trade-off and the IES is not analyzed
err_R_experiment <- predicted_errors(glmm_R_acc, ~ Experiment)
err_R_experiment

# No-reward phase: first two no-reward blocks of each experiment (exploratory) ----
cells_NR <- bind_rows(
  cell_means(d_e1, 9:10, by = by_cells),
  cell_means(d_e2, 9:10, by = by_cells)
)

aov_NR_rt <- rm_anova(cells_NR, "RT", within, between = "Experiment")
aov_NR_rt

cue_NR <- cueing_effects(aov_NR_rt)
cue_NR

rt_by_experiment <- as.data.frame(emmeans(aov_NR_rt, ~ Experiment))
cueing_by_experiment <- as.data.frame(summary(
  pairs(pairs(emmeans(aov_NR_rt, ~ Cueing | Experiment)), by = NULL),
  infer = TRUE
))

# Follow-up analyses at each SOA
aov_NR_long <- rm_anova(filter(cells_NR, SOA == "Long"), "RT", c("Value", "Cueing"))
aov_NR_short <- rm_anova(filter(cells_NR, SOA == "Short"), "RT", c("Value", "Cueing"))
ior_by_value_NR <- cueing_effects(aov_NR_long, by = "Value", sides = c(High = "<", Low = "<"))
aov_NR_long
aov_NR_short
ior_by_value_NR

# Accuracy: logistic mixed model on the proportion of correct responses in each
# cell, weighted by the number of trials (same fit as the trial-level model)
d_glm <- raw %>%
  filter(Block %in% 9:10) %>%
  summarise(ACC = mean(correct), n = n(), .by = c(ID, Experiment, SOA, Value, Cueing)) %>%
  mutate(across(c(SOA, Value, Cueing, Experiment), factor))
contrasts(d_glm$SOA) <- contr.sum(2) / 2
contrasts(d_glm$Value) <- contr.sum(2) / 2
contrasts(d_glm$Cueing) <- contr.sum(2) / 2
contrasts(d_glm$Experiment) <- contr.sum(2) / 2

glmm_NR_acc_max <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA * Value * Cueing | ID),
  data = d_glm,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit

glmm_NR_acc_2way <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + ((SOA + Value + Cueing)^2 | ID),
  data = d_glm,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit

glmm_NR_acc_main <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA + Value + Cueing | ID),
  data = d_glm,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit (Value has the smallest variance)

glmm_NR_acc_main2 <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA + Cueing | ID),
  data = d_glm,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit (Cueing has the smallest variance)

glmm_NR_acc <- glmer(
  ACC ~ SOA * Value * Cueing * Experiment + (SOA | ID),
  data = d_glm,
  family = binomial(),
  weights = n,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Maximal feasible model
summary(glmm_NR_acc)

acc_anova <- Anova(glmm_NR_acc, type = 3) # ANOVA-like table
acc_anova

# Predicted proportion of errors by SOA and by Experiment
err_NR_soa <- predicted_errors(glmm_NR_acc, ~ SOA)
err_NR_experiment <- predicted_errors(glmm_NR_acc, ~ Experiment)
err_NR_soa
err_NR_experiment

# IES
aov_NR_ies <- rm_anova(cells_NR, "IES", within, between = "Experiment")
aov_NR_ies
cue_NR_ies <- cueing_effects(aov_NR_ies)
aov_NR_long_ies <- rm_anova(filter(cells_NR, SOA == "Long"), "IES", c("Value", "Cueing"))
aov_NR_short_ies <- rm_anova(filter(cells_NR, SOA == "Short"), "IES", c("Value", "Cueing"))
ior_by_value_NR_ies <- cueing_effects(aov_NR_long_ies, by = "Value", sides = c(High = "<", Low = "<"))

# IES with the four no-reward blocks of Experiment 2 (footnote)
cells_NR_all <- bind_rows(
  cell_means(d_e1, 9:10, by = by_cells),
  cell_means(d_e2, 9:12, by = by_cells)
)
aov_NR_all_ies <- rm_anova(cells_NR_all, "IES", within, between = "Experiment")
aov_NR_all_ies

# Comparison between phases ----
# Last two reward blocks vs. first two no-reward blocks (adjacent, same trials)
cells_phase <- cell_means(raw, 7:10, by = c("Phase", by_cells))

aov_phase_rt <- rm_anova(cells_phase, "RT", c("Phase", within), between = "Experiment")
aov_phase_rt

aov_phase_long <- rm_anova(
  filter(cells_phase, SOA == "Long"), "RT", c("Phase", "Value", "Cueing"),
  between = "Experiment"
)
aov_phase_long

# Bayes factors: change of the critical interactions from one phase to the other
phase_scores <- full_join(
  interaction_scores(filter(cells_phase, Phase == "Reward"), "RT") %>%
    select(ID, Experiment, long_R = long, three_way_R = three_way),
  interaction_scores(filter(cells_phase, Phase == "No-reward"), "RT") %>%
    select(ID, Experiment, long_NR = long, three_way_NR = three_way),
  by = c("ID", "Experiment")
) %>%
  mutate(
    long_change = long_NR - long_R,
    four_way = three_way_NR - three_way_R
  )

bf_phase_4way <- bf_contrast(phase_scores$four_way)
bf_phase_long <- bf_contrast(phase_scores$long_change)
bf_phase_4way
bf_phase_long

# Time course of the effect of value on IOR ----
# LMM on the RTs at the long SOA in bins of two blocks: bins 1-4 are the reward
# blocks (in Experiment 2, bin 1 only includes block 2) and bin 5 the first two
# no-reward blocks. With this coding:
#   cue:val          IOR_Low - IOR_High (ms) at the end of the reward phase
#   cue:val:bin_rel  change of that difference per reward bin
#   cue:val:phase    change of that difference in the no-reward bin
d_bins <- raw %>%
  filter(SOA == "Long", correct == 1, rt >= 200, rt <= 1200, Block <= 10) %>%
  mutate(
    Bin = ceiling(Block / 2),
    bin_rel = pmin(Bin, 4) - 4,
    phase = ifelse(Phase == "No-reward", 1, 0),
    cue = ifelse(Cueing == "Cued", .5, -.5),
    val = ifelse(Value == "Low", .5, -.5),
    exp = ifelse(Experiment == "Experiment 2", .5, -.5)
  )

lmm_bins_max <- lmer(
  rt ~ cue * val * (bin_rel + phase) + exp + (cue * val * (bin_rel + phase) | ID),
  data = d_bins,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit

lmm_bins_2way <- lmer(
  rt ~ cue * val * (bin_rel + phase) + exp +
    (cue * val + cue:bin_rel + cue:phase + val:bin_rel + val:phase + bin_rel + phase | ID),
  data = d_bins,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Singular fit

lmm_bins <- lmer(
  rt ~ cue * val * (bin_rel + phase) + exp + (cue + val + bin_rel + phase | ID),
  data = d_bins,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Maximal feasible model
summary(lmm_bins)

bins_anova <- Anova(lmm_bins, type = 3) # ANOVA-like table
bins_anova

# Model predictions (fixed effects) of IOR (Cued - Uncued) for each target value
# and of their difference (Low - High) by bin
grid_bins <- expand.grid(val = c(-.5, .5), Bin = c(seq(1, 4, by = .05), 5)) %>%
  mutate(bin_rel = pmin(Bin, 4) - 4, phase = as.numeric(Bin > 4.5), exp = 0, cue = 0, ID = NA)

ior_pred <- comparisons(
  lmm_bins,
  variables = list(cue = c(-.5, .5)),
  newdata = grid_bins,
  re.form = NA
)

diff_pred <- comparisons(
  lmm_bins,
  variables = list(cue = c(-.5, .5)),
  newdata = grid_bins,
  re.form = NA,
  hypothesis = difference ~ reference | Bin
)

pred_bins <- bind_rows(
  as.data.frame(ior_pred) %>%
    transmute(Bin, Value = ifelse(val > 0, "Low", "High"), estimate, conf.low, conf.high),
  as.data.frame(diff_pred) %>%
    transmute(Bin, Value = "Difference", estimate, statistic, p.value, conf.low, conf.high)
)

# Differences between experiments (Experiment 2 had one reward block fewer and
# two no-reward blocks more). Experiment is between participants, so the random
# effects are those of lmm_bins
lmm_bins_exp <- lmer(
  rt ~ cue * val * (bin_rel + phase) * exp + (cue + val + bin_rel + phase | ID),
  data = d_bins,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)
summary(lmm_bins_exp)

bins_exp_anova <- Anova(lmm_bins_exp, type = 3) # ANOVA-like table
bins_exp_anova

# IOR_Low - IOR_High in each experiment at the end of the reward phase (phase = 0)
# and in the no-reward bin (phase = 1)
grid_exp <- expand.grid(val = c(-.5, .5), phase = 0:1, exp = c(-.5, .5)) %>%
  mutate(
    Experiment = ifelse(exp > 0, "Experiment 2", "Experiment 1"),
    cell = paste(Experiment, phase), bin_rel = 0, cue = 0, ID = NA
  )

diff_exp <- comparisons(
  lmm_bins_exp,
  variables = list(cue = c(-.5, .5)),
  newdata = grid_exp,
  re.form = NA,
  hypothesis = difference ~ reference | cell
) %>%
  as.data.frame() %>%
  left_join(distinct(grid_exp, cell, Experiment, phase), by = "cell") %>%
  select(Experiment, phase, estimate, statistic, p.value, conf.low, conf.high)
diff_exp

# Observed IOR by bin (mean and SE across participants)
ior_bins <- d_bins %>%
  group_by(ID, Bin, Value, Cueing) %>%
  summarise(RT = mean(rt), .groups = "drop") %>%
  pivot_wider(names_from = Cueing, values_from = RT) %>%
  mutate(IOR = Cued - Uncued) %>%
  select(ID, Bin, Value, IOR) %>%
  pivot_wider(names_from = Value, values_from = IOR) %>%
  mutate(Difference = Low - High) %>%
  pivot_longer(c(High, Low, Difference), names_to = "Value", values_to = "IOR") %>%
  group_by(Bin, Value) %>%
  summarise(
    estimate = mean(IOR, na.rm = TRUE),
    se = sd(IOR, na.rm = TRUE) / sqrt(sum(!is.na(IOR))),
    .groups = "drop"
  )
ior_bins

# Awareness (exploratory, footnote) ----
# Recognition accuracy (standardized) as a moderator of the effect of value in
# the reward phase: val:acc_z is the change of the effect of value (ms) per SD
awareness_all <- bind_rows(
  mutate(awareness1, ID = paste0("e1_", ID)),
  mutate(awareness2, ID = paste0("e2_", ID))
) %>%
  mutate(acc_z = as.numeric(scale(recognition_acc)))

d_aware <- raw %>%
  filter(Phase == "Reward", correct == 1, rt >= 200, rt <= 1200) %>%
  mutate(ID = as.character(ID)) %>%
  inner_join(select(awareness_all, ID, acc_z), by = "ID") %>%
  mutate(
    val = ifelse(Value == "Low", .5, -.5),
    cue = ifelse(Cueing == "Cued", .5, -.5),
    soa = ifelse(SOA == "Long", .5, -.5),
    exp = ifelse(Experiment == "Experiment 2", .5, -.5)
  )

lmm_aware <- lmer(
  rt ~ val * acc_z + soa * cue + exp + (val + soa * cue | ID),
  data = d_aware,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
) # Maximal model
summary(lmm_aware)

aware_anova <- Anova(lmm_aware, type = 3) # ANOVA-like table
aware_anova

# Figures ----
fig_pooled <- plot_two_panels(
  plot_cells(cells_R, limits = c(330, 390)),
  plot_cells(cells_NR, limits = c(330, 390))
)
ggsave("output/figures/fig-5.png", fig_pooled, height = 9, width = 20, dpi = 600, units = "cm")

# IOR by bin (A) and the difference between high and low value targets (B). Green
# and purple, as red and blue are used for Cueing.
pal_value <- c(High = "#4DAF4A", Low = "#984EA3")
labels_value <- c(High = "High value", Low = "Low value")

plot_bins_main <- plot_bins(filter(pred_bins, Value != "Difference"), filter(ior_bins, Value != "Difference")) +
  scale_color_manual(values = pal_value, labels = labels_value) +
  scale_fill_manual(values = pal_value, labels = labels_value) +
  labs(x = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_bins_diff <- plot_bins(filter(pred_bins, Value == "Difference"), filter(ior_bins, Value == "Difference"), "Low - High (ms)") +
  scale_color_manual(values = c(Difference = "black"), guide = "none") +
  scale_fill_manual(values = c(Difference = "grey40"), guide = "none") +
  theme(strip.background = element_blank(), strip.text = element_blank())

fig_blocks <- (plot_bins_main / plot_bins_diff) +
  plot_layout(heights = c(3, 2), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
fig_blocks
ggsave("output/figures/fig-blocks.png", fig_blocks, height = 13, width = 16, dpi = 600, units = "cm")

save_analysis_objects(
  "analysis_pooled",
  aov_R_rt = aov_R_rt,
  cue_R = cue_R,
  rt_by_experiment_R = rt_by_experiment_R,
  rt_by_value_R = rt_by_value_R,
  value_by_soa_R = value_by_soa_R,
  aov_R_long = aov_R_long,
  bf_R_3way = bf_R_3way,
  bf_R_long = bf_R_long,
  acc_R_anova = acc_R_anova,
  err_R_experiment = err_R_experiment,
  aov_NR_rt = aov_NR_rt,
  cue_NR = cue_NR,
  rt_by_experiment = rt_by_experiment,
  cueing_by_experiment = cueing_by_experiment,
  aov_NR_long = aov_NR_long,
  aov_NR_short = aov_NR_short,
  ior_by_value_NR = ior_by_value_NR,
  acc_anova = acc_anova,
  err_NR_soa = err_NR_soa,
  err_NR_experiment = err_NR_experiment,
  aov_NR_ies = aov_NR_ies,
  cue_NR_ies = cue_NR_ies,
  aov_NR_long_ies = aov_NR_long_ies,
  aov_NR_short_ies = aov_NR_short_ies,
  ior_by_value_NR_ies = ior_by_value_NR_ies,
  aov_NR_all_ies = aov_NR_all_ies,
  aov_phase_rt = aov_phase_rt,
  aov_phase_long = aov_phase_long,
  bf_phase_4way = bf_phase_4way,
  bf_phase_long = bf_phase_long,
  bins_anova = bins_anova,
  pred_bins = pred_bins,
  bins_exp_anova = bins_exp_anova,
  diff_exp = diff_exp,
  aware_anova = aware_anova
)
