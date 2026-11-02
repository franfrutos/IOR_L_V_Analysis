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
       lmerTest, # P-values in summary
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

# Accuracy: logistic mixed model
d_glm <- raw %>%
  filter(Block %in% 9:10) %>%
  mutate(across(c(SOA, Value, Cueing, Experiment), factor))
contrasts(d_glm$SOA) <- contr.sum(2) / 2
contrasts(d_glm$Value) <- contr.sum(2) / 2
contrasts(d_glm$Cueing) <- contr.sum(2) / 2
contrasts(d_glm$Experiment) <- contr.sum(2) / 2

glmm_NR_acc <- glmer(
  correct ~ SOA * Value * Cueing * Experiment + (1 | ID),
  data = d_glm,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)
summary(glmm_NR_acc)
acc_terms <- as.data.frame(coef(summary(glmm_NR_acc)))

# Predicted proportion of errors by SOA and by Experiment
predicted_errors <- function(spec) {
  as.data.frame(emmeans(glmm_NR_acc, spec, type = "response")) %>%
    transmute(
      level = .data[[all.vars(spec)[1]]],
      emmean = 1 - prob, lower.CL = 1 - asymp.UCL, upper.CL = 1 - asymp.LCL
    )
}
err_NR_soa <- predicted_errors(~ SOA)
err_NR_experiment <- predicted_errors(~ Experiment)
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

# Reward phase of both experiments ----
cells_R <- bind_rows(
  cell_means(d_e1, 1:8, by = by_cells),
  cell_means(d_e2, 2:8, by = by_cells)
)

aov_R_rt <- rm_anova(cells_R, "RT", within, between = "Experiment")
aov_R_rt
aov_R_long <- rm_anova(filter(cells_R, SOA == "Long"), "RT", c("Value", "Cueing"), between = "Experiment")
aov_R_long

bf_R_3way <- bf_contrast(interaction_scores(cells_R, "RT")$three_way)
bf_R_long <- bf_contrast(ior_scores(cells_R, "RT")$IOR_diff)
bf_R_3way
bf_R_long # BF01 > 3

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

lmm_bins <- lmer(
  rt ~ cue * val * (bin_rel + phase) + exp + (cue * val + bin_rel + phase || ID),
  data = d_bins,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)
summary(lmm_bins)

bins_terms <- as.data.frame(coef(summary(lmm_bins)))[
  c("cue", "cue:bin_rel", "cue:phase", "cue:val", "cue:val:bin_rel", "cue:val:phase"),
]
bins_terms

# Model predictions of IOR for each target value and of their difference (Low -
# High) by bin (fixed effects, Wald 95% CI)
predict_bins <- function(model, bins) {
  b <- fixef(model)
  V <- as.matrix(vcov(model))
  grid <- expand.grid(Bin = bins, Value = c("High", "Low", "Difference"), stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    phase <- as.numeric(grid$Bin[i] > 4.5)
    bin_rel <- min(grid$Bin[i], 4) - 4
    L <- setNames(rep(0, length(b)), names(b))
    if (grid$Value[i] == "Difference") {
      L[c("cue:val", "cue:val:bin_rel", "cue:val:phase")] <- c(1, bin_rel, phase)
    } else {
      v <- if (grid$Value[i] == "Low") .5 else -.5
      L[c("cue", "cue:val", "cue:bin_rel", "cue:val:bin_rel", "cue:phase", "cue:val:phase")] <-
        c(1, v, bin_rel, v * bin_rel, phase, v * phase)
    }
    est <- sum(L * b)
    se <- sqrt(drop(t(L) %*% V %*% L))
    data.frame(Bin = grid$Bin[i], Value = grid$Value[i], estimate = est,
               conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)
  }))
}

pred_bins <- predict_bins(lmm_bins, c(seq(1, 4, by = .05), 5))

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
  rt ~ val * acc_z + soa * cue + exp + (val | ID),
  data = d_aware,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)
summary(lmm_aware)
aware_terms <- as.data.frame(coef(summary(lmm_aware)))[c("val", "val:acc_z"), ]
aware_terms

# Figures ----
fig_pooled <- plot_cells(cells_NR, limits = c(320, 400))
ggsave("output/figures/fig-5.png", fig_pooled, height = 9, width = 14, dpi = 600, units = "cm")

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
  aov_NR_rt = aov_NR_rt,
  cue_NR = cue_NR,
  rt_by_experiment = rt_by_experiment,
  cueing_by_experiment = cueing_by_experiment,
  aov_NR_long = aov_NR_long,
  aov_NR_short = aov_NR_short,
  ior_by_value_NR = ior_by_value_NR,
  acc_terms = acc_terms,
  err_NR_soa = err_NR_soa,
  err_NR_experiment = err_NR_experiment,
  aov_NR_ies = aov_NR_ies,
  cue_NR_ies = cue_NR_ies,
  aov_NR_long_ies = aov_NR_long_ies,
  aov_NR_short_ies = aov_NR_short_ies,
  ior_by_value_NR_ies = ior_by_value_NR_ies,
  aov_NR_all_ies = aov_NR_all_ies,
  aov_R_rt = aov_R_rt,
  aov_R_long = aov_R_long,
  bf_R_3way = bf_R_3way,
  bf_R_long = bf_R_long,
  aov_phase_rt = aov_phase_rt,
  aov_phase_long = aov_phase_long,
  bf_phase_4way = bf_phase_4way,
  bf_phase_long = bf_phase_long,
  bins_terms = bins_terms,
  pred_bins = pred_bins,
  aware_terms = aware_terms
)
