## -----------------------------------------------------------------------------
## Author: Francisco Garre-Frutos
## -----------------------------------------------------------------------------

# Installing (if needed) and loading the packages ----
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(here, dplyr, tidyr)

setwd(here::here())

# Trial exclusions by phase: responses outside the 200-1200 ms window and
# incorrect responses (of the remaining ones). The data of Experiment 1 were
# already trimmed to the window, so those trials are counted as missing.
report_exclusions <- function(d, expected) {
  d %>%
    filter(Phase %in% names(expected)) %>%
    group_by(Phase) %>%
    summarise(
      n_expected = n_distinct(ID) * expected[[first(Phase)]],
      n_window = sum(rt >= 200 & rt <= 1200),
      outliers = (1 - n_window / n_expected) * 100,
      incorrect = mean(correct[rt >= 200 & rt <= 1200] == 0) * 100,
      .groups = "drop"
    ) %>%
    select(Phase, outliers, incorrect)
}

# The block counter logged by OpenSesame is updated one trial late, so blocks are
# computed from the trial counter of each phase (64 trials per block). Repeated
# log lines of the same trial are removed.

# Experiment 1 ----
# Practice, 8 reward blocks (1-8), 2 no-reward blocks (9-10) and awareness test
raw1_full <- read.csv("Input/data_e1.csv", na.strings = "None")

raw1 <- raw1_full %>%
  filter(pract == "no", recognition == "no", !is.na(value)) %>%
  mutate(
    Phase = case_when(
      noreward == "no" & trial_count_rew > 0 ~ "Reward",
      noreward == "yes" & trial_count_norew > 0 ~ "No-reward"
    ),
    Trial = ifelse(Phase == "Reward", trial_count_rew, trial_count_norew),
    Block = case_when(
      Phase == "Reward" ~ ceiling(Trial / 64),
      Phase == "No-reward" ~ 8 + ceiling(Trial / 64)
    )
  ) %>%
  filter(!is.na(Phase)) %>%
  distinct(code, Phase, Trial, .keep_all = TRUE) %>%
  transmute(
    ID = factor(code),
    Experiment = "Experiment 1",
    Phase,
    Block,
    Trial,
    SOA = ifelse(SOA == 50, "Short", "Long"),
    Value = ifelse(value == 10, "High", "Low"),
    Cueing = factor(cueing, levels = c("Uncued", "Cued")),
    Color = color,
    rt = response_time_keyboard_response,
    correct = correct_keyboard_response,
    points = counter_value
  )

# Check
# Trials per participant and phase (512 reward and 128 no-reward trials, minus
# the responses outside the 200-1200 ms window)
trials1 <- raw1 %>% count(ID, Phase)
length(unique(raw1$ID)) # N = 40 participants
table(trials1$Phase, trials1$n)

# Experiment 2 ----
# Practice, calibration block (block 1, without points, used to set the RT
# threshold), 7 reward blocks (2-8), 4 no-reward blocks (9-12) and awareness test
raw2_full <- read.csv("Input/data_e2.csv", na.strings = "None")

raw2 <- raw2_full %>%
  filter(recognition == "no", !is.na(value)) %>%
  mutate(
    Phase = case_when(
      pract == "yes" & block_count == 1 ~ "Calibration",
      pract == "no" & noreward == "no" & trial_count_rew > 0 ~ "Reward",
      pract == "no" & noreward == "yes" & trial_count_norew > 0 ~ "No-reward"
    ),
    Trial = case_when(
      Phase == "Calibration" ~ trial_count,
      Phase == "Reward" ~ trial_count_rew,
      Phase == "No-reward" ~ trial_count_norew
    ),
    Block = case_when(
      Phase == "Calibration" ~ 1,
      Phase == "Reward" ~ 1 + ceiling(Trial / 64),
      Phase == "No-reward" ~ 8 + ceiling(Trial / 64)
    )
  ) %>%
  filter(!is.na(Phase)) %>%
  distinct(code, Phase, Trial, .keep_all = TRUE) %>%
  transmute(
    ID = factor(code),
    Experiment = "Experiment 2",
    Phase,
    Block,
    Trial,
    SOA = ifelse(SOA == 50, "Short", "Long"),
    Value = ifelse(value == 10, "High", "Low"),
    Cueing = factor(cueing, levels = c("Uncued", "Cued")),
    Color = color,
    rt = response_time_keyboard_response,
    correct = correct_keyboard_response,
    points = counter_value,
    threshold = cut_rt_above,
    slow = 1 - slow_response
  )

# Check
# Trials per participant and phase (64 calibration, 448 reward and 256 no-reward)
trials2 <- raw2 %>% count(ID, Phase)
length(unique(raw2$ID)) # N = 60 participants
table(trials2$Phase, trials2$n)

# Awareness test ----
# Recognition: 32 sequences, indicating whether they would have earned 10 or 1
# points. Q2: explicit question about the rule (correct = the high-value color,
# regardless of the letter).
get_awareness <- function(d_full, experiment) {
  ids <- unique(d_full$code)
  # 32 sequences (the logger repeats the last one)
  recognition <- d_full %>%
    filter(recognition == "yes", !is.na(correct_recognition_response)) %>%
    group_by(code) %>%
    slice_head(n = 32) %>%
    summarise(recognition_acc = mean(correct_recognition_response), .groups = "drop")
  q2 <- d_full %>%
    filter(!is.na(correct_second_question_response), recognition == "yes" | noreward == "yes") %>%
    group_by(code) %>%
    summarise(Q2 = max(correct_second_question_response), .groups = "drop")
  tibble(code = ids) %>%
    left_join(recognition, by = "code") %>%
    left_join(q2, by = "code") %>%
    transmute(ID = factor(code), Experiment = experiment, recognition_acc, Q2)
}

awareness1 <- get_awareness(raw1_full, "Experiment 1")
awareness2 <- get_awareness(raw2_full, "Experiment 2")

# Report trial exclusions:
exclusions1 <- report_exclusions(raw1, c(Reward = 512, `No-reward` = 128))
exclusions2 <- report_exclusions(raw2, c(Reward = 448, `No-reward` = 256))
print("Experiment 1:")
exclusions1
print("Experiment 2:")
exclusions2
