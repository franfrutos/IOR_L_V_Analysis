## -----------------------------------------------------------------------------
## Author: Francisco Garre-Frutos
## -----------------------------------------------------------------------------

# Sensitivity power analysis (Morey, 2020): power to detect the SOA x Value x
# Cueing interaction (F test with 1 and N - 1 df) as a function of Cohen's f.

# Loading packages----
source("scripts/functions.R")

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(dplyr, ggplot2)

ensure_project_dirs()

power_f <- function(f, n, alpha = .05) {
  ncp <- f^2 * n
  1 - pf(qf(1 - alpha, 1, n - 1), 1, n - 1, ncp = ncp)
}

power_curves <- expand.grid(f = seq(0, .8, by = .01), n = c(40, 60, 100)) %>%
  mutate(power = power_f(f, n) * 100, N = factor(n))

# Smallest f with 80% power for each sample size
f_80 <- power_curves %>%
  filter(power >= 80) %>%
  group_by(n) %>%
  summarise(f = min(f), .groups = "drop")
f_80

fig_power <- ggplot(power_curves, aes(x = f, y = power, color = N)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_x_continuous(breaks = seq(0, .8, .05)) +
  labs(
    x = expression("Standardized Effect Size (Cohen's" ~ italic(f) * ")"),
    y = "Power (%)",
    color = "Sample Size"
  ) +
  theme_bw()
fig_power

ggsave("output/figures/fig-4.png", fig_power, height = 15, width = 20, dpi = 600, units = "cm")

save_analysis_objects(
  "power",
  f_80 = f_80
)
