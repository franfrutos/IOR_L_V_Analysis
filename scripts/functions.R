## -----------------------------------------------------------------------------
## Author: Francisco Garre-Frutos
## -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "afex", "emmeans", "BayesFactor", "ggplot2", "patchwork",
  "Rmisc"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "Missing required packages: %s",
      paste(missing_packages, collapse = ", ")
    )
  )
}

# Rmisc is only called with :: because it attaches plyr, which masks dplyr
invisible(lapply(setdiff(required_packages, "Rmisc"), library, character.only = TRUE))

afex_options(include_aov = TRUE) # Needed by emmeans for the contrasts
emm_options(lmer.df = "asymptotic")

# Data functions ----
# Cell means per participant. RT: correct responses within 200-1200 ms. IES: mean
# RT of all the responses within the window / proportion of correct responses.
cell_means <- function(d, blocks, by = c("SOA", "Value", "Cueing")) {
  d %>%
    filter(Block %in% blocks, rt >= 200, rt <= 1200) %>%
    group_by(across(all_of(c("ID", by)))) %>%
    summarise(
      RT = mean(rt[correct == 1]),
      ACC = mean(correct),
      Error = 1 - ACC,
      IES = mean(rt) / ACC,
      .groups = "drop"
    ) %>%
    mutate(across(any_of(c("SOA", "Value", "Cueing", "Experiment", "Phase")), factor))
}

# Repeated measures ANOVA (partial eta squared as effect size)
rm_anova <- function(data, dv, within, between = NULL) {
  suppressMessages(
    aov_ez(
      id = "ID",
      dv = dv,
      data = data,
      within = within,
      between = between,
      anova_table = list(es = "pes")
    )
  )
}

# Cueing effect (Uncued - Cued) within each level of `by`. Facilitation (short
# SOA) and IOR (long SOA) are tested in the preregistered direction.
cueing_effects <- function(model, by = "SOA", sides = c(Short = ">", Long = "<")) {
  emm <- emmeans(model, as.formula(paste("~ Cueing |", by)))
  con <- as.data.frame(summary(pairs(emm), infer = TRUE))
  out <- lapply(seq_len(nrow(con)), function(i) {
    lvl <- as.character(con[[by]][i])
    side <- if (lvl %in% names(sides)) sides[[lvl]] else "="
    s <- as.data.frame(summary(pairs(emm)[i], infer = TRUE, side = side))
    data.frame(
      level = lvl,
      side = side,
      estimate = s$estimate,
      se = s$SE,
      df = s$df,
      t = s$t.ratio,
      p = s$p.value,
      p_two = con$p.value[i],
      lower = s$lower.CL,
      upper = s$upper.CL
    )
  })
  do.call(rbind, out)
}

# IOR (Cued - Uncued) per participant for each target value at the long SOA,
# and IOR_diff = IOR_Low - IOR_High (positive: smaller IOR for high value)
ior_scores <- function(cells, dv = "RT") {
  cells %>%
    filter(SOA == "Long") %>%
    select(any_of(c("ID", "Experiment")), Value, Cueing, y = all_of(dv)) %>%
    pivot_wider(names_from = c(Value, Cueing), values_from = y) %>%
    mutate(
      IOR_High = High_Cued - High_Uncued,
      IOR_Low = Low_Cued - Low_Uncued,
      IOR_diff = IOR_Low - IOR_High
    )
}

# SOA x Value x Cueing interaction score per participant (t^2 = ANOVA F)
interaction_scores <- function(cells, dv = "RT") {
  cells %>%
    select(any_of(c("ID", "Experiment")), SOA, Value, Cueing, y = all_of(dv)) %>%
    pivot_wider(names_from = c(SOA, Value, Cueing), values_from = y) %>%
    mutate(
      long = (Long_Low_Cued - Long_Low_Uncued) - (Long_High_Cued - Long_High_Uncued),
      short = (Short_Low_Cued - Short_Low_Uncued) - (Short_High_Cued - Short_High_Uncued),
      three_way = long - short
    )
}

# Default Bayesian t test (Cauchy prior, r = sqrt(2)/2) on a contrast score
bf_contrast <- function(x, rscale = "medium") {
  x <- x[!is.na(x)]
  bf <- extractBF(ttestBF(x, mu = 0, rscale = rscale))$bf
  tt <- t.test(x)
  data.frame(
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p = tt$p.value,
    BF10 = bf,
    BF01 = 1 / bf
  )
}

# Final earnings: points converted to euros and rounded up to the next 0.50
# euros (maximum of 10 euros)
earnings <- function(points, points_per_euro) {
  pmin(10, ceiling(2 * points / points_per_euro) / 2)
}

# Preregistered awareness analysis: correlation between recognition accuracy and
# the effect of value in the reward phase (RT low - high value targets)
awareness_correlation <- function(awareness, cells_reward) {
  d <- cells_reward %>%
    group_by(ID, Value) %>%
    summarise(RT = mean(RT), .groups = "drop") %>%
    pivot_wider(names_from = Value, values_from = RT) %>%
    transmute(ID = as.character(ID), capture = Low - High) %>%
    inner_join(mutate(awareness, ID = as.character(ID)), by = "ID")

  test <- cor.test(d$recognition_acc, d$capture)
  data.frame(r = unname(test$estimate), df = unname(test$parameter), p = test$p.value)
}

# Plotting ----
theme_ior <- function() {
  font <- "sans"
  theme_bw() %+replace%
    theme(
      axis.title.x = element_text(family = font, size = 11, face = "bold"),
      axis.text = element_text(family = font, size = 9),
      axis.text.x = element_text(margin = margin(5, b = 10)),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0),
        angle = 90, family = font, size = 11
      ),
      strip.background = element_rect(fill = "grey95"),
      panel.grid.minor = element_blank()
    )
}

# Mean RT by Value, Cueing and SOA with Cousineau-Morey 95% CIs
plot_cells <- function(cells, dv = "RT", ylab = "RT (ms)", limits = NULL) {
  g <- Rmisc::summarySEwithin(
    as.data.frame(cells),
    measurevar = dv,
    withinvars = c("SOA", "Cueing", "Value"),
    idvar = "ID"
  )
  g$SOA <- factor(g$SOA, levels = c("Short", "Long"), labels = c("Short SOA", "Long SOA"))
  g$Cueing <- factor(g$Cueing, levels = c("Cued", "Uncued"))
  g$y <- g[[dv]]
  pd <- position_dodge(0.2)
  p <- ggplot(g, aes(x = Value, y = y, color = Cueing, group = Cueing)) +
    geom_errorbar(aes(ymin = y - ci, ymax = y + ci), width = .1, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd) +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Target value", y = ylab, color = NULL) +
    facet_grid(. ~ SOA) +
    theme_ior()
  if (!is.null(limits)) {
    p <- p + coord_cartesian(ylim = limits)
  }
  p
}

# Two panels (A and B) with a shared legend
plot_two_panels <- function(p1, p2) {
  (p1 | p2) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom", plot.tag = element_text(face = "bold"))
}

# IOR by bin (observed mean and SE, and model predictions with 95% CIs), with the
# reward bins (1-4) and the no-reward bin (5) in separate panels
plot_bins <- function(pred, obs, ylab = "IOR (ms)") {
  phase <- function(d) {
    mutate(d, Phase = factor(ifelse(Bin > 4.5, "No reward", "Reward"), levels = c("Reward", "No reward")))
  }
  pred <- phase(pred)
  obs <- phase(obs)
  pd <- position_dodge(0.2)
  ggplot(mapping = aes(x = Bin, y = estimate, color = Value, fill = Value, group = Value)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(data = filter(pred, Bin <= 4), aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    geom_line(data = filter(pred, Bin <= 4)) +
    geom_rect(
      data = filter(pred, Bin == 5),
      aes(xmin = Bin - .2, xmax = Bin + .2, ymin = conf.low, ymax = conf.high, fill = Value),
      alpha = 0.2, color = NA, inherit.aes = FALSE
    ) +
    geom_segment(
      data = filter(pred, Bin == 5),
      aes(x = Bin - .2, xend = Bin + .2, y = estimate, yend = estimate, color = Value),
      inherit.aes = FALSE
    ) +
    geom_errorbar(data = obs, aes(ymin = estimate - se, ymax = estimate + se), width = .1, position = pd) +
    geom_point(data = obs, position = pd) +
    scale_x_continuous(breaks = 1:5, expand = expansion(add = .4)) +
    facet_grid(. ~ Phase, scales = "free_x", space = "free_x") +
    labs(x = "Bin", y = ylab, color = NULL, fill = NULL) +
    theme_ior()
}

# Reporting functions (inline statistics of the manuscript) ----
format_p_latex <- function(p, digits = 3) {
  if (is.na(p)) {
    return("NA")
  }
  threshold <- 10^(-digits)
  if (p < threshold) {
    return(paste0("< .", paste(rep("0", digits - 1), collapse = ""), "1"))
  }
  out <- formatC(p, digits = digits, format = "f")
  paste0("= ", sub("^0", "", out))
}

format_num_latex <- function(x, digits = 2, drop_leading_zero = FALSE) {
  if (is.na(x)) {
    return("NA")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "\\infty" else "-\\infty")
  }
  out <- formatC(x, digits = digits, format = "f")
  if (drop_leading_zero) {
    out <- sub("^0", "", out)
    out <- sub("^-0", "-", out)
  }
  out
}

extract_afex_term <- function(model, term) {
  anova_table <- model$anova_table
  if (is.null(anova_table) || !term %in% rownames(anova_table)) {
    stop(sprintf("Term '%s' was not found in the afex ANOVA table.", term))
  }

  out <- as.data.frame(anova_table[term, , drop = FALSE])
  out$term <- term
  out$num_df <- out$`num Df`
  out$den_df <- out$`den Df`
  out$f_value <- out$F
  out$mse <- out$MSE
  out$pes <- out$pes
  out$p_value <- out$`Pr(>F)`

  out[, c("term", "num_df", "den_df", "f_value", "mse", "pes", "p_value")]
}

latex_afex_term <- function(model,
                            term,
                            effect_label = NULL,
                            include_label = FALSE,
                            f_digits = 2,
                            df_digits = 0,
                            pes_digits = 3) {
  res <- extract_afex_term(model, term)
  effect_label <- if (is.null(effect_label)) term else effect_label
  # Greenhouse-Geisser corrected df are not integers
  df_digits <- if (res$num_df %% 1 != 0) 2 else df_digits

  stats_text <- paste0(
    "$F(",
    format_num_latex(res$num_df, digits = df_digits), ", ",
    format_num_latex(res$den_df, digits = df_digits),
    ") = ",
    format_num_latex(res$f_value, digits = f_digits),
    "$, $p ",
    format_p_latex(res$p_value),
    "$, $\\eta_p^2 ",
    if (res$pes < .0005) "< .001" else paste("=", format_num_latex(res$pes, digits = pes_digits, drop_leading_zero = TRUE)),
    "$"
  )

  if (include_label) {
    paste0(effect_label, " ", stats_text)
  } else {
    stats_text
  }
}

# One row of cueing_effects()
latex_contrast <- function(row, digits = 2, two_tailed = FALSE) {
  p <- if (two_tailed) row$p_two else row$p
  paste0(
    "$\\Delta M = ", format_num_latex(row$estimate, digits), "$, ",
    "95\\% CI $[", format_num_latex(row$lower, digits), ", ",
    format_num_latex(row$upper, digits), "]$, ",
    "$t(", format_num_latex(row$df, 0), ") = ", format_num_latex(row$t, digits), "$, ",
    "$p ", format_p_latex(p), "$"
  )
}

# Smallest p value among some terms of an afex ANOVA (`terms`), or among all the
# terms except some (`exclude`), rounded down (for "ps > ...")
min_p <- function(model, terms = NULL, exclude = NULL) {
  tab <- model$anova_table
  keep <- if (!is.null(terms)) rownames(tab) %in% terms else !rownames(tab) %in% exclude
  p <- min(tab[keep, "Pr(>F)"])
  paste0("> ", sub("^0", "", formatC(floor(p * 100) / 100, digits = 2, format = "f")))
}

# One row of emmeans pairwise comparisons
latex_emm_row <- function(row, digits = 2, ci = FALSE) {
  ci_part <- if (ci) {
    paste0(
      "95\\% CI $[", format_num_latex(row$lower.CL, digits), ", ",
      format_num_latex(row$upper.CL, digits), "]$, "
    )
  } else {
    ""
  }
  paste0(
    "$\\Delta M = ", format_num_latex(row$estimate, digits), "$, ", ci_part,
    "$t(", format_num_latex(row$df, 0), ") = ", format_num_latex(row$t.ratio, digits), "$, ",
    "$p ", format_p_latex(row$p.value), "$"
  )
}

# GLMM coefficient (Wald 95% CI)
latex_glmm_term <- function(terms, term, digits = 2) {
  b <- terms[term, "Estimate"]
  se <- terms[term, "Std. Error"]
  paste0(
    "$\\hat{\\beta} = ", format_num_latex(b, digits), "$, ",
    "95\\% CI $[", format_num_latex(b - 1.96 * se, digits), ", ", format_num_latex(b + 1.96 * se, digits), "]$, ",
    "$z = ", format_num_latex(terms[term, "z value"], digits), "$, ",
    "$p ", format_p_latex(terms[term, "Pr(>|z|)"]), "$"
  )
}

# LMM coefficient (lmerTest, Satterthwaite df)
latex_lmm_beta <- function(terms, term, label, digits = 2) {
  paste0(
    "$\\beta_{", label, "} = ", format_num_latex(terms[term, "Estimate"], digits), "$, ",
    "$t_{", format_num_latex(terms[term, "df"], 0), "} = ", format_num_latex(terms[term, "t value"], digits), "$, ",
    "$p ", format_p_latex(terms[term, "Pr(>|t|)"]), "$"
  )
}

# Estimated mean and 95% CI (e.g., proportion of errors)
latex_mean_ci <- function(row, digits = 3) {
  paste0(
    "$M = ", format_num_latex(row$emmean, digits, drop_leading_zero = TRUE), "$, ",
    "95\\% CI $[", format_num_latex(row$lower.CL, digits, drop_leading_zero = TRUE), ", ",
    format_num_latex(row$upper.CL, digits, drop_leading_zero = TRUE), "]$"
  )
}

latex_bf <- function(bf_row, null = FALSE, digits = 2) {
  if (null) {
    paste0("$\\text{BF}_{01} = ", format_num_latex(bf_row$BF01, digits), "$")
  } else {
    paste0("$\\text{BF}_{10} = ", format_num_latex(bf_row$BF10, digits), "$")
  }
}

# Saving and loading the results ----
analysis_results_path <- function(name, dir = "output/analysis") {
  file.path(dir, paste0(name, ".rds"))
}

ensure_project_dirs <- function() {
  dirs <- c("output/analysis", "output/figures", "output/manuscript")
  invisible(lapply(dirs, function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }))
}

# afex objects keep the environment of their formula (with all the data), so
# only their ANOVA table is saved
slim_object <- function(x) {
  if (inherits(x, "afex_aov")) {
    return(list(anova_table = x$anova_table))
  }
  x
}

save_analysis_objects <- function(name, ..., dir = "output/analysis") {
  objects <- lapply(list(...), slim_object)
  if (is.null(names(objects)) || any(names(objects) == "")) {
    stop("All objects passed to save_analysis_objects() must be named.")
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  saveRDS(objects, analysis_results_path(name, dir = dir))
  invisible(objects)
}

load_analysis_objects <- function(name, dir = "output/analysis") {
  path <- analysis_results_path(name, dir = dir)
  if (!file.exists(path)) {
    stop(sprintf("Could not find saved analysis objects at '%s'.", path))
  }
  readRDS(path)
}
