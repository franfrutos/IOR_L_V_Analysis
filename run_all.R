## -----------------------------------------------------------------------------
## Author: Francisco Garre-Frutos
## -----------------------------------------------------------------------------

# Edit to run the analyses again even if cached results exist
force_main <- FALSE

dirs <- c("output/analysis", "output/figures", "output/manuscript")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

run_cached <- function(name, script) {
  if (force_main || !file.exists(file.path("output/analysis", paste0(name, ".rds")))) {
    message("Running ", script, "...")
    source(script, local = new.env())
  } else {
    message("Using cached results: output/analysis/", name, ".rds")
  }
}

# Results from Experiment 1
run_cached("analysis_e1", "scripts/analysis_e1.R")

# Results from Experiment 2
run_cached("analysis_e2", "scripts/analysis_e2.R")

# Pooled analyses, comparison between phases and time course across blocks
run_cached("analysis_pooled", "scripts/analysis_pooled.R")

# Meta-analysis
run_cached("meta_analysis", "scripts/meta_analysis.R")

# Sensitivity power analysis
run_cached("power", "scripts/power.R")

# Versions of R and packages used to generate the results
writeLines(capture.output(sessionInfo()), "output/session_info.txt")

message("Pipeline completed.")
