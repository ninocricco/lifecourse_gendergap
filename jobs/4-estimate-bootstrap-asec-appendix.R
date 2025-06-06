#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: BOOTSTRAPPED ESTIMATES, ASEC APPENDIX FILE FOR ARRAY
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3)
  stop("Need exactly 3 args: <start> <end> <outfile>")

start_i <- as.integer(args[1])
end_i   <- as.integer(args[2])
outfile <- args[3]

cat("Running iterations", start_i, "to", end_i, "→", outfile, "\n")
## ---- setup -------------------------------------------------------
suppressPackageStartupMessages({
  source("jobs/3-data-prep-functions.R")
})

asec <- read_rds("clean_data/analytic_sample_asec.rds") %>%
  filter(YEAR >= 1982)

group_vars <- c("AGE", "YEAR", "FEMALE")

## ---- helper: one iteration --------------------------------------
do_one <- function(i) {
  set.seed(i)
  
  # Bootstrap samples ------------------------------------------------
  # Create bootstrap sample
  bootstrap_sample <- asec %>%
    group_by(across(all_of(group_vars))) %>%
    # Sample with replacement within each group
    group_modify(~ {
      sample_n(.x, size = nrow(.x), replace = TRUE)
    }) %>%
    ungroup()
  
  # Process data for different plots
  data_descplot <- gen_outcome_sumstats(
    bootstrap_sample, use_birth_group = "BIRTHYEAR_DECADES",
    outcome_var = "INCWAGE", weight_var = "ASECWT")
  
  
  bs_q5 = gen_outcome_sumstats(
    bootstrap_sample, use_birth_group = "BIRTHYEAR_DECADES",
    outcome_var = "INCWAGE", weight_var = "ASECWT",
    sumstat = "quantile", probs = 0.5)
  # Store results in named lists (using string names ensures proper indexing)
  list(
  plot1_asec = prepare_plot1_data(data_descplot),
  plot2_asec= prepare_plot2_data(data_descplot),
  data_restplot <- gen_outcome_sumstats(bootstrap_sample, 
                                        outcome_var = "INCWAGE",
                                        weight_var = "ASECWT"),
  plot1_asec_median = prepare_plot1_data(bs_q5),
  plot2_asec_median = prepare_plot2_data(bs_q5)
  )
}

## ---- run the chunk ----------------------------------------------
out <- lapply(start_i:end_i, do_one)
names(out) <- as.character(start_i:end_i)

saveRDS(out, file = outfile, compress = "xz")
cat("Saved", outfile, "\n")