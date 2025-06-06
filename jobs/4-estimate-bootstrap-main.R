#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: BOOTSTRAPPED ESTIMATES, MAIN ANALYSIS FILE FOR ARRAY
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

org <- read_rds("clean_data/analytic_sample_org_elig_weights.rds") %>%
  mutate(UHRSWORK1_PRED = as.numeric(zap_labels(UHRSWORK1_PRED))) %>%
  filter(UHRSWORK1_PRED > 0)

group_vars <- c("AGE", "YEAR", "FEMALE")

## ---- helper: one iteration --------------------------------------
do_one <- function(i) {
  set.seed(i)
  
  # Bootstrap samples ------------------------------------------------
  # Create bootstrap sample
  bootstrap_sample <- org %>%
    group_by(across(all_of(group_vars))) %>%
    # Sample with replacement within each group
    group_modify(~ {
      sample_n(.x, size = nrow(.x), replace = TRUE)
    }) %>%
    ungroup()
  
  # Process data for different plots
  data_descplot <- gen_outcome_sumstats(bootstrap_sample, use_birth_group = "BIRTHYEAR_DECADES")
  data_restplot <- gen_outcome_sumstats(bootstrap_sample)
  
  list(
    plot1_data <- prepare_plot1_data(data_descplot),
    plot2_data <- prepare_plot2_data(data_descplot),
    plotcount_data<- prepare_counterfactual_data(data_restplot, 
                                                 scenarios_to_include = "appendix")$data
  )
}

## ---- run the chunk ----------------------------------------------
out <- lapply(start_i:end_i, do_one)
names(out) <- as.character(start_i:end_i)

saveRDS(out, file = outfile, compress = "xz")
cat("Saved", outfile, "\n")