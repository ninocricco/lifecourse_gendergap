#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: BOOTSTRAPPED ESTIMATES, APPENDIX FILE FOR ARRAY
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
  bootstrap_sample <- org %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(~ sample_n(.x, size = nrow(.x), replace = TRUE)) %>%
    ungroup()
  
  bs_ft <- org %>% filter(UHRSWORK1_PRED >= 35) %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(~ sample_n(.x, size = nrow(.x), replace = TRUE)) %>%
    ungroup()
  
  # Helper to avoid recomputing stats repeatedly
  stats_ft     <- gen_outcome_sumstats(bs_ft, use_birth_group = "BIRTHYEAR_DECADES")
  stats_base   <- gen_outcome_sumstats(bootstrap_sample)
  stats_quant  <- function(p) gen_outcome_sumstats(
    bootstrap_sample, use_birth_group = "BIRTHYEAR_DECADES",
    sumstat = "quantile", probs = p)
  stats_quant_f5  <- function(p) gen_outcome_sumstats(
    bootstrap_sample, sumstat = "quantile", probs = p)
  
  # Results ----------------------------------------------------------
  list(
    plot1_ft   = prepare_plot1_data(stats_ft),
    plot2_ft   = prepare_plot2_data(stats_ft),
    plot1_q10  = prepare_plot1_data(stats_quant(0.10)),
    plot2_q10  = prepare_plot2_data(stats_quant(0.10)),
    plot1_q25  = prepare_plot1_data(stats_quant(0.25)),
    plot1_q50  = prepare_plot1_data(stats_quant(0.50)),
    plot2_q50  = prepare_plot2_data(stats_quant(0.50)),
    plot1_q75  = prepare_plot1_data(stats_quant(0.75)),
    plot2_q75  = prepare_plot2_data(stats_quant(0.75)),
    plot1_q90  = prepare_plot1_data(stats_quant(0.90)),
    plot2_q90  = prepare_plot2_data(stats_quant(0.90)),
    plot3_age30_data = prepare_counterfactual_data(
      stats_base,start_age = 30)$data,
    plot3_ref60_data = prepare_counterfactual_data(
      stats_base,refcohort = 1960)$data,
    plot3_ref62_data = prepare_counterfactual_data(
      stats_base, refcohort = 1965)$data,
    plot3_hrscap_data = prepare_counterfactual_data(
      gen_outcome_sumstats(bootstrap_sample,
                           outcome_var = "EARNHRLY_HRSCAP"))$data,
    plot3_race = prepare_counterfactual_data(
      gen_outcome_sumstats(
        org %>% group_by(across(all_of(c(group_vars,"RACEETH")))) %>%
          group_modify(~ sample_n(.x,nrow(.x),replace = TRUE)) %>% ungroup(),
        group_var = "RACEETH"),
      group_var = "RACEETH")$data,
    plot3_educ = prepare_counterfactual_data(
      gen_outcome_sumstats(
        org %>% group_by(across(all_of(c(group_vars,"EDUC2")))) %>%
          group_modify(~ sample_n(.x,nrow(.x),replace = TRUE)) %>% ungroup(),
        group_var = "EDUC2"),
      group_var = "EDUC2")$data,
    plot3_selection1 = prepare_counterfactual_data(
      gen_outcome_sumstats(bootstrap_sample,
                           weight_var = "ELIGORG_PRED_WEIGHT"))$data,
    plot3_selection2 = prepare_counterfactual_data(
      gen_outcome_sumstats(bootstrap_sample,
                           weight_var = "ELIGORG_PRED_REFCOHORT_WEIGHT"))$data
  )
}

## ---- run the chunk ----------------------------------------------
out <- lapply(start_i:end_i, do_one)
names(out) <- as.character(start_i:end_i)

saveRDS(out, file = outfile, compress = "xz")
cat("Saved", outfile, "\n")