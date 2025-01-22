#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: GENERATING PROJECT OUTPUT
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

source("jobs/0-helperfunctions.R")
source("jobs/2-figures-functions.R")

org <- read_rds("clean_data/analytic_sample_org_elig_weights.rds") %>%
  filter(UHRSWORK1_PRED > 0) %>%
  mutate(RACEETH = case_when(RACEETH == "black" ~ "Black",
                             RACEETH == "white" ~ "White",
                             RACEETH == "latino" ~ "Latino",
                             RACEETH == "other" ~ "Other"), 
         EDUC2 = ifelse(EDUC == "ba.plus", "BA+", "<BA")
         )

asec <- read_rds("clean_data/analytic_sample_asec.rds") %>%
  filter(YEAR >= 1982)

# Main text figures
plot1 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"))

plot2 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"))

plot3 <- create_plot3(gen_outcome_sumstats(org))

plot4 <- create_plot4(gen_outcome_sumstats(org, use_age_group = T))

plot5 <- create_plot5(gen_outcome_sumstats(org))

plot6 <- create_plot3(gen_outcome_sumstats(org), scenario_type = "all")

# Appendix figures- main

plot6_age30 <- create_plot3(gen_outcome_sumstats(org), 
                      start_age = 30, scenario_type = "all")

plot6_ref60 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1960, scenario_type = "all")

plot6_ref62 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1962, scenario_type = "all")

plot6_hrscap <- create_plot3(gen_outcome_sumstats(org, outcome_var = "EARNHRLY_HRSCAP"),
                            outcome_label = "Hourly Wage, Hours Capped at 40 hrs/week",
                            scenario_type = "all")

plot6_race <- create_plot3(gen_outcome_sumstats(org, group_var = "RACEETH"),
                           group_var = "RACEETH",
                           scenario_type = "baseline")

plot6_educ <- create_plot3(gen_outcome_sumstats(org, group_var = "EDUC2"),
                           group_var = "EDUC2",
                           scenario_type = "all")

plot6_selection1 <- create_plot3(
  gen_outcome_sumstats(org, weight_var = "ELIGORG_PRED_WEIGHT"), 
  outcome_label = "Hourly Wage, Reweighted to Adjust for ORG eligibility, Cohort-Specific",
  scenario_type = "all")

plot6_selection2 <- create_plot3(
  gen_outcome_sumstats(org, weight_var = "ELIGORG_PRED_REFCOHORT_WEIGHT"),
  outcome_label = "Hourly Wage, Reweighted to Adjust for ORG eligibility, Reference Cohort",
  scenario_type = "all")

# Appendix figures- median
plot1_median <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.5),
  sumstat_label = "Median")

plot2_median <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.5),
  sumstat_label = "Median")

plot4_median <- create_plot4(
  gen_outcome_sumstats(org, use_age_group = T,
                       sumstat = "quantile",
                       probs = 0.5),
  sumstat_label = "Median")

plot5_median <- create_plot5(
  gen_outcome_sumstats(org, sumstat = "quantile",
                       probs = 0.5), sumstat_label = "Median")

# Appendix figures- ASEC

# Main text figures
plot1_asec <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE.TOPCODE",
                       weight_var = "ASECWT"),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings")

plot2_asec <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE.TOPCODE",
                       weight_var = "ASECWT"),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings")

plot3_asec <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label = "Wage and Salary Income, Prior Calendar Year")

plot4_asec <- create_plot4(gen_outcome_sumstats(
  asec, use_age_group = T, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label = "Earnings")

plot5_asec <- create_plot5(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label = "Earnings")

plot6 <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"),
  scenario_type = "all", data_source_label = "ASEC March Supplement",
  outcome_label = "Wage and Salary Income, Prior Calendar Year")


