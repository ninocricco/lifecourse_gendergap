#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: GENERATING PROJECT OUTPUT
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

source("jobs/0-helperfunctions.R")
source("jobs/2-figures-functions.R")

org <- read_rds("clean_data/analytic_sample_org_elig_weights.rds") %>%
  mutate(UHRSWORK1_PRED = as.numeric(zap_labels(UHRSWORK1_PRED))) %>%
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
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"),
  title = "", caption = "", with_bootstrap = T, conf_level = .95)

plot2 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"),
  title = "", caption = "", with_bootstrap = T, conf_level = .95)

plot3 <- create_plot3(gen_outcome_sumstats(org),
                      title = "", caption = "", with_bootstrap = T, conf_level = .95)

plot4 <- create_plot4(gen_outcome_sumstats(org, use_age_group = T),
                      use_grayscale = F,
                      title = "", caption = "", with_bootstrap = T, conf_level = .95)

plot5 <- create_plot5(gen_outcome_sumstats(org), use_grayscale = F,
                      title = "", caption = "", with_bootstrap = T, conf_level = .95)

plot6 <- create_plot3(gen_outcome_sumstats(org), scenario_type = "all",
                      title = "", caption = "", with_bootstrap = T, conf_level = .95)

plots_main <- list(plot1, plot2, plot3, plot4, plot5, plot6)

for (i in seq_along(plots_main)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_main", paste0("plot_", i, ".pdf")),
    plot     = plots_main[[i]],
    device   = "pdf",
    width    = 10,
    height   = 6,
    units    = "in"
  )
}



# Appendix figures- main

plot6_age30 <- create_plot3(gen_outcome_sumstats(org), 
                      start_age = 30, scenario_type = "all",
                      title = "", caption = "")

plot6_ref60 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1960, scenario_type = "all",
                            title = "", caption = "")

plot6_ref62 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1962, scenario_type = "all",
                            title = "", caption = "")

plot6_hrscap <- create_plot3(gen_outcome_sumstats(org, outcome_var = "EARNHRLY_HRSCAP"),
                             outcome_label_axis = "Hourly Wage",
                            outcome_label_caption = "Hourly Wage, Hours Capped at 40 hrs/week",
                            scenario_type = "all", 
                            title = "", caption = "")

plot6_race <- create_plot3(gen_outcome_sumstats(org, group_var = "RACEETH"),
                           group_var = "RACEETH",
                           scenario_type = "all",
                           title = "", caption = "")

plot6_educ <- create_plot3(gen_outcome_sumstats(org, group_var = "EDUC2"),
                           group_var = "EDUC2",
                           scenario_type = "all")

plot6_selection1 <- create_plot3(
  gen_outcome_sumstats(org, weight_var = "ELIGORG_PRED_WEIGHT"), 
  outcome_label_axis = "Hourly Wage",
  outcome_label_caption = "Hourly Wage, Reweighted to Adjust for ORG eligibility, Cohort-Specific",
  scenario_type = "all",
  title = "", caption = "")

plot6_selection2 <- create_plot3(
  gen_outcome_sumstats(org, weight_var = "ELIGORG_PRED_REFCOHORT_WEIGHT"),
  outcome_label_axis = "Hourly Wage",
  outcome_label_caption = "Hourly Wage, Reweighted to Adjust for ORG eligibility, Reference Cohort",
  scenario_type = "all",
  title = "", caption = "")

# Appendix figures- ASEC

# Main text figures
plot1_asec <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE.TOPCODE",
                       weight_var = "ASECWT"),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",
  title = "", caption = "")

plot2_asec <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE.TOPCODE",
                       weight_var = "ASECWT"),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",
  title = "", caption = "")

plot3_asec <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label_axis = "Earnings",
  outcome_label_caption = "Wage and Salary Income, Prior Calendar Year",
  title = "", caption = "")

plot4_asec <- create_plot4(gen_outcome_sumstats(
  asec, use_age_group = T, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label = "Earnings",
  title = "", caption = "")

plot5_asec <- create_plot5(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label = "Earnings",
  title = "", caption = "")

plot6_asec <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT"),
  scenario_type = "all", data_source_label = "ASEC March Supplement",
  outcome_label_axis = "Earnings",
  outcome_label_caption = "Wage and Salary Income, Prior Calendar Year",
  title = "", caption = "")

plot1_asec_median <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE.TOPCODE",
                       weight_var = "ASECWT", sumstat = "quantile",
                       probs = 0.5),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings", sumstat_label = "Median",
  title = "", caption = "")

plot2_asec_median <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE.TOPCODE",
                       weight_var = "ASECWT", sumstat = "quantile",
                       probs = 0.5),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",  sumstat_label = "Median",
  title = "", caption = "")

plot4_asec_median <- create_plot4(gen_outcome_sumstats(
  asec, use_age_group = T, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT", sumstat = "quantile",
  probs = 0.5), data_source_label = "ASEC March Supplement",
  outcome_label = "Earnings",  sumstat_label = "Median",
  title = "", caption = "")

plot5_asec_median <- create_plot5(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE.TOPCODE",
  weight_var = "ASECWT", sumstat = "quantile",
  probs = 0.5), data_source_label = "ASEC March Supplement",
  outcome_label = "Earnings",  sumstat_label = "Median",
  title = "", caption = "")

# All figures using FT sample
plot1_ft <- create_plot1(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"), 
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "", caption = "")

plot2_ft <- create_plot2(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"),
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "", caption = "")

plot4_ft <- create_plot4(gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_age_group = T),
                      use_grayscale = F,
                      data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
                      title = "", caption = "")

plot5_ft <- create_plot5(gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35)), use_grayscale = F,
                         data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
                         title = "", caption = "")

plot6_ft <- create_plot3(gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35)), scenario_type = "all",
                         data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
                         title = "", caption = "")


# Get all plot object names from environment
plot_objects <- ls(pattern = "^plot")
plot_objects <- c("plot4_ft")

# Save each plot
for(plot_name in plot_objects) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_supplement", paste0(plot_name, ".pdf")),
    plot = get(plot_name),
    width = 10,
    height = 7,
    dpi = 500
  )
}

# Appendix figures- Quantiles
plot1_q75 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.75),
  sumstat_label = "q75",
  title = "75th Quantile", caption = "")

plot2_q75 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.75),
  sumstat_label = "q75", 
  title = "75th Quantile", caption = "")

plot4_q75 <- create_plot4(
  gen_outcome_sumstats(org, use_age_group = T,
                       sumstat = "quantile",
                       probs = 0.75),
  sumstat_label = "q75",
  title = "75th Quantile", caption = "")

plot5_q75 <- create_plot5(
  gen_outcome_sumstats(org, sumstat = "quantile",
                       probs = 0.75), sumstat_label = "q75",
  title = "75th Quantile", caption = "")

grid.arrange(plot1_q10 + labs(x = "", y = "", caption = ""), 
             plot1_q25 + labs(x = "", y = "", caption = ""), 
             plot1_q50 + labs(x = "", y = "", caption = ""), 
             plot1_q75 + labs(x = "", y = "", caption = ""), 
             plot1_q90 + labs(x = "", y = ""), 
             left = "Women's Q/ Men's Q", 
             bottom = "Age",
             top = textGrob("",
                            gp = gpar(fontsize = 20, fontface = "bold")),
             nrow = 1)


grid.arrange(plot2_q10 + labs(x = "", y = "", caption = ""), 
             plot2_q25 + labs(x = "", y = "", caption = ""), 
             plot2_q50 + labs(x = "", y = "", caption = ""), 
             plot2_q75 + labs(x = "", y = "", caption = ""), 
             plot2_q9 + labs(x = "", y = ""), 
             left = "Percentage Change from Age 25, Gap", 
             bottom = "Age",
             top = textGrob("Age-Graded Change in the Gender Wage Gap by Cohort, 1982-2023, by Quantile",
                            gp = gpar(fontsize = 20, fontface = "bold")),
             nrow = 1)

grid.arrange(plot5_q10 + labs(x = "", y = "", caption = "") + theme(legend.position = "none"), 
             plot5_q25 + labs(x = "", y = "", caption = "") + theme(legend.position = "none"), 
             plot5_q50 + labs(x = "", y = "", caption = "") + theme(legend.position = "none"), 
             plot5_q75 + labs(x = "", y = "", caption = "") + theme(legend.position = "none"), 
             plot5_q90 + labs(x = "", y = "") + theme(legend.position = "bottom"), 
             left = "Age-Specific Pay Relative to Age 25", 
             bottom = "Age",
             top = textGrob("Women's and Men's Pay Trajectories by Birth Cohort and Quantile",
                            gp = gpar(fontsize = 20, fontface = "bold")),
             ncol = 1)


