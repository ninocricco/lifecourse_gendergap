#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: GENERATING PROJECT OUTPUT
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

source("jobs/0-helperfunctions.R")
source("jobs/3-figures-functions.R")

org <- read_rds("clean_data/analytic_sample_org_elig_weights.rds") %>%
  mutate(UHRSWORK1_PRED = as.numeric(zap_labels(UHRSWORK1_PRED))) %>%
  filter(UHRSWORK1_PRED > 0)

asec <- read_rds("clean_data/analytic_sample_asec.rds") %>%
  filter(YEAR >= 1982)

# Main text figures
plot1 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"),
  title = "", caption = "", with_bootstrap = F, conf_level = .95)

plot2 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"),
  title = "", caption = "", with_bootstrap = F, conf_level = .95)

plot3 <- create_plot3(gen_outcome_sumstats(org),
                      title = "", caption = "", with_bootstrap = F, conf_level = .95)

plots_main <- list(plot1, plot2, plot3)

for(i in seq_along(plots_main)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_main", paste0("plot_", i, ".pdf")),
    plot     = plots_main[[i]],
    device   = "pdf",
    width    = 8,
    height   = 8,
    units    = "in",
    dpi = 500
  )
}

# Supplement figures

plot6 <- create_plot3(gen_outcome_sumstats(org),
                      title = "", caption = "", 
                      scenarios_to_include = "appendix",
                      with_bootstrap = F, conf_level = .95)


plot3_age30 <- create_plot3(gen_outcome_sumstats(org), 
                      start_age = 30, scenarios_to_include = "main",
                      title = "", caption = "")

plot3_ref60 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1960, scenarios_to_include = "main",
                            title = "", caption = "")

plot3_ref65 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1962, scenarios_to_include = "main",
                            title = "", caption = "")

plot3_hrscap <- create_plot3(gen_outcome_sumstats(org, outcome_var = "EARNHRLY_HRSCAP"),
                             outcome_label_axis = "Hourly Wage",
                            outcome_label_caption = "Hourly Wage, Hours Capped at 40 hrs/week",
                            scenarios_to_include = "main", 
                            title = "", caption = "")

plot3_race <- create_plot3(gen_outcome_sumstats(org, group_var = "RACEETH"),
                           group_var = "RACEETH",
                           scenarios_to_include = "main",
                           title = "", caption = "")

plot3_educ <- create_plot3(gen_outcome_sumstats(org, group_var = "EDUC2"),
                           group_var = "EDUC2", title = "",
                           scenarios_to_include = "main")

plot_selection_ratio <- prepare_counterfactual_data(
  gen_outcome_sumstats(org))$data %>%
  filter(key == "Observed") %>%
  bind_rows(
    prepare_counterfactual_data(
      gen_outcome_sumstats(
        org, weight_var = "ELIGORG_PRED_REFCOHORT_WEIGHT"))$data %>%
      filter(key == "Observed") %>%
      mutate(key = "Reweighted, 1957 Selection")) %>%
  bind_rows(
    prepare_counterfactual_data(
      gen_outcome_sumstats(
        org, weight_var = "ELIGORG_PRED_WEIGHT"))$data %>%
      filter(key == "Observed") %>%
      mutate(key = "Reweighted, Cohort Selection")) %>%
  rename(Scenario = key) %>%
  ggplot(aes(
    x = YEAR, y = value, shape = Scenario, linetype = Scenario)) +
  geom_line() +
  geom_point() +
  create_common_theme(base_size, "bottom") +
  labs(title = "",
       x = "",
       y = "") 

plot3_selection1 <- create_plot3(
  gen_outcome_sumstats(org, weight_var = "ELIGORG_PRED_WEIGHT"), 
  outcome_label_axis = "Hourly Wage",
  outcome_label_caption = "Hourly Wage, Reweighted to Adjust for ORG eligibility, Cohort-Specific",
  scenarios_to_include = "main",
  title = "", caption = "")

plot3_selection2 <- create_plot3(
  gen_outcome_sumstats(org, weight_var = "ELIGORG_PRED_REFCOHORT_WEIGHT"),
  outcome_label_axis = "Hourly Wage",
  outcome_label_caption = "Hourly Wage, Reweighted to Adjust for ORG eligibility, Reference Cohort",
  scenarios_to_include = "main",
  title = "", caption = "")

plot1_asec <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT"),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",
  title = "", caption = "")

plot2_asec <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT"),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",
  title = "", caption = "")

plot3_asec <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE",
  weight_var = "ASECWT"), data_source_label = "ASEC March Supplement",
  outcome_label_axis = "Earnings",
  outcome_label_caption = "Wage and Salary Income, Prior Calendar Year",
  title = "", caption = "")

plot1_asec_median <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT", sumstat = "quantile",
                       probs = 0.5),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings", sumstat_label = "Median",
  title = "", caption = "")

plot2_asec_median <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT", sumstat = "quantile",
                       probs = 0.5),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",  sumstat_label = "Median",
  title = "", caption = "")

plot1_ft <- create_plot1(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"), 
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "", caption = "")

plot2_ft <- create_plot2(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"),
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "", caption = "")

plot3_ft <- create_plot3(gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35)), scenarios_to_include = "main",
                         data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
                         title = "", caption = "")

age_dist_fig <- gen_outcome_sumstats(org) %>%
  mutate(lf_cohortprop = n/n_year) %>%
  left_join(gen_outcome_sumstats(org) %>%
              mutate(lf_cohortprop_82 = n/n_year) %>%
              filter(YEAR == 1982) %>%
              select(AGE, FEMALE, lf_cohortprop_82),
            by = c("AGE", "FEMALE")) %>%
  mutate(observed = MEAN.OUTCOME * lf_cohortprop, 
         age_dist_82 = MEAN.OUTCOME * lf_cohortprop_82) %>%
  group_by(YEAR, FEMALE) %>%
  summarise(observed = sum(observed), 
            age_dist_82 = sum(age_dist_82)) %>%
  pivot_wider(names_from = FEMALE, values_from = c(observed, age_dist_82)) %>%
  mutate("Cohort Age Distribution, Wage Ratio" = observed_Women/observed_Men, 
         "1982 Age Distribution, Wage Ratio" = age_dist_82_Women/age_dist_82_Men) %>%
  select(YEAR, ends_with("Ratio")) %>%
  gather(key, value, -YEAR) %>%
  ggplot(aes(x = YEAR, y = value, linetype = key)) +
  geom_line() +
  create_common_theme(base_size = 14, 'none') +
  theme(
    legend.position      = c(0.9, 0.2), 
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(
    x = "Year",
    y = "Women's / Men's Average Hourly Wage"
  ) +
  scale_x_continuous(breaks = round(seq(1970, 2020, by = 10))) +
  guides(linetype = guide_legend(title="",  ncol = 1))

# Appendix figures- Quantiles
plot1_q10 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.10),
  sumstat_label = "q10",
  title = "10th Quantile", caption = "") +
  labs(x = "", y = "")

plot1_q25 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.25),
  sumstat_label = "q25",
  title = "25th Quantile", caption = "") +
  labs(x = "", y = "")

plot1_q50 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.50),
  sumstat_label = "q50",
  title = "50th Quantile", caption = "") +
  labs(x = "", y = "")

plot1_q75 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.75),
  sumstat_label = "q75",
  title = "75th Quantile", caption = "") +
  labs(x = "", y = "")

plot1_q90 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.90),
  sumstat_label = "q90",
  title = "90th Quantile", caption = "") +
  labs(x = "", y = "")


supplement_plot_objects <- list("plot1_asec" = plot1_asec, "plot2_asec" = plot2_asec, 
                                "plot1_asec_median" = plot1_asec_median, "plot2_asec_median" = plot2_asec_median,
                                "plot1_ft" = plot1_ft, "plot2_ft" = plot2_ft, 
                                "plot3_asec" = plot3_asec, "plot3_ft" = plot3_ft, "plot3_age30"= plot3_age30,
                                "plot3_educ" = plot3_educ,"plot3_hrscap" = plot3_hrscap, "plot3_race"= plot3_race,
                                "plot3_ref60" = plot3_ref60, "plot3_ref62" = plot3_ref62, 
                                "plot3_selection1" = plot3_selection1, "plot3_selection2" = plot3_selection2,
                                "age_dist_fig" = age_dist_fig,
                                "plot6" = plot6, "plot1_q10" = plot1_q10, 
                                "plot1_q25" = plot1_q25, "plot1_q50" = plot1_q50, 
                                "plot1_q75" = plot1_q75, "plot1_q90" = plot1_q90)

# Save each plot
for(plot_name in names(supplement_plot_objects)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_supplement_new", paste0(plot_name, ".pdf")),
    plot = supplement_plot_objects[[plot_name]],
    width = 8,
    height = 8,
    dpi = 500
  )
}


plot2_q10 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.10),
  sumstat_label = "q10", 
  title = "10th Quantile", caption = "")


plot2_q25 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.25),
  sumstat_label = "q25", 
  title = "25th Quantile", caption = "")

plot2_q50 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.50),
  sumstat_label = "q50", 
  title = "50th Quantile", caption = "")

plot2_q75 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.75),
  sumstat_label = "q75", 
  title = "75th Quantile", caption = "")

plot2_q90 <- create_plot2(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.90),
  sumstat_label = "q90", 
  title = "90th Quantile", caption = "")

plot2_quantiles <- grid.arrange(plot2_q10 + labs(x = "", y = "", caption = "") + theme(legend.position = "none"), 
                                plot2_q25 + labs(x = "", y = "", caption = "")+ theme(legend.position = "none"), 
                                plot2_q50 + labs(x = "", y = "", caption = "")+ theme(legend.position = "none"), 
                                plot2_q75 + labs(x = "", y = "", caption = "")+ theme(legend.position = "none"), 
                                plot2_q90 + labs(x = "", y = ""), 
                                left = textGrob("Percentage Change from Age 25, Gap",
                                                gp = gpar(fontsize = base_size, fontface = "bold"),
                                                rot = 90), 
                                bottom = textGrob("Age",
                                                  gp = gpar(fontsize = base_size, fontface = "bold")), 
                                top = textGrob("",
                                               gp = gpar(fontsize = base_size *1.2, fontface = "bold")),
                                nrow = 1)

ggsave(
  filename = file.path("figures/draft_paper/submission_supplement_new", paste0(plot2_quantiles, ".pdf")),
  plot = plot2_quantiles,
  width = 16,
  height = 8,
  dpi = 500
)