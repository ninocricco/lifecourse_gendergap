#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: GENERATING PROJECT OUTPUT
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

source("jobs/0-helperfunctions.R")
source("jobs/3-figures-functions.R")

org <- read_rds("clean_data/analytic_sample_org.rds")

plot0 <- org %>%
  group_by(YEAR, FEMALE) %>%
  summarise(
    "Average" = wtd.mean(
      EARNHRLY2_EXCTIPS_TC_HRSPRED, weight = EARNWT),
    "Median" = wtd.quantile(
      EARNHRLY2_EXCTIPS_TC_HRSPRED, weight = EARNWT, probs = .5),
    ) %>%
  gather(key, value, -c(YEAR, FEMALE))%>% 
  ggplot(aes(x = YEAR, y = value, shape = FEMALE, linetype = FEMALE)) +
  geom_point() +
  geom_line() +
  facet_grid(~key) +
  create_common_theme(14, "bottom") +
  guides(shape = guide_legend(""), linetype = "none") +
  labs(x = "Year", y = "Hourly Wages")

# Main text figures
plot1 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"),
  title = "", caption = "", with_bootstrap = F, conf_level = .95)

plot2 <- create_plot2(
  gen_outcome_sumstats(org,
                       use_birth_group = "BIRTHYEAR_DECADES"),
  title = "", caption = "", with_bootstrap = F, conf_level = .95)

plot3 <- create_plot3(gen_outcome_sumstats(org),
                      title = "", caption = "", 
                      with_bootstrap = F, conf_level = .95)

plots_main <- list(plot1, plot2, plot3)

for(i in seq_along(plots_main)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_main_13.06.25", paste0("plot_", i, ".pdf")),
    plot     = plots_main[[i]],
    device   = "pdf",
    width    = 8,
    height   = 8,
    units    = "in",
    dpi = 500
  )
}

series_plots <- bind_rows(
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS_TC_HRSPRED"))$data %>%
    mutate(wage_series = "Main (Rs Paid Hourly Wage Exc. Tips)"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS_TC"))$data %>%
    mutate(wage_series = "Excludes Rs report Hours Vary"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS_HRSPRED"))$data %>%
    mutate(wage_series = "Excludes Top-Code Wages Estimated"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS"))$data %>%
    mutate(wage_series = "Excludes Top-Code Wages Estimated, Rs report Hours Vary"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_INCTIPS_TC_HRSPRED"))$data %>%
    mutate(wage_series = "Inc. Tips (Estimate Weekly Earnings for Rs Paid Hourly Wage)"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_INCTIPS_TC"))$data %>%
    mutate(wage_series = "Inc. Tips, Excludes Rs report Hours Vary"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_INCTIPS_HRSPRED"))$data %>%
    mutate(wage_series = "Inc. Tips, Excludes Top-Code Wages Estimated"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_INCTIPS"))$data %>%
    mutate(wage_series = "Inc. Tips, Excludes Top-Code Wages Estimated, Rs report Hours Vary")
  )

plot_observed_series <- series_plots %>%
  filter(key == "Observed") %>%
  ggplot(aes(x = YEAR, y = value, 
             linetype = wage_series, 
             shape = wage_series)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(key)) +
  scale_linetype_manual(values = c(
    "Main (Rs Paid Hourly Wage Exc. Tips)" = "solid", 
    "Inc. Tips, Excludes Top-Code Wages Estimated, Rs report Hours Vary" = "dashed", 
    "Inc. Tips, Excludes Top-Code Wages Estimated" = "dashed", 
    "Inc. Tips, Excludes Rs report Hours Vary" = "dashed", 
    "Inc. Tips (Estimate Weekly Earnings for Rs Paid Hourly Wage)" = "dashed", 
    "Excludes Top-Code Wages Estimated, Rs report Hours Vary" = "dotdash",
    "Excludes Top-Code Wages Estimated" = "dotdash",
    "Excludes Rs report Hours Vary" = "dotdash"
    )) + 
  scale_shape_manual(values = c(
    "Main (Rs Paid Hourly Wage Exc. Tips)" = 16, 
    "Inc. Tips, Excludes Top-Code Wages Estimated, Rs report Hours Vary" = 15, 
    "Inc. Tips, Excludes Top-Code Wages Estimated" = 17, 
    "Inc. Tips, Excludes Rs report Hours Vary" = 8, 
    "Inc. Tips (Estimate Weekly Earnings for Rs Paid Hourly Wage)" = 16, 
    "Excludes Top-Code Wages Estimated, Rs report Hours Vary" = 15,
    "Excludes Top-Code Wages Estimated" = 17,
    "Excludes Rs report Hours Vary" = 8
  )) +
  create_common_theme(base_size, "none") +
  theme(
    legend.position      = c(0.4, 0.9), 
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(y = "Women's / Men's Average Hourly Wage", x = "Year") +
  guides(shape = guide_legend(""), linetype = "none")

plot_counterfactuals_series <- series_plots %>%
  filter(key != "Observed") %>%
  ggplot(aes(x = YEAR, y = value, shape = wage_series,
             color = key, linetype = wage_series)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~key, nrow = 2) +
  scale_colour_manual(values = c(
    "Observed"                                          = "#000000",
    "Baseline"                                          = "#7F7F7F",
    "No Change for Men"                                 = "#ED7953",
    "Changing Starting Points"                          = "#9FDA3A",
    "Changing Trajectories"                             = "#9C179E",
    "Changing Starting Points, No Change for Men"       = "#277F8E",
    "Changing Trajectories, No Change for Men"          = "#0D0887"
  )) +
  scale_linetype_manual(values = c(
    "Main (Rs Paid Hourly Wage Exc. Tips)" = "solid", 
    "Inc. Tips, Excludes Top-Code Wages Estimated, Rs report Hours Vary" = "dashed", 
    "Inc. Tips, Excludes Top-Code Wages Estimated" = "dashed", 
    "Inc. Tips, Excludes Rs report Hours Vary" = "dashed", 
    "Inc. Tips (Estimate Weekly Earnings for Rs Paid Hourly Wage)" = "dashed", 
    "Excludes Top-Code Wages Estimated, Rs report Hours Vary" = "dotdash",
    "Excludes Top-Code Wages Estimated" = "dotdash",
    "Excludes Rs report Hours Vary" = "dotdash"
  )) + 
  scale_shape_manual(values = c(
    "Main (Rs Paid Hourly Wage Exc. Tips)" = 19, 
    "Inc. Tips, Excludes Top-Code Wages Estimated, Rs report Hours Vary" = 15, 
    "Inc. Tips, Excludes Top-Code Wages Estimated" = 17, 
    "Inc. Tips, Excludes Rs report Hours Vary" = 8, 
    "Inc. Tips (Estimate Weekly Earnings for Rs Paid Hourly Wage)" = 19, 
    "Excludes Top-Code Wages Estimated, Rs report Hours Vary" = 15,
    "Excludes Top-Code Wages Estimated" = 17,
    "Excludes Rs report Hours Vary" = 8
  )) +
  create_common_theme(base_size, "none") +
  theme(
    legend.position      = c(0.4, 0.9), 
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(y = "Women's / Men's Average Hourly Wage", x = "Year") +
  guides(shape = "none", linetype = "none",
         color = "none")


plot3_age30 <- create_plot3(gen_outcome_sumstats(org), 
                      start_age = 30, 
                      title = "Start Age 30", caption = "")

plot3_ref60 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1960, 
                            title = "Reference Cohort 1960", caption = "")

plot3_ref62 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1962,
                            title = "Reference Cohort 1962", caption = "")

plot3_ref65 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1965,
                            title = "Reference Cohort 1965", caption = "")

plot3_refcohorts <- grid.arrange(plot3_ref60, 
                                 plot3_ref62 + theme(legend.position = "none"),
                                 plot3_ref65 + theme(legend.position = "none"),
                                 nrow = 1)

plot3_hrscap <- create_plot3(gen_outcome_sumstats(
  org, outcome_var = "EARNHRLY2_TC_HRSCAP_EXCTIPS"),
  outcome_label_axis = "Hourly Wage",
  outcome_label_caption = "Hourly Wage, Hours Capped at 40 hrs/week",
  title = "Hours Capped at 40 hrs/week", caption = "")

plot3_race <- create_plot3(gen_outcome_sumstats(org, group_var = "RACEETH"),
                           group_var = "RACEETH",
                           title = "", caption = "")

plot3_educ <- create_plot3(gen_outcome_sumstats(org, group_var = "EDUC2"),
                           group_var = "EDUC2", title = "",
                           scenarios_to_include = "main")

plot1_ft <- create_plot1(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"), 
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "Restricted to Full-Time Workers (35+ hrs/week)", caption = "")

plot2_ft <- create_plot2(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"),
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "Restricted to Full-Time Workers (35+ hrs/week)", caption = "")

plot3_ft <- create_plot3(gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35)), scenarios_to_include = "main",
                         data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
                         title = "Restricted to Full-Time Workers (35+ hrs/week)", caption = "")

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
  title = "10th Quantile", caption = "") 

plot1_q25 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.25),
  sumstat_label = "q25",
  title = "25th Quantile", caption = "") 

plot1_q50 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.50),
  sumstat_label = "q50",
  title = "50th Quantile", caption = "")

plot1_q75 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.75),
  sumstat_label = "q75",
  title = "75th Quantile", caption = "") 

plot1_q90 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES",
                       sumstat = "quantile",
                       probs = 0.90),
  sumstat_label = "q90",
  title = "90th Quantile", caption = "")

asec <- read_rds("clean_data/analytic_sample_asec.rds") 

plot1_asec <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT",
                       winsorize = c("bottom" = .005, "top" = .995)),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",
  title = "", caption = "")

plot2_asec <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT",
                       winsorize = c("bottom" = .005, "top" = .995)),
  data_source_label = "ASEC March Supplement", 
  outcome_label = "Earnings",
  title = "", caption = "")

plot3_asec <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE", weight_var = "ASECWT", 
  winsorize = c("bottom" = .005, "top" = .995)),
  data_source_label = "ASEC March Supplement",
  outcome_label_axis = "Earnings",
  outcome_label_caption = "Wage and Salary Income, Prior Calendar Year",
  title = "", caption = "")


supplement_plot_objects <- list("plot1_asec" = plot1_asec, "plot2_asec" = plot2_asec, "plot3_asec" = plot3_asec,
                                "plot1_ft" = plot1_ft, "plot2_ft" = plot2_ft, 
                                "plot3_ft" = plot3_ft, "plot3_age30"= plot3_age30,
                                "plot3_educ" = plot3_educ,"plot3_hrscap" = plot3_hrscap, "plot3_race"= plot3_race,
                                "plot3_refcohorts" = plot3_refcohorts, 
                                "age_dist_fig" = age_dist_fig,
                                "plot1_q10" = plot1_q10, 
                                "plot1_q25" = plot1_q25, "plot1_q50" = plot1_q50, 
                                "plot1_q75" = plot1_q75, "plot1_q90" = plot1_q90,
                                "plot_counterfactuals_series" = plot_counterfactuals_series, 
                                "plot_observed_series" = plot_observed_series,
                                "plot_periodtrends_meansmedians" = plot0)

# Save each plot
for(plot_name in names(supplement_plot_objects)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_supplement_13.06.25", paste0(plot_name, ".pdf")),
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
  filename = file.path("figures/draft_paper/submission_supplement_13.06.25", paste0(plot2_quantiles, ".pdf")),
  plot = plot2_quantiles,
  width = 16,
  height = 8,
  dpi = 500
)

