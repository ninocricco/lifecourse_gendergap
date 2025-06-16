#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: GENERATING PROJECT OUTPUT
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

source("jobs/0-helperfunctions.R")
source("jobs/3-figures-functions.R")

org <- read_rds("clean_data/analytic_sample_org.rds")

#------------------------------------------------------------------------------
# MAIN TEXT FIGURES
#------------------------------------------------------------------------------
plot1 <- create_plot1(
  gen_outcome_sumstats(org, use_birth_group = "BIRTHYEAR_DECADES"),
  title = "Figure 1: Mean Wages and Ratio by Age and Cohort, 1982-2024",
  with_bootstrap = T, conf_level = .95)

plot2 <- create_plot2(
  gen_outcome_sumstats(org,
                       use_birth_group = "BIRTHYEAR_DECADES"),
  title = "Figure 2: Mean Wage Changes since Age 25, 1982-2024",
  with_bootstrap = T, conf_level = .95)

plots_rect <- list(plot1, plot2)

for(i in seq_along(plots_rect)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_main_16.06.25",
                         paste0("plot_", i, ".pdf")),
    plot     = plots_rect[[i]],
    device   = "pdf",
    width    = 8,
    height   = 10,
    units    = "in",
    dpi = 500
  )
}

plot3 <- create_plot3(
  gen_outcome_sumstats(org),
  with_bootstrap = T, conf_level = .95,
  title = "Figure 3: Counterfactual Trends, Gender Wage Gap 1982-2024")

ggsave(filename = file.path("figures/draft_paper/submission_main_16.06.25",
                            "plot3.pdf"),
       plot     = plot3,
       device   = "pdf",
       width    = 8,
       height   = 8,
       units    = "in",
       dpi = 500)

#------------------------------------------------------------------------------
# APPENDIX FIGURES
#------------------------------------------------------------------------------
period_means_medians_plot <- org %>%
  group_by(YEAR, FEMALE) %>%
  summarise(
    "Average" = wtd.mean(
      EARNHRLY2_EXCTIPS_TC_HRSPRED, weight = EARNWT),
    "Median" = wtd.quantile(
      EARNHRLY2_EXCTIPS_TC_HRSPRED, weight = EARNWT, probs = .5),
  ) %>%
  gather(key, value, -c(YEAR, FEMALE))%>% 
  ggplot(aes(x = YEAR, y = value,
             shape = key, linetype = key, color = FEMALE)) +
  geom_point() +
  geom_line() +
  create_common_theme(14, "right") +
  theme(
    legend.position      = c(0.25, 0.989), 
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("dodgerblue4", "orange2")) +
  labs(x = "Year", y = "Hourly Wages", 
       title = "Period Trends, Mean and Median Hourly Wages 1982-2024")

series_plots <- bind_rows(
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS_TC_HRSPRED"))$data %>%
    mutate(wage_series = "Main Specification"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_INCTIPS"))$data %>%
    mutate(wage_series =  "Inc. Tips, Commissions, Overtime"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS_TC"))$data %>%
    mutate(wage_series =  "Excludes Rs report Hours Vary"),
  prepare_counterfactual_data(
    gen_outcome_sumstats(org, outcome_var = "EARNHRLY2_EXCTIPS_HRSPRED"))$data %>%
    mutate(wage_series = "Without Estimating Mean Above the Top-Code"))

plot_observed_series <- series_plots %>%
  filter(key == "Observed") %>%
  ggplot(aes(x = YEAR, y = value, 
             linetype = wage_series, 
             shape = wage_series)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(key)) +
  scale_linetype_manual(values = c(
    "Main Specification" = "solid", 
    "Inc. Tips, Commissions, Overtime" = "dotdash", 
    "Without Estimating Mean Above the Top-Code" = "dotdash",
    "Excludes Rs report Hours Vary" = "dotdash"
    )) + 
  scale_shape_manual(values = c(
    "Main Specification" = 16, 
    "Inc. Tips, Commissions, Overtime" = 15, 
    "Without Estimating Mean Above the Top-Code" = 17,
    "Excludes Rs report Hours Vary" = 8
  )) +
  create_common_theme(base_size, "none") +
  theme(
    legend.position      = c(0.02, 0.9), 
    legend.justification = c("left", "top"),
    legend.direction     = "vertical",
    legend.title = element_blank(),
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(y = "Women's / Men's Average Hourly Wage", x = "Year",
       title = "Trends in the Gender Pay Gap 1982-2024 by Wage Series")

plot_counterfactuals_series <- series_plots %>%
  filter(key %in% c(
    "Baseline", "Changing Starting Points", "Changing Trajectories",
    "Men's Changing Starting Points, Women Baseline", 
    "Men's Changing Trajectories, Women Baseline")) %>%
  ggplot(aes(x = YEAR, y = value, shape = wage_series,
             color = key, linetype = wage_series)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~key, ncol = 2) +
  scale_colour_manual(values = c(
    "Observed"                                          = "#000000",
    "Baseline"                                          = "#7F7F7F",
    "Changing Starting Points"                          = "#9FDA3A",
    "Changing Trajectories"                             = "#9C179E",
    "Men's Changing Starting Points, Women Baseline"       = "#006633",
    "Men's Changing Trajectories, Women Baseline"          = "#330066"
  )) +
  scale_linetype_manual(values = c(
    "Main Specification" = "solid", 
    "Inc. Tips, Commissions, Overtime" = "dotdash", 
    "Without Estimating Mean Above the Top-Code" = "dotdash",
    "Excludes Rs report Hours Vary" = "dotdash"
  )) + 
  scale_shape_manual(values = c(
    "Main Specification" = 16, 
    "Inc. Tips, Commissions, Overtime" = 15, 
    "Without Estimating Mean Above the Top-Code" = 17,
    "Excludes Rs report Hours Vary" = 8
  )) +
  create_common_theme(base_size  = 10, "none") +
  theme(
    legend.position      = c(0.9, 0.1), 
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.title = element_blank(),
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(y = "Women's / Men's Average Hourly Wage", x = "Year") +
  guides(color = "none")

ggsave(filename = file.path(
  "figures/draft_paper/submission_supplement_16.06.25",
  "plot_counterfactual_series.pdf"),
  plot     = plot_counterfactuals_series,
  device   = "pdf",
  width    = 8,
  height   = 12,
  units    = "in",
  dpi = 500)

plot3_ref60 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1960, 
                            title = "Reference Cohort 1960", caption = "")

plot3_ref62 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1962,
                            title = "Reference Cohort 1962", caption = "")

plot3_ref65 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1965,
                            title = "Reference Cohort 1965", caption = "")

plot3_refcohorts <- grid.arrange(
  plot3_ref60, plot3_ref62 + theme(legend.position = "none"),
  plot3_ref65 + theme(legend.position = "none"),
  nrow = 1, 
  top = textGrob("Figure 3: Counterfactual Trends in the Gender Wage Gap, 1982-2024", 
                 gp = gpar(fontsize = base_size *1.2, fontface = "bold")))

ggsave(filename = file.path(
  "figures/draft_paper/submission_supplement_16.06.25",
  "plot3_refcohorts.pdf"),
  plot     = plot3_refcohorts,
  device   = "pdf",
  width    = 16,
  height   = 8,
  units    = "in",
  dpi = 500)

plot3_age30 <- create_plot3(gen_outcome_sumstats(org), 
                            start_age = 30, 
                            title = "Figure 3, Start Age 30")

plot3_hrscap <- create_plot3(gen_outcome_sumstats(
  org, outcome_var = "EARNHRLY2_TC_HRSCAP_EXCTIPS"),
  outcome_label_axis = "Hourly Wage",
  outcome_label_caption = "Hourly Wage, Hours Capped at 40 hrs/week",
  title = "Figure 3, Hours Capped at 40 hrs/week")

plot1_ft <- create_plot1(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), 
                       use_birth_group = "BIRTHYEAR_DECADES"), 
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "Figure 1: Restricted to Full-Time Workers (35+ hrs/week)")

plot2_ft <- create_plot2(
  gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35), use_birth_group = "BIRTHYEAR_DECADES"),
  data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
  title = "Restricted to Full-Time Workers (35+ hrs/week)")

plot3_ft <- create_plot3(gen_outcome_sumstats(org %>% filter(UHRSWORK1_PRED >= 35)),
                         data_source_label = "Outgoing Rotation Group, Restricted to Full-Time Workers (35+ hrs/week)",
                         title = "Restricted to Full-Time Workers (35+ hrs/week)")

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
    legend.position      = c(0.05, 0.9), 
    legend.justification = c("left", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(
    x = "Year",
    y = "Women's / Men's Average Hourly Wage",
    title = "Figure 3, Holding Age Distribution Fixed"
  ) +
  scale_x_continuous(breaks = round(seq(1970, 2020, by = 10))) +
  guides(linetype = guide_legend(title="",  ncol = 1))

asec <- read_rds("clean_data/analytic_sample_asec.rds") 

plot1_asec <- create_plot1(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT",
                       winsorize = c("bottom" = .005, "top" = .995)),
  data_source_label = "ASEC March Supplement", title = "Figure 1, ASEC",
  caption = "Data from the Current Population Survey ASEC March Supplement. \n Sample members aged 25-55. Outcome: Annual Earnings")

plot2_asec <- create_plot2(
  gen_outcome_sumstats(asec, use_birth_group = "BIRTHYEAR_DECADES", 
                       outcome_var = "INCWAGE",
                       weight_var = "ASECWT",
                       winsorize = c("bottom" = .005, "top" = .995)),
  data_source_label = "ASEC March Supplement", title = "Figure 2, ASEC",
  caption = "Data from the Current Population Survey ASEC March Supplement. \n Sample members aged 25-55. Outcome: Annual Earnings")

plot3_asec <- create_plot3(gen_outcome_sumstats(
  asec, outcome_var = "INCWAGE", weight_var = "ASECWT", 
  winsorize = c("bottom" = .005, "top" = .995)),
  data_source_label = "ASEC March Supplement", title = "Figure 3, ASEC",
  caption = "Data from the Current Population Survey ASEC March Supplement. \n Sample members aged 25-55. Outcome: Annual Earnings")

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

supplement_plot_rect <- list("plot1_asec" = plot1_asec, "plot2_asec" = plot2_asec, 
                             "plot1_ft" = plot1_ft, "plot2_ft" = plot2_ft, 
                             "plot1_q10" = plot1_q10, "plot1_q25" = plot1_q25,
                             "plot1_q50" = plot1_q50, "plot1_q75" = plot1_q75, 
                             "plot1_q90" = plot1_q90,
                             "plot2_q10" = plot2_q10, "plot2_q25" = plot2_q25,
                             "plot2_q50" = plot2_q50, "plot2_q75" = plot2_q75, 
                             "plot2_q90" = plot2_q90)
                          
# Save each plot
for(plot_name in names(supplement_plot_rect)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_supplement_16.06.25",
                         paste0(plot_name, ".pdf")),
    plot = supplement_plot_rect[[plot_name]],
    width = 8,
    height = 10,
    dpi = 500
  )
}

supplement_plot_sq <- list(
  "plot_periodtrends_meansmedians" = period_means_medians_plot,
"plot_observed_series" = plot_observed_series,
"plot_agedistfig" = age_dist_fig,
"plot3_asec" = plot3_asec,
"plot3_ft" = plot3_ft, "plot3_age30"= plot3_age30,
"plot3_hrscap" = plot3_hrscap)

# Save each plot
for(plot_name in names(supplement_plot_sq)) {
  ggsave(
    filename = file.path("figures/draft_paper/submission_supplement_16.06.25",
                         paste0(plot_name, ".pdf")),
    plot = supplement_plot_sq[[plot_name]],
    width = 8,
    height = 8,
    dpi = 500
  )
}

plot3_race <- create_plot3(gen_outcome_sumstats(org, group_var = "RACEETH"),
                           group_var = "RACEETH",
                           title = "Figure 3 by Racial and Ethnic Group")

ggsave(
  filename = file.path("figures/draft_paper/submission_supplement_16.06.25",
                       paste0("plot3_race", i, ".pdf")),
  plot     = plot3_race,
  device   = "pdf",
  width    = 8,
  height   = 10,
  units    = "in",
  dpi = 500
)

plot3_educ <- create_plot3(gen_outcome_sumstats(org, group_var = "EDUC2"),
                           group_var = "EDUC2",
                           title = "Figure 3 by Education Group")
ggsave(
  filename = file.path("figures/draft_paper/submission_supplement_16.06.25",
                       paste0("plot3_educ", i, ".pdf")),
  plot     = plot3_educ,
  device   = "pdf",
  width    = 10,
  height   = 8,
  units    = "in",
  dpi = 500
)

