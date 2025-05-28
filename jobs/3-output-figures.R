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
         ) %>%
  group_by(YEAR) %>% 
  mutate(
    p005 = wtd.quantile(EARNHRLY_MAIN, 0.005, weight = EARNWT, na.rm = TRUE),
    p995 = wtd.quantile(EARNHRLY_MAIN, 0.995, weight = EARNWT, na.rm = TRUE),
    EARNHRLY_MAIN = pmin(pmax(EARNHRLY_MAIN, p005), p995),
    BIRTHYEAR_DECADES = case_when(BIRTHYEAR %in% c(1913:1919) ~ "1913-1919",
                                  BIRTHYEAR %in% c(1920:1929) ~ "1920s",
                                  BIRTHYEAR %in% c(1930:1939) ~ "1930s",
                                  BIRTHYEAR %in% c(1940:1949) ~ "1940s",
                                  BIRTHYEAR %in% c(1950:1959) ~ "1950s",
                                  BIRTHYEAR %in% c(1960:1969) ~ "1960s",
                                  BIRTHYEAR %in% c(1970:1979) ~ "1970s",
                                  BIRTHYEAR %in% c(1980:1989) ~ "1980s", 
                                  BIRTHYEAR %in% c(1990:1998) ~ "1990-1998" 
    )
  ) %>%
  ungroup() %>% 
  select(-p005, -p995)

asec <- read_rds("clean_data/analytic_sample_asec.rds") %>%
  filter(YEAR >= 1982) %>%
  mutate(BIRTHYEAR_DECADES = case_when(BIRTHYEAR %in% c(1913:1919) ~ "1913-1919",
                                       BIRTHYEAR %in% c(1920:1929) ~ "1920s",
                                       BIRTHYEAR %in% c(1930:1939) ~ "1930s",
                                       BIRTHYEAR %in% c(1940:1949) ~ "1940s",
                                       BIRTHYEAR %in% c(1950:1959) ~ "1950s",
                                       BIRTHYEAR %in% c(1960:1969) ~ "1960s",
                                       BIRTHYEAR %in% c(1970:1979) ~ "1970s",
                                       BIRTHYEAR %in% c(1980:1989) ~ "1980s", 
                                       BIRTHYEAR %in% c(1990:1998) ~ "1990-1998" 
  ))

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

plot3_age30 <- create_plot3(gen_outcome_sumstats(org), 
                      start_age = 30, scenarios_to_include = "main",
                      title = "", caption = "")

plot3_ref60 <- create_plot3(gen_outcome_sumstats(org), 
                            refcohort = 1960, scenarios_to_include = "main",
                            title = "", caption = "")

plot3_ref62 <- create_plot3(gen_outcome_sumstats(org), 
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
         "1981 Age Distribution, Wage Ratio" = age_dist_82_Women/age_dist_82_Men) %>%
  select(YEAR, ends_with("Ratio")) %>%
  gather(key, value, -YEAR) %>%
  ggplot(aes(x = YEAR, y = value, linetype = key)) +
  geom_line(size = 1) +
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

supplement_plot_objects <- list("plot1_asec" = plot1_asec, "plot2_asec", 
                                "plot1_asec_median", "plot2_asec_median",
                                "plot1_ft", "plot2_ft", 
                                "plot3_asec", "plot3_ft", "plot3_age30",
                                "plot3_educ","plot3_hrscap", "plot3_race",
                                "plot3_ref60", "plot3_ref62", 
                                "plot3_selection1", "plot3_selection2",
                                "age_dist_fig")

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
