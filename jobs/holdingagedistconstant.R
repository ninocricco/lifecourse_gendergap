group_var = NULL 
title = NULL
base_size = 14
caption = NULL
use_grayscale = F
sumstat_label = "Average"
outcome_label_axis = "Hourly Wage"
outcome_label_caption = "Hourly Wage"
data_source_label = "Outgoing Rotation Group"
refcohort = 1957
start_age = 25
scenario_type = "baseline"

org_sum <- gen_outcome_sumstats(org)

counterfactual_data <- org_sum %>%
    left_join(., org_sum %>%
                group_by(YEAR, FEMALE) %>%
                summarise(
                  n_year = n()), by = c("YEAR", "FEMALE")) %>%
    arrange(BIRTHYEAR, YEAR, AGE) %>%
    mutate(
      lf_cohortprop = n/n_year,
      mean_c_start = ifelse(AGE == start_age, MEAN.OUTCOME, NA),
      mean_men_start = ifelse(AGE == start_age & FEMALE == "Men", MEAN.OUTCOME, NA),
      mean_refcohort_start = ifelse(AGE == start_age & BIRTHYEAR == refcohort, MEAN.OUTCOME, NA),
      mean_refcohort_age = ifelse(BIRTHYEAR == refcohort, MEAN.OUTCOME, NA)
    ) %>%
    ungroup() %>%
    arrange(FEMALE) %>%
    group_by(BIRTHYEAR) %>%
    fill(mean_men_start, mean_c_start, .direction = "down") %>%
    ungroup() %>%
    fill(mean_refcohort_start, .direction = "down") %>%
    arrange(AGE) %>%
    group_by(AGE) %>%
    fill(mean_refcohort_age, .direction = "down") %>%
    ungroup() %>%
    mutate(
      pct.change_refcohort_start = mean_refcohort_age/mean_refcohort_start,
      pct.change_c_start = MEAN.OUTCOME/mean_c_start
    )

counterfactual_fig <- counterfactual_data %>%
  left_join(counterfactual_data %>% filter(BIRTHYEAR == 1957) %>%
  transmute(AGE, FEMALE, lf_cohortprop_1957 = lf_cohortprop), 
  by = c("AGE", "FEMALE")) %>%
    mutate(
      mean_age_adj = MEAN.OUTCOME * lf_cohortprop,
      mean_age_adj_57 = MEAN.OUTCOME * lf_cohortprop_1957) %>%
    group_by(YEAR, FEMALE) %>%
    summarise(
      agg = sum(mean_age_adj),
      agg_age = sum(mean_age_adj_57),
      .groups = "drop") %>%
    gather(counterfactual, value, -c(YEAR, FEMALE)) %>%
    pivot_wider(names_from = c(FEMALE, counterfactual), values_from = value) %>%
    mutate("Observed" = Women_agg/Men_agg,
      "Holding 1957 Age Distribution Constant" = Women_agg_age/Men_agg_age
    ) %>%
    gather(key), value, -YEAR) %>%
  filter(key %in% c("Observed", "Holding 1957 Age Distribution Constant")) 

counterfactual_fig %>% 
  ggplot(aes(x = YEAR, y = value, linetype = key, color = key)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Women's/Men's Average Hourly Wage", 
    title = "",
    caption = "") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("gray37", "gray2")) +
  theme(
    # Text elements
    plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
    axis.title = element_text(size = base_size * 0.9, face = "bold"),
    axis.text = element_text(size = base_size * 0.8),
    legend.text = element_text(size = base_size * 0.8),
    strip.text = element_text(size = base_size * 0.9, face = "bold"),
    # Plot elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray40", fill = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white"),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "gray95"),
    # Overall aesthetics
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  theme(
  legend.position = c(0.6, 0.25),
legend.justification = c(0, 1))
