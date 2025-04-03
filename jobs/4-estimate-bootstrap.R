plot1_data <- list()
plot2_data <- list()
plot4_data <- list()
plot5_data <- list()
plotcount_data <- list()

tic()
for(i in 1:10000){
  
  # Create bootstrap sample
  set.seed(i)
  
  bootstrap_sample <- org %>%
    group_by(across(all_of(group_vars))) %>%
    # Sample with replacement within each group
    group_modify(~ {
      sample_n(.x, size = nrow(.x), replace = TRUE)
    }) %>%
    ungroup()
  
  data_descplot <- gen_outcome_sumstats(bootstrap_sample, use_birth_group = "BIRTHYEAR_DECADES")
  
  plot1_data[[i]] <- prepare_plot1_data(data_descplot)
  
  plot2_data[[i]] <- prepare_plot2_data(data_descplot)
  
  plot4_data[[i]] <- prepare_plot4_data(gen_outcome_sumstats(bootstrap_sample, use_age_group = TRUE))
  
  data_restplot <- gen_outcome_sumstats(bootstrap_sample)
                                    
  plot5_data[[i]] <- prepare_plot5_data(data_restplot)
  
  plotcount_data[[i]] <- prepare_counterfactual_data(data_restplot, scenario_type = "all")$data
  
  print(paste0("Completed iteration: ", i))
  
}
toc()

process_and_save <- function(data, name) {
  
  # Transform the data
  df_combined <- do.call(rbind, data) 
  
  # Save to CSV
  write.csv(df_combined, file = paste0("clean_data/", name, ".csv"), row.names = FALSE)
  
  cat(paste0(name, " saved to clean_data/", name, ".csv\n"))
}

process_and_save(plot1_data, "plot1_data")
process_and_save(plot2_data, "plot2_data")
process_and_save(plot4_data, "plot4_data")
process_and_save(plot5_data, "plot5_data")

plotcount_data_df <- plotcount_data %>%
  do.call(rbind, .) 

write_csv(plotcount_data_df, "plotcount_data_df.csv")

plotcount_data_df %>%
  select(-group) %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), 
                   list(q05 = ~quantile(., probs = 0.05, na.rm = T),
                        q95 = ~quantile(., probs = 0.95, na.rm = T))))
  




return_cis <- function(data, level = .95){
  
  # Combine all data frames into one data frame
  df_combined <- do.call(rbind, data) %>%
    pivot_wider(values_from = value, names_from = c(key)) %>%
    unnest()
  
  # Calculate 5th and 95th percentiles for each column
  df_percentiles <- sapply(df_combined, function(x) quantile(x, probs=c(1-level, level)))
  
  return(df_percentiles)
  
}


df_combined <- do.call(rbind, plotcount_data) %>%
  unnest() %>% group_by(YEAR, key) %>%
  summarise(across(where(is.numeric), 
                   list(q05 = ~quantile(., probs = 0.05),
                        q95 = ~quantile(., probs = 0.95))))

test <- prepare_counterfactual_data(gen_outcome_sumstats(org))$data
test2 <- left_join(test, df_combined, by = c("key", "YEAR"))

refcohort = 1957
test2 %>%
  mutate(
    label = case_when(
      YEAR == 1982 & key == "Observed" ~ key,
      YEAR == 1985 & key == "Baseline" ~ key,
      YEAR == 1995 & key == sprintf("Cohort Starting Wage, %d Trajectory", refcohort) ~ key,
      YEAR == 2000 & key == sprintf("%d Starting Wage, Cohort Trajectory", refcohort) ~ key,
      TRUE ~ NA_character_
    )
  ) %>%
  ggplot(aes(x = YEAR, y = value, color = key, linetype = key, label = label)) +
  geom_ribbon(aes(ymin = value_q05, ymax = value_q95, fill = key), 
              alpha = 0.2, color = NA) +
  scale_fill_manual(values = colors, guide = "none") +
  geom_line(size = 1.5) +
  create_common_theme(base_size, c(0.05, .95)) +
  theme(legend.justification = c(0, 1)) +
  labs(
    x = "Year",
    y = sprintf("Women's / Men's %s %s",
                str_to_title(sumstat_label), outcome_label_axis),
    title = title,
    caption = caption
  ) +
  scale_colour_manual(values = colors) +
  scale_linetype_manual(guide = "none", values = line_types) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(1970, 2020, by = 10))) +
  guides(color = guide_legend(title = "", ncol = 1, order = 1))

  

