plot1_data <- list()
plot2_data <- list()
plot4_data <- list()
plot5_data <- list()
plotcount_data <- list()

tic()
for(i in 1001:10000){
  
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

  
  
  

