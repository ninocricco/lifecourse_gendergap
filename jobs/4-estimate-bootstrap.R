library(dplyr)
library(tictoc) # For timing

org <- read_rds("clean_data/analytic_sample_org_elig_weights.rds") %>%
  mutate(UHRSWORK1_PRED = as.numeric(zap_labels(UHRSWORK1_PRED))) %>%
  filter(UHRSWORK1_PRED > 0) %>%
  mutate(RACEETH = case_when(RACEETH == "black" ~ "Black",
                             RACEETH == "white" ~ "White",
                             RACEETH == "latino" ~ "Latino",
                             RACEETH == "other" ~ "Other"), 
         EDUC2 = ifelse(EDUC == "ba.plus", "BA+", "<BA")
  )

# File to save checkpoint data
saved_results_file_app <- "clean_data/bootstrap_results_checkpoint.RData"

# Check if we have saved results to resume from
if(file.exists(saved_results_file_app)) {
  # Load previous progress
  load(saved_results_file_app)
  
  # Find the maximum index that has data
  max_indices <- c(
    max(as.numeric(names(plot1_data))), 
    max(as.numeric(names(plot2_data))),
    max(as.numeric(names(plot4_data))),
    max(as.numeric(names(plot5_data))),
    max(as.numeric(names(plotcount_data)))
  )
  
  # Start from the next iteration after the minimum complete index
  # This ensures all lists have complete data up to the starting point
  start_i <- min(max_indices) + 1
  cat("Resuming from iteration", start_i, "\n")
} else {
  # Initialize empty named lists
  plot1_data <- list()
  plot2_data <- list()
  plot4_data <- list()
  plot5_data <- list()
  plotcount_data <- list()
  start_i <- 1
  cat("Starting new bootstrap run\n")
}

# Total number of bootstrap iterations
total_iterations <- 10000

# Run bootstrap process
tic()
for(i in start_i:total_iterations) {
  # Set seed for reproducibility
  set.seed(i)
  
  # Create bootstrap sample
  bootstrap_sample <- org %>%
    group_by(across(all_of(group_vars))) %>%
    # Sample with replacement within each group
    group_modify(~ {
      sample_n(.x, size = nrow(.x), replace = TRUE)
    }) %>%
    ungroup()
  
  # Process data for different plots
  data_descplot <- gen_outcome_sumstats(bootstrap_sample, use_birth_group = "BIRTHYEAR_DECADES")
  
  # Store results in named lists (using string names ensures proper indexing)
  plot1_data[[as.character(i)]] <- prepare_plot1_data(data_descplot) %>% select(-label)
  plot2_data[[as.character(i)]] <- prepare_plot2_data(data_descplot)  %>% select(-label)
  plot4_data[[as.character(i)]] <- prepare_plot4_data(gen_outcome_sumstats(bootstrap_sample, use_age_group = TRUE))
  
  data_restplot <- gen_outcome_sumstats(bootstrap_sample)
  plot5_data[[as.character(i)]] <- prepare_plot5_data(data_restplot)
  plotcount_data[[as.character(i)]] <- prepare_counterfactual_data(data_restplot, scenario_type = "all")$data
  
  # Save checkpoint every 100 iterations
  if(i %% 100 == 0) {
    save(plot1_data, plot2_data, plot4_data, plot5_data, plotcount_data, 
         file = saved_results_file)
    cat("Saved checkpoint at iteration ", i, " (", round(i/total_iterations*100), "% complete)\n", sep="")
  }

    cat("Completed iteration: ", i, "/", total_iterations, "\n", sep="")
}
toc()

# Validate results for issues before combining
validate_results <- function() {
  cat("\n------ Validation Report ------\n")
  
  # Lists to check
  lists_to_check <- list(
    plot1_data = plot1_data,
    plot2_data = plot2_data, 
    plot4_data = plot4_data,
    plot5_data = plot5_data,
    plotcount_data = plotcount_data
  )
  
  for(name in names(lists_to_check)) {
    data_list <- lists_to_check[[name]]
    
    # Check for NULL entries
    null_count <- sum(sapply(data_list, is.null))
    
    # Check for NA values
    na_count <- sum(sapply(data_list, function(df) {
      if(is.data.frame(df)) any(is.na(df)) else FALSE
    }))
    
    # Check for infinite values
    inf_count <- sum(sapply(data_list, function(df) {
      if(is.data.frame(df)) any(is.infinite(as.matrix(df))) else FALSE
    }))
    
    # Report
    cat(name, "summary:\n")
    cat("- Total entries:", length(data_list), "\n")
    cat("- NULL entries:", null_count, "\n")
    cat("- Dataframes with NA values:", na_count, "\n")
    cat("- Dataframes with infinite values:", inf_count, "\n")
    
    # If there are issues, report the first few problematic indices
    if(inf_count > 0) {
      inf_indices <- which(sapply(data_list, function(df) {
        if(is.data.frame(df)) any(is.infinite(as.matrix(df))) else FALSE
      }))
      cat("- First few indices with infinites:", head(inf_indices, 5), "\n")
      
      # Examine first problematic dataframe
      first_prob_df <- data_list[[inf_indices[1]]]
      inf_cols <- which(sapply(first_prob_df, function(col) any(is.infinite(col))))
      cat("- Columns with infinites in first problematic dataframe:", 
          names(first_prob_df)[inf_cols], "\n")
    }
    
    cat("\n")
  }
}

# Run validation
validate_results()

# Save final results
saveRDS(plot1_data %>% bind_rows(), file = file.path("clean_data", "plot1_data.rds"))
saveRDS(plot2_data %>% bind_rows(), file = file.path("clean_data", "plot2_data.rds"))
saveRDS(plot4_data %>% bind_rows(), file = file.path("clean_data", "plot4_data.rds"))
saveRDS(plot5_data %>% bind_rows(), file = file.path("clean_data", "plot5_data.rds"))
saveRDS(plotcount_data %>% bind_rows(), file = file.path("clean_data", "plotcount_data.rds"))
cat("Bootstrap completed. Final results saved.\n")


# Function to safely combine dataframes, handling NULLs and infinites
combine_results <- function() {
  # For each list, create a combined dataframe
  cat("\n------ Combining Results ------\n")
  
  lists_to_combine <- list(
    plot1_data = plot1_data,
    plot2_data = plot2_data, 
    plot4_data = plot4_data,
    plot5_data = plot5_data,
    plotcount_data = plotcount_data
  )
  
  combined_results <- list()
  
  for(name in names(lists_to_combine)) {
    cat("Processing", name, "...\n")
    data_list <- lists_to_combine[[name]]
    
    # Add source index as a column
    for(i in names(valid_entries)) {
      if(is.data.frame(valid_entries[[i]])) {
        valid_entries[[i]]$bootstrap_index <- as.integer(i)
      }
    }
    
    # Handle infinites by replacing with NA
    for(i in names(valid_entries)) {
      if(is.data.frame(valid_entries[[i]])) {
        df <- valid_entries[[i]]
        if(any(is.infinite(as.matrix(df)))) {
          # Replace infinites with NA
          for(col in names(df)) {
            if(any(is.infinite(df[[col]]))) {
              inf_count <- sum(is.infinite(df[[col]]))
              cat("  Replacing", inf_count, "infinite values with NA in column", col, 
                  "of iteration", i, "\n")
              df[[col]][is.infinite(df[[col]])] <- NA
            }
          }
          valid_entries[[i]] <- df
        }
      }
    }
    
    # Combine all valid entries
    if(length(valid_entries) > 0) {
      combined_df <- bind_rows(valid_entries)
      combined_results[[name]] <- combined_df
      cat("  Combined", length(valid_entries), "dataframes with", nrow(combined_df), "total rows\n")
    } else {
      cat("  No valid entries to combine\n")
      combined_results[[name]] <- NULL
    }
  }
  
  # Save combined results
  save(combined_results, file = "clean_data/bootstrap_combined_results.rds")
  cat("\nAll results combined and saved to 'bootstrap_combined_results.rds'\n")
  
  return(combined_results)
}

# Combine all results
combined_data <- combine_results()

# Summary of combined data
cat("\n------ Combined Data Summary ------\n")
for(name in names(combined_data)) {
  df <- combined_data[[name]]
  if(!is.null(df)) {
    na_cols <- names(df)[sapply(df, function(col) any(is.na(col)))]
    na_count <- sum(sapply(df, function(col) sum(is.na(col))))
    
    cat(name, "summary:\n")
    cat("- Dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
    cat("- Total NA values:", na_count, "\n")
    if(length(na_cols) > 0) {
      cat("- Columns with NA values:", paste(na_cols, collapse=", "), "\n")
    }
    cat("\n")
  }
}

cat("Bootstrap analysis complete!\n")