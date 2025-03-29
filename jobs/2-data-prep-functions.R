#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: DATA CALCULATION FUNCTIONS
# AUTHOR: NINO CRICCO (REORGANIZED)
#------------------------------------------------------------------------------
# Loading libraries
library(tidyverse)
library(janitor)
library(Hmisc)
library(weights)
library(boot)

#------------------------------------------------------------------------------
# SUMMARY STATISTICS FUNCTIONS
#------------------------------------------------------------------------------

gen_outcome_sumstats <- function(data,
                                 outcome_var = "EARNHRLY_MAIN",
                                 weight_var = "EARNWT",
                                 sumstat = "mean",
                                 probs = 0.5,
                                 group_var = NULL,
                                 use_age_group = FALSE,
                                 use_birth_group = "BIRTHYEAR") {
  
  if (use_age_group) {
    use_birth_group <- "BIRTHYEAR_GROUP"
  }
  
  # Setting which summary function to use
  summary_func <- if(sumstat == "mean") {
    function(x, w) weighted.mean(x, w, na.rm = TRUE)
  } else if(sumstat == "quantile") {
    function(x, w) wtd.quantile(x, weights = w, probs = probs)
  } else {
    stop("sumstat must be either 'mean' or 'quantile'")
  }
  
  # Defining Age/Cohort/Comparison group variables
  group_cols <- list(
    if(use_age_group) "AGE_GROUP" else "AGE",
    use_birth_group,
    # Only include YEAR if not using BIRTHYEAR_DECADES
    if(use_birth_group != "BIRTHYEAR_DECADES") "YEAR" else NULL,
    "FEMALE"
  )
  
  # Remove NULL elements from group_cols
  group_cols <- group_cols[!sapply(group_cols, is.null)]
  
  # Adding grouping variable if doing subgroup analyses
  if(!is.null(group_var)) {
    group_cols <- c(group_cols, group_var)
  }
  
  # Calculate summary statistics
  sumstats <- data %>%
    group_by(across(all_of(unlist(group_cols)))) %>%
    summarise(
      MEAN.OUTCOME = summary_func(!!sym(outcome_var), !!sym(weight_var)),
      n = n(),
      .groups = "drop"
    )
  
  return(sumstats)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 1
#------------------------------------------------------------------------------

prepare_plot1_data <- function(data, 
                              decades_to_include = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980-1995")) {
  
  transformed_data <- data %>%
    select(AGE, BIRTHYEAR_DECADES, FEMALE, MEAN.OUTCOME) %>% 
    filter(complete.cases(BIRTHYEAR_DECADES)) %>%
    mutate(drop = case_when(BIRTHYEAR_DECADES == "1970s" & AGE > 45 ~ 1,
                            BIRTHYEAR_DECADES == "1980-1995" & AGE > 35 ~ 1,
                            TRUE ~ 0)) %>%
    filter(drop == 0, AGE <= 55) %>%
    pivot_wider(names_from = FEMALE, values_from = MEAN.OUTCOME) %>% 
    mutate(ratio = Women/Men) %>%
    mutate(label = case_when(AGE == 35 & BIRTHYEAR_DECADES == "1980-1995" ~ BIRTHYEAR_DECADES,
                             AGE == 44 & BIRTHYEAR_DECADES == "1970s" ~ BIRTHYEAR_DECADES,
                             AGE == 47 & BIRTHYEAR_DECADES == "1960s" ~ BIRTHYEAR_DECADES,
                             AGE == 55 & BIRTHYEAR_DECADES == "1950s" ~ BIRTHYEAR_DECADES,
                             AGE == 35 & BIRTHYEAR_DECADES == "1940s" ~ BIRTHYEAR_DECADES,
                             AGE == 52 & BIRTHYEAR_DECADES == "1930s" ~ BIRTHYEAR_DECADES)) %>%
    filter(BIRTHYEAR_DECADES %in% decades_to_include)
  
  return(transformed_data)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 2
#------------------------------------------------------------------------------

prepare_plot2_data <- function(data, 
                              decades_to_include = c("1940s", "1950s", "1960s", "1970s", "1980-1995"),
                              start_age = 25) {
  
  transformed_data <- data %>%
    select(AGE, BIRTHYEAR_DECADES, FEMALE, MEAN.OUTCOME) %>%
    filter(complete.cases(BIRTHYEAR_DECADES)) %>%
    mutate(drop = case_when(BIRTHYEAR_DECADES == "1970s" & AGE > 45 ~ 1,
                            BIRTHYEAR_DECADES == "1980-1995" & AGE > 35 ~ 1,
                            TRUE ~ 0), 
           label = case_when(AGE == start_age & BIRTHYEAR_DECADES == "1980-1995" ~ BIRTHYEAR_DECADES,
                             AGE == 45 & BIRTHYEAR_DECADES == "1970s" ~ BIRTHYEAR_DECADES,
                             AGE == 51 & BIRTHYEAR_DECADES == "1960s" ~ BIRTHYEAR_DECADES,
                             AGE == 52 & BIRTHYEAR_DECADES == "1950s" ~ BIRTHYEAR_DECADES,
                             AGE == 35 & BIRTHYEAR_DECADES == "1940s" ~ BIRTHYEAR_DECADES)) %>%
    filter(drop == 0, AGE <= 55, AGE >= start_age) %>%
    pivot_wider(names_from = FEMALE, values_from = MEAN.OUTCOME) %>%
    mutate(ratio = Women/Men) %>%
    filter(BIRTHYEAR_DECADES %in% decades_to_include) %>%
    group_by(BIRTHYEAR_DECADES) %>%
    mutate(
      ratio.start = ifelse(AGE == start_age, ratio, NA),
      ratio.start = tidyr::fill(data.frame(ratio.start), ratio.start, .direction = "down")$ratio.start,
      ratio.change = (ratio - ratio.start) * 100
    )
  
  return(transformed_data)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 4
#------------------------------------------------------------------------------

prepare_plot4_data <- function(data, 
                              refcohort = 1957,
                              ages_selection = c(25, 34, 43, 55)) {
  
  # Get reference data using specified age
  ref_data <- data %>% 
    filter(BIRTHYEAR_GROUP == refcohort) %>%
    ungroup() %>%  
    transmute(
      FEMALE, 
      AGE_GROUP, 
      REF_OUTCOME = MEAN.OUTCOME
    )
  
  transformed_data <- data %>%
    left_join(ref_data, by = c("FEMALE", "AGE_GROUP")) %>%
    filter(BIRTHYEAR_GROUP >= refcohort, 
           AGE_GROUP %in% c(ages_selection)) %>%
    mutate(
      WAGE_REL_REF = MEAN.OUTCOME/REF_OUTCOME,
      AGE_GROUP = factor(AGE_GROUP)  # Convert to factor for proper legend ordering
    )
  
  return(transformed_data)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 5
#------------------------------------------------------------------------------

prepare_plot5_data <- function(data, 
                              refcohort = 1957,
                              start_age = 25,
                              cohort_groups = list(
                                c(1958:1960, "1958-1960"),
                                c(1968:1970, "1968-1970"),
                                c(1978:1980, "1978-1980"),
                                c(1988:1990, "1988-1990")
                              )) {
  
  # Create reference data at start age
  ref_data <- data %>% 
    filter(AGE == start_age) %>%
    ungroup() %>% 
    transmute(
      FEMALE, 
      BIRTHYEAR, 
      START_OUTCOME = MEAN.OUTCOME
    )
  
  # Create cohort grouping expression dynamically
  cohort_cases <- lapply(cohort_groups, function(group) {
    years <- group[1:(length(group)-1)]
    label <- group[length(group)]
    quo(BIRTHYEAR %in% !!years ~ !!label)
  })
  
  transformed_data <- data %>%
    left_join(ref_data, by = c("FEMALE", "BIRTHYEAR")) %>%
    mutate(WAGE.GROWTH = MEAN.OUTCOME/START_OUTCOME) %>%
    filter(BIRTHYEAR > refcohort, AGE >= start_age) %>%
    mutate(
      BIRTHYEAR_GROUP = case_when(!!!cohort_cases),
      BIRTHYEAR_GROUP = factor(BIRTHYEAR_GROUP, 
                               levels = sapply(cohort_groups, function(x) x[length(x)])))
  
  summarized_data <- transformed_data %>% 
    group_by(AGE, BIRTHYEAR_GROUP, FEMALE) %>%
    filter(complete.cases(BIRTHYEAR_GROUP)) %>%
    summarise(WAGE.GROWTH = mean(WAGE.GROWTH), .groups = "drop")
  
  return(summarized_data)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 3 (COUNTERFACTUAL ANALYSIS)
#------------------------------------------------------------------------------

prepare_counterfactual_data <- function(data,
                                       group_var = NULL,
                                       refcohort = 1957,
                                       start_age = 25,
                                       scenario_type = "baseline") {
  
  # If no group_var specified, create a dummy group with constant value
  if (is.null(group_var)) {
    data$dummy_group <- "All Data"
    group_var <- "dummy_group"
  }
  
  # Ensure group variable is a factor
  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }
  
  # Define scenarios and styling
  scenario_names <- c(
    "Observed",
    "Baseline",
    sprintf("Men %d, Women Observed", refcohort),
    sprintf("Cohort Starting Wage, %d Trajectory", refcohort),
    sprintf("%d Starting Wage, Cohort Trajectory", refcohort),
    sprintf("Cohort Starting Wage, %d Trajectory, Men %d", refcohort, refcohort),
    sprintf("%d Starting Wage, Cohort Trajectory, Men %d", refcohort, refcohort)
  )
  
  scenarios_to_include <- if(scenario_type == "baseline") {
    c(
      "Observed",
      "Baseline",
      sprintf("Cohort Starting Wage, %d Trajectory", refcohort),
      sprintf("%d Starting Wage, Cohort Trajectory", refcohort)
    )
  } else if(scenario_type == "all") {
    c(
      "Observed",
      "Baseline",
      sprintf("Cohort Starting Wage, %d Trajectory", refcohort),
      sprintf("%d Starting Wage, Cohort Trajectory", refcohort),
      sprintf("Cohort Starting Wage, %d Trajectory, Men %d", refcohort, refcohort),
      sprintf("%d Starting Wage, Cohort Trajectory, Men %d", refcohort, refcohort),
      sprintf("Men %d, Women Observed", refcohort)
    )
  }
  
  # Process each group
  groups <- levels(data[[group_var]])
  counterfactual_list <- list()
  
  for (group in groups) {
    # Filter data for current group
    group_data <- data %>% 
      filter(!!sym(group_var) == group) %>%
      filter(AGE >= start_age)
    
    # Calculate counterfactuals
    counterfactual_data <- group_data %>%
      left_join(., group_data %>%
                  group_by(YEAR, FEMALE) %>%
                  summarise(
                    n_year = sum(n)), by = c("YEAR", "FEMALE")) %>%
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
      mutate(
        mean_c1_age = ifelse(BIRTHYEAR <= refcohort, MEAN.OUTCOME,
                             mean_c_start * pct.change_refcohort_start),
        mean_c2_age = ifelse(BIRTHYEAR <= refcohort, MEAN.OUTCOME,
                             mean_refcohort_start * pct.change_c_start),
        mean_c3_refcohort = ifelse(BIRTHYEAR <= refcohort, MEAN.OUTCOME,
                                   mean_refcohort_start * pct.change_refcohort_start),
        mean_age_adj = MEAN.OUTCOME * lf_cohortprop,
        mean_c1_age_adj = mean_c1_age * lf_cohortprop,
        mean_c2_age_adj = mean_c2_age * lf_cohortprop,
        mean_c3_age_adj = mean_c3_refcohort * lf_cohortprop
      ) %>%
      group_by(YEAR, FEMALE) %>%
      summarise(
        c1 = sum(mean_c1_age_adj),
        c2 = sum(mean_c2_age_adj),
        c3 = sum(mean_c3_age_adj),
        agg = sum(mean_age_adj),
        .groups = "drop"
      ) %>%
      gather(counterfactual, value, -c(YEAR, FEMALE)) %>%
      pivot_wider(names_from = c(FEMALE, counterfactual), values_from = value) %>%
      mutate(
        !!sprintf("Cohort Starting Wage, %d Trajectory", refcohort) := Women_c1/Men_c1,
        !!sprintf("%d Starting Wage, Cohort Trajectory", refcohort) := Women_c2/Men_c2,
        !!sprintf("Cohort Starting Wage, %d Trajectory, Men %d", refcohort, refcohort) := Women_c1/Men_c3,
        !!sprintf("%d Starting Wage, Cohort Trajectory, Men %d", refcohort, refcohort) := Women_c2/Men_c3,
        "Baseline" = Women_c3/Men_c3,
        "Observed" = Women_agg/Men_agg,
        !!sprintf("Men %d, Women Observed", refcohort) := Women_agg/Men_c3
      ) %>%
      select(YEAR, all_of(scenarios_to_include)) %>%
      gather(key, value, -YEAR)
    
    counterfactual_list[[group]] <- counterfactual_fig %>%
      mutate(
        group = group,
        key = factor(key, levels = scenarios_to_include)
      )
  }
  
  # Combine all groups
  combined_counterfactual <- bind_rows(counterfactual_list)
  
  return(list(
    data = combined_counterfactual,
    groups = groups,
    scenarios = scenarios_to_include
  ))
}
