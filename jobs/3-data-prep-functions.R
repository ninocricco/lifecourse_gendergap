#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: DATA PREP FUNCTIONS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

source("jobs/0-helperfunctions.R")

#------------------------------------------------------------------------------
# FUNCTION TO GENERATE SUMMARY STATISTICS
#------------------------------------------------------------------------------

gen_outcome_sumstats <- function(data,
                                 outcome_var = "EARNHRLY_MAIN",
                                 weight_var = "EARNWT",
                                 sumstat = "mean",
                                 probs = 0.5,
                                 group_var = NULL,
                                 use_birth_group = "BIRTHYEAR", 
                                 winsorize = c("bottom" = .005, "top" = .995)) {

  # Setting which statistic to use
  summary_func <- if(sumstat == "mean") {
    function(x, w) weighted.mean(x, w, na.rm = TRUE)
  } else if(sumstat == "quantile") {
    function(x, w) wtd.quantile(x, weights = w, probs = probs)
  } else {
    stop("sumstat must be either 'mean' or 'quantile'")
  }
  
  # Defining Age/Cohort/Comparison group variables
  group_cols <- c(
    "AGE", "FEMALE", use_birth_group,
    # Only include YEAR if not using BIRTHYEAR_DECADES
    if(use_birth_group != "BIRTHYEAR_DECADES") "YEAR" 
  )
  
  # Adding grouping variable if doing subgroup analyses
  if(!is.null(group_var)) {
    group_cols <- c(group_cols, group_var)
  }
  
  if(!is.null(winsorize)){
    data <- data %>%
      group_by(YEAR) %>% 
      mutate(
        bottom = wtd.quantile(!!sym(outcome_var), winsorize[["bottom"]], weight = !!sym(weight_var), na.rm = TRUE),
        top = wtd.quantile(!!sym(outcome_var), winsorize[["top"]], weight = !!sym(weight_var), na.rm = TRUE),
        MEAN.OUTCOME = pmin(pmax(!!sym(outcome_var), bottom), top))
  }
  else if(is.null(winsorize)){
    data <- data %>%
      mutate(MEAN.OUTCOME = !!sym(outcome_var))
  }
  
  if(!is.null(group_var)) {
  # Calculate summary statistics
  sumstats <- data %>%
    ungroup() %>%
    add_count(FEMALE, YEAR, !!sym(group_var), name = "n_year") %>% 
    group_by(across(all_of(group_cols))) %>%
    summarise(MEAN.OUTCOME = summary_func(MEAN.OUTCOME, !!sym(weight_var)),
              n = n(), 
              n_year = first(n_year),
              .groups = "drop"
    )
  }
  else if(is.null(group_var)){
    sumstats <- data %>%
      ungroup() %>%
      add_count(FEMALE, YEAR, name = "n_year") %>% 
      group_by(across(all_of(group_cols))) %>%
      summarise(MEAN.OUTCOME = summary_func(MEAN.OUTCOME, !!sym(weight_var)),
                n = n(), 
                n_year = first(n_year),
                .groups = "drop"
      )
  }

  return(sumstats)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 1
#------------------------------------------------------------------------------

prepare_plot1_data <- function(data, decades_to_include = c(
  "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990-1998")) {
  
  transformed_data <- data %>%
    select(AGE, BIRTHYEAR_DECADES, FEMALE, MEAN.OUTCOME) %>% 
    filter(complete.cases(BIRTHYEAR_DECADES)) %>%
    mutate(drop = case_when(BIRTHYEAR_DECADES == "1970s" & AGE > 45 ~ 1,
                            BIRTHYEAR_DECADES == "1980s" & AGE > 35 ~ 1,
                            BIRTHYEAR_DECADES == "1990-1998" & AGE > 25 ~ 1,
                            TRUE ~ 0)) %>%
    filter(drop == 0, AGE < 55) %>%
    pivot_wider(names_from = FEMALE, values_from = MEAN.OUTCOME) %>% 
    mutate(ratio = Women/Men) %>%
    filter(BIRTHYEAR_DECADES %in% decades_to_include)
  
  return(transformed_data)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 2
#------------------------------------------------------------------------------

prepare_plot2_data <- function(data, start_age = 25, decades_to_include = c(
  "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990-1998")) {
  
  transformed_data <- data %>%
    select(AGE, BIRTHYEAR_DECADES, FEMALE, MEAN.OUTCOME) %>%
    filter(complete.cases(BIRTHYEAR_DECADES)) %>%
    mutate(drop = case_when(BIRTHYEAR_DECADES == "1970s" & AGE > 45 ~ 1,
                            BIRTHYEAR_DECADES == "1980s" & AGE > 35 ~ 1,
                            BIRTHYEAR_DECADES == "1990-1998" & AGE > 25 ~ 1,
                            TRUE ~ 0)) %>%
    filter(drop == 0, AGE <= 55, AGE >= start_age) %>%
    pivot_wider(names_from = FEMALE, values_from = MEAN.OUTCOME) %>%
    mutate(ratio = Women/Men) %>%
    filter(BIRTHYEAR_DECADES %in% decades_to_include) %>%
    group_by(BIRTHYEAR_DECADES) %>%
    mutate(
      ratio.start = ifelse(AGE == start_age, ratio, NA),
      ratio.start = tidyr::fill(
        data.frame(ratio.start), ratio.start, .direction = "down")$ratio.start,
      ratio.change = (ratio - ratio.start) * 100
    )
  
  return(transformed_data)
}

#------------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS FOR PLOT 3 (COUNTERFACTUAL ANALYSIS)
#------------------------------------------------------------------------------

prepare_counterfactual_data <- function(data,
                                       group_var = NULL,
                                       refcohort = 1957,
                                       start_age = 25,
                                       scenarios_to_include = "main") {
  
  # If no group_var specified, create a dummy group with constant value
  if (is.null(group_var)) {
    data$dummy_group <- "All Data"
    group_var <- "dummy_group"
  }
  
  # Ensure group variable is a factor
  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }

  scenarios_to_include <- if(scenarios_to_include == "main") {
    c(
      "Observed", "Baseline", "No Change for Men",
      "Changing Starting Points", "Changing Trajectories"
    )
  } else if(scenarios_to_include == "appendix") {
    c(
      "Observed", "Baseline", "No Change for Men",
      "Changing Starting Points", "Changing Trajectories",
      "Changing Starting Points, No Change for Men",
      "Changing Trajectories, No Change for Men"
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
      mutate("Changing Starting Points" = Women_c1/Men_c1,
             "Changing Trajectories" = Women_c2/Men_c2,
             "Changing Starting Points, No Change for Men" = Women_c1/Men_c3,
             "Changing Trajectories, No Change for Men" = Women_c2/Men_c3,
             "Baseline" = Women_c3/Men_c3,
             "Observed" = Women_agg/Men_agg,
             "No Change for Men" = Women_agg/Men_c3
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
