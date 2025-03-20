#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: CREATING FUNCTIONS THAT GENERATE PROJECT OUTPUT
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
# Loading libraries
library(tidyverse)
library(ipumsr)
library(janitor)
library(Hmisc)
library(ggrepel)
library(gridExtra)
library(grid)
library(broom)
library(weights)

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

figs_cohort_colors <- function(use_grayscale = T){
  if(use_grayscale == T){
    return(c(
      "1930s" = "grey80",
      "1940s" = "grey66",
      "1950s" = "grey52",
      "1960s" = "grey38",
      "1970s" = "grey24",
      "1980-1995" = "grey10"
    ))
  }
  else{
    color_palette <- c("#0d0887", "#46039f", "#9c179e", "#bd3786",
                       "#d8576b", "#ed7953", "#fdb42f")
    return(c("1930s" = color_palette[7],
             "1940s" = color_palette[6],
             "1950s" = color_palette[2],
             "1960s" = color_palette[3],
             "1970s" = color_palette[4],
             "1980-1995" = color_palette[5]
             ))
  }
}


create_plot1 <- function(data, 
                         use_grayscale = FALSE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage", 
                         data_source_label = "Outgoing Rotation Group", 
                         title = NULL,
                         caption = NULL,
                         base_size = 14, legend.position = "none",
                         decades_to_include = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980-1995")) {
  
  if(is.null(title)){
    title <- "Gender Wage Gap by Age and Cohort, 1982-2023"
  }
  
  if(is.null(caption)){
    caption <- sprintf("Note: Data from the Current Population Survey %s.", data_source_label)
  }
  
  data %>%
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
    filter(BIRTHYEAR_DECADES %in% decades_to_include) %>%
    ggplot(aes(x = AGE, y = ratio, color = BIRTHYEAR_DECADES, label = label,
               linetype = "solid")) +
    geom_label_repel(box.padding   = 1.5, 
                     point.padding = 0.3,
                     show.legend = FALSE, 
                     fill = "white",
                     size = 5) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Age", 
         y = sprintf("%s %s, Women/Men",
                    str_to_title(sumstat_label), outcome_label,
                    str_to_title(sumstat_label), outcome_label), 
         title = title,
        caption = caption
    ) +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort"), 
           linetype = "none") + 
    theme(
      # Text elements
      plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
      axis.title = element_text(size = base_size * 0.9, face = "bold"),
      axis.text = element_text(size = base_size * 0.8),
      legend.title = element_text(size = base_size * 0.9, face = "bold"),
      legend.text = element_text(size = base_size * 0.8),
      strip.text = element_text(size = base_size * 0.9, face = "bold"),
      
      # Plot elements
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.border = element_rect(color = "gray40", fill = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white"),
      strip.background = element_rect(fill = "gray95"),

      # Overall aesthetics
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    theme(legend.position = "none") +
    ylim(.5, 1) +
    xlim(25, 55) +
    geom_hline(yintercept = 1, linetype = "dashed")
  
}


create_plot2 <- function(data, 
                         use_grayscale = FALSE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage",
                         title = NULL, caption = NULL,
                         base_size = 14,
                         data_source_label = "Outgoing Rotation Group", 
                         decades_to_include = c("1940s", "1950s", "1960s", "1970s", "1980-1995"),
                         start_age = 25) {
  
  if(is.null(title)){
    title <- "Age-Graded Change in the Gender Wage Gap by Cohort, 1982-2023"
  }
  
  if(is.null(caption)){
    caption <- sprintf("Data from the Current Population Survey %s", data_source_label)
  }
  
  
  data %>%
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
      ratio.change = (ratio - ratio.start) * 100,
    ) %>%
    ggplot(aes(x = AGE, y = ratio.change, color = BIRTHYEAR_DECADES, 
               label = label, linetype = "solid")) +
    geom_line(size = 1.1) +
    geom_label_repel(
      box.padding = 4,
      point.padding = 0.1,
      show.legend = FALSE,
      fill = "white",
      size = 5
    ) +
    theme_bw() +
    labs(
      x = "Age",
      y = sprintf(" %% Change from Age %d, %s %s",   
                 start_age, sumstat_label, outcome_label), 
      title = title,
      caption = caption) +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(
      color = guide_legend(title = "Birth Cohort"),
      linetype = "none"
    ) +
    ylim(-15, 2) +
    xlim(start_age, 55) + 
    theme(
      # Text elements
      plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
      axis.title = element_text(size = base_size * 0.9, face = "bold"),
      axis.text = element_text(size = base_size * 0.8),
      legend.title = element_text(size = base_size * 0.9, face = "bold"),
      legend.text = element_text(size = base_size * 0.8),
      strip.text = element_text(size = base_size * 0.9, face = "bold"),
      
      # Plot elements
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.border = element_rect(color = "gray40", fill = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white"),
      strip.background = element_rect(fill = "gray95"),
      
      # Overall aesthetics
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 0, linetype = "dashed")
}

fig_age_colors <- function(use_grayscale = T){
  if(use_grayscale == T){
    return(c(
      "25" = "grey80",
      "34" = "grey52",
      "43" = "grey24",
      "55" = "grey10"
    ))
  }
  else{
    color_palette <- c("#46039f", "#d8576b", "#ed7953", "#fdb42f")
    return(c("25" = color_palette[4],
             "34" = color_palette[3],
             "43" = color_palette[2],
             "55" = color_palette[1]
    ))
  }
}

  
create_plot4 <- function(data, 
                         use_grayscale = F,
                         save_plot = FALSE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage",
                         data_source_label = "Outgoing Rotation Group",
                         title = NULL, caption = NULL,
                         refcohort = 1957,
                         start_age = 25,
                         base_size = 14,
                         ages_selection = c(25, 34, 43, 55)) {
  
  if(is.null(title)){
    title <- sprintf("Women's and Men's Age-Specific %s %s\nRelative to %d Birth Cohort",
                     str_to_title(sumstat_label), outcome_label, refcohort)
  }
  
  if(is.null(caption)){
    caption <- sprintf("Data from the Current Population Survey %s", data_source_label)
  }
  
  # Get reference data using specified age
  ref_data <- data %>% 
    filter(BIRTHYEAR_GROUP == refcohort) %>%
    ungroup() %>%  
    transmute(
      FEMALE, 
      AGE_GROUP, 
      REF_OUTCOME = MEAN.OUTCOME
    )
  

  data %>%
    left_join(ref_data, by = c("FEMALE", "AGE_GROUP")) %>%
    filter(BIRTHYEAR_GROUP >= refcohort, 
           AGE_GROUP %in% c(ages_selection)) %>%
    mutate(
      WAGE_REL_REF = MEAN.OUTCOME/REF_OUTCOME,
      AGE_GROUP = factor(AGE_GROUP)  # Convert to factor for proper legend ordering
    ) %>%
    ggplot(aes(x = BIRTHYEAR_GROUP, y = WAGE_REL_REF, color = AGE_GROUP)) +
    geom_line(size = 1) +
    facet_wrap(~FEMALE) +
    theme_bw() +
    labs(
      x = "Birth Cohort",
      y = sprintf("%s %s Relative to %d Birth Cohort", 
                  str_to_title(sumstat_label), outcome_label, refcohort),
      title = title,
      caption = caption
    ) +
    scale_color_manual(values = fig_age_colors(use_grayscale)) +
    guides(
      color = guide_legend(title = "Age Group", order = 1)
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme(
    # Text elements
    plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
    axis.title = element_text(size = base_size * 0.9, face = "bold"),
    axis.text = element_text(size = base_size * 0.8),
    legend.title = element_text(size = base_size * 0.9, face = "bold"),
    legend.text = element_text(size = base_size * 0.8),
    strip.text = element_text(size = base_size * 0.9, face = "bold"),
    
    # Plot elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray40", fill = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white"),
    strip.background = element_rect(fill = "gray95"),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    
    # Overall aesthetics
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )
  
}

fig_cohort_colors <- function(use_grayscale = T){
  if(use_grayscale == T){
    return(c(
      "1988-1990" = "grey80",
      "1978-1980" = "grey52",
      "1968-1970" = "grey38",
      "1958-1960" = "grey10"
    ))
  }
  else{
    color_palette <- c("#46039f", "#d8576b", "#ed7953", "#fdb42f")
    return(c(
      "1988-1990" = color_palette[1],
      "1978-1980" = color_palette[2],
      "1968-1970" = color_palette[3],
      "1958-1960" = color_palette[4]
    ))
  }
}


create_plot5 <- function(data, 
                         use_grayscale = TRUE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage",
                         data_source_label = "Outgoing Rotation Group",
                         refcohort = 1957,
                         start_age = 25,
                         base_size = 14,
                         title = NULL, caption = NULL,
                         cohort_groups = list(
                           c(1958:1960, "1958-1960"),
                           c(1968:1970, "1968-1970"),
                           c(1978:1980, "1978-1980"),
                           c(1988:1990, "1988-1990")
                         )) {
  
  if(is.null(title)){
    title <-  sprintf("Women's and Men's %s Pay Trajectories by Cohort",
                      str_to_title(sumstat_label))
  }
  
  if(is.null(caption)){
    caption <- sprintf("Data from the Current Population Survey %s", data_source_label)
  }
  
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
  
  data <- data %>%
    left_join(ref_data, by = c("FEMALE", "BIRTHYEAR")) %>%
    mutate(WAGE.GROWTH = MEAN.OUTCOME/START_OUTCOME) %>%
    filter(BIRTHYEAR > refcohort, AGE >= start_age) %>%
    mutate(
      BIRTHYEAR_GROUP = case_when(!!!cohort_cases),
      BIRTHYEAR_GROUP = factor(BIRTHYEAR_GROUP, 
                               levels = sapply(cohort_groups, function(x) x[length(x)])))
    
    data %>% 
    group_by(AGE, BIRTHYEAR_GROUP, FEMALE) %>%
    filter(complete.cases(BIRTHYEAR_GROUP)) %>%
    summarise(WAGE.GROWTH = mean(WAGE.GROWTH), .groups = "drop") %>%
    ggplot(aes(x = AGE, y = WAGE.GROWTH, color = BIRTHYEAR_GROUP)) +
    geom_line(size = 1) +
    facet_wrap(~FEMALE) +
    theme_bw() +
    labs(
      x = "Age",
      y = sprintf("Age-Specific %s %s Relative to Age %d",
                  str_to_title(sumstat_label), outcome_label, start_age),
      title = title,
      caption = caption
    ) +
      scale_color_manual(values = fig_cohort_colors(use_grayscale)) +
    guides(
      color = guide_legend(title = "Birth Cohort", order = 1),
      linetype = "none"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      theme(
        # Text elements
        plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
        axis.title = element_text(size = base_size * 0.9, face = "bold"),
        axis.text = element_text(size = base_size * 0.8),
        legend.title = element_text(size = base_size * 0.9, face = "bold"),
        legend.text = element_text(size = base_size * 0.8),
        strip.text = element_text(size = base_size * 0.9, face = "bold"),
        
        # Plot elements
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.border = element_rect(color = "gray40", fill = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white"),
        strip.background = element_rect(fill = "gray95"),
        legend.position = c(0.35, .45),
        legend.justification = c(0, 1),
        
        # Overall aesthetics
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(15, 15, 15, 15)
      )
}

create_plot3 <- function(data,
                                 group_var = NULL, 
                         title = NULL,
                         base_size = 14,
                         caption = NULL,
                                 use_grayscale = F,
                                 sumstat_label = "Average",
                                 outcome_label_axis = "Hourly Wage",
                         outcome_label_caption = "Hourly Wage",
                                 data_source_label = "Outgoing Rotation Group",
                                 refcohort = 1957,
                                 start_age = 25,
                                 scenario_type = "baseline") {
  
  if(is.null(title)){
    title <-  sprintf("Counterfactual Trends in the Gender Wage Gap, 1982-2023",
                      start_age, refcohort)
  }
  
  if(is.null(caption)){
    caption <- sprintf(
            "Data from the Current Population Survey %s.\nSample members aged %d-55, excluding self-employed, military, agricultural, and unpaid family workers.\nOutcome: %s. Reference Cohort: %d.",
            data_source_label, start_age, outcome_label_caption, refcohort
          )
  }
  
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
  
  colors <- if(use_grayscale) {
    setNames(c("gray69", "gray37", "gray45", "gray25", "gray15", "gray55", "gray75"), scenario_names)
  } else {
    setNames(c("gray69", "gray37", "#ed7953", "#9FDA3AFF", "#9c179e", "#277F8EFF", "#0d0887"), scenario_names)
  }
  
  line_types <- setNames(
    c("solid", "solid", "dashed", "dashed", "dotted", "dashed", "dotted"),
    scenario_names
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
  plot_list <- list()
  
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
    
    # Create plot with correct styling based on group count
    if (length(groups) == 1 && group == "All Data") {
      # Single plot (original style)
      plot_list[[group]] <- counterfactual_fig %>%
        mutate(
          key = factor(key, levels = scenarios_to_include),
          label = case_when(
            YEAR == 1982 & key == "Observed" ~ key,
            YEAR == 1985 & key == "Baseline" ~ key,
            YEAR == 1995 & key == sprintf("Cohort Starting Wage, %d Trajectory", refcohort) ~ key,
            YEAR == 2000 & key == sprintf("%d Starting Wage, Cohort Trajectory", refcohort) ~ key,
            TRUE ~ NA_character_
          )
        ) %>%
        ggplot(aes(x = YEAR, y = value, color = key, linetype = key, label = label)) +
        geom_line(size = 1.5) +
        theme_bw() +
        labs(
          x = "Year",
          y = sprintf("Women's / Men's %s %s",
                      str_to_title(sumstat_label), outcome_label_axis),
          title = title,
          caption = caption
        ) +
        scale_colour_manual(values = colors) +
        scale_linetype_manual(guide = "none", values = line_types) +
        theme(
          # Text elements
          plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
          plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
          plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
          axis.title = element_text(size = base_size * 0.9, face = "bold"),
          axis.text = element_text(size = base_size * 0.8),
          legend.title = element_text(size = base_size * 0.9, face = "bold"),
          legend.text = element_text(size = base_size * 0.8),
          strip.text = element_text(size = base_size * 0.9, face = "bold"),
          
          # Plot elements
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90"),
          panel.border = element_rect(color = "gray40", fill = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white"),
          strip.background = element_rect(fill = "gray95"),
          legend.position = c(0.05, .95),
          legend.justification = c(0, 1),
          
          # Overall aesthetics
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(15, 15, 15, 15)
        ) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        scale_x_continuous(breaks = round(seq(1970, 2020, by = 10))) +
        guides(color = guide_legend(title = "", ncol = 1, order = 1))
    } else {
      # Grouped plot style
      plot_list[[group]] <- counterfactual_fig %>%
        ggplot(aes(x = YEAR, y = value, color = key, linetype = key)) +
        geom_line(size = 1.5) +
        theme_bw() +
        labs(
          x = "Year",
          y = "",
          title = group
        ) +
        scale_colour_manual(values = colors) +
        scale_linetype_manual(guide = "none", values = line_types) +
        theme(
          # Text elements
          plot.title = element_text(hjust = 0.5, size = base_size * 1.2, face = "bold", margin = margin(0, 0, 10, 0)),
          plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
          plot.caption = element_text(size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
          axis.title = element_text(size = base_size * 0.9, face = "bold"),
          axis.text = element_text(size = base_size * 0.8),
          legend.title = element_text(size = base_size * 0.9, face = "bold"),
          legend.text = element_text(size = base_size * 0.8),
          strip.text = element_text(size = base_size * 0.9, face = "bold"),
          
          # Plot elements
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90"),
          panel.border = element_rect(color = "gray40", fill = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white"),
          strip.background = element_rect(fill = "gray95"),
          legend.position = c(0.05, .95),
          legend.justification = c(0, 1),
          
          # Overall aesthetics
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(15, 15, 15, 15)
        ) +
        theme(legend.position = "none") +
        geom_hline(yintercept = 1, linetype = "dashed") +
        scale_x_continuous(breaks = round(seq(1970, 2020, by = 10)))
    }
  }
  
  # Return appropriate layout
  if (length(groups) == 1) {
    return(plot_list[[1]])
  } else {
    # Create legend from the first plot
    legend_plot <- plot_list[[1]] + 
      theme(legend.position = "bottom") +
      guides(color = guide_legend(title = "", ncol = 3, order = 1))
    legend <- g_legend(legend_plot)
    
    # Arrange plots in grid with legend at bottom
    grid.arrange(
      do.call(arrangeGrob, c(plot_list, ncol = 2)),
      legend,
      left = sprintf("Women's/ Men's %s %s",
                     str_to_title(sumstat_label), outcome_label_axis),
      ncol = 1,
      heights = c(10, 1)
    )
  }
}
