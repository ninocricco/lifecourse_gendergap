#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: CREATING FIGURES FUNCTIONS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

# Source data calculation AND HELPER functions
source("jobs/0-helperfunctions.R")
source("jobs/2-data-prep-functions.R")

#------------------------------------------------------------------------------
# PLOT 1: GENDER WAGE GAP BY AGE AND COHORT
#------------------------------------------------------------------------------

create_plot1 <- function(data, 
                         use_grayscale = FALSE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage", 
                         data_source_label = "Outgoing Rotation Group", 
                         title = NULL,
                         caption = NULL,
                         base_size = 14, 
                         legend.position = "none",
                         decades_to_include = c(
                           "1927-1936", "1937-1946", "1947-1956", "1957-1966",
                           "1967-1976", "1977-1986", "1987-1996"),
                         with_bootstrap = FALSE,
                         bootstrap_path = "bootstrap_estimates/main/fig1_bootstrapped_estimates.rds",
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- "Gender Wage Gap by Age and Cohort, 1982-2024"
  }
  
  if(is.null(caption)) {
    additional_text <- sprintf("\n Sample members aged %d-55, excludes self-employed, military, and unpaid family workers. \n Outcome: %s.",
                               start_age, outcome_label)
    caption <- create_caption(data_source_label, additional_text)
  }
  
  # Prepare data for plotting
  plot_data <- prepare_plot1_data(data, decades_to_include)
  
  p_gender_data <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, Men, Women) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES)) %>%
    mutate(key = factor(key, levels = c("Men", "Women")))
  
  p_ratio_data <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, ratio) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES))
  
  if (with_bootstrap) {
    
    bootstrap_data <- read_rds(bootstrap_path) %>%
      gather(key, value, -c(AGE, BIRTHYEAR_DECADES, drop)) %>%
      group_by(AGE, BIRTHYEAR_DECADES, key) %>% 
      summarise(ci_lower = quantile(value, probs =  (1-conf_level)/2),
                ci_upper = quantile(value, probs = 1-(1-conf_level)/2))
    
    p_gender_data <- left_join(p_gender_data,
                          bootstrap_data %>% filter(key != "ratio"),
                          by = c("AGE", "BIRTHYEAR_DECADES", "key"))
    
    p_ratio_data <- left_join(p_ratio_data,
                               bootstrap_data %>% filter(key == "ratio"),
                               by = c("AGE", "BIRTHYEAR_DECADES", "key"))
    
  }
  
  p_gender <- p_gender_data %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, "none") +
    labs(x = "", y = "", caption = caption) +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort")) + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    facet_grid(cols = vars(key), scales = "free_y")
  
  p_ratio <- p_ratio_data %>%
    mutate(key = "Ratio") %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, "right") +
    theme(
      legend.position      = c(0.25, 0.6),  
      legend.justification = c("right", "top"),  
      legend.direction     = "vertical",
      legend.background    = element_rect(fill = alpha("white", 0.8), colour = NA)
    ) +
    labs(x = "", y = "",
         title = title) +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort"), 
           linetype = "none") + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    facet_grid(cols = vars(key), scales = "free_y")
  
  if (with_bootstrap) {
    p_gender <- p_gender +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                      fill = BIRTHYEAR_DECADES), 
                  alpha = 0.5, color = NA) +
      scale_fill_manual(values = figs_cohort_colors(use_grayscale)) +
      guides(fill = "none")
    
    p_ratio <- p_ratio +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                      fill = BIRTHYEAR_DECADES), 
                  alpha = 0.5, color = NA) +
      scale_fill_manual(values = figs_cohort_colors(use_grayscale)) +
      guides(fill = "none")
  }
  
  p <- grid.arrange(p_ratio, p_gender, 
                    left  = textGrob(
                      sprintf("%s %s",sumstat_label, outcome_label),
                      rot = 90, 
                      gp  = gpar(fontsize = base_size, fontface = "bold")
                    ),
                    bottom  = textGrob(
                      "Age",    
                      gp  = gpar(fontsize = base_size, fontface = "bold")
                    ))
  
  return(p)
}

#------------------------------------------------------------------------------
# PLOT 2: AGE-GRADED CHANGE IN GENDER WAGE GAP BY COHORT
#------------------------------------------------------------------------------

create_plot2 <- function(data, 
                         use_grayscale = FALSE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage",
                         title = NULL, 
                         caption = NULL,
                         base_size = 14,
                         data_source_label = "Outgoing Rotation Group.", 
                         decades_to_include = c(
                           "1957-1966","1967-1976", "1977-1986", "1987-1996"),
                         start_age = 25,
                         with_bootstrap = FALSE,
                         bootstrap_path = "bootstrap_estimates/main/fig2_bootstrapped_estimates.rds",
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- "Percentage Point Change in the Gender Wage Gap since Age 25, by Cohort, 1982-2024"
  }
  
  if(is.null(caption)) {
    additional_text <- sprintf("\n Sample members aged %d-55, excludes self-employed, military, and unpaid family workers. \n Outcome: %s.",
                               start_age, outcome_label)
    caption <- create_caption(data_source_label, additional_text)
  }
  
  # Prepare data for plotting
  plot_data <- prepare_plot2_data(
    data, decades_to_include = decades_to_include,
    start_age = start_age)
  
  p_gender_data <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, Men.change, Women.change) %>%
    rename(Men = Men.change, Women = Women.change) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES)) %>%
    mutate(key = factor(key, levels = c("Men", "Women")))
  
  p_ratio_data <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, ratio.change) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES))
  
  if (with_bootstrap) {
    
    bootstrap_data <- read_rds(bootstrap_path) %>%
      select(AGE, BIRTHYEAR_DECADES, drop,
             ratio.change, Men.change, Women.change) %>%
      gather(key, value, -c(AGE, BIRTHYEAR_DECADES, drop)) %>%
      group_by(AGE, BIRTHYEAR_DECADES, key) %>% 
      summarise(ci_lower = quantile(value, probs =  (1-conf_level)/2),
                ci_upper = quantile(value, probs = 1-(1-conf_level)/2))
    
    p_gender_data <- left_join(p_gender_data,
                               bootstrap_data %>% 
                                 mutate(key = case_when(
                                   key == "Men.change" ~ "Men", 
                                   key == "Women.change" ~ "Women", 
                                   key == "ratio.change" ~ "ratio.change")) %>%
                                 filter(key != "ratio.change"),
                               by = c("AGE", "BIRTHYEAR_DECADES", "key"))
    
    p_ratio_data <- left_join(p_ratio_data,
                              bootstrap_data %>% filter(key == "ratio.change"),
                              by = c("AGE", "BIRTHYEAR_DECADES", "key"))
    
  }
  
  
  # Create base plot
  p_ratio <- p_ratio_data %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, "right") +
    theme(
      legend.position      = c(0.2, 0.4),
      legend.justification = c("right", "top"),
      legend.direction     = "vertical",
      legend.background    = element_rect(fill = alpha("white", 0.8), colour = NA)
    ) +
    labs(x = "", y = "") +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort")) + 
    labs(
      x = "",
      y = sprintf(" %% Point Change from Age %d, %s %s",   
                 start_age, sumstat_label, outcome_label), 
      title = title) +
    xlim(start_age, 55) + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  p_gender <- p_gender_data %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, "right") +
    theme(
      legend.position = c(0.05, 0.05),  # Near top-left, but offset
      legend.justification = c("left", "bottom"),  # Anchor the top-left of the legend box to that position
      legend.direction = "vertical",
      legend.background = element_rect(fill = alpha("white", 0.8), colour = NA)
    ) +
    labs(x = "", y = sprintf("%% Change from Age %d, %s %s",   
                             start_age, sumstat_label, outcome_label),
         caption = caption) +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort")) + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    facet_grid(cols = vars(key), scales = "free_y")
  
  if (with_bootstrap) {
    p_gender <- p_gender +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                      fill = BIRTHYEAR_DECADES), 
                  alpha = 0.5, color = NA) +
      scale_fill_manual(values = figs_cohort_colors(use_grayscale)) +
      guides(fill = "none")
    
    p_ratio <- p_ratio +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                      fill = BIRTHYEAR_DECADES), 
                  alpha = 0.5, color = NA) +
      scale_fill_manual(values = figs_cohort_colors(use_grayscale)) +
      guides(fill = "none")
  }
  
  p <- grid.arrange(p_ratio, p_gender + theme(legend.position = "none"),
                    bottom  = textGrob(
                      "Age",    
                      gp  = gpar(fontsize = base_size, fontface = "bold")
                    ))
  
  return(p)
}

#------------------------------------------------------------------------------
# PLOT 3: COUNTERFACTUAL TRENDS IN THE GENDER WAGE GAP
#------------------------------------------------------------------------------

create_plot3 <- function(data,
                         group_var = NULL, 
                         title = NULL,
                         base_size = 14,
                         caption = NULL,
                         use_grayscale = FALSE,
                         sumstat_label = "Average",
                         outcome_label_axis = "Hourly Wage",
                         outcome_label_caption = "Hourly Wage",
                         data_source_label = "Outgoing Rotation Group.",
                         refcohort = 1957,
                         start_age = 25,
                         scenarios_to_include = "main",
                         with_bootstrap = FALSE,
                         bootstrap_path = "bootstrap_estimates/main/fig3_bootstrapped_estimates.rds",
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- sprintf("Counterfactual Trends, Gender Wage Gap, 1982-2024")
  }
  
  if(is.null(caption)) {
    additional_text <- sprintf("\n Sample members aged %d-55, excludes self-employed, military, and unpaid family workers. \n Outcome: %s.",
                               start_age, outcome_label)
    caption <- create_caption(data_source_label, additional_text)
  }
  
  # Prepare counterfactual data
  counterfactual_result <- prepare_counterfactual_data(
    data,
    group_var = group_var,
    refcohort = refcohort,
    start_age = start_age,
    scenarios_to_include = scenarios_to_include
  )
  
  plot_data <- counterfactual_result$data %>%
    mutate(key = factor(key, levels = c("Observed", "Baseline",
      "Changing Starting Points",
      "Changing Trajectories",
      "No Change for Men",
      "Women's Changing Starting Points, Men Baseline",
      "Women's Changing Trajectories, Men Baseline",
      "No Change for Women",
      "Men's Changing Starting Points, Women Baseline",
      "Men's Changing Trajectories, Women Baseline"
    )))
  
  # Get bootstrapped data if requested
  if (with_bootstrap) {
     bootstrap_data <- read_rds(bootstrap_path) %>%
      group_by(YEAR, key, group) %>% 
      summarise(ci_lower = quantile(value, probs =  (1-conf_level)/2),
                ci_upper = quantile(value, probs = 1-(1-conf_level)/2)) %>%
       ungroup()
     
     plot_data <- left_join(plot_data, 
                            bootstrap_data %>%
                              filter(key %in% unique(plot_data$key)), 
                            by = c("YEAR", "key", "group"))
       
  }
  
  # Define colors and line types
  scenario_names <- levels(plot_data$key)[unique(plot_data$key)] 
  
  scenario_colours <- if (use_grayscale) {
    c(
      "Observed"                                          = "gray10",
      "Baseline"                                          = "gray10",
      "No Change for Men"                                 = "gray70",
      "No Change for Women"                                 = "gray70",
      "Changing Starting Points"                          = "gray55",
      "Changing Trajectories"                             = "gray30",
      "Women's Changing Starting Points, Men Baseline"       = "gray55",
      "Women's Changing Trajectories, Men Baseline"          = "gray30",
      "Men's Changing Starting Points, Women Baseline"       = "gray55",
      "Men's Changing Trajectories, Women Baseline"          = "gray30"
    )
  } else {
    c(
      "Observed"                                          = "#000000",
      "Baseline"                                          = "#7F7F7F",
      "No Change for Men"                                 = "#ED7953",
      "No Change for Women"                                 = "#660000",
      "Changing Starting Points"                          = "#9FDA3A",
      "Changing Trajectories"                             = "#9C179E",
      "Women's Changing Starting Points, Men Baseline"       = "#277F8E",
      "Women's Changing Trajectories, Men Baseline"          = "#0D0887",
      "Men's Changing Starting Points, Women Baseline"       = "#006633",
      "Men's Changing Trajectories, Women Baseline"          = "#330066"
    )
  }
  
  scenario_linetype <- c(
    "Observed"                                          = "solid",
    "Baseline"                                          = "solid",
    "No Change for Women"                                 = "dotdash", 
    "No Change for Men"                                 = "dotdash", 
    "Changing Starting Points"                          = "dashed", 
    "Changing Trajectories"                             = "dotted", 
    "Women's Changing Starting Points, Men Baseline"       = "dashed",
    "Women's Changing Trajectories, Men Baseline"          = "dotted",
    "Men's Changing Starting Points, Women Baseline"       = "dashed",
    "Men's Changing Trajectories, Women Baseline"          = "dotted"
  )
  
  scenario_shapes <- c(
    "Observed"                                          = 16,
    "Baseline"                                          = 16,
    "No Change for Men"                                 = 15, 
    "No Change for Women"                                 = 15, 
    "Changing Starting Points"                          = 18, 
    "Changing Trajectories"                             = 17, 
    "Women's Changing Starting Points, Men Baseline"       = 18,
    "Women's Changing Trajectories, Men Baseline"          = 17,
    "Men's Changing Starting Points, Women Baseline"       = 18,
    "Men's Changing Trajectories, Women Baseline"          = 17
  )
  
  # Process each group
  groups <- counterfactual_result$groups
  plot_list <- list()
  
  for (group in groups) {
    # Filter data for current group
    group_plot_data <- plot_data %>% 
      filter(group == !!group)
    
    p <- group_plot_data %>%
        ggplot(aes(
          x = YEAR, y = value, color = key, shape = key, linetype = key)) +
        geom_line() +
        geom_point() +
        create_common_theme(base_size, "none") +
        labs(title = "",
             subtitle = "",
          x = "",
          y = "") +
        scale_colour_manual(name = "Scenario",
                            values = scenario_colours,
                            breaks = scenario_names) +
        scale_shape_manual (name = "Scenario",
                            values = scenario_shapes,
                            breaks = scenario_names) +
        scale_linetype_manual(name = "Scenario",
                            values = scenario_linetype,
                            breaks = scenario_names) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        scale_x_continuous(breaks = round(seq(1970, 2020, by = 10)))
      
      # Add confidence intervals if bootstrap data is available
      if (with_bootstrap) {
        p <- p + 
          geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = key), 
                      alpha = 0.2, color = NA) +
          scale_fill_manual(name = "Scenario",
                              values = scenario_colours,
                              breaks = scenario_names, guide = "none") 
      }
      
      plot_list[[group]] <- p
      
  } 
  
  # Return appropriate layout
  if (length(groups) == 1) {
    return(
      plot_list[[1]] + labs(
        x = "Year",
        y = sprintf("Women's / Men's %s %s",
                    str_to_title(sumstat_label), outcome_label_axis),
        title = title,
        caption = caption) +
        theme(
          legend.position      = c(0.02, 0.7),  # Near top-left, but offset
          legend.justification = c("left", "bottom"),  # Anchor the top-left of the legend box to that position
          legend.direction     = "vertical",
          legend.background    = element_rect(fill = alpha("white", 0.8), colour = NA)
        ))
  } else {
    # Arrange plots in grid with legend at bottom
    return(grid.arrange(
      do.call(arrangeGrob, c(plot_list, ncol = 2)),
      left =  textGrob(
        sprintf("Women's/ Men's %s %s",
                str_to_title(sumstat_label), outcome_label_axis),
        rot = 90, 
        gp  = gpar(fontsize = base_size, fontface = "bold")),
      bottom =  textGrob("Year",
                         gp  = gpar(fontsize = base_size, fontface = "bold")),
      top = textGrob(title, 
                     gp  = gpar(fontsize = base_size * 1.5, fontface = "bold")),
      ncol = 1,
      heights = c(10, 1)
    ))
  }
}
