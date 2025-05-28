#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: VISUALIZATION FUNCTIONS
# AUTHOR: NINO CRICCO (REORGANIZED)
#------------------------------------------------------------------------------
# Loading libraries
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(grid)
library(viridis)

# Source data calculation functions
source("jobs/2-data-prep-functions.R")

#------------------------------------------------------------------------------
# COMMON THEME AND COLOR PALETTE FUNCTIONS
#------------------------------------------------------------------------------

# Color palettes
color_palettes <- list(
  # Color palette for figures
  viridis = magma(7, alpha = 1, begin = 0, end = .9, direction = 1),
  
  # Grayscale palettes
  decades_gray = c(
    "1930s" = "grey80",
    "1940s" = "grey66",
    "1950s" = "grey52",
    "1960s" = "grey38",
    "1970s" = "grey24",
    "1980s" = "grey10",
    "1990-1998" = "grey1"
  ),
  
  age_gray = c(
    "25" = "grey80",
    "34" = "grey52",
    "43" = "grey24",
    "55" = "grey10"
  ),
  
  cohort_gray = c(
    "1988-1990" = "grey80",
    "1978-1980" = "grey52",
    "1968-1970" = "grey38",
    "1958-1960" = "grey10"
  )
)

# Function to get decades cohort colors
figs_cohort_colors <- function(use_grayscale = TRUE) {
  if(use_grayscale) {
    return(color_palettes$decades_gray)
  } else {
    return(c(
      "1930s" = color_palettes$viridis[7],
      "1940s" = color_palettes$viridis[6],
      "1950s" = color_palettes$viridis[5],
      "1960s" = color_palettes$viridis[4],
      "1970s" = color_palettes$viridis[3],
      "1980s" = color_palettes$viridis[2],
      "1990-1998" = color_palettes$viridis[1]
    ))
  }
}

# Function to get age colors
fig_age_colors <- function(use_grayscale = TRUE) {
  if(use_grayscale) {
    return(color_palettes$age_gray)
  } else {
    return(c(
      "25" = color_palettes$viridis[7],
      "34" = color_palettes$viridis[5],
      "43" = color_palettes$viridis[3],
      "55" = color_palettes$viridis[1]
    ))
  }
}

# Function to get cohort colors
fig_cohort_colors <- function(use_grayscale = TRUE) {
  if(use_grayscale) {
    return(color_palettes$cohort_gray)
  } else {
    return(c(
      "1988-1990" = color_palettes$viridis[1],
      "1978-1980" = color_palettes$viridis[3],
      "1968-1970" = color_palettes$viridis[5],
      "1958-1960" = color_palettes$viridis[7]
    ))
  }
}

# Common theme function for all plots
create_common_theme <- function(base_size = 14, legend_position = "none") {
  theme_bw() +
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
    legend.position = legend_position,
    
    # Overall aesthetics
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )
}

# Helper function to extract legend from a ggplot
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Helper function to create standard caption
create_caption <- function(data_source_label, additional_text = NULL) {
  caption <- sprintf("Data from the Current Population Survey %s", data_source_label)
  if (!is.null(additional_text)) {
    caption <- paste(caption, additional_text, sep = ". ")
  }
  return(caption)
}

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
                           "1930s", "1940s", "1950s", "1960s", 
                           "1970s", "1980s", "1990-1998"),
                         with_bootstrap = FALSE,
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- "Gender Wage Gap by Age and Cohort, 1982-2023"
  }
  
  if(is.null(caption)) {
    caption <- create_caption(data_source_label, "Note")
  }
  
  # Prepare data for plotting
  plot_data <- prepare_plot1_data(data, decades_to_include)
  
  p_gender <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, Men, Women) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES)) %>%
    mutate(key = factor(key, levels = c("Men", "Women"))) %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, "right") +
    theme(
      legend.position      = c(0.47, 0.8), 
      legend.justification = c("right", "top"),
      legend.direction     = "vertical",
      legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
    ) +
    labs(x = "", y = "") +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort")) + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    facet_grid(cols = vars(key), scales = "free_y")
  
  p_ratio <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, Ratio = ratio) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES)) %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, legend.position) +
    labs(x = "", y = "") +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort"), 
           linetype = "none") + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    facet_grid(cols = vars(key), scales = "free_y")
  
  p <- grid.arrange(p_gender, p_ratio, 
                    left  = textGrob(
                      sprintf("%s %s",sumstat_label, outcome_label),
                      rot = 90, 
                      gp  = gpar(fontsize = base_size, fontface = "bold")
                    ),
                    bottom  = textGrob(
                      "Age",    
                      gp  = gpar(fontsize = base_size, fontface = "bold")
                    ))

  # Add confidence intervals if bootstrap is requested
  if (with_bootstrap) {
    
    bootstrap_data <- read_rds("clean_data/plot1_data.rds") %>%
      group_by(AGE, BIRTHYEAR_DECADES) %>% 
      summarise(ci_lower = quantile(ratio, probs =  1-conf_level),
                ci_upper = quantile(ratio, probs = conf_level))
    
    # Add confidence intervals as ribbons
    p <- p + 
      geom_ribbon(
        data = bootstrap_data,
        aes(x = AGE, ymin = ci_lower, ymax = ci_upper, fill = BIRTHYEAR_DECADES),
        alpha = 0.2,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values = figs_cohort_colors(use_grayscale), guide = "none")
  }
  
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
                         data_source_label = "Outgoing Rotation Group", 
                         decades_to_include = c("1950s", "1960s", 
                           "1970s", "1980s"),
                         start_age = 25,
                         with_bootstrap = FALSE,
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- "Age-Graded Change in the Gender Wage Gap by Cohort, 1982-2023"
  }
  
  if(is.null(caption)) {
    caption <- create_caption(data_source_label)
  }
  
  # Prepare data for plotting
  plot_data <- prepare_plot2_data(data, decades_to_include = decades_to_include,
                                  start_age = start_age)
  
  # Create base plot
  p <- plot_data %>%
    ggplot(aes(x = AGE, y = ratio.change, color = BIRTHYEAR_DECADES)) +
    geom_point() +
    geom_line() +
    create_common_theme(base_size, "right") +
    theme(
      legend.position      = c(0.9, 0.3), 
      legend.justification = c("right", "top"),
      legend.direction     = "vertical",
      legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
    ) +
    labs(x = "", y = "") +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort")) + 
    labs(
      x = "Age",
      y = sprintf(" %% Change from Age %d, %s %s",   
                 start_age, sumstat_label, outcome_label), 
      title = title,
      caption = caption) +
    ylim(-15, 2) +
    xlim(start_age, 55) + 
    geom_hline(yintercept = 0, linetype = "dashed")
  
  # Add confidence intervals if bootstrap is requested
  if (with_bootstrap) {
    # Generate bootstrapped data
    bootstrap_data <- read_rds("clean_data/plot2_data.rds") %>%
      filter(BIRTHYEAR_DECADES != "1940s") %>%
      group_by(AGE, BIRTHYEAR_DECADES) %>% 
      summarise(ci_lower = quantile(ratio.change, probs =  1-conf_level),
                ci_upper = quantile(ratio.change, probs = conf_level))
    
    # Add confidence intervals as ribbons
    p <- p + 
      geom_ribbon(
        data = bootstrap_data,
        aes(x = AGE, ymin = ci_lower, ymax = ci_upper, fill = BIRTHYEAR_DECADES),
        alpha = 0.2,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values = figs_cohort_colors(use_grayscale), guide = "none")
  }
  
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
                         data_source_label = "Outgoing Rotation Group",
                         refcohort = 1957,
                         start_age = 25,
                         scenarios_to_include = "main",
                         with_bootstrap = FALSE,
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- sprintf("Counterfactual Trends in the Gender Wage Gap, 1982-2023")
  }
  
  if(is.null(caption)) {
    additional_text <- sprintf("Sample members aged %d-55, excluding self-employed, military, agricultural, and unpaid family workers. Outcome: %s. Reference Cohort: %d",
                              start_age, outcome_label_caption, refcohort)
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
  
  plot_data <- counterfactual_result$data
  
  # Get bootstrapped data if requested
  if (with_bootstrap) {
     bootstrap_data <- read_rds("clean_data/plotcount_data.rds") %>%
      group_by(YEAR, key, group) %>% 
      summarise(ci_lower = quantile(value, probs =  1-conf_level),
                ci_upper = quantile(value, probs = conf_level))
     
     plot_data <- left_join(plot_data, 
                            bootstrap_data %>%
                              filter(key %in% unique(counterfactual_result$data$key)), 
                            by = c("YEAR", "key", "group"))
       
  }
  
  # Define colors and line types
  scenario_names <- unique(plot_data$key)
  
  scenario_colours <- if (use_grayscale) {
    c(
      "Observed"                                          = "gray10",
      "Baseline"                                          = "gray10",
      "No Change for Men"                                 = "gray70",
      "Changing Starting Points"                          = "gray55",
      "Changing Trajectories"                             = "gray30",
      "Changing Starting Points, No Change for Men"       = "gray55",
      "Changing Trajectories, No Change for Men"          = "gray30"
    )
  } else {
    c(
      "Observed"                                          = "#000000",
      "Baseline"                                          = "#7F7F7F",
      "No Change for Men"                                 = "#ED7953",
      "Changing Starting Points"                          = "#9FDA3A",
      "Changing Trajectories"                             = "#9C179E",
      "Changing Starting Points, No Change for Men"       = "#277F8E",
      "Changing Trajectories, No Change for Men"          = "#0D0887"
    )
  }
  
  scenario_linetype <- c(
    "Observed"                                          = "solid",
    "Baseline"                                          = "solid",
    "No Change for Men"                                 = "dotdash", 
    "Changing Starting Points"                          = "dashed", 
    "Changing Trajectories"                             = "dotted", 
    "Changing Starting Points, No Change for Men"       = "dashed",
    "Changing Trajectories, No Change for Men"          = "dotted"
  )
  
  scenario_shapes <- c(
    "Observed"                                          = 16,
    "Baseline"                                          = 16,
    "No Change for Men"                                 = 15, 
    "Changing Starting Points"                          = 18, 
    "Changing Trajectories"                             = 17, 
    "Changing Starting Points, No Change for Men"       = 18,
    "Changing Trajectories, No Change for Men"          = 17
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
        labs(title = group,
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
          scale_fill_manual(values = colors, guide = "none")
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
        caption = caption) + theme(legend.justification = c(0, 1)) + 
        theme(
          legend.position      = c(0.4, 0.9), 
          legend.justification = c("right", "top"),
          legend.direction     = "vertical",
          legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
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
