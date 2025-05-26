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

# Source data calculation functions
source("jobs/2-data-prep-functions.R")

#------------------------------------------------------------------------------
# COMMON THEME AND COLOR PALETTE FUNCTIONS
#------------------------------------------------------------------------------

# Color palettes
color_palettes <- list(
  # Color palette for figures
  viridis = c("#0d0887", "#46039f", "#9c179e", "#bd3786", "#d8576b", "#ed7953", "#fdb42f"),
  
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
                         decades_to_include = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990-1998"),
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
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES, 
               linetype = "solid")) +
    geom_line(size = 1) +
    create_common_theme(base_size, legend.position) +
    labs(x = "Age", y = "") +
    scale_color_manual(values = figs_cohort_colors(use_grayscale)) +
    guides(color = guide_legend(title="Birth Cohort"), 
           linetype = "none") + 
    scale_x_continuous(
      breaks  = c(seq(25, 55, 5))) +
    facet_grid(cols = vars(key), scales = "free_y")
  
  p_ratio <- plot_data %>%
    select(AGE, BIRTHYEAR_DECADES, Ratio = ratio) %>%
    gather(key, value, -c(AGE, BIRTHYEAR_DECADES)) %>%
    ggplot(aes(x = AGE, y = value, color = BIRTHYEAR_DECADES, 
               linetype = "solid")) +
    geom_line(size = 1) +
    create_common_theme(base_size, "bottom") +
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
                      rot = 90,                             # vertical text
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
                         decades_to_include = c("1940s", "1950s", "1960s", "1970s", "1980-1995"),
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
  plot_data <- prepare_plot2_data(data, decades_to_include, start_age)
  
  # Create base plot
  p <- plot_data %>%
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
    create_common_theme(base_size, "none") +
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
                         scenario_type = "baseline",
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
    scenario_type = scenario_type
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
  
  # Define scenario colors
  scenario_colors <- list(
    grayscale = c("gray69", "gray37", "gray45", "gray25", "gray15", "gray55", "gray75"),
    color = c("gray69", "gray37", "#ed7953", "#9FDA3AFF", "#9c179e", "#277F8EFF", "#0d0887")
  )
  
  colors <- if(use_grayscale) {
    setNames(scenario_colors$grayscale[1:length(scenario_names)], scenario_names)
  } else {
    setNames(scenario_colors$color[1:length(scenario_names)], scenario_names)
  }
  
  line_types <- setNames(
    c("solid", "solid", "dashed", "dashed", "dotted", "dashed", "dotted")[1:length(scenario_names)],
    scenario_names
  )
  
  # Process each group
  groups <- counterfactual_result$groups
  plot_list <- list()
  
  for (group in groups) {
    # Filter data for current group
    group_plot_data <- plot_data %>% 
      filter(group == !!group)
    
    # Create plot with correct styling based on group count
    if (length(groups) == 1 && group == "All Data") {
      # Single plot (original style)
      p <- group_plot_data %>%
        mutate(
          label = case_when(
            YEAR == 1982 & key == "Observed" ~ key,
            YEAR == 1985 & key == "Baseline" ~ key,
            YEAR == 1995 & key == sprintf("Cohort Starting Wage, %d Trajectory", refcohort) ~ key,
            YEAR == 2000 & key == sprintf("%d Starting Wage, Cohort Trajectory", refcohort) ~ key,
            TRUE ~ NA_character_
          )
        ) %>%
        ggplot(aes(x = YEAR, y = value, color = key, linetype = key, label = label))
      
      p <- p +
        geom_line(size = 1, alpha = .5) +
        geom_point() +
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
      
      # Add confidence intervals if bootstrap data is available
      if (with_bootstrap) {
        p <- p + 
          geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = key), 
                      alpha = 0.2, color = NA) +
          scale_fill_manual(values = colors, guide = "none")
      }
      
      plot_list[[group]] <- p
      
    } else {
      # Grouped plot style
      p <- group_plot_data %>%
        ggplot(aes(x = YEAR, y = value, color = key, linetype = key))
      p <- p +
       geom_line(size = 1, alpha = .5) +
        geom_point() +
        create_common_theme(base_size, "none") +
        labs(
          x = "Year",
          y = "",
          title = group
        ) +
        scale_colour_manual(values = colors) +
        scale_linetype_manual(guide = "none", values = line_types) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        scale_x_continuous(breaks = round(seq(1970, 2020, by = 10)))
      
      # Add confidence intervals if bootstrap data is available
      if (with_bootstrap) {
        p <- p + 
          geom_ribbon(data = bootstrap_data,
                        aes(ymin = ci_lower, ymax = ci_upper, fill = key), 
                      alpha = 0.2, color = NA) +
          scale_fill_manual(values = colors, guide = "none")
      }
      
      plot_list[[group]] <- p
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
    return(grid.arrange(
      do.call(arrangeGrob, c(plot_list, ncol = 2)),
      legend,
      left = sprintf("Women's/ Men's %s %s",
                     str_to_title(sumstat_label), outcome_label_axis),
      ncol = 1,
      heights = c(10, 1)
    ))
  }
}

#------------------------------------------------------------------------------
# PLOT 4: WOMEN'S AND MEN'S AGE-SPECIFIC WAGES RELATIVE TO REFERENCE COHORT
#------------------------------------------------------------------------------

create_plot4 <- function(data, 
                         use_grayscale = FALSE,
                         save_plot = FALSE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage",
                         data_source_label = "Outgoing Rotation Group",
                         title = NULL, 
                         caption = NULL,
                         refcohort = 1957,
                         start_age = 25,
                         base_size = 14,
                         ages_selection = c(25, 34, 43, 55),
                         with_bootstrap = FALSE,
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- sprintf("Women's and Men's Age-Specific %s %s\nRelative to %d Birth Cohort",
                     str_to_title(sumstat_label), outcome_label, refcohort)
  }
  
  if(is.null(caption)) {
    caption <- create_caption(data_source_label)
  }
  
  # Prepare data for plotting
  plot_data <- prepare_plot4_data(data, refcohort, ages_selection)
  
  # Create base plot
  p <- plot_data %>%
    ggplot(aes(x = BIRTHYEAR_GROUP, y = WAGE_REL_REF, color = AGE_GROUP)) +
    geom_line(size = 1) +
    facet_wrap(~FEMALE) +
    create_common_theme(base_size, c(0.05, 0.95)) +
    theme(legend.justification = c(0, 1)) +
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
    geom_hline(yintercept = 1, linetype = "dashed", color = "red")
  
  # Add confidence intervals if bootstrap is requested
  if (with_bootstrap) {

    bootstrap_data <- read_rds("clean_data/plot4_data.rds") %>%
      group_by(AGE_GROUP, BIRTHYEAR_GROUP, YEAR, FEMALE) %>% 
      summarise(ci_lower = quantile(WAGE_REL_REF, probs =  1-conf_level),
                ci_upper = quantile(WAGE_REL_REF, probs = conf_level))
    
    # Add confidence intervals as ribbons
    p <- p + 
      geom_ribbon(
        data = bootstrap_data,
        aes(x = BIRTHYEAR_GROUP, ymin = ci_lower, ymax = ci_upper, fill = AGE_GROUP),
        alpha = 0.2,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values = fig_age_colors(use_grayscale), guide = "none")
  }
  
  return(p)
}

#------------------------------------------------------------------------------
# PLOT 5: WOMEN'S AND MEN'S PAY TRAJECTORIES BY COHORT
#------------------------------------------------------------------------------

create_plot5 <- function(data, 
                         use_grayscale = TRUE,
                         sumstat_label = "Average",
                         outcome_label = "Hourly Wage",
                         data_source_label = "Outgoing Rotation Group",
                         refcohort = 1957,
                         start_age = 25,
                         base_size = 14,
                         title = NULL, 
                         caption = NULL,
                         cohort_groups = list(
                           c(1958:1960, "1958-1960"),
                           c(1968:1970, "1968-1970"),
                           c(1978:1980, "1978-1980"),
                           c(1988:1990, "1988-1990")
                         ),
                         with_bootstrap = FALSE,
                         conf_level = 0.95) {
  
  if(is.null(title)) {
    title <- sprintf("Women's and Men's %s Pay Trajectories by Cohort",
                     str_to_title(sumstat_label))
  }
  
  if(is.null(caption)) {
    caption <- create_caption(data_source_label)
  }
  
  # Prepare data for plotting
  plot_data <- prepare_plot5_data(data, refcohort, start_age, cohort_groups)
  
  # Create base plot
  p <- plot_data %>%
    ggplot(aes(x = AGE, y = WAGE.GROWTH, color = BIRTHYEAR_GROUP)) +
    geom_line(size = 1) +
    facet_wrap(~FEMALE) +
    create_common_theme(base_size, c(0.35, .45)) +
    theme(legend.justification = c(0, 1)) +
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
    geom_hline(yintercept = 1, linetype = "dashed", color = "red")
  
  # Add confidence intervals if bootstrap is requested
  if (with_bootstrap) {
    
    bootstrap_data <- read_rds("clean_data/plot5_data.rds") %>%
      group_by(AGE, BIRTHYEAR_GROUP, FEMALE) %>% 
      summarise(ci_lower = quantile(WAGE.GROWTH, probs =  1-conf_level),
                ci_upper = quantile(WAGE.GROWTH, probs = conf_level))
    
    # Add confidence intervals as ribbons
    p <- p + 
      geom_ribbon(
        data = bootstrap_data,
        aes(x = AGE, ymin = ci_lower, ymax = ci_upper, fill = BIRTHYEAR_GROUP),
        alpha = 0.2,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values = fig_cohort_colors(use_grayscale), guide = "none")
  }
  
  return(p)
}

