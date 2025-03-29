library(tidyverse)
library(gganimate)
library(scales)

means <- org %>%
  group_by(YEAR, BIRTHYEAR, FEMALE) %>%
  summarise(wage = weighted.mean(EARNHRLY_MAIN, weight = EARNWT, na.rm = T)) %>%
  pivot_wider(names_from = FEMALE, values_from = wage) %>%
  mutate(ratio = Women / Men)

cohort_props <- org %>%
  group_by(YEAR, BIRTHYEAR) %>%
  summarise(weight = sum(EARNWT),
            .groups = 'drop')

# Combine means and cohort_props data
prepare_wage_data <- function(means_df, props_df) {
  combined_data <- means_df %>%
    left_join(props_df, by = c("YEAR", "BIRTHYEAR")) %>%
    # Calculate relative weights for each cohort within year
    group_by(YEAR) %>%
    mutate(
      cohort_prop = weight / sum(weight),
      # Convert ratio to gap percentage (1 - ratio) * 100
      gender_gap = (1 - ratio) * 100
    ) %>%
    ungroup()
  
  return(combined_data)
}

# Create points data for animation
create_cohort_points <- function(combined_data, n_points = 2000) {
  points_data <- combined_data %>%
    group_by(YEAR, BIRTHYEAR) %>%
    do({
      n <- round(.$cohort_prop * n_points)
      tibble(
        point_id = 1:n,
        gap_jitter = rnorm(n, mean = .$gender_gap, sd = 1),
        wage_women = .$Women[1],
        wage_men = .$Men[1]
      )
    }) %>%
    ungroup()
  
  return(points_data)
}

# First prepare all the data to get consistent scales and aggregate trends
prepare_plot_data <- function(means, cohort_props) {
  # Get full data for consistent scales and aggregate trend
  full_data <- create_cohort_points(prepare_wage_data(means, cohort_props))
  
  # Calculate aggregate trend once
  agg_trend <- full_data %>%
    group_by(YEAR) %>%
    summarise(avg_gap = mean(gap_jitter), .groups = 'drop')
  
  # Calculate cohort-specific trends
  cohort_trends <- full_data %>%
    group_by(YEAR, BIRTHYEAR) %>%
    summarise(cohort_gap = mean(gap_jitter), .groups = 'drop')
  
  # Get ranges for consistent scales
  y_range <- range(full_data$gap_jitter)
  year_range <- range(full_data$YEAR)
  cohort_range <- range(full_data$BIRTHYEAR)
  
  return(list(
    full_data = full_data,
    agg_trend = agg_trend,
    cohort_trends = cohort_trends,
    y_range = y_range,
    year_range = year_range,
    cohort_range = cohort_range
  ))
}

# Function to create plot with selected cohorts
plot_selected_cohorts <- function(plot_data, selected_cohorts = NULL) {
  # Filter points if cohorts are selected
  points_data <- plot_data$full_data
  if (!is.null(selected_cohorts)) {
    points_data <- points_data %>%
      filter(BIRTHYEAR %in% selected_cohorts)
  }
  
  # Base plot
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "black" 
    )
  
  # Add either aggregate trend or cohort-specific lines
  if (is.null(selected_cohorts)) {
    # Full dataset: show aggregate trend
    p <- p + 
      geom_point(
        data = points_data,
        aes(x = YEAR, y = gap_jitter, color = BIRTHYEAR),
        size = 1,
        alpha = 0.6
      ) +
      geom_line(
        data = plot_data$agg_trend,
        aes(x = YEAR, y = avg_gap),
        color = "black"
      )
  } else {
    # Selected cohorts: show cohort-specific trajectories
    p <- p +
      geom_point(
      data = points_data,
        aes(x = YEAR, y = gap_jitter, color = BIRTHYEAR),
        size = 1,
        alpha = 0.6
      ) #+
   #   geom_line(
    #    data = plot_data$cohort_trends %>% 
    #      filter(BIRTHYEAR %in% selected_cohorts),
    #    aes(x = YEAR, y = cohort_gap, color = BIRTHYEAR),
    #  )
  }
  
  # Add scales and theme
  p + scale_color_viridis_c(
    "Birth Year",
    option = "plasma",
    limits = plot_data$cohort_range,
    breaks = seq(plot_data$cohort_range[1], plot_data$cohort_range[2], by = 5),
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(20, "lines"),
      barwidth = unit(1, "lines"),
      ticks.linewidth = 1,
      frame.colour = "black",
      frame.linewidth = 0.5
    )
  ) +
    scale_x_continuous(
      "Year",
      limits = plot_data$year_range,
      breaks = seq(plot_data$year_range[1], plot_data$year_range[2], by = 2)
    ) +
    scale_y_continuous(
      "Gender Wage Gap (%)",
      limits = plot_data$y_range,
      labels = function(x) paste0(round(x, 1), "%")
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      text = element_text(color = "gray20"),
      axis.text = element_text(color = "gray20"),
      legend.background = element_rect(fill = "white"),
      legend.text = element_text(color = "gray20"),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(face = "italic"),
      legend.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = "Trends in the Gender Pay Gap",
      subtitle = "Narrowing Starting Points and Persistent Life Course Divergence",
      caption = "Data from the Current Population Survey Outgoing Rotation Groups, non self-employed workers aged 25-55. \nSolid lines indicate cohort-specific trends in gender wage gap."
    )
}

# Usage:
# First prepare the data (do this once)
plot_data <- prepare_plot_data(means, cohort_props)

# Show all cohorts with aggregate trend:
plot_selected_cohorts(plot_data)

# Show specific cohorts with their trajectories:
plot_selected_cohorts(plot_data, selected_cohorts = c(1957, 1970, 1985))
