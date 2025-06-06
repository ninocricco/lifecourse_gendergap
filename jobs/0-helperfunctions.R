#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: HELPER FUNCTIONS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
source("jobs/0-libraries.r")

#------------------------------------------------------------------------------
# DATA PREP FUNCTIONS
#------------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

na_codes <- function(x, ...) {
  x[x %in% c(...)] <- NA
  x
}

numextract <- function(string){ 
  stringr::str_extract(string, "\\-*\\d+\\.*\\d*")
} 

#------------------------------------------------------------------------------
# VISUALIZATION THEME AND COLOR PALETTE FUNCTIONS
#------------------------------------------------------------------------------

viridis = magma(7, alpha = 1, begin = 0, end = .9, direction = 1)

# Function to get decades cohort colors
figs_cohort_colors <- function(use_grayscale = TRUE) {
  if(use_grayscale) {
    return(c(
    "1930s" = "grey80",
    "1940s" = "grey66",
    "1950s" = "grey52",
    "1960s" = "grey38",
    "1970s" = "grey24",
    "1980s" = "grey10",
    "1990-1998" = "grey1"
  ))
  } else {
    return(c(
      "1930s" = viridis[7],
      "1940s" = viridis[6],
      "1950s" = viridis[5],
      "1960s" = viridis[4],
      "1970s" = viridis[3],
      "1980s" = viridis[2],
      "1990-1998" = viridis[1]
    ))
  }
}


# Common theme function for all plots
create_common_theme <- function(base_size = 14, legend_position = "none") {
  theme_bw() +
    theme(
      # Text elements
      plot.title = element_text(
        hjust = 0.5, size = base_size * 1.2, face = "bold", 
        margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(
        hjust = 0.5, size = base_size * 0.9, margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(
        size = base_size * 0.7, face = "italic", margin = margin(10, 0, 0, 0)),
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
  caption <- sprintf(
    "Data from the Current Population Survey %s", data_source_label)
  if (!is.null(additional_text)) {
    caption <- paste(caption, additional_text, sep = ". ")
  }
  return(caption)
}

base_size = 14