#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: HELPER FUNCTIONS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
source("jobs/0-libraries.r")

#------------------------------------------------------------------------------
# DATA PREP FUNCTIONS
#------------------------------------------------------------------------------

# Custom inverse of the built in %in% operator, check if element is NOT in a set
'%!in%' <- function(x,y)!('%in%'(x,y))

# Assigns elements in that are %in% a set of NA codes to missing
na_codes <- function(x, ...) {
  x[x %in% c(...)] <- NA
  x
}

# Given a string, extracts first number in the string
numextract <- function(string){ 
  stringr::str_extract(string, "\\-*\\d+(\\.\\d+)?*")
} 

#------------------------------------------------------------------------------
# VISUALIZATION THEME AND COLOR PALETTE FUNCTIONS
#------------------------------------------------------------------------------

# Selects colors from a palette
viridis = magma(7, alpha = 1, begin = 0, end = .9, direction = 1)

# Function to assign cohort decades to color schemes, using colors or grayscale
figs_cohort_colors <- function(use_grayscale = TRUE) {
  if(use_grayscale) {
    return(c(
      "1927-1936" = "grey80",
      "1937-1946"= "grey66",
      "1947-1956" = "grey52",
      "1957-1966" = "grey38",
      "1967-1976" = "grey24",
      "1977-1986" = "grey10",
      "1987-1996" = "grey1"
  ))
  } else {
    return(c(
      "1927-1936" = viridis[7],
      "1937-1946"= viridis[6],
      "1947-1956" = viridis[5],
      "1957-1966" = viridis[4],
      "1967-1976" = viridis[3],
      "1977-1986" = viridis[2],
      "1987-1996" = viridis[1]
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

# Extracts legend from a ggplot
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Creates a caption that varies by data source
create_caption <- function(data_source_label, additional_text = NULL) {
  caption <- sprintf(
    "Data from the Current Population Survey %s", data_source_label)
  if (!is.null(additional_text)) {
    caption <- paste(caption, additional_text, sep = ". ")
  }
  return(caption)
}

# Setting the base font size for all plots
base_size = 14