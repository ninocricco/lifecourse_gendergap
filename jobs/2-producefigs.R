#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: ANALYSIS FILE
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

source("jobs/0-helperfunctions.R")

analytic.sample <- read_rds("clean_data/analytic_sample_org_elig.rds") %>%
  filter(UHRSWORK1_PRED > 0) %>% 
  mutate(WEIGHT = EARNWT,
         OUTCOME = EARNWEEK_ADJ/UHRSWORK1_PRED)

#For re-running with ASEC data
#analytic.sample <- read_rds("clean_data/analytic_sample_asec.rds") %>%
#  mutate(WEIGHT = ASECWT,
#         OUTCOME = INCWAGE.TOPCODE) %>%
#  filter(YEAR >= 1982)

#------------------------------------------------------------------------------
# DESCRIPTIVE FIGURES BY TEN YEAR COHORT WINDOW
#------------------------------------------------------------------------------
# First, creating descriptive figures by birth cohort decade
means.age.bc_dec <- analytic.sample %>%
  # Restrict to individuals born on or before 1995
  filter(BIRTHYEAR <= 1995) %>%
  # Creates categorical birth cohort decade variable
  mutate(BIRTHCOHORT = case_when(BIRTHYEAR %in% c(1913:1919) ~ "1913-1919",
                                 BIRTHYEAR %in% c(1920:1929) ~ "1920s",
                                 BIRTHYEAR %in% c(1930:1939) ~ "1930s",
                                 BIRTHYEAR %in% c(1940:1949) ~ "1940s",
                                 BIRTHYEAR %in% c(1950:1959) ~ "1950s",
                                 BIRTHYEAR %in% c(1960:1969) ~ "1960s",
                                 BIRTHYEAR %in% c(1970:1979) ~ "1970s",
                                 BIRTHYEAR %in% c(1980:1995) ~ "1980-1995")) %>%
  # Finds mean (top-coded) wage by age, gender, and birth cohort
  group_by(BIRTHCOHORT, AGE, FEMALE) %>%
  summarise(MEAN.OUTCOME = wtd.mean(OUTCOME, weight = WEIGHT)) %>% 
  # Turns to wide format and creates measure of the ratio of men/women's mean inc
  pivot_wider(names_from = FEMALE, values_from = c(MEAN.OUTCOME)) %>%
  mutate(ratio = Women/Men)


color_palette <- c(ggthemes::colorblind_pal()(8)[-1])

cohort_colors <- c("1930s" = color_palette[7],
                   "1940s" = color_palette[6],
                   "1950s" = color_palette[2],
                   "1960s" = color_palette[3],
                   "1970s" = color_palette[4],
                   "1980-1995" = color_palette[5])

cohort_colors <- c(
  "1930s" = "grey80",
  "1940s" = "grey66",
  "1950s" = "grey52",
  "1960s" = "grey38",
  "1970s" = "grey24",
  "1980-1995" = "grey10"
)

# Creates first descriptive plot showing ratio of women's to men's mean
# income by age and birth cohort
plot0 <- means.age.bc_dec %>%
  mutate(drop = case_when(BIRTHCOHORT == "1970s" & AGE > 45 ~ 1,
                          BIRTHCOHORT == "1980-1995" & AGE > 35 ~ 1,
                          TRUE ~ 0)) %>%
  filter(drop == 0, AGE <= 55) %>%
  mutate(label = case_when(AGE == 31 & BIRTHCOHORT == "1980-1995" ~ BIRTHCOHORT,
                           AGE == 35 & BIRTHCOHORT == "1970s" ~ BIRTHCOHORT,
                           AGE == 33 & BIRTHCOHORT == "1960s" ~ BIRTHCOHORT,
                           AGE == 25 & BIRTHCOHORT == "1950s" ~ BIRTHCOHORT,
                           AGE == 35 & BIRTHCOHORT == "1940s" ~ BIRTHCOHORT,
                           AGE == 52 & BIRTHCOHORT == "1930s" ~ BIRTHCOHORT)) %>%
  filter(BIRTHCOHORT %!in% c("1920s", "1913-1919")) %>%
  ggplot(aes(x = AGE, y = ratio, color = BIRTHCOHORT, label = label,
             linetype = "solid")) +
  geom_label_repel(box.padding   = 1.5, 
                   point.padding = 0.3,
                   show.legend = FALSE, 
                   size = 5) +
  geom_line(size = 1) +
  theme_bw() +
  labs(x = "Age", 
       y= " Women's Average Hourly Wage / Men's Average Hourly Wage", 
       title = "Figure 1: Gender Wage Gap by Age and Cohort, 1982-2023",
       caption = "Note: Data from the Current Population Survey Outgoing Rotation Group."
  ) +
  scale_color_manual(values = cohort_colors) +
  #scale_colour_grey() +
  guides(color = guide_legend(title="Birth Cohort"), 
         linetype = "none") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title = element_text(size = 14), 
        axis.text.y = element_text(size =  12),
        axis.text.x = element_text(size =  12),
        legend.text = element_text(size = 18)) +
  ylim(.5, 1) +
  xlim(25, 55) +
  geom_hline(yintercept = 1, linetype = "dashed")

ggsave("figures/draft_paper/f1.jpeg", plot0, width = 8.5, height = 8, units = "in")

cohort_colors <- c(
  "1940s" = "grey80",
  "1950s" = "grey65",
  "1960s" = "grey40",
  "1970s" = "grey25",
  "1980-1995" = "grey10"
)

# Creates second descriptive plot showing how women to men's cohort-specific
# mean wage ratios change as they age relative to cohort-specific ratios at 25 
plot1 <-  means.age.bc_dec %>%
  mutate(drop = case_when(BIRTHCOHORT == "1970s" & AGE > 45 ~ 1,
                          BIRTHCOHORT == "1980-1995" & AGE > 35 ~ 1,
                          TRUE ~ 0)) %>%
  filter(drop == 0, AGE <= 55) %>%
  mutate(label = case_when(AGE == 30 & BIRTHCOHORT == "1980-1995" ~ BIRTHCOHORT,
                           AGE == 45 & BIRTHCOHORT == "1970s" ~ BIRTHCOHORT,
                           AGE == 51 & BIRTHCOHORT == "1960s" ~ BIRTHCOHORT,
                           AGE == 52 & BIRTHCOHORT == "1950s" ~ BIRTHCOHORT,
                           AGE == 35 & BIRTHCOHORT == "1940s" ~ BIRTHCOHORT,
                           AGE == 50 & BIRTHCOHORT == "1930s" ~ BIRTHCOHORT)) %>%
  filter(BIRTHCOHORT %in% c("1940s", "1950s", "1960s", "1970s", "1980-1995"
  )) %>%
  group_by(BIRTHCOHORT) %>%
  mutate(ratio.25 = ifelse(AGE == 25, ratio, NA)) %>%
  fill(ratio.25, .direction = "down") %>%
  mutate(ratio.change = (ratio-ratio.25)*100) %>%
  group_by(BIRTHCOHORT) %>%
  ggplot(aes(x = AGE, y = ratio.change, color = BIRTHCOHORT, label = label,
             linetype = "solid")) +
  geom_line(size = 1.1) +
  theme_bw() +
  labs(x = "Age", #y= "",
       y = "Percentage Change from Age 25",
        title = "Figure 2: Age-Graded Change in the Gender Wage Gap by Cohort, 1982-2023",
       caption = "Data from the Current Population Survey Outgoing Rotation Group"
  ) +
  scale_color_manual(values = cohort_colors) +
  geom_label_repel(box.padding   = 4, 
                   point.padding = 0.1,
                   show.legend = FALSE,
                   size = 5) +
  guides(color = guide_legend(title="Birth Cohort"), 
         linetype = "none") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title = element_text(size = 14), 
        axis.text.y = element_text(size =  12),
        axis.text.x = element_text(size =  12),
        legend.text = element_text(size = 18)) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("figures/draft_paper/f2.jpeg", plot1, width = 8.5, height = 8, units = "in")

#------------------------------------------------------------------------------
# ANALYSIS FIGURES 
#------------------------------------------------------------------------------
# Setting different reference cohort based on whether we're using ASEC or ORG
refcohort = 1957

# For results that don't use rolling averages, restrict
# age range to ages 25-60
analytic.sample <- analytic.sample %>%
  filter(AGE >= 25, AGE <= 55)

#------------------------------------------------------------------------------
# BY GENDER DESCRIPTIVE FIGURE
#------------------------------------------------------------------------------
means.age.bc_yr <- analytic.sample %>%
  # Create age groups that align with birth cohorts
  mutate(
    AGE_GROUP = case_when(
      between(AGE, 25, 27) ~ 26,
      between(AGE, 35, 37) ~ 36,
      between(AGE, 45, 47) ~ 46,
      between(AGE, 53, 55) ~ 54
    ),
    BIRTHYEAR_GROUP = YEAR - AGE_GROUP
  ) %>%
  filter(!is.na(AGE_GROUP)) %>%
  group_by(BIRTHYEAR_GROUP, YEAR, AGE_GROUP, FEMALE) %>%
  summarise(MEAN.OUTCOME = wtd.mean(OUTCOME, weight = WEIGHT),
            n = n())

plot4 <- means.age.bc_yr %>%
  left_join(means.age.bc_yr %>% 
              filter(BIRTHYEAR_GROUP == 1957) %>%
              ungroup() %>%  
              transmute(FEMALE, AGE_GROUP, MEAN.OUTCOME_25_1957 = MEAN.OUTCOME), 
            by = c("FEMALE", "AGE_GROUP")) %>%
  filter(BIRTHYEAR_GROUP >= 1957) %>%
  mutate(WAGE_REL_REF = MEAN.OUTCOME/MEAN.OUTCOME_25_1957) %>%
  ggplot(aes(x = BIRTHYEAR_GROUP, y = WAGE_REL_REF, color = factor(AGE_GROUP))) +
  geom_line(size = 1) +
  facet_wrap(~FEMALE) +
  theme_bw() +
  labs(x = "Birth Cohort", #y= "",
       y = "% Age-Specific Wages Relative to 1957 Cohort",
       title = "Figure 4: Women and Men's Mean Wages by Age Relative to Reference Cohort",
       caption = "Data from the Current Population Survey Outgoing Rotation Group"
  ) +
  scale_colour_grey() +
  guides(color = guide_legend(title = "Age"), 
         linetype = "none") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title = element_text(size = 14), 
        axis.text.y = element_text(size =  12),
        legend.text = element_text(size = 10), 
        axis.text.x = element_text(size =  12),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))

ggsave("figures/draft_paper/f4_org_mean.jpeg", plot4, width = 9, height = 8, units = "in")

means.age.bc_yr <- analytic.sample %>%
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
  summarise(MEAN.OUTCOME = wtd.mean(OUTCOME,
                                        weight = WEIGHT),
            n = n())

plot5 <- means.age.bc_yr %>%
  left_join(means.age.bc_yr %>% filter(AGE == 25) %>%
              ungroup() %>% transmute(FEMALE, BIRTHYEAR, MEAN.OUTCOME_25 = MEAN.OUTCOME), 
            by = c("FEMALE", "BIRTHYEAR")) %>%
  mutate(WAGE.GROWTH = MEAN.OUTCOME/MEAN.OUTCOME_25) %>%
  filter(BIRTHYEAR > 1957) %>% 
  mutate(BIRTHYEAR_GROUP = case_when(BIRTHYEAR %in% c(1958:1960) ~ "1958-1960", 
                                     BIRTHYEAR %in% c(1968:1970) ~ "1968-1970",
                                     BIRTHYEAR %in% c(1978:1980) ~ "1978-1980", 
                                     BIRTHYEAR %in% c(1988:1990) ~ "1988-1990")) %>%
  group_by(AGE, BIRTHYEAR_GROUP, FEMALE) %>%
  filter(complete.cases(BIRTHYEAR_GROUP)) %>%
  summarise(WAGE.GROWTH = mean(WAGE.GROWTH)) %>%
  ggplot(aes(x = AGE, y = WAGE.GROWTH, color = BIRTHYEAR_GROUP)) +
  geom_line(size = 1) +
  facet_wrap(~FEMALE) +
  theme_bw() +
  labs(x = "Age", #y= "",
       y = "Age-Specific Wages Relative to Age 25",
       title = "Figure 5: Women and Men's Mean Pay Trajectories by Cohort",
       caption = "Data from the Current Population Survey Outgoing Rotation Group"
  ) +
  scale_colour_grey() +
  guides(color = guide_legend(title = "Birth Cohort"), 
         linetype = "none") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title = element_text(size = 14), 
        axis.text.y = element_text(size =  12),
        legend.text = element_text(size = 10), 
        axis.text.x = element_text(size =  12),
        legend.position = c(0.80, 0.30),
        legend.justification = c(0, 1))
 
ggsave("figures/draft_paper/f5_org_mean.jpeg", plot5, width = 9, height = 8, units = "in")

#------------------------------------------------------------------------------
# COUNTERFACTUAL FIGURES
#------------------------------------------------------------------------------
data_counterfactual <- analytic.sample %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
  summarise(MEAN.OUTCOME = wtd.quantile(OUTCOME, probs = .5,
                                        weight = WEIGHT),
            n = n())

data_counterfactual <- data_counterfactual %>%
  # Computes and merges column calculating total # of women in sample by year
  left_join(., data_counterfactual %>%
              group_by(YEAR, FEMALE) %>%
              summarise(
                n_year = sum(n)), by = c("YEAR", "FEMALE")) %>%
  # Creates measure of the proportion of women in each year belonging to each year bc
  mutate(lf_cohortprop = n/n_year)

# Creating the counterfactual data
data_counterfactual <- data_counterfactual %>%
  mutate(mean_c_25 = ifelse(AGE == 30, MEAN.OUTCOME, NA),
         mean_men_25 = ifelse(AGE == 30 & FEMALE == "Men", MEAN.OUTCOME, NA),
         mean_refcohort_25 = ifelse(AGE == 30 & BIRTHYEAR == refcohort, MEAN.OUTCOME, NA), 
         mean_1965_25 = ifelse(AGE == 30 & BIRTHYEAR == 1965, MEAN.OUTCOME, NA),
         mean_refcohort_age = ifelse(BIRTHYEAR == refcohort, MEAN.OUTCOME, NA), 
         mean_1965_age = ifelse(BIRTHYEAR == 1965, MEAN.OUTCOME, NA)) %>%
  # For each birth cohort, fill in var with its wage ratio at age 25
  ungroup() %>%
  fill(mean_men_25, .direction = "down") %>% 
  ungroup() %>%
  arrange(FEMALE) %>%
  fill(mean_c_25, .direction = "down") %>% 
  ungroup() %>%
  # For all cohorts (older than refcohort), fill in var with the refcohort cohort's age 25 wage ratio
  fill(mean_refcohort_25, .direction = "down") %>%
  arrange(AGE) %>%
  group_by(AGE) %>%
  fill(mean_refcohort_age, .direction = "down") %>%
  ungroup()  %>%
  mutate(pct.change_refcohort_25 = mean_refcohort_age/mean_refcohort_25,
         pct.change_c_25 = MEAN.OUTCOME/mean_c_25)

data_counterfactual_fig <- data_counterfactual %>%
  # Compute the counterfactual 
  # First multiply means by their cohort labor force proportions
  mutate(mean_c1_age = ifelse(BIRTHYEAR <= refcohort, MEAN.OUTCOME,
                              mean_c_25 * pct.change_refcohort_25), 
         mean_c2_age = ifelse(BIRTHYEAR <= refcohort, MEAN.OUTCOME, 
                              mean_refcohort_25 * pct.change_c_25), 
         mean_c3_refcohort = ifelse(BIRTHYEAR <= refcohort, MEAN.OUTCOME,
                                    mean_refcohort_25 * pct.change_refcohort_25),
         mean_age_adj = MEAN.OUTCOME * lf_cohortprop,
         mean_c1_age_adj = mean_c1_age * lf_cohortprop, 
         mean_c2_age_adj = mean_c2_age * lf_cohortprop,
         mean_c3_age_adj = mean_c3_refcohort * lf_cohortprop) %>%
  group_by(YEAR, FEMALE) %>%
  summarise(c1 = sum(mean_c1_age_adj), 
            c2 = sum(mean_c2_age_adj), 
            c3 = sum(mean_c3_age_adj),
            agg = sum(mean_age_adj)) %>%
  gather(counterfactual, value, -c(YEAR, FEMALE)) %>%
  pivot_wider(names_from = c(FEMALE, counterfactual), values_from = value) %>%
  mutate("Cohort Starting Wage, 1957 Trajectory" = Women_c1/Men_c1, 
         "1957 Starting Wage, Cohort Trajectory" = Women_c2/Men_c2, 
         "Cohort Starting Wage, 1957 Trajectory, Men 1957" = Women_c1/Men_c3, 
         "1957 Starting Wage, Cohort Trajectory, Men 1957" = Women_c2/Men_c3, 
         "Baseline" = Women_c3/Men_c3,
         "Observed" = Women_agg/Men_agg, 
         "Men 1957, Women Observed" = Women_agg/Men_c3) %>%
  ungroup() %>%
  dplyr::select(YEAR, "Cohort Starting Wage, 1957 Trajectory",
                "1957 Starting Wage, Cohort Trajectory", 
                "Cohort Starting Wage, 1957 Trajectory, Men 1957",
                "1957 Starting Wage, Cohort Trajectory, Men 1957",
                "Men 1957, Women Observed",
                Observed, Baseline) %>%
  gather(key, value, -YEAR)

fig3 <- data_counterfactual_fig %>%
  mutate(key = factor(key, levels = c("Observed", 
                                     "Baseline",
                               "Cohort Starting Wage, 1957 Trajectory",
                               "1957 Starting Wage, Cohort Trajectory",
                               "Cohort Starting Wage, 1957 Trajectory, Men 1957",
                               "1957 Starting Wage, Cohort Trajectory, Men 1957",
                               "Men 1957, Women Observed"
                               ))) %>%
  group_by(key) %>%
  filter(key %in%  c("Observed",
                     "Baseline",
                    "Cohort Starting Wage, 1957 Trajectory",
                     "1957 Starting Wage, Cohort Trajectory",
                     "Cohort Starting Wage, 1957 Trajectory, Men 1957",
                     "1957 Starting Wage, Cohort Trajectory, Men 1957",
                     "Men 1957, Women Observed"
  )) %>%
  mutate(label = case_when(YEAR == 1982 & key == "Observed" ~ key,
                           YEAR == 1985 & key == "Baseline" ~ key,
                           YEAR == 1990 & key == "Men 1957, Women Observed" ~ key,
                           YEAR == 1995 & key == "Cohort Starting Wage, 1957 Trajectory" ~ key,
                           YEAR == 2000 & key == "1957 Starting Wage, Cohort Trajectory" ~ key,
                           YEAR == 1985 & key == "Same Starting Wage, 1965 Cohort Trajectory" ~ key,
                         YEAR == 2010 & key == "Cohort Starting Wage, 1957 Trajectory, Men 1957" ~ key,
                           YEAR == 2015 & key == "1957 Starting Wage, Cohort Trajectory, Men 1957" ~ key
                          )) %>%
  ggplot(aes(x = YEAR, y = value, color = key, linetype = key, label = label)) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(x = "", y = "Women's Average Hourly Wage / Men's Average Hourly Wage", 
       title = "Figure 6: Counterfactual Trends in the Gender Wage Gap, 1982-2023",
       #title = "Main Specification",
    #   title = "Age Range 30-55, Reference Cohort 1957", 
      caption = "Data from the Current Population Survey Outgoing Rotation Group. \
      Sample members aged 25-55, excluding self-employed, military, agricultural, and unpaid family workers. \
      Outcome: Hourly Wage. \
    Reference Cohort: 1957.",
       color = NULL) +
#  labs(x = "", y = "Women's Average Hourly Wage / Men's Average Hourly Wage", 
 #      title = "Observed and Counterfactual Trends in the Gender Wage Gap, 1982-2020",
  #     caption = "ASEC, Sample members aged 25-55. \
   #    Excludes self-employed, military, agricultural workers. \ 
    #   Outcome: Wage and Salary Income. \
     #  Reference Cohort: 1957.",
      # color = NULL) +
  scale_colour_manual(values = c(
    "Observed" = "gray69",
    "Baseline" = "gray37",
    "Men 1957, Women Observed" = "tomato",
    "Cohort Starting Wage, 1957 Trajectory" = "#9FDA3AFF",
    "1957 Starting Wage, Cohort Trajectory" = "darkorchid1",
    "Cohort Starting Wage, 1957 Trajectory, Men 1957" = "#277F8EFF",
    "1957 Starting Wage, Cohort Trajectory, Men 1957" = "#440154FF"
  ))+
  scale_linetype_manual(
    guide = "none", values = c(
      "Observed" ="solid",
      "Baseline" = "solid",
      "Men 1957, Women Observed" = "dashed",
      "Cohort Starting Wage, 1957 Trajectory" = "dashed",
      "1957 Starting Wage, Cohort Trajectory" = "dotted",
      "Cohort Starting Wage, 1957 Trajectory, Men 1957" = "dashed",
      "1957 Starting Wage, Cohort Trajectory, Men 1957" = "dotted"
      ))+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title = element_text(size = 14), 
  axis.text.y = element_text(size =  12),
  legend.text = element_text(size = 10), 
  axis.text.x = element_text(size =  12),
  legend.position = c(0.05, 0.95),
  legend.justification = c(0, 1)) +
  geom_hline(yintercept = 1, linetype = "dashed")+ 
  scale_x_continuous(breaks = round(seq(1970, 2020, by = 10))) +
  guides(color = guide_legend(title = "", ncol = 1, order = 1))

fig3


ggsave("figures/draft_paper/f3_age30.jpeg", fig3, units = "in", width = 9, height = 8)

#------------------------------------------------------------------------------
# By Education Subgroup Figures
#-------------------------------------------------------------------------------
# Define the main function
grouped_analysis <- function(data, group_var, refcohort = 1958) {
  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }
  
  groups <- levels(data[[group_var]])
  plot_list <- list()
  
  for (group in groups) {
    group_data <- data %>% filter(!!sym(group_var) == group) %>%
      filter(AGE >= 25, AGE <= 55) %>%
      mutate(YEAR = YEAR-1)
    
    means_age_bc_yr <- group_data %>%
      group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
      summarise(MEAN_OUTCOME = weighted.mean(OUTCOME, WEIGHT, na.rm = T),
                n = n())
    
    means_age_bc_yr <- means_age_bc_yr %>%
      # Computes and merges column calculating total # of women in sample by year
      left_join(., means_age_bc_yr %>%
                  group_by(YEAR, FEMALE) %>%
                  summarise(
                    n_year = sum(n)), by = c("YEAR", "FEMALE")) %>%
      # Creates measure of the proportion of women in each year belonging to each year bc
      mutate(lf_cohortprop = n/n_year)
    
    data_counterfactual <- means_age_bc_yr %>%
      mutate(mean_c_25 = ifelse(AGE == 25, MEAN_OUTCOME, NA),
             mean_men_25 = ifelse(AGE == 25 & FEMALE == "Men", MEAN_OUTCOME, NA),
             mean_refcohort_25 = ifelse(AGE == 25 & BIRTHYEAR == refcohort, MEAN_OUTCOME, NA), 
             mean_1965_25 = ifelse(AGE == 25 & BIRTHYEAR == 1965, MEAN_OUTCOME, NA),
             mean_refcohort_age = ifelse(BIRTHYEAR == refcohort, MEAN_OUTCOME, NA), 
             mean_1965_age = ifelse(BIRTHYEAR == 1965, MEAN_OUTCOME, NA)) %>%
      # For each birth cohort, fill in var with its wage ratio at age 25
      ungroup() %>%
      fill(mean_men_25, .direction = "down") %>% 
      ungroup() %>%
      arrange(FEMALE) %>%
      fill(mean_c_25, .direction = "down") %>% 
      ungroup() %>%
      # For all cohorts (older than refcohort), fill in var with the refcohort cohort's age 25 wage ratio
      fill(mean_refcohort_25, .direction = "down") %>%
      arrange(AGE) %>%
      group_by(AGE) %>%
      fill(mean_refcohort_age, .direction = "down") %>%
      ungroup()  %>%
      mutate(pct.change_refcohort_25 = mean_refcohort_age/mean_refcohort_25,
             pct.change_c_25 = MEAN_OUTCOME/mean_c_25)
    
    data_counterfactual_fig <- data_counterfactual %>%
      # Compute the counterfactual 
      # First multiply means by their cohort labor force proportions
      mutate(mean_c1_age = ifelse(BIRTHYEAR <= refcohort, MEAN_OUTCOME,
                                  mean_c_25 * pct.change_refcohort_25), 
             mean_c2_age = ifelse(BIRTHYEAR <= refcohort, MEAN_OUTCOME, 
                                  mean_refcohort_25 * pct.change_c_25), 
             mean_c3_refcohort = ifelse(BIRTHYEAR <= refcohort, MEAN_OUTCOME,
                                        mean_refcohort_25 * pct.change_refcohort_25),,
             mean_age_adj = MEAN_OUTCOME * lf_cohortprop,
             mean_c1_age_adj = mean_c1_age * lf_cohortprop, 
             mean_c2_age_adj = mean_c2_age * lf_cohortprop,
             mean_c3_age_adj = mean_c3_refcohort * lf_cohortprop) %>%
      group_by(YEAR, FEMALE) %>%
      summarise(c1 = sum(mean_c1_age_adj), 
                c2 = sum(mean_c2_age_adj), 
                c3 = sum(mean_c3_age_adj),
                agg = sum(mean_age_adj)) %>%
      gather(counterfactual, value, -c(YEAR, FEMALE)) %>%
      pivot_wider(names_from = c(FEMALE, counterfactual), values_from = value) %>%
      mutate("Cohort Starting Wage, 1958 Trajectory" = Women_c1/Men_c1, 
             "1958 Starting Wage, Cohort Trajectory" = Women_c2/Men_c2, 
             "Cohort Starting Wage, 1958 Trajectory, Men 1958" = Women_c1/Men_c3, 
             "1958 Starting Wage, Cohort Trajectory, Men 1958" = Women_c2/Men_c3, 
             "Baseline" = Women_c3/Men_c3,
             "Observed" = Women_agg/Men_agg) %>%
      ungroup() %>%
      dplyr::select(YEAR, "Cohort Starting Wage, 1958 Trajectory",
                    "1958 Starting Wage, Cohort Trajectory", 
                    "Cohort Starting Wage, 1958 Trajectory, Men 1958",
                    "1958 Starting Wage, Cohort Trajectory, Men 1958",
                    Observed, Baseline) %>%
      gather(key, value, -YEAR)
    
    fig <- data_counterfactual_fig %>%
      group_by(key) %>%
      filter(key %in%  c("Observed",
                         "Baseline",
                         "Cohort Starting Wage, 1958 Trajectory",
                         "1958 Starting Wage, Cohort Trajectory",
                         "Cohort Starting Wage, 1958 Trajectory, Men 1958",
                         "1958 Starting Wage, Cohort Trajectory, Men 1958"
      )) %>%
      ggplot(aes(x = YEAR, y = value, color = key, linetype = key)) +
      geom_line(size = 1.5) +
      theme_bw() +
      labs(x = "",
           y = "",
           title = group,
           color=NULL) +
      scale_colour_manual(values = c(
        "Observed" = "gray69",
        "Baseline" = "gray37",
        "Cohort Starting Wage, 1958 Trajectory" = "#9FDA3AFF",
        "1958 Starting Wage, Cohort Trajectory" = "darkorchid1",
        "Cohort Starting Wage, 1958 Trajectory, Men 1958" = "#277F8EFF",
        "1958 Starting Wage, Cohort Trajectory, Men 1958" = "#440154FF"
      ))+
      scale_linetype_manual(
        guide = "none", values = c(
          "Observed" ="solid",
          "Baseline" = "solid",
          "Cohort Starting Wage, 1958 Trajectory" = "dashed",
          "1958 Starting Wage, Cohort Trajectory" = "dotted",
          "Cohort Starting Wage, 1958 Trajectory, Men 1958" = "dashed",
          "1958 Starting Wage, Cohort Trajectory, Men 1958" = "dotted"
        ))+
      guides(color=guide_legend(nrow=2,byrow=TRUE, title = "")) +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 20#, face ="bold"
            ),
            axis.title=element_text(size = 18), 
            axis.text.y = element_text(size =  13),
            legend.text = element_text(size = 15), 
            axis.text.x = element_text(size =  15)) +
      geom_hline(yintercept = 1, linetype = "dashed")+ 
      scale_x_continuous(breaks = round(seq(1970, 2020, by = 10))) +
      guides(color =guide_legend(order = 1, nrow = 3, title = ""))
    
    
    legend <- g_legend(fig)
    plot_list[[group]] <- fig + theme(legend.position = "none")
    
  }
  
  grid.arrange(
    do.call(arrangeGrob, c(plot_list, ncol = 2)),
    legend,
    ncol = 1,
    heights = c(10, 1)
  )
}

analytic.sample <- analytic.sample %>%
  filter(AGE >= 25, AGE <= 55) %>%
  mutate(YEAR = YEAR-1) %>% 
  mutate(EDUC_2 = ifelse(EDUC != "ba.plus", "< BA", "BA+"),
         RACEETH = str_to_title(RACEETH))
  

# Example usage
fig3_educ <- grouped_analysis(analytic.sample, "EDUC_2", refcohort = 1958)
ggsave("figures/draft_paper/fig6_educ.jpeg", fig3_educ, width = 12, height = 9, units = "in")


fig3_race <- grouped_analysis(analytic.sample, "RACEETH", refcohort = 1958)

ggsave("figures/draft_paper/fig6_race.jpeg", fig3_race, width = 14, height = 12, units = "in")