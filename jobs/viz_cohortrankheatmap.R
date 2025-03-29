# Loading libraries
library(janitor)
library(tidyverse)
library(Hmisc)
library(ggsci)
library(gridExtra)
library(ggthemes)
library(grid)
library(apyramid)
library(gifski)
library(gganimate)

# Loading helper functions and CPS data
source("jobs/0-helperfunctions.R")
source("jobs/0-cps_dataloading.R")

cpi99 <- read_csv("raw_data/cpi99.csv")

analytic.sample <- data %>%
  mutate(AGE == AGE - 1) %>%
  filter(complete.cases(ASECWT)) %>% # Selecting only individuals w/ sample weights
  left_join(., cpi99, by = c("YEAR" = "year")) %>%
  # Creating variables: rough measure of birth cohort, log wage, categorical ed
  mutate(BIRTHYEAR = YEAR-AGE, 
         FEMALE = ifelse(SEX == 2, "Women", "Men"),
         MARST = na_codes(MARST, 9),
         FULLPART = case_when(FULLPART == 0 ~ "NotEmployed",
                              FULLPART == 1 ~ "FullTime",
                              FULLPART == 2 ~ "PartTime"),
         MARRIED = ifelse(MARST %in% c(1,2), 1, 0), 
         MARST = case_when(MARST %in% c(1,2) ~ "Married",
                           MARST == 6 ~ "Never_Married",
                           MARST %in% c(3,4,5,7) ~ "PrevMarried"),
         CHILD = ifelse(NCHILD > 0, 1, 0), 
         INCTOT = ifelse(INCTOT <= 0, NA, INCTOT),
         FTOTVAL = ifelse(FTOTVAL <= 0, NA, FTOTVAL),
         INCTOT = na_if(INCTOT, 999999999), 
         FTOTVAL = na_if(FTOTVAL, 999999999),
         INCSPOUSE = FTOTVAL-INCTOT,
         GROUP = interaction(CHILD, FULLPART, sep = "_"),
         GROUP = factor(GROUP, levels = c("1_FullTime", "1_NotEmployed", "1_PartTime",
                                          "0_FullTime", "0_NotEmployed", "0_PartTime")),
         EDUC = na_codes(EDUC, 999),
         EDUC = case_when(EDUC  <= 73 ~ "High School or Less", 
                          EDUC < 110 ~ "Some College", 
                          EDUC >= 110 ~ "BA+"))

sample_1 <- analytic.sample %>% 
  filter(complete.cases(INCTOT)) %>%
  filter(FEMALE == "Men") %>%
  filter(AGE >= 20, AGE <= 55) %>%
  group_by(YEAR) %>%
  mutate(across(c(INCTOT, FTOTVAL), 
                ~ rank(., ties = "first") / length(.) * 100, 
                .names = "{.col}_RANK")) %>%
  ungroup()

sample_2 <- analytic.sample %>% 
  filter(complete.cases(INCTOT)) %>%
  filter(FEMALE == "Men") %>%
  filter(AGE >= 20, AGE <= 55) %>%
  group_by(YEAR, AGE) %>%
  mutate(across(c(INCTOT, FTOTVAL), 
                ~ rank(., ties = "first") / length(.) * 100, 
                .names = "{.col}_RANK_C")) %>%
  ungroup()


test_data <- bind_cols(sample_1, 
                       sample_2 %>% select(ends_with("RANK_C")))

ageplot <- test_data %>%
  filter(BIRTHYEAR %in% c(1946, 1956, 1966, 1976)) %>%
  ggplot(aes(x = INCTOT_RANK_C, y = INCTOT_RANK, color = factor(BIRTHYEAR))) +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Cohort Rank", y = "Overall Rank", 
       title = "Age: {frame_time}") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  transition_time(AGE) +
  view_follow(fixed_x = TRUE)

# Render the animation
animate(ageplot, renderer = gifski_renderer(), duration = 20, fps = 1)

# Save the animation
anim_save("ageplot.gif")


heatmap_1950 <- test_data %>% filter(BIRTHYEAR == 1950) %>%
  mutate(INCTOT_RANK_C = cut(INCTOT_RANK_C, breaks = 100, labels = 1:100)) %>%
  group_by(AGE, INCTOT_RANK_C) %>%
  summarise(INCTOT_RANK = mean(INCTOT_RANK)) %>%
  ungroup() %>%
  select(AGE, INCTOT_RANK_C, INCTOT_RANK) %>%
  ggplot(aes(x = AGE, y = INCTOT_RANK_C, fill = INCTOT_RANK)) +
  geom_tile() +
  scale_fill_gradient(low="#FEE8C8", high="#E34A33",
                      breaks = 1:100) +
  theme_minimal() +
  labs(x = "", y = "", 
       fill = "Income Rank, Overall Distribution", 
       title = "Cohort: 1950") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank())

heatmap_1960 <- test_data %>% filter(BIRTHYEAR == 1960) %>%
  mutate(INCTOT_RANK_C = cut(INCTOT_RANK_C, breaks = 100, labels = 1:100)) %>%
  group_by(AGE, INCTOT_RANK_C) %>%
  summarise(INCTOT_RANK = mean(INCTOT_RANK)) %>%
  ungroup() %>%
  select(AGE, INCTOT_RANK_C, INCTOT_RANK) %>%
  ggplot(aes(x = AGE, y = INCTOT_RANK_C, fill = INCTOT_RANK)) +
  geom_tile() +
  scale_fill_gradient(low="#FEE8C8", high="#E34A33",
                      breaks = 1:100) +
  theme_minimal() +
  labs(x = "", y = "", 
       fill = "Income Rank, Overall Distribution", 
       title = "Cohort: 1960") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank())

heatmap_1970 <- test_data %>% filter(BIRTHYEAR == 1970) %>%
  bind_rows(data.frame(AGE = c(54, 55), INCTOT_RANK_C = c(0,0), INCTOT_RANK = c(0,0))) %>%
  mutate(INCTOT_RANK_C = cut(INCTOT_RANK_C, breaks = 100, labels = 1:100)) %>%
  group_by(AGE, INCTOT_RANK_C) %>%
  summarise(INCTOT_RANK = mean(INCTOT_RANK)) %>%
  ungroup() %>%
  select(AGE, INCTOT_RANK_C, INCTOT_RANK) %>%
  ggplot(aes(x = AGE, y = INCTOT_RANK_C, fill = INCTOT_RANK)) +
  geom_tile() +
  scale_fill_gradient(low="#FEE8C8", high="#E34A33",
                      breaks = 1:100) +
  theme_minimal() +
  labs(x = "", y = "", 
       fill = "Income Rank, Overall Distribution", 
       title = "Cohort: 1970") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank())

heatmap_1980 <- test_data %>% filter(BIRTHYEAR == 1980) %>%
  bind_rows(data.frame(AGE = c(43:55), INCTOT_RANK_C = c(rep(0, 13)), INCTOT_RANK = c(rep(0, 13)))) %>%
  mutate(INCTOT_RANK_C = cut(INCTOT_RANK_C, breaks = 100, labels = 1:100)) %>%
  group_by(AGE, INCTOT_RANK_C) %>%
  summarise(INCTOT_RANK = mean(INCTOT_RANK)) %>%
  ungroup() %>%
  select(AGE, INCTOT_RANK_C, INCTOT_RANK) %>%
  ggplot(aes(x = AGE, y = INCTOT_RANK_C, fill = INCTOT_RANK)) +
  geom_tile() +
  scale_fill_gradient(low="#FEE8C8", high="#E34A33",
                      breaks = 1:100) +
  theme_minimal() +
  labs(x = "", y = "", 
       fill = "Income Rank, Overall Distribution", 
       title = "Cohort: 1980") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank())

heatmap_1990 <- test_data %>% filter(BIRTHYEAR == 1990) %>%
  bind_rows(data.frame(AGE = c(33:55), INCTOT_RANK_C = c(rep(0, 23)), INCTOT_RANK = c(rep(0, 23)))) %>%
  mutate(INCTOT_RANK_C = cut(INCTOT_RANK_C, breaks = 100, labels = 1:100)) %>%
  group_by(AGE, INCTOT_RANK_C) %>%
  summarise(INCTOT_RANK = mean(INCTOT_RANK)) %>%
  ungroup() %>%
  select(AGE, INCTOT_RANK_C, INCTOT_RANK) %>%
  ggplot(aes(x = AGE, y = INCTOT_RANK_C, fill = INCTOT_RANK)) +
  geom_tile() +
  scale_fill_gradient(low="#FEE8C8", high="#E34A33",
                      breaks = 1:100) +
  theme_minimal() +
  labs(x = "", y = "", 
       fill = "Income Rank, Overall Distribution", 
       title = "Cohort: 1990") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank())

grid.arrange(heatmap_1950, heatmap_1960, heatmap_1970, 
             heatmap_1980, nrow = 2, left = "Income Rank, Age-Specific Distribution",
             bottom = "Age")


heat <- test_data %>% 
  group_by(AGE, BIRTHYEAR) %>%
  filter(BIRTHYEAR %in% c(1946:1975)) %>%
  mutate(FTOTVAL_RANK_C = cut(FTOTVAL_RANK_C, breaks = 100, labels = 1:100)) %>%
  group_by(AGE, BIRTHYEAR, FTOTVAL_RANK_C) %>%
  summarise(FTOTVAL_RANK = mean(FTOTVAL_RANK)) %>%
  mutate(FTOTVAL_RANK = as.numeric(cut(FTOTVAL_RANK, breaks = 100, labels = 1:100))) %>% 
  ungroup() %>% 
  ggplot(aes(x = AGE, y = FTOTVAL_RANK_C, fill = FTOTVAL_RANK)) +
  geom_tile() +
  scale_fill_gradient(low="#FEE8C8", high="#E34A33",
                      breaks = 1:100) +
  theme_minimal() +
  labs(x = "", y = "", 
       fill = "Income Rank, Overall Distribution", 
       title = "Cohort: {frame_time}") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        axis.text.y = element_blank()) +
  transition_time(BIRTHYEAR) +
  view_follow(fixed_x = TRUE)

# Render the animation
animate(heat, renderer = gifski_renderer(), duration = 20, fps = 1)

# Save the animation
anim_save("heatmanp_faminc.gif")

fill_colors <- c("1_NotEmployed" = "skyblue", "1_PartTime" = "royalblue2","1_FullTime" = "midnightblue",
                 "0_FullTime" = "darkorange4", "0_NotEmployed" ="goldenrod2" , "0_PartTime" = "chocolate2")

year = 1976

data_filtered <- sample %>%
  select(YEAR, AGE, FEMALE, ASECWT, GROUP)

# Calculate proportions for each age by gender
data_summary <- data_filtered %>%
  group_by(AGE, FEMALE, GROUP, YEAR) %>%
  summarise(n_cat = sum(ASECWT)) %>%
  filter(AGE >= 18) %>%
  left_join(., data_filtered %>%
              group_by(AGE, FEMALE, YEAR) %>%
              summarise(n_group = sum(ASECWT)) %>%
              filter(AGE >= 18), by = c("AGE", "FEMALE")) %>%
  mutate(prop_cat = n_cat/n_group,
         AGE = as.factor(AGE))

data_filtered_inc <- sample %>%
  select(YEAR, AGE, FEMALE, ASECWT, INCTOT, INCSPOUSE) %>%
  gather(GROUP, VALUE, -c(YEAR, AGE, FEMALE, ASECWT)) %>%
  mutate(VALUE = VALUE*ASECWT)

# Calculate proportions for each age by gender
data_summary_inc <- data_filtered_inc %>%
  group_by(AGE, FEMALE, GROUP, YEAR) %>%
  summarise(n_cat = sum(VALUE, na.rm = T)) %>%
  filter(AGE >= 18) %>%
  mutate(AGE = as.factor(AGE))

fill_colors <- c("royalblue2", "goldenrod2")

p_income <- data_summary_inc %>%
  ggplot(aes(x = AGE, y = n_cat/10000000, fill = GROUP)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = fill_colors) +
  facet_wrap(~FEMALE) +
  theme(legend.position = "bottom") + 
  labs(y = "Dollars (Million)", x = "Age", title = 'Year: {frame_time}') +
  transition_time(YEAR) +
  view_follow(fixed_x = TRUE)

animate(p_income, renderer = gifski_renderer(), duration = 6, fps = 1)
anim_save("p_income.gif")

# Generate animated plot
p <- age_pyramid(data_for_animation, age_group = AGE, split_by = FEMALE, stack_by = GROUP, count = n_cat, show_midpoint = FALSE) +
  scale_fill_manual(values = fill_colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = 'Year: {frame_time}', y = 'Population Count', x = 'Age Group') +
  transition_time(YEAR) +
  view_follow(fixed_x = TRUE)

# Render the animation
animate(p, renderer = gifski_renderer(), duration = 6, fps = 1)

# Save the animation
anim_save("age_pyramid_animation.gif")


p_income <- age_pyramid(data_summary_inc, 
                        age_group = AGE, 
                        split_by = FEMALE, 
                        stack_by = GROUP, 
                        count = n_cat, show_midpoint = F) +
  theme_minimal() +
  scale_fill_manual(values = fill_colors) +
  theme(legend.position = "bottom")

p_gen <- age_pyramid(data_summary, age_group = AGE, split_by = FEMALE, stack_by = GROUP, count = n_cat, show_midpoint = F)

p_gen + theme_minimal() +
  scale_fill_manual(values = fill_colors) +
  theme(legend.position = "bottom")


# Prepare data for animation
data_for_animation <- sample %>%
  #filter(YEAR %in% c(1970, 1980, 1990, 2000, 2010, 2000)) %>%
  select(YEAR, AGE, FEMALE, ASECWT, GROUP) %>%
  group_by(YEAR, AGE, FEMALE, GROUP) %>%
  summarise(n_cat = sum(ASECWT), .groups = 'drop') %>%
  filter(AGE >= 18, AGE <= 70, n_cat > 0) %>%
  left_join(., sample %>%
              filter(YEAR >= start_year & YEAR <= end_year) %>%
              group_by(YEAR, AGE, FEMALE) %>%
              summarise(n_group = sum(ASECWT), .groups = 'drop') %>%
              filter(AGE >= 18), by = c("YEAR", "AGE", "FEMALE")) %>%
  mutate(prop_cat = n_cat / n_group,
         AGE = as.factor(AGE))

# Generate animated plot
p <- age_pyramid(data_for_animation, age_group = AGE, split_by = FEMALE, stack_by = GROUP, count = n_cat, show_midpoint = FALSE) +
  scale_fill_manual(values = fill_colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = 'Year: {frame_time}', y = 'Population Count', x = 'Age Group') +
  transition_time(YEAR) +
  view_follow(fixed_x = TRUE)

# Render the animation
animate(p, renderer = gifski_renderer(), duration = 6, fps = 1)

# Save the animation
anim_save("age_pyramid_animation.gif")


# Prepare data for animation
data_for_animation <- sample %>%
  #filter(YEAR %in% c(1970, 1980, 1990, 2000, 2010, 2000)) %>%
  select(YEAR, AGE, FEMALE, ASECWT, INCTOT, INCSPOUSE) %>%
  gather(GROUP, VALUE, -c(YEAR, AGE, FEMALE, ASECWT)) %>%
  mutate(VALUE = VALUE*ASECWT) %>%
  group_by(YEAR, AGE, FEMALE, GROUP) %>%
  summarise(n_cat = sum(VALUE, na.rm = T)) %>%
  filter(AGE >= 18, AGE <= 70) %>%
  mutate(AGE = as.factor(AGE),
         GROUP = ifelse(GROUP == "INCTOT", "PERSONAL", "FAMILY-PERSONAL"),
         SOURCE = factor(GROUP, levels = c("PERSONAL", "FAMILY-PERSONAL")))

# Generate animated plot
p <- age_pyramid(data_for_animation, age_group = AGE, split_by = FEMALE, stack_by = SOURCE, count = n_cat, show_midpoint = FALSE) +
  scale_fill_manual(values = fill_colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = 'Year: {frame_time}', y = 'Income', x = 'Age Group') +
  transition_time(YEAR) +
  view_follow(fixed_x = TRUE)

# Render the animation
animate(p, renderer = gifski_renderer(), duration = 6, fps = 1)

# Save the animation
anim_save("age_pyramid_income.gif")

