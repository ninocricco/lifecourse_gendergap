library(tidyverse)
library(haven)
library(janitor)
library(broom)
library(Hmisc)
library(weights)

# Reading in full ORG data and creating year groups for pooled estimation
sample_all <- readRDS("clean_data/analytic_sample_org_all.rds") %>%
  mutate(MARRIED = ifelse(MARRIED == "married", 1, 0),
         SEX = haven::as_factor(SEX),
         year_group = ((BIRTHYEAR - 1955) %/% 3) + 1)

# Estimating ORG eligibility by year group, sex, and age
selection_models <- sample_all %>%
  group_by(year_group, SEX, AGE) %>%
  # Nest the data for each group
  nest() %>%
  # Fit logistic regression for each group
  mutate(model = map(data, ~glm(
    ELIGORG ~ EDUC + MARRIED + RACEETH + YNGCH.REC + NCHILD,
    family = binomial(link = "logit"), data = .x)),
    # Extract model coefficients and statistics
    model_info = map(model, tidy)) %>%
  # Unnest the model information
  unnest(model_info)

# Generating Inverse Pr ORG Selection Weights on ORG eligible subset
org_w_selection <- read_rds("clean_data/analytic_sample_org_elig.rds") %>%
  mutate(WEIGHT = EARNWT, OUTCOME = EARNHRLY, 
         MARRIED = ifelse(MARRIED == "married", 1, 0),
         SEX = haven::as_factor(SEX),
         year_group = ((BIRTHYEAR - 1955) %/% 3) + 1) %>%
  filter(AGE >= 25) %>%
  group_by(year_group, AGE, SEX) %>%
  nest() %>%
  # Join with original models
  left_join(selection_models %>%
              select(year_group, AGE, SEX, model) %>%
              distinct(), by = c("year_group", "AGE", "SEX")) %>%
  # Join with models for reference cohort
  left_join(selection_models %>%
              ungroup() %>%
              filter(year_group == 1) %>% 
              select(AGE, SEX, model) %>%
              rename(ref_model = model) %>%
              distinct(), by = c("AGE", "SEX")) %>%
  # Generate predictions
  mutate(ELIGORG_PRED = map2(data, model, ~predict(.y, newdata = .x, type = "response")),
         ELIGORG_PRED_REFCOHORT = map2(data, ref_model, ~predict(.y, newdata = .x, type = "response"))) %>%
  select(-model, -ref_model) %>%
  unnest(c(data, ELIGORG_PRED, ELIGORG_PRED_REFCOHORT)) %>%
  mutate(ELIGORG_PRED_WEIGHT = WEIGHT/ELIGORG_PRED, 
         ELIGORG_PRED_REFCOHORT_WEIGHT = WEIGHT/ELIGORG_PRED_REFCOHORT)

write_rds(org_w_selection, "clean_data/analytic_sample_org_elig_weights.rds")

org_w_selection <- read_rds("clean_data/analytic_sample_org_elig_weights.rds")

# Creating means with regular and different sample weights
means.age.bc_yr_sel_ed <- org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
  summarise(EDUC_PCT = wpct(EDUC, weight = WEIGHT), 
            EDUC_PCT_RW = wpct(EDUC, weight = ELIGORG_PRED_WEIGHT), 
            EDUC_PCT_RW_REF = wpct(EDUC, weight = ELIGORG_PRED_REFCOHORT_WEIGHT), 
            EDUC = factor(c("ba.plus", "hs.or.less", "some.college")))

means.age.bc_yr_sel_ed  %>% filter(AGE == 25) %>% ungroup() %>% 
  select(YEAR, FEMALE, starts_with("EDUC")) %>% 
  gather(key, value, -c(YEAR, FEMALE, EDUC)) %>% 
  mutate(EDUC = factor(EDUC, levels = c("ba.plus", "some.college", "hs.or.less"))) %>%
  ggplot(aes(x = YEAR, y = value, color = EDUC, linetype = key)) +
  geom_line() + theme_bw() + facet_grid(rows = vars(FEMALE), cols = vars(EDUC)) +
  theme(legend.position = "bottom")

# Creating means with regular and different sample weights
means.age.bc_yr_sel_race <- org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(YEAR, FEMALE) %>%
  summarise(RACEETH_PCT = wpct(RACEETH, weight = WEIGHT), 
            RACEETH_PCT_RW = wpct(RACEETH, weight = ELIGORG_PRED_WEIGHT), 
            RACEETH_PCT_RW_REF = wpct(RACEETH, weight = ELIGORG_PRED_REFCOHORT_WEIGHT), 
            RACEETH = factor(c("black", "latino", "other", "white")))

means.age.bc_yr_sel_race  %>% filter(RACEETH == "white") %>% ungroup() %>% 
  select(YEAR, FEMALE, starts_with("RACEETH")) %>% 
  gather(key, value, -c(YEAR, FEMALE, RACEETH)) %>% 
  #mutate(RACEETH = factor(RACEETH, levels = c("ba.plus", "some.college", "hs.or.less"))) %>%
  ggplot(aes(x = YEAR, y = value, color = RACEETH, linetype = key)) +
  geom_line() + theme_bw() + facet_grid(rows = vars(FEMALE), cols = vars(RACEETH)) +
  theme(legend.position = "bottom")

# Creating means with regular and different sample weights
means.age.bc_yr_sel <- org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
  summarise(MEAN.OUTCOME_SEL = wtd.mean(OUTCOME, 
                                        weight = ELIGORG_PRED_WEIGHT),
            MEAN.OUTCOME_REF = wtd.mean(OUTCOME, 
                                        weight = ELIGORG_PRED_REFCOHORT_WEIGHT),
            MEAN.OUTCOME = wtd.mean(OUTCOME, 
                                    weight = WEIGHT),
            n = n())

means.age.bc_yr_sel %>% filter(AGE %in% c(25, 35, 45)) %>%
  ungroup() %>% select(YEAR, AGE, FEMALE, starts_with("MEAN")) %>%
  gather(key, value, -c(YEAR, AGE, FEMALE)) %>%
  ggplot(aes(x = YEAR, y = value, linetype = key)) +
  geom_line() +
  theme_bw() +
  facet_grid(cols = vars(FEMALE), rows = vars(AGE), scales = "free")

means.age.bc_yr_sel <- means.age.bc_yr_sel %>%
  # Computes and merges column calculating total # of women in sample by year
  left_join(., means.age.bc_yr_sel %>%
              group_by(YEAR, FEMALE) %>%
              summarise(
                n_year = sum(n)), by = c("YEAR", "FEMALE")) %>%
  # Creates measure of the proportion of women in each year belonging to each year bc
  mutate(lf_cohortprop = n/n_year)

# Diagnostic plots: Weighted mean wages by age for a selection of cohorts
means.age.bc_yr_sel %>% 
  select(BIRTHYEAR, YEAR, AGE, FEMALE, starts_with("MEAN.OUTCOME")) %>%
  mutate(RATIO_ALLCOHORTS = MEAN.OUTCOME_SEL/MEAN.OUTCOME, 
         RATIO_REFCOHORT = MEAN.OUTCOME_REF/MEAN.OUTCOME) %>%
  select(BIRTHYEAR, YEAR, AGE, FEMALE, starts_with("RATIO")) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key)) +
  geom_line() +
  theme_bw() +
  labs(x = "Age", y = "Ratio") +
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
    legend.position = "none",
    legend.justification = c(0, 1),
    
    # Overall aesthetics
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  facet_grid(FEMALE ~ BIRTHYEAR)


# Diagnostic plots: Weighted mean wages by age for a selection of cohorts
means.age.bc_yr_sel %>% 
  select(BIRTHYEAR, YEAR, AGE, FEMALE, starts_with("MEAN.OUTCOME")) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key)) +
  geom_line() +
  theme_bw() +
  labs(title = "Mean Wages by Age, Gender, and Cohort, CPS ORG, with Selection Weights") +
  facet_grid(rows = vars(BIRTHYEAR), cols = vars(FEMALE), scales = "free") +
  theme(legend.position = "bottom", 
        title = element_text(hjust = .5))

# Diagnostic plots: Predicted ORG Eligibility by age, gender, and cohort
org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
  summarise(PREDPROB_COHORT = mean(ELIGORG_PRED),
            PREDPROB_REFCOHORRT = mean(ELIGORG_PRED_REFCOHORT)) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key)) +
  geom_line() +
  theme_bw() +
  labs(title = "Predicted ORG Eligibility by Age, Gender, and Cohort, CPS") +
  theme(legend.position = "bottom", 
        title = element_text(hjust = .5)) +
  facet_grid(rows = vars(BIRTHYEAR), cols = vars(FEMALE))

# Diagnostic plots: Predicted ORG Eligibility by age, gender, and cohort
org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE, EDUC) %>%
  summarise(PREDPROB_COHORT = mean(ELIGORG_PRED),
            PREDPROB_REFCOHORRT = mean(ELIGORG_PRED_REFCOHORT)) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, EDUC, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key, color = EDUC)) +
  geom_line() +
  theme_bw() +
  labs(title = "Predicted ORG Eligibility by Age, Gender, Education, and Cohort, CPS") +
  theme(legend.position = "bottom", 
        title = element_text(hjust = .5)) +
  facet_grid(rows = vars(BIRTHYEAR), cols = vars(FEMALE))

# Diagnostic plots: Predicted ORG Eligibility by age, gender, and cohort
org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE, RACEETH) %>%
  summarise(PREDPROB_COHORT = mean(ELIGORG_PRED),
            PREDPROB_REFCOHORRT = mean(ELIGORG_PRED_REFCOHORT)) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, RACEETH, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key, color = RACEETH)) +
  geom_line() +
  theme_bw() +
  labs(title = "Predicted ORG Eligibility by Age, Gender, Race, and Cohort, CPS") +
  theme(legend.position = "bottom", 
        title = element_text(hjust = .5)) +
  facet_grid(rows = vars(BIRTHYEAR), cols = vars(FEMALE))


# Diagnostic plots: selection model coefficients
selection_models %>%
  filter(year_group %in% c(1, 5, 10, 15)) %>% 
  mutate(lower_ci = estimate - (std.error*1.96),
         upper_ci = estimate + (std.error*1.96)) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = AGE, y = estimate, color = SEX)) +
  geom_point(size = 2, position = position_dodge(width = .6)) +  
  theme_bw() + 
  # geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),     
  #              width = 0.2, position = position_dodge(width = .6)) +
  facet_grid(rows = vars(year_group), cols = vars(term))

