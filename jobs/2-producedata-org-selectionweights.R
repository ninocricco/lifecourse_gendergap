#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: GENERATING ORG SELECTION WEIGHTS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
source("jobs/0-helperfunctions.R")

# Reading in full monthly data and creating year groups for
# pooled estimation of ORG eligibility
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

# Diagnostic plots: selection model coefficients
selection_model_estimates <- selection_models %>%
  filter(year_group %in% c(1, 5, 10, 15)) %>% 
  mutate(year_group = case_when(year_group == 1 ~ "1955-1957 cohort",
                                year_group == 5 ~ "1967-1969 cohort",
                                year_group == 10 ~ "1982-1984 cohort",
                                year_group == 15 ~ "1997-1999 cohort"
  )) %>%
  mutate(lower_ci = estimate - (std.error*1.96),
         upper_ci = estimate + (std.error*1.96)) %>%
  filter(term != "(Intercept)") %>%
  mutate(SEX = ifelse(SEX == "Male", "Men", "Women")) %>%
  ggplot(aes(x = AGE, y = estimate, color = SEX)) +
  geom_point(size = 2, position = position_dodge(width = .6)) +  
  create_common_theme(14, "bottom") +
  facet_grid(rows = vars(year_group), cols = vars(term)) +
  guides(color = guide_legend("")) +
  labs(x = "Age", "Estimate")

ggsave(
  filename = file.path("figures/draft_paper/diagnostic_plots", 
                       paste0("selection_model_estimates", ".pdf")),
  plot = selection_model_estimates,
  width = 8,
  height = 8,
  dpi = 500
)

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

prop_org_bydecade <- sample_all %>%
  group_by(BIRTHYEAR_DECADES, AGE, FEMALE) %>%
  summarise(ELIGORG_PCT = mean(ELIGORG)) %>%
  filter(BIRTHYEAR_DECADES %in% c("1960s", "1970s", "1980s", "1990-1998")) %>%
  ggplot(aes(x = AGE, y = ELIGORG_PCT, color = BIRTHYEAR_DECADES)) +
  geom_line() +
  scale_color_manual(values = figs_cohort_colors(use_grayscale = F)) +
  facet_grid(rows = vars(FEMALE)) +
  create_common_theme(base_size, "right") +
  theme(
    legend.position      = c(0.9, 0.8), 
    legend.justification = c("right", "top"),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", .8), colour = NA)
  ) +
  labs(x = "Age", y = "Percent Eligible, Outgoing Rotation Group")

ggsave("figures/draft_paper/submission_supplement/prop_org_bydecade.pdf",
       prop_org_bydecade,
       width = 8, height = 8, dpi = 500, units = "in")

pred_org_byage <- org_w_selection %>%
  ungroup() %>%
  select(AGE, FEMALE, ELIGORG_PRED, ELIGORG_PRED_REFCOHORT) %>%
  gather(key, value, -c(AGE, FEMALE)) %>%
  filter(AGE %in% c(25, 35, 45)) %>%
  ggplot(aes(x = value, linetype = key)) +
  geom_density() +
  facet_grid(rows = vars (FEMALE), cols =vars(AGE)) +
  guides(linetype = guide_legend("")) +
  create_common_theme(base_size, "bottom") +
  labs(title = "Distribution of Predicted Probabilities for ORG Inclusion by Age, All Cohorts")

ggsave("figures/draft_paper/diagnostic_plots/pred_org_byage.pdf",
       pred_org_byage,
       width = 8, height = 8, dpi = 500, units = "in")

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
            n = sum(WEIGHT))

means.age.bc_yr_sel <- means.age.bc_yr_sel %>%
  # Computes and merges column calculating total # of women in sample by year
  left_join(., means.age.bc_yr_sel %>%
              group_by(YEAR, FEMALE) %>%
              summarise(
                n_year = sum(n)), by = c("YEAR", "FEMALE")) %>%
  # Creates measure of the proportion of women in each year belonging to each year bc
  mutate(lf_cohortprop = n/n_year)

# Diagnostic plots: Weighted mean wages by age for a selection of cohorts

org_means <- means.age.bc_yr_sel %>% 
  select(BIRTHYEAR, YEAR, AGE, FEMALE, starts_with("MEAN.OUTCOME")) %>%
  mutate(RATIO_ALLCOHORTS = MEAN.OUTCOME_SEL/MEAN.OUTCOME, 
         RATIO_REFCOHORT = MEAN.OUTCOME_REF/MEAN.OUTCOME) %>%
  select(BIRTHYEAR, YEAR, AGE, FEMALE, starts_with("RATIO")) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key)) +
  geom_line() +
  labs(x = "Age", y = "Ratio") +
  create_common_theme(base_size, "bottom") +
  facet_grid(FEMALE ~ BIRTHYEAR) +
  guides(linetype = guide_legend(""))

ggsave(
  filename = file.path("figures/draft_paper/submission_supplement_new", paste0("org_means_selection_ratio", ".pdf")),
  plot = org_means,
  width = 8,
  height = 8,
  dpi = 500
)


# Diagnostic plots: Predicted ORG Eligibility by age, gender, and cohort
org_selection <- org_w_selection %>%
  # First create year birth cohort age-specific mean wages and n's
  group_by(BIRTHYEAR, YEAR, AGE, FEMALE) %>%
  summarise(PREDPROB_COHORT = mean(ELIGORG_PRED),
            PREDPROB_REFCOHORRT = mean(ELIGORG_PRED_REFCOHORT)) %>%
  gather(key, value, -c(BIRTHYEAR, YEAR, AGE, FEMALE)) %>%
  filter(BIRTHYEAR %in% c(1957, 1967, 1977, 1987)) %>%
  ggplot(aes(y = value, x = AGE, linetype = key)) +
  geom_line() +
  theme_bw() +
  labs(title = "", y = "Probability", x = "Age") +
  create_common_theme(base_size, "bottom") +
  facet_grid(rows = vars(BIRTHYEAR), cols = vars(FEMALE)) +
  guides(linetype = guide_legend(""))

ggsave(
  filename = file.path("figures/draft_paper/diagnostic_plots", paste0("org_selection", ".pdf")),
  plot = org_selection,
  width = 8,
  height = 8,
  dpi = 500
)

