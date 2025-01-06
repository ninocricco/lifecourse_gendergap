#------------------------------------------------------------------------------
# PROJECT: TRENDS IN THE GENDER PAY GAP: NARROWING STARTING POINTS AND 
# PERSISTENT LIFE COURSE DIVERGENCE
# FILE: GENERATES ANALYTIC DATA FROM RAW IPUMS DATA- OUTGOING ROTATION GROUP
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

# NOTE: To load data, you must download both the extract's data and the DDI and 
# also set the working directory to the folder with these files 
# (or change the path below).

source("jobs/0-helperfunctions.R")

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi_monthly <- read_ipums_ddi("raw_data/cps_00018.xml")

# Read in IPUMS CPS data
data_monthly <- read_ipums_micro(ddi_monthly) %>%
  filter(YEAR >= 1980) %>%
  filter(YEAR < 2024) %>%
  filter(MISH %in% c(4, 8)) # Select only months eligible for ORG interview

# Read in CPI adjustment spreadsheet
cpi99 <- read_csv("raw_data/cpi99.csv")

# Filtering and removing unnecessary raw data for space constraints
analytic_sample <- data_monthly %>%
  filter(AGE >= 21 & AGE <= 55) %>%
  # Excluding Agriculture and Military
  filter(IND1990 > 032) %>%
  filter(IND1990 < 940) %>%
  # Excluding self-employed
  filter(CLASSWKR %in% c(20, 21, 22, 23, 24, 25, 27, 28)) %>%
  left_join(., cpi99, by = c("YEAR" = "year"))

rm(data_monthly)

# Recoding IPUMS variables
analytic_sample_recoded <- analytic_sample %>% 
  mutate(AGE = AGE - 1,
         BIRTHYEAR = YEAR-AGE, 
         FEMALE = ifelse(SEX == 2, "Women", "Men"),
         EDUC = ifelse(EDUC == 999, NA, EDUC),
         EDUC = case_when(EDUC  <= 73 ~ "hs.or.less", 
                          EDUC < 110 ~ "some.college", 
                          EDUC >= 110 ~ "ba.plus"), 
         HOURWAGE = na_codes(HOURWAGE, 999.99),
         EARNWEEK = na_codes(EARNWEEK, 9999.99), 
         UHRSWORKORG = na_codes(UHRSWORKORG, c(998, 999)), 
         UHRSWORK1 = na_codes(UHRSWORK1, c(997, 999)),
         EARNWEEK_TOPCODE = case_when(
           YEAR %in% c(1982:1988) & EARNWEEK == 999 ~ 1,
           YEAR %in% c(1989:1997) & EARNWEEK == 1923 ~ 1,
           YEAR > 1997 & EARNWEEK > 2884.61 ~ 1,
           TRUE ~ 0))

rm(analytic_sample)

# Procedure to adjust for changes in top-coding using log-normal distribution 
# to estimate mean and variance above the topcode 
# see: https://ceprdata.s3.amazonaws.com/data/cps/CEPR_ORG_Wages.pdf

analytic_sample_recoded_topcode <- analytic_sample_recoded %>% 
  group_by(YEAR) %>%
  mutate(
    log_mean = ifelse(EARNWEEK_TOPCODE == 1, 
                      mean(log(EARNWEEK), na.rm = TRUE), NA),
    log_sd = ifelse(EARNWEEK_TOPCODE == 1, 
                    sd(log(EARNWEEK), na.rm = TRUE), NA),
    log_topcode = case_when(
      YEAR %in% c(1982:1988) ~ log(999), 
      YEAR %in% c(1989:1997) ~ log(1923),
      YEAR > 1997 ~ log(2884.61)),
    t1 = exp(log_mean + log_sd^2 / 2), 
    t2 = (1 - pnorm((log_topcode - log_mean - log_sd^2) / log_sd)),
    t3 = (1 - pnorm((log_topcode - log_mean) / log_sd)),
    mean_above_topcode = t1 * t2 / t3,
    EARNWEEK_ADJ = ifelse(EARNWEEK_TOPCODE == 1, mean_above_topcode, EARNWEEK)
  ) %>% 
  ungroup()

rm(analytic_sample_recoded)

# Creating a figure showing percent eligible for ORG by age and year 
fig_org_eligibility_age_year <- analytic_sample_recoded_topcode %>% 
  group_by(BIRTHYEAR, FEMALE, YEAR, AGE) %>%
  filter(BIRTHYEAR %in% c(1958, 1965, 1970, 1975, 1980, 1985, 1990)) %>%
  summarise(PROP_ELIGORG = mean(ELIGORG)) %>%
  ggplot(aes(x = AGE, y = PROP_ELIGORG, linetype = FEMALE)) +
  geom_line() +
  labs(x = "Age", y = "Proportion Eligible", 
       title = "ORG Eligibility based on Employed Wage/Salary Criteria") + 
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = .5)) +
  facet_grid(~BIRTHYEAR) 

ggsave("figures/org_eligibility.pdf", fig_org_eligibility_age_year, 
       height = 5, width = 10, units = "in")
         
# From NBER :
# Earnings are collected per hour for hourly workers, and per week
# for other workers. If you want a consistent hourly wage series
# during entire period, you should use earnwke/uhourse. This gives
# imputed hourly wage for weekly workers and actual hourly wage for
# hourly workers. But check earnwke for top-coding. Do not use any
# wage data that may be present for self-employed workers.

analytic_sample_org <- analytic_sample_recoded_topcode %>%
  select(-c(log_mean, log_sd, log_topcode, t1, t2, t3, mean_above_topcode)) %>%
  mutate(HOURWAGE_RAW = HOURWAGE,
         EARNWEEK_RAW = EARNWEEK,
         HOURWAGE = HOURWAGE * cpi1999 * 1.553,
         EARNWEEK = EARNWEEK * cpi1999 * 1.553,
         EARNWEEK_ADJ = EARNWEEK_ADJ * cpi1999 * 1.553,
         EARNHRLY = case_when(PAIDHOUR == 2 ~ HOURWAGE, 
                              UHRSWORK1 == 0 ~ NA,
                              PAIDHOUR != 2 ~ EARNWEEK_ADJ/UHRSWORK1),
         EARNWT = EARNWT/12,
         RACEETH = case_when(HISPAN %!in% c(0, 901, 902) ~ "latino",
                             RACE == 200 ~ "black",
                             RACE == 100 ~ "white",
                             TRUE ~ "other"), 
         MARRIED = case_when(MARST %in% c(1,2) ~ "married", 
                             MARST == 6 ~ "never.married", 
                             TRUE ~ "prev.married"), 
         FBORN = factor(ifelse(NATIVITY == 5, 1, 0)),
         YNGCH.REC = case_when(YNGCH == 99 ~ "none", 
                               YNGCH <= 5 ~ "under5", 
                               TRUE ~ "olderthan5"), 
         NCHILD.REC = case_when(NCHILD == 0 ~ "none", 
                                NCHILD == 1 ~ "one", 
                                NCHILD == 2 ~ "two", 
                                TRUE ~ "threeplus"))

analytic_sample_org_elig <- analytic_sample_org %>%
  filter(ELIGORG == 1)
         
write_rds(analytic_sample_org, "clean_data/analytic_sample_org_all.rds")
write_rds(analytic_sample_org_elig, "clean_data/analytic_sample_org_elig.rds")