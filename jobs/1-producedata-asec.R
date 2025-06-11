#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: GENERATING ASEC ANALYTIC SAMPLE FROM RAW IPUMS DATA
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the wd to the folder with these files (or change the path below).

source("jobs/0-helperfunctions.R")
library(ipumsr)

ddi <- read_ipums_ddi("raw_data/cps_00019.xml")
data_asec <- read_ipums_micro(ddi)

#Read in CPI adjustment spreadsheet
cpi99 <- read_csv("raw_data/cpi99.csv")

# Creating the analytic sample
analytic_sample <- data_asec %>%
mutate(
  BIRTHYEAR = YEAR-AGE, 
  BIRTHYEAR_DECADES = case_when(
    BIRTHYEAR %in% c(1913:1919) ~ "1913-1919",
    BIRTHYEAR %in% c(1920:1929) ~ "1920s",
    BIRTHYEAR %in% c(1930:1939) ~ "1930s",
    BIRTHYEAR %in% c(1940:1949) ~ "1940s",
    BIRTHYEAR %in% c(1950:1959) ~ "1950s",
    BIRTHYEAR %in% c(1960:1969) ~ "1960s",
    BIRTHYEAR %in% c(1970:1979) ~ "1970s",
    BIRTHYEAR %in% c(1980:1989) ~ "1980s",
    BIRTHYEAR %in% c(1990:1998) ~ "1990-1998"),
  FEMALE = ifelse(SEX == 2, "Women", "Men"),
  EDUC = ifelse(EDUC == 999, NA, EDUC),
  EDUC = case_when(
    EDUC  <= 73 ~ "hs.or.less", 
    EDUC < 110 ~ "some.college", 
    EDUC >= 110 ~ "ba.plus"),
  EDUC2 = ifelse(EDUC == "ba.plus", "BA+", "<BA"),
  RACEETH = case_when(
    RACE == 200 ~ "Black",
    RACE == 100 ~ "White",
    TRUE ~ "Other"), 
  MARRIED = case_when(
    MARST %in% c(1,2) ~ "married",
    MARST == 6 ~ "never.married", 
    TRUE ~ "prev.married"), 
  INCWAGE = ifelse(INCWAGE == 99999999, NA, INCWAGE)) %>%
  filter(complete.cases(ASECWT)) %>% # Selecting only individuals w/ + weights
  filter(AGE >= 25 & AGE <= 55) %>% # Age Restrictions
  left_join(., cpi99, by = c("YEAR" = "year")) %>%
# Creating variables: rough measure of birth cohort, log wage, categorical ed
mutate(across(c(INCWAGE, FTOTVAL, INCTOT), ~.x * cpi1999 * 1.553),
  LOG.INCWAGE = log(INCWAGE)) %>%
# Creates income top-code by year
group_by(YEAR) %>%
mutate(
  topcode = wtd.quantile(INCWAGE, weight = ASECWT, probs = .98)[[1]],
  INCWAGE.TOPCODE   = pmin(INCWAGE, topcode)) %>%
ungroup() %>%
mutate(
  # Creating predicted hourly wage variable
  UHRSWORKLY = na_if(UHRSWORKLY, 999),
  PRED.ANNHRSWRK = UHRSWORKLY * WKSWORK1,
  PRED.HRLYWAGE.TOPCODE = INCWAGE.TOPCODE/PRED.ANNHRSWRK
)

write_rds(analytic_sample, "clean_data/analytic_sample_asec.rds")
write_csv(analytic_sample, "clean_data/analytic_sample_asec.csv")
