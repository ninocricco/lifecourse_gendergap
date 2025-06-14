#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: GENERATING ASEC ANALYTIC SAMPLE FROM RAW IPUMS DATA
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the wd to the folder with these files (or change the path below).

source("jobs/0-helperfunctions.R")
library(ipumsr)

ddi <- read_ipums_ddi("raw_data/cps_00025.xml")
data_asec <- read_ipums_micro(ddi)

#Read in CPI adjustment spreadsheet
cpi99 <- read_csv("raw_data/cpi99.csv")

# Creating the analytic sample
analytic_sample <- data_asec %>%
mutate(
  BIRTHYEAR = YEAR-AGE, 
  BIRTHYEAR_DECADES = case_when(BIRTHYEAR %in% c(1927:1936) ~ "1927-1936",
                                BIRTHYEAR %in% c(1937:1946) ~ "1937-1946",
                                BIRTHYEAR %in% c(1947:1956) ~ "1947-1956",
                                BIRTHYEAR %in% c(1957:1966) ~ "1957-1966",
                                BIRTHYEAR %in% c(1967:1976) ~ "1967-1976",
                                BIRTHYEAR %in% c(1977:1986) ~ "1977-1986",
                                BIRTHYEAR %in% c(1987:1996) ~ "1987-1996"
  ),
  FEMALE = ifelse(SEX == 2, "Women", "Men"),
  EDUC = ifelse(EDUC == 999, NA, EDUC),
  EDUC2 = ifelse(EDUC >= 110, "BA+", "<BA"),
  RACEETH = case_when(HISPAN %!in% c(0, 901, 902) ~ "Latino",
                      RACE == 200 ~ "Black",
                      RACE == 100 ~ "White",
                      TRUE ~ "Other"), 
  INCWAGE = ifelse(INCWAGE == 99999999, NA, INCWAGE)) %>%
  filter(complete.cases(ASECWT)) %>% # Selecting only individuals w/ + weights
  filter(AGE >= 25 & AGE <= 55) %>% # Age Restrictions
  left_join(., cpi99, by = c("YEAR" = "year")) %>%
mutate(across(c(INCWAGE, FTOTVAL, INCTOT), ~.x * cpi1999 * 1.829)) %>%
mutate(
  # Creating predicted hourly wage variable
  UHRSWORKLY = na_if(UHRSWORKLY, 999),
  PRED.ANNHRSWRK = UHRSWORKLY * WKSWORK1,
  PRED.HRLYWAGE = INCWAGE/PRED.ANNHRSWRK
)

write_rds(analytic_sample, "clean_data/analytic_sample_asec.rds")
write_csv(analytic_sample, "clean_data/analytic_sample_asec.csv")
