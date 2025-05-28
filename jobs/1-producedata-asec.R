library(tidyverse)
library(ipumsr)
library(janitor)
library(Hmisc)
library(ggrepel)
library(gridExtra)
library(grid)

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("raw_data/cps_00019.xml")
data_asec <- read_ipums_micro(ddi)

#Read in CPI adjustment spreadsheet
cpi99 <- read_csv("raw_data/cpi99.csv")

# Creating the analytic sample
analytic.sample <- data_asec %>%
  mutate(AGE == AGE - 1,
         BIRTHYEAR = YEAR-AGE, 
         BIRTHYEAR_DECADES = case_when(BIRTHYEAR %in% c(1913:1919) ~ "1913-1919",
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
         EDUC = case_when(EDUC  <= 73 ~ "hs.or.less", 
                          EDUC < 110 ~ "some.college", 
                          EDUC >= 110 ~ "ba.plus"), 
         EDUC2 = ifelse(EDUC == "ba.plus", "BA+", "<BA"),
         RACEETH = case_when(RACE == 200 ~ "Black",
                             RACE == 100 ~ "White",
                             TRUE ~ "Other"), 
         MARRIED = case_when(MARST %in% c(1,2) ~ "married", 
                             MARST == 6 ~ "never.married", 
                             TRUE ~ "prev.married"), 
         INCWAGE = ifelse(INCWAGE == 99999999, NA, INCWAGE)) %>%
  filter(complete.cases(ASECWT)) %>% # Selecting only individuals w/ sample weights
  filter(AGE >= 25 & AGE <= 55) %>% # Age Restrictions
  left_join(., cpi99, by = c("YEAR" = "year")) %>%
  # Creating variables: rough measure of birth cohort, log wage, categorical ed
  mutate(# Adjusting income for inflation, setting to 2020 dollar values
    INCWAGE = INCWAGE * cpi1999 * 1.553,
    INCWAGE = INCWAGE * cpi1999 * 1.553,
    FTOTVAL = FTOTVAL * cpi1999 * 1.553,
    INCTOT = INCTOT * cpi1999 * 1.553,
    LOG.INCWAGE = log(INCWAGE))

# Creates income top-code by year and joins back to main data, 
# creates top-coded income measure
analytic.sample.asec <- analytic.sample %>%
  group_by(YEAR) %>%
  summarise(topcode = wtd.quantile(INCWAGE, weight = ASECWT, probs = .98)) %>%
  left_join(., analytic.sample) %>%
  mutate(INCWAGE.TOPCODE = ifelse(INCWAGE > topcode, topcode, INCWAGE),
         # Creating predicted hourly wage variable
         UHRSWORKLY = na_if(UHRSWORKLY, 999),
         PRED.ANNHRSWRK = UHRSWORKLY * WKSWORK1,
         PRED.HRLYWAGE.TOPCODE = INCWAGE.TOPCODE/PRED.ANNHRSWRK) 

write_rds(analytic.sample.asec, "clean_data/analytic_sample_asec.rds")