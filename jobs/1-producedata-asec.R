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
         FEMALE = ifelse(SEX == 2, "Women", "Men"),
         EDUC = ifelse(EDUC == 999, NA, EDUC),
         EDUC = case_when(EDUC  <= 73 ~ "hs.or.less", 
                          EDUC < 110 ~ "some.college", 
                          EDUC >= 110 ~ "ba.plus"), 
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
                               YNGCH <= 18 ~ "under18", 
                               TRUE ~ "over18"), 
         NCHILD.REC = case_when(NCHILD == 0 ~ "none", 
                                NCHILD == 1 ~ "one", 
                                NCHILD == 2 ~ "two", 
                                TRUE ~ "threeplus"),
         INCWAGE = ifelse(INCWAGE == 99999999, NA, INCWAGE)) %>%
  filter(complete.cases(ASECWT)) %>% # Selecting only individuals w/ sample weights
  # Excluding Agriculture and Military
  filter(IND1990 > 032) %>%
  filter(IND1990 < 940) %>%
  # Excluding self-employed
  filter(CLASSWKR %in% c(20, 21, 22, 23, 24, 25, 27, 28)) %>%
  #filter(INCWAGE > 0) %>% # Restricting to indivs. reporting positive wages
  filter(AGE >= 25 & AGE <= 55) %>% # Age Restrictions
  left_join(., cpi99, by = c("YEAR" = "year")) %>%
  # Creating variables: rough measure of birth cohort, log wage, categorical ed
  mutate(# Adjusting income for inflation, setting to 2020 dollar values
    INCWAGE = INCWAGE * cpi1999 * 1.553,
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


