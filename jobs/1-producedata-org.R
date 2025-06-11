#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: GENERATING ORG ANALYTIC SAMPLE FROM RAW IPUMS DATA
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
# NOTE: To load data, you must download both the extract's data and the DDI and 
# also set the wd to the folder with these files (or change the path below).

source("jobs/0-helperfunctions.R")
library(ipumsr)

ddi_monthly <- read_ipums_ddi("raw_data/cps_00021.xml")

# Read in CPI adjustment spreadsheet
cpi99 <- read_csv("raw_data/cpi99.csv")

# Read in IPUMS CPS data
data_monthly <- read_ipums_micro(
  ddi_monthly, 
  vars = c("YEAR", "MISH", "AGE", "IND1990", "CLASSWKR", "SEX", 
           "EDUC", "ELIGORG", "HOURWAGE", "EARNWEEK", "PAIDHOUR", 
           "UHRSWORKORG", "UHRSWORK1", "WKSTAT", "EARNWT", "HISPAN",
           "RACE", "MARST", "NATIVITY", "YNGCH", "NCHILD")) %>%
  # Filtering and removing unnecessary raw data for space constraints
  filter(YEAR >= 1980) %>%
  filter(YEAR < 2024) %>%
  # Select only months eligible for ORG interview
  filter(MISH %in% c(4, 8)) %>% 
  filter(AGE >= 21 & AGE <= 55) %>%
  # Excluding Agriculture and Military
  filter(IND1990 > 032) %>%
  filter(IND1990 < 940) %>%
  # Excluding self-employed
  filter(CLASSWKR %in% c(20, 21, 22, 23, 24, 25, 27, 28)) 

# Recoding IPUMS variables
analytic_sample <- data_monthly %>% 
  # Merging in CPI inflation adjustment 
  left_join(., cpi99, by = c("YEAR" = "year")) %>%
  mutate(BIRTHYEAR = YEAR-AGE, 
         FEMALE = ifelse(SEX == 2, "Women", "Men"),
         EDUC = ifelse(EDUC == 999, NA, EDUC),
         EDUC = case_when(EDUC  <= 73 ~ "hs.or.less", 
                          EDUC < 110 ~ "some.college", 
                          EDUC >= 110 ~ "ba.plus"), 
         EDUC2 = ifelse(EDUC == "ba.plus", "BA+", "<BA"),
         HOURWAGE = na_codes(HOURWAGE, 999.99),
         EARNWEEK = na_codes(EARNWEEK, 9999.99), 
         UHRSWORKORG = na_codes(UHRSWORKORG, c(998, 999)), 
         UHRSWORK1_HRSVARY_FLAG = ifelse(UHRSWORK1 == 997, 1, 0),
         UHRSWORK1 = na_codes(UHRSWORK1, c(997, 999)),
         WKSTAT_SUM = case_when(WKSTAT %in% c(10:15) ~ "ft", 
                                WKSTAT %in% c(20:42) ~ "pt", 
                                WKSTAT %in% c(50, 60)~ "unemp"), 
         EARNWT = EARNWT/12,
         RACEETH = case_when(HISPAN %!in% c(0, 901, 902) ~ "Latino",
                             RACE == 200 ~ "Black",
                             RACE == 100 ~ "White",
                             TRUE ~ "Other"), 
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
                                TRUE ~ "threeplus"),
         EARNWEEK_TOPCODE = case_when(
           YEAR %in% c(1982:1988) & EARNWEEK == 999 ~ 1,
           YEAR %in% c(1989:1997) & EARNWEEK == 1923 ~ 1,
           YEAR > 1997 & EARNWEEK > 2884.61 ~ 1,
           TRUE ~ 0))

#------------------------------------------------------------------------------
# Recoding main earnings variable 
#------------------------------------------------------------------------------
# From NBER MORG documentation ~ 2007
# https://cps.ipums.org/cps/resources/earner/cpsxNBER.pdf

# Earnings are collected per hour for hourly workers, and per week for other 
# workers. If you want a consistent hourly wage series during entire period, 
# you should use earnwke/uhourse. This gives imputed hourly wage for weekly 
# workers and actual hourly wage for hourly workers. But check earnwke for
# top-coding. Do not use any wage data that may be present for 
# self-employed workers.

# From IPUMS documentation
# https://cps.ipums.org/cps-action/variables/EARNWEEK#description_section
# Interviewers asked directly about total weekly earnings and also collected 
# information about the usual number of hours worked per week and the hourly 
# rate of pay at the current job. The figure given in EARNWEEK is the higher of 
# the values derived from these two sources:
#   1) the respondent's answer to the question, "How much do you usually earn
#   per week at this job before deductions?" or 
#   2) for workers paid by the hour (and coded as "2" in PAIDHOUR), the 
#   reported number of hours the respondent usually worked at the job,
#   multiplied by the hourly wage rate given in HOURWAGE.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Procedure to impute hours worked for respondents who report hours vary after
# 1994 CPS questionnaire redesign
# see: https://ceprdata.s3.amazonaws.com/data/cps/CEPR_ORG_Wages.pdf
#------------------------------------------------------------------------------

# Diagnostics figure to show what % of ORG eligible respondents report 
# "hours vary" by year and sex
fig_diag_hrsvary <- analytic_sample %>%
  filter(ELIGORG == 1) %>%
  group_by(FEMALE, YEAR) %>%
  summarise(share_hrsvary = wtd.mean(UHRSWORK1_HRSVARY_FLAG, weight = EARNWT)) %>%
  ggplot(aes(x = YEAR, y = share_hrsvary, linetype = FEMALE)) +
  geom_line() +
  theme_bw() +
  labs(title = "Diagnostics Plot: Workers Reporting Hours Vary, CPS Outgoing Rotation Group",
       y = "Share", x = "Year") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = .5),
        legend.title = element_blank()) +
  geom_vline(xintercept = 1994, color = "red", linetype = "dashed")

# Among those with UHRSWORK1_HRSVARY_FLAG == 0, 
# fit separate models by FEMALE and WKSTAT_SUM predicting UHRSWORK1 
# as a function of age, race, education, marital status, and nativity

# First split data into estimation and prediction samples
hrsvary_estimation <- analytic_sample %>%
  filter(UHRSWORK1_HRSVARY_FLAG != 1)

# Get a sense for missingness for reasons other than "Hours vary"
# Note: When we filter to ELIGORG eligibile respondents, there is no missingness
# beyond hours vary, but at this stage we keep all respondents in the ORG
# months even if they are not ORG eligible for later selection into 
# ORG prediction models
length(which(is.na(hrsvary_estimation$UHRSWORK1)))/dim(hrsvary_estimation)[1]

# Create nested dataframes by group for men, women by reported ft/pt status
model_hrsvary <- hrsvary_estimation %>%
  filter(ELIGORG == 1) %>%
  group_by(FEMALE, WKSTAT_SUM) %>%
  nest() %>%
  # Fit models for each group
  mutate(
    model = map(
      data, ~ lm(UHRSWORK1 ~ AGE + RACEETH + MARRIED + FBORN + EDUC, data = .x))
  )

# For workers who report hours vary, benerate predicted hours
hrsvary_data <- analytic_sample %>%
  filter(UHRSWORK1_HRSVARY_FLAG == 1) %>%
  group_by(FEMALE, WKSTAT_SUM) %>%
  nest() %>%
  # Join with model data to get appropriate models
  left_join(model_hrsvary %>% select(-data), by = c("FEMALE", "WKSTAT_SUM")) %>%
  # Generate predictions
  mutate(predictions = map2(model, data, predict)) %>%
  select(-model) %>%
  unnest(cols = c(data, predictions))

# Combine dataframes, workers reporting hours + workers reporting hours vary
analytic_sample_hrsvary <- bind_rows(
  hrsvary_estimation %>% mutate(UHRSWORK1_PRED = UHRSWORK1),
  hrsvary_data %>% mutate(UHRSWORK1_PRED = predictions)) %>%
  arrange(row_number()) %>%
  # Additional variable capping hours worked (obs or predicted) at 40
  mutate(UHRSWORK1_PRED_CAP = ifelse(UHRSWORK1_PRED > 40, 40, UHRSWORK1_PRED))

# Diagnostic plot comparing mean hours worked with list-wise deletion of workers
# reporting hours vary vs. imputing hours worked

fig_hrsvary_pred <- analytic_sample_hrsvary %>%
  filter(ELIGORG == 1) %>%
  group_by(FEMALE, YEAR) %>%
  summarise(UHRSWORK1_PRED = wtd.mean(UHRSWORK1_PRED, weight = EARNWT), 
            UHRSWORK1 = wtd.mean(UHRSWORK1, weight = EARNWT)) %>%
  gather(key, value, -c(FEMALE, YEAR)) %>% 
  ggplot(aes(x = YEAR, y = value, linetype = key)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~FEMALE) +
  labs(title = "Diagnostics Plot: Mean Usual Hours Worked, List-wise Deletion 
       and Imputation, CPS Outgoing Rotation Group",
       y = "Share", x = "Year") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = .5),
        legend.title = element_blank()) +
  geom_vline(xintercept = 1994, color = "red", linetype = "dashed")

ggsave("figures/draft_paper/diagnostic_plots/hoursvary.pdf",
       grid.arrange(fig_diag_hrsvary, fig_hrsvary_pred, ncol = 1),
       width = 10, height = 10, dpi = 500, units = "in")

#------------------------------------------------------------------------------
# Procedure to adjust for changes in top-coding using log-normal distribution 
# to estimate mean and variance above the topcode 
# see: https://ceprdata.s3.amazonaws.com/data/cps/CEPR_ORG_Wages.pdf
#------------------------------------------------------------------------------

# Diagnostic plot, share of workers reporting weekly earnings above the topcode
fig_diag_topcode <- analytic_sample_hrsvary %>%
  filter(ELIGORG == 1) %>%
  group_by(FEMALE, YEAR) %>%
  summarise(share_topcoded = wtd.mean(EARNWEEK_TOPCODE, weight = EARNWT)) %>%
  ggplot(aes(x = YEAR, y = share_topcoded, linetype = FEMALE)) +
  geom_line() +
  theme_bw() +
  labs(title = "Diagnostics Plot: Workers w/ Top-Coded Weekly Earnings, ORG",
       y = "Share", x = "Year") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = .5),
        legend.title = element_blank()) +
  geom_vline(xintercept = 1989, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1998, color = "red", linetype = "dashed")

analytic_sample_recoded_topcode <- analytic_sample_hrsvary %>% 
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
    EARNWEEK_TC = ifelse(EARNWEEK_TOPCODE == 1, mean_above_topcode, EARNWEEK)
  ) %>% 
  ungroup()

# Diagnostic plot, mean earnings w/topcode vs. adjusting topcode
fig_mean_earnings_topcode_adj <- analytic_sample_recoded_topcode %>%
  group_by(FEMALE, YEAR) %>%
  summarise(EARNWEEK_TC = wtd.mean(EARNWEEK_TC, weight = EARNWT), 
            EARNWEEK = wtd.mean(EARNWEEK, weight = EARNWT)) %>%
  gather(KEY, VALUE, -c(FEMALE, YEAR)) %>%
  ggplot(aes(x = YEAR, y = VALUE, linetype = KEY)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~FEMALE) + 
  labs(title = "Diagnostics Plot: Mean Weekly Earnigns w/ Top-Code Adjustment, ORG",
       y = "Weekly Earnigns", x = "Year") +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = .5),
        legend.title = element_blank()) +
  geom_vline(xintercept = 1989, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1998, color = "red", linetype = "dashed")

ggsave("figures/draft_paper/diagnostic_plots/topcode.pdf",
       grid.arrange(fig_diag_topcode, fig_mean_earnings_topcode_adj, ncol = 1),
       width = 8, height = 8, dpi = 500, units = "in")

# For the period 1979-2002, the most consistent and
# robust hourly wage series:  (2) excludes
# data below $1 and above $100 per hour (in constant 2002 dollars); (3) excludes overtime, tip,
# and commission earnings for hourly paid workers; and (4) uses a simple procedure to impute
# usual weekly hours for those who report their "hours vary" after 1994. For the period 1994-2002,
# the best hourly wage series follows the same procedure, but includes overtime, tips, and
# commissions for workers that report their earnings by the hour.

analytic_sample_org <- analytic_sample_recoded_topcode %>%
  select(-c(log_mean, log_sd, log_topcode, t1, t2, t3, mean_above_topcode)) %>%
  mutate(HOURWAGE_RAW = HOURWAGE,
         EARNWEEK_RAW = EARNWEEK,
         HOURWAGE = HOURWAGE * cpi1999 * 1.553,
         EARNWEEK = EARNWEEK * cpi1999 * 1.553,
         EARNWEEK_TC = EARNWEEK_TC * cpi1999 * 1.553,
         EARNHRLY = case_when(PAIDHOUR == 2 ~ HOURWAGE, 
                              UHRSWORK1 == 0 ~ NA,
                              PAIDHOUR != 2 ~ EARNWEEK/UHRSWORK1),
         EARNHRLY_TC = case_when(PAIDHOUR == 2 ~ HOURWAGE, 
                                 UHRSWORK1 == 0 ~ NA,
                                 PAIDHOUR != 2 ~ EARNWEEK_TC/UHRSWORK1),
         EARNHRLY_TC_HRSVP = case_when(PAIDHOUR == 2 ~ HOURWAGE, 
                                       UHRSWORK1 == 0 ~ NA,
                                       PAIDHOUR != 2 ~ EARNWEEK_TC/UHRSWORK1_PRED),
         EARNHRLY_FLAG = case_when(PAIDHOUR == 2 ~ "Hourly",
                                   # THERE ARE 987 OBS REPORTING PAID WEEKLY BUT 0 HOURS WORKED
                                   UHRSWORK1 == 0 ~ "None", 
                                   PAIDHOUR != 2 ~ "Weekly"), 
         EARNHRLY_MAIN = EARNWEEK_TC/UHRSWORK1_PRED, 
         EARNHRLY_HRSCAP = EARNWEEK_TC/UHRSWORK1_PRED_CAP,
         BIRTHYEAR_DECADES = case_when(BIRTHYEAR %in% c(1913:1919) ~ "1913-1919",
                                       BIRTHYEAR %in% c(1920:1929) ~ "1920s",
                                       BIRTHYEAR %in% c(1930:1939) ~ "1930s",
                                       BIRTHYEAR %in% c(1940:1949) ~ "1940s",
                                       BIRTHYEAR %in% c(1950:1959) ~ "1950s",
                                       BIRTHYEAR %in% c(1960:1969) ~ "1960s",
                                       BIRTHYEAR %in% c(1970:1979) ~ "1970s",
                                       BIRTHYEAR %in% c(1980:1989) ~ "1980s", 
                                       BIRTHYEAR %in% c(1990:1998) ~ "1990-1998" 
         ))

# Diagnostic plot, mean wages at age 25 with different wage measures

means_25_wagemeasures <- analytic_sample_org %>%
  filter(ELIGORG == 1) %>% filter(AGE == 25) %>% 
  group_by(BIRTHYEAR, FEMALE, YEAR) %>% 
  summarise(across(
    .cols = c(EARNHRLY, EARNWEEK_TC,
              EARNWEEK, HOURWAGE),
    .fns = ~ weighted.mean(., w = EARNWT, na.rm = TRUE)
  )) %>% 
  gather(KEY, VALUE, -c(BIRTHYEAR, FEMALE, YEAR)) %>%
  mutate(KEY_2 = ifelse(KEY %in% c("EARNHRLY", "HOURWAGE"), "Hourly", "Weekly")) %>%
  ggplot(aes(x = YEAR, y = VALUE, linetype = FEMALE)) +
  facet_grid(rows = vars(KEY), scales = "free") +
  geom_line() +
  theme_bw() +
  labs(title = "Weighted Means, Age 25 Wage Measures Across Years") +
  geom_vline(xintercept = 1994, color = "red", linetype = "dashed") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5)) +
  guides(linetype = guide_legend(""))

ggsave("figures/draft_paper/diagnostic_plots/means_25_wagemeasures.pdf",
       means_25_wagemeasures,
       width = 8, height = 8, dpi = 500, units = "in")

# Diagnostic table and plot, share reporting earnings weekly/hourly

table_reporting_wages_type_pct <- analytic_sample_org %>%
  filter(ELIGORG == 1) %>% 
  filter(FEMALE == "Women", EARNHRLY_FLAG != "None") %>%
  tabyl(YEAR, EARNHRLY_FLAG) %>%
  rename(Women_Hourly = Hourly, Women_Weekly = Weekly) %>%
  adorn_percentages("row") %>%
  left_join(analytic_sample_org %>%
              filter(FEMALE == "Men", EARNHRLY_FLAG!= "None") %>%
              tabyl(YEAR, EARNHRLY_FLAG) %>%
              rename(Men_Hourly = Hourly, Men_Weekly = Weekly) %>%
              adorn_percentages("row"), by = "YEAR") %>%
  mutate_if(is.numeric, round, digits = 3)

write_csv(table_reporting_wages_type_pct, 
          "figures/draft_paper/diagnostic_plots/table_reporting_wages_type_pct.csv")

reporting_wages_pct <- table_reporting_wages_type_pct %>%
  gather(key, value, -YEAR) %>%
  separate(key, c("gender", "type"), "_") %>%
  filter(type == "Hourly") %>%
  ggplot(aes(x = YEAR, y = value)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~gender) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = .5)) +
  labs(y = "Percent", x = "Year", 
       title = "Share of Workers Reporting Hourly (vs. Weekly) Earnings") +
  guides(linetype = guide_legend(""))

# Diagnostic plot, mean wages among those reporting earnings weekly/hourly

reporting_wages_means  <- analytic_sample_org %>%
  filter(ELIGORG == 1, EARNHRLY_FLAG != "None") %>% 
  group_by(YEAR, FEMALE, EARNHRLY_FLAG) %>%
  summarise(mean_earnings = wtd.mean(EARNHRLY, weight = EARNWT)) %>%
  ggplot(aes(x = YEAR, y = mean_earnings, linetype = EARNHRLY_FLAG)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~FEMALE) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = .5)) +
  labs(y = "Mean", x = "Year",
       title = "Mean Wages Among Workers Reporting Hourly/Weekly Earnings") +
  geom_vline(xintercept = 1988, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1993, linetype = "dashed", color = "red") +
  guides(linetype = guide_legend("Earnings Reported"))

ggsave("figures/draft_paper/diagnostic_plots/type_reporting.pdf",
       grid.arrange(reporting_wages_pct, reporting_wages_means, ncol = 1),
       width = 8, height = 8, dpi = 500, units = "in")

# Writing out clean analytic data files, both all monthly + only ORG eligible

analytic_sample_org_elig <- analytic_sample_org %>%
  filter(ELIGORG == 1)

write_rds(analytic_sample_org, "clean_data/analytic_sample_org_all.rds")
write_rds(analytic_sample_org_elig, "clean_data/analytic_sample_org_elig.rds")
write_csv(analytic_sample_org, "clean_data/analytic_sample_org_all.csv")
write_csv(analytic_sample_org_elig, "clean_data/analytic_sample_org_elig.csv")
