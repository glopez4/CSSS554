rm(list = ls())

library(tidyverse)
library(sf)
library(spdep)
library(INLA)
library(ggplot2)
library(tmap)
library(spatialreg)
library(viridis)
library(readstata13)
library(cdlTools) 
library(modelsummary)
library(arsenal) 

## lines 17-656 were provided from the data repository in order to combine the different surveys
nsch_2016_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2016_topical.dta")
nsch_2017_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2017_topical.dta")
nsch_2018_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2018_topical.dta")
nsch_2019_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2019_topical.dta")
nsch_2020_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2020_topical.dta")
nsch_2021_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2021_topical.dta")
nsch_2022_top <- read.dta13("dataverse_files-7/NSCH data/nsch_2022e_topical.dta")

# Treat stratum as character
# Necessary for a smooth merge
nsch_2016_top$stratum <- as.character(nsch_2016_top$stratum)

# Bind all survey years together
nsch <- bind_rows(nsch_2016_top, nsch_2017_top, nsch_2018_top, nsch_2019_top,
                  nsch_2020_top, nsch_2021_top, nsch_2022_top)

# Redefine strata
# Recommended by the NSCH for multi-year analyses
nsch <- nsch %>% mutate(
  strata = case_when(
    stratum %in% c("1")       ~ "1",
    stratum %in% c("2", "2A") ~ "2"
  ))

# Read minimum wage dataset into R
min_wage_df <- read.csv("dataverse_files-7/Supplemental data/State minimum wages, raw.csv")

# Copy forward missing years
# Note: These are only relevant for supplemental analyses
# We have complete information for the main study years (2000 onward)
min_wage_df$X1982 <- min_wage_df$X1981
min_wage_df$X1983 <- min_wage_df$X1981
min_wage_df$X1984 <- min_wage_df$X1981
min_wage_df$X1985 <- min_wage_df$X1981
min_wage_df$X1986 <- min_wage_df$X1981
min_wage_df$X1987 <- min_wage_df$X1981
min_wage_df$X1989 <- min_wage_df$X1988
min_wage_df$X1990 <- min_wage_df$X1988
min_wage_df$X1993 <- min_wage_df$X1992
min_wage_df$X1995 <- min_wage_df$X1994
min_wage_df$X1999 <- min_wage_df$X1998

# Convert dataset to long format
min_wage_long <- min_wage_df %>%
  select(order(colnames(min_wage_df))) %>%
  select(State.or.other.jurisdiction, X1980:X2023) %>%
  pivot_longer(cols = X1980:X2023, names_to = "year", values_to = "bls_min_wage")

# Convert state names to FIPS
# Necessary for a smooth merge
min_wage_long$fipsst <- cdlTools::fips(min_wage_long$State.or.other.jurisdiction, to="FIPS")

# Treat year as numeric
# Remove X before each value
min_wage_long$year <- as.numeric(gsub("X", "", min_wage_long$year))

# Generate new column for cleaned minimum wages
min_wage_long$state_min_wage <- min_wage_long$bls_min_wage

# Correct typo (double period) in select rows
min_wage_long$state_min_wage <- gsub("\\.\\.", "\\.", min_wage_long$state_min_wage)

# For ranges, select both values
min_wage_long <- min_wage_long %>% mutate(
  
  # Get values before hyphen or other symbols
  state_min_wage_1 = case_when(
    grepl("[-–/&]", state_min_wage) ~ gsub("[-–/&].*", "", min_wage_long$state_min_wage),
    TRUE ~ gsub("[-–/&].*", "", min_wage_long$state_min_wage)),
  
  # Get values after hyphen or other symbols
  state_min_wage_2 = case_when(
    grepl("[-–/&]", state_min_wage) ~ gsub(".*[-–/&]", "", min_wage_long$state_min_wage),
    TRUE ~ gsub(".*[-–/&]", "", min_wage_long$state_min_wage))
)

# Remove any remaining symbols or letters
# In practice, discard text starting with symbol
min_wage_long$state_min_wage_1 <- gsub("[-–&\\[\\(].*$", "", min_wage_long$state_min_wage_1)
min_wage_long$state_min_wage_2 <- gsub("[-–&\\[\\(].*$", "", min_wage_long$state_min_wage_2)

# Remove leading dollar signs and treat as numeric
min_wage_long$state_min_wage_1 <- as.numeric(gsub("\\$", "", min_wage_long$state_min_wage_1))
min_wage_long$state_min_wage_2 <- as.numeric(gsub("\\$", "", min_wage_long$state_min_wage_2))

# Get upper and lower ends of ranges
min_wage_long$state_min_wage_u <- pmax(min_wage_long$state_min_wage_1, min_wage_long$state_min_wage_2, na.rm = TRUE)
min_wage_long$state_min_wage_l <- pmin(min_wage_long$state_min_wage_1, min_wage_long$state_min_wage_2, na.rm = TRUE)

# Get federal minimum wages
min_wage_fed <- subset(min_wage_long, State.or.other.jurisdiction == "Federal (FLSA)")
min_wage_fed$federal_min_wage <- min_wage_fed$state_min_wage_1
min_wage_fed <- min_wage_fed %>% select(year, federal_min_wage)

# Merge state and federal minimum wages
min_wage_long <- left_join(min_wage_long, min_wage_fed, by="year")

# Get effective minimum wages
# Higher of state or federal minimums
min_wage_long$effective_min_wage_u <- pmax(min_wage_long$state_min_wage_u, min_wage_long$federal_min_wage, na.rm = TRUE)
min_wage_long$effective_min_wage_l <- pmax(min_wage_long$state_min_wage_l, min_wage_long$federal_min_wage, na.rm = TRUE)

# Read CPI dataset into R
cpi_df <- read.csv("dataverse_files-7/Supplemental data/CPI by month, raw.csv")

# Select January CPI estimates
# Note: Wage data are effective as of January 1 in each year
cpi_df$year        <- cpi_df$Year
cpi_df$cpi_january <- cpi_df$Jan
cpi_df <- cpi_df %>% select(cpi_january, year)

# Merge CPI estimates
min_wage_long <- left_join(min_wage_long, cpi_df, by="year")

# Compute inflation-adjusted wages
# Use CPI in January 2020: 257.971
min_wage_long$inflation_min_wage_u <- 257.971 / min_wage_long$cpi_january * min_wage_long$effective_min_wage_u
min_wage_long$inflation_min_wage_l <- 257.971 / min_wage_long$cpi_january * min_wage_long$effective_min_wage_l

# Reorder dataset
min_wage_long <- min_wage_long %>% arrange(fipsst, year)

# Generate lagged minimum wages
min_wage_long <- min_wage_long %>%
  group_by(fipsst) %>%
  mutate(
    
    # Not inflation adjusted
    lag_by_1  = lag(effective_min_wage_l, n=1, default=NA),
    lag_by_2  = lag(effective_min_wage_l, n=2, default=NA),
    lag_by_3  = lag(effective_min_wage_l, n=3,  default=NA),
    lag_by_4  = lag(effective_min_wage_l, n=4,  default=NA),
    lag_by_5  = lag(effective_min_wage_l, n=5,  default=NA),
    lag_by_6  = lag(effective_min_wage_l, n=6,  default=NA),
    lag_by_7  = lag(effective_min_wage_l, n=7,  default=NA),
    lag_by_8  = lag(effective_min_wage_l, n=8,  default=NA),
    lag_by_9  = lag(effective_min_wage_l, n=9,  default=NA),
    lag_by_10 = lag(effective_min_wage_l, n=10, default=NA),
    lag_by_11 = lag(effective_min_wage_l, n=11, default=NA),
    lag_by_12 = lag(effective_min_wage_l, n=12, default=NA),
    lag_by_13 = lag(effective_min_wage_l, n=13, default=NA),
    lag_by_14 = lag(effective_min_wage_l, n=14, default=NA),
    lag_by_15 = lag(effective_min_wage_l, n=15, default=NA),
    lag_by_16 = lag(effective_min_wage_l, n=16, default=NA),
    lag_by_17 = lag(effective_min_wage_l, n=17, default=NA),
    
    # Inflation adjusted
    lag_by_1_inf  = lag(inflation_min_wage_l, n=1,  default=NA),
    lag_by_2_inf  = lag(inflation_min_wage_l, n=2,  default=NA),
    lag_by_3_inf  = lag(inflation_min_wage_l, n=3,  default=NA),
    lag_by_4_inf  = lag(inflation_min_wage_l, n=4,  default=NA),
    lag_by_5_inf  = lag(inflation_min_wage_l, n=5,  default=NA),
    lag_by_6_inf  = lag(inflation_min_wage_l, n=6,  default=NA),
    lag_by_7_inf  = lag(inflation_min_wage_l, n=7,  default=NA),
    lag_by_8_inf  = lag(inflation_min_wage_l, n=8,  default=NA),
    lag_by_9_inf  = lag(inflation_min_wage_l, n=9,  default=NA),
    lag_by_10_inf = lag(inflation_min_wage_l, n=10, default=NA),
    lag_by_11_inf = lag(inflation_min_wage_l, n=11, default=NA),
    lag_by_12_inf = lag(inflation_min_wage_l, n=12, default=NA),
    lag_by_13_inf = lag(inflation_min_wage_l, n=13, default=NA),
    lag_by_14_inf = lag(inflation_min_wage_l, n=14, default=NA),
    lag_by_15_inf = lag(inflation_min_wage_l, n=15, default=NA),
    lag_by_16_inf = lag(inflation_min_wage_l, n=16, default=NA),
    lag_by_17_inf = lag(inflation_min_wage_l, n=17, default=NA)
  )

# Merge minimum wage and NSCH data
nsch_all <- left_join(nsch, min_wage_long, by=c("fipsst", "year"))

# Clear old datasets from R
rm(nsch_2016_top, nsch_2017_top, nsch_2018_top, nsch_2019_top,
   nsch_2020_top, nsch_2021_top, nsch_2022_top)

# Read Medicaid datasets into R
medicaid_1_5_df  <- read.csv("dataverse_files-7/Supplemental data/Medicaid eligibility, ages 1-5, state-year, raw.csv")
medicaid_6_18_df <- read.csv("dataverse_files-7/Supplemental data/Medicaid eligibility, ages 6-18, state-year, raw.csv")

# Add columns for 2001 and 2007
# Duplicate data from 2000 and 2006
medicaid_1_5_df$X2001  <- medicaid_1_5_df$X2000
medicaid_1_5_df$X2007  <- medicaid_1_5_df$X2006
medicaid_6_18_df$X2001 <- medicaid_6_18_df$X2000
medicaid_6_18_df$X2007 <- medicaid_6_18_df$X2006

# Restructure data to long format
library(reshape2)
medicaid_1_5_df  <- melt(medicaid_1_5_df, id.vars = c("X"),
                         variable.name = "year", value.name = "elig_1_5")
medicaid_6_18_df <- melt(medicaid_6_18_df, id.vars = c("X"),
                         variable.name = "year", value.name = "elig_6_18")

# Clean year variable
# Drop leading "X" in strings
medicaid_1_5_df$year  <- substring(medicaid_1_5_df$year,  2)
medicaid_6_18_df$year <- substring(medicaid_6_18_df$year, 2)

# Rename columns
colnames(medicaid_1_5_df)  <- c("state", "year", "elig_1_5")
colnames(medicaid_6_18_df) <- c("state", "year", "elig_6_18")

# Convert names to FIPS
# Necessary for smooth merge
medicaid_1_5_df$fipsst  <- cdlTools::fips(medicaid_1_5_df$state,  to="FIPS")
medicaid_6_18_df$fipsst <- cdlTools::fips(medicaid_6_18_df$state, to="FIPS")

# Treat year as numeric
medicaid_1_5_df$year  <- as.numeric(medicaid_1_5_df$year)
medicaid_6_18_df$year <- as.numeric(medicaid_6_18_df$year)

# Merge Medicaid and YRBS data
nsch_all <- left_join(nsch_all, medicaid_1_5_df,  by=c("fipsst", "year"))
nsch_all <- left_join(nsch_all, medicaid_6_18_df, by=c("fipsst", "year"))

# Read EITC dataset into R
eitc <- read.csv("dataverse_files-7/Supplemental data/EITC policies, state-year, clean.csv")

# Merge EITC and YRBS data
nsch_all <- left_join(nsch_all, eitc, by=c("fipsst", "year"))

# Read TANF dataset into R
tanf <- read.csv("dataverse_files-7/Supplemental data/TANF benefits family 3, state-year, clean.csv")

# Merge TANF and YRBS data
nsch_all <- left_join(nsch_all, tanf, by=c("fipsst", "year"))

# Treat fixed effects as factors
nsch_all$year   <- as.factor(nsch_all$year)
nsch_all$fipsst <- as.factor(nsch_all$fipsst)

# Child's age
nsch_all <- nsch_all %>% mutate(
  age = case_when(
    sc_age_years %in% c(0:17) ~ sc_age_years
  ))

# Treat age as factor
nsch_all$age <- as.factor(nsch_all$age)

# Birth year
# Note: For 2019-2022, there is a reported birth year variable
# However, the data quality is questionable for many respondents
# As such, we use the survey year and child's reported age
nsch_all <- nsch_all %>% mutate(
  birth_year = case_when(
    # Subtract age from survey year
    TRUE ~ as.numeric(as.character(year)) - sc_age_years
  ))

# Dichotomize age for interaction models
nsch_all <- nsch_all %>% mutate(
  age_cat = case_when(
    sc_age_years %in% c(13:17) ~ 0, # Adolescents
    sc_age_years %in% c(0:12)  ~ 1  # All other children
  ))

# Child's sex
nsch_all <- nsch_all %>% mutate(
  sex = case_when(
    sc_sex == 1 ~ "Male",
    sc_sex == 2 ~ "Female"
  ))
nsch_all$sex <- factor(nsch_all$sex, levels = c("Male", "Female"))

# Child's race/ethnicity
nsch_all <- nsch_all %>% mutate(
  race_eth = case_when(
    sc_hispanic_r == 1    ~ "Hispanic/Latino",
    sc_race_r == 1        ~ "White",
    sc_race_r == 2        ~ "Black or African American",
    sc_race_r == 3        ~ "American Indian or Alaska Native",
    sc_race_r %in% c(4:5) ~ "Asian, Native Hawaiian, or Pacific Islander",
    sc_race_r %in% c(6:7) ~ "Other or two or more races",
  ))
nsch_all$race_eth <- factor(nsch_all$race_eth, levels = c("American Indian or Alaska Native", "Asian, Native Hawaiian, or Pacific Islander", "Black or African American", "Hispanic/Latino", "White", "Other or two or more races"))

# Dichotomoize race/ethnicity
# Black/Latino vs. other for interaction models
nsch_all <- nsch_all %>% mutate(
  race_eth_cat = case_when(
    sc_hispanic_r == 1      ~ 0, # Black or Hispanic/Latino
    sc_race_r == 2          ~ 0, # Black or Hispanic/Latino
    sc_race_r %in% c(1,3:7) ~ 1  # All other races
  ))

# Adults' highest educational attainment
nsch_all <- nsch_all %>% mutate(
  adult_edu = case_when(
    higrade_tvis == 1 ~ "Less than high school",
    higrade_tvis == 2 ~ "High school (including vocational)",
    higrade_tvis == 3 ~ "Some college or associate degree",
    higrade_tvis == 4 ~ "College degree or higher",
    TRUE ~ "Not provided"
  ))
nsch_all$adult_edu <- factor(nsch_all$adult_edu, levels = c("Less than high school", "High school (including vocational)", "Some college or associate degree", "College degree or higher", "Not provided"))

# Dichotomize educational attainment
# High school (or less) vs. some college (or more)
nsch_all <- nsch_all %>% mutate(
  adult_edu_cat = case_when(
    higrade_tvis %in% c(1:2) ~ 0, # High school or less
    higrade_tvis %in% c(3:4) ~ 1  # All other education levels
  ))

# Generate mean estimated FPL
# Later NSCH years generated 6 imputed FPLs if a household was missing income
nsch_all$fpl_mean <- rowMeans(cbind(nsch_all$fpl_i1, nsch_all$fpl_i2, nsch_all$fpl_i3,
                                    nsch_all$fpl_i4, nsch_all$fpl_i5, nsch_all$fpl_i6), na.rm=T)

# Household's federal poverty level
nsch_all <- nsch_all %>% mutate(
  fpl_category = case_when(
    fpl %in% c(50:99)   | fpl_mean < 100 ~ "Less than 100%",
    fpl %in% c(100:199) | fpl_mean < 200 ~ "100% to 199%",
    fpl %in% c(200:299) | fpl_mean < 300 ~ "200% to 299%",
    fpl %in% c(300:399) | fpl_mean < 400 ~ "300% to 399%",
    fpl %in% c(400:999) | fpl_mean < 999 ~ "400% or greater"
  ))
nsch_all$fpl_category <- factor(nsch_all$fpl_category, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))

# Dichotomize FPL
# Low-income (<200% FPL) vs. higher-income
nsch_all <- nsch_all %>% mutate(
  low_income = case_when(
    fpl %in% c(50:199)  | fpl_mean < 200 ~ 0, # Lower income
    fpl %in% c(200:999) | fpl_mean < 999 ~ 1  # Higher income
  ))

# Family structure
nsch_all <- nsch_all %>% mutate(
  family_struc = case_when(
    family %in% c(1,3)  | family_r %in% c(1,3)  ~ "Two parents, married",
    family %in% c(2,4)  | family_r %in% c(2,4)  ~ "Two parents, not married",
    family %in% c(5:8)  | family_r %in% c(5:6)  ~ "Single parent",
    family %in% c(1:99) | family_r %in% c(1:99) ~ "Another family structure",
    TRUE ~ "Not provided"
  ))
nsch_all$family_struc <- factor(nsch_all$family_struc, levels = c("Two parents, married", "Two parents, not married", "Single parent", "Another family structure", "Not provided"))

# Household nativity
nsch_all <- nsch_all %>% mutate(
  nativity = case_when(
    house_gen == 1 ~ "First-generation household",
    house_gen == 2 ~ "Second-generation household",
    house_gen == 3 ~ "Third-generation household or higher",
    TRUE ~ "Not provided"
  ))
nsch_all$nativity <- factor(nsch_all$nativity, levels = c("First-generation household", "Second-generation household", "Third-generation household or higher", "Not provided"))

# Dichotomize household nativity
# First/second-generation vs. higher
nsch_all <- nsch_all %>% mutate(
  nativity_cat = case_when(
    house_gen %in% c(1:2) ~ 0,
    house_gen %in% c(3)   ~ 1
  ))

# Current depression
nsch_all <- nsch_all %>% mutate(
  depression = case_when(
    k2q32b %in% c(1)                      ~ 1,
    k2q32b %in% c(2) | k2q32a %in% c(1:2) ~ 0
  ))

# Current moderate or severe depression
nsch_all <- nsch_all %>% mutate(
  dep_mod_sev = case_when(
    k2q32c %in% c(2:3)                                         ~ 1,
    k2q32c %in% c(1) | k2q32b %in% c(1:2) | k2q32a %in% c(1:2) ~ 0
  ))

# Current diagnosed anxiety
nsch_all <- nsch_all %>% mutate(
  anxiety = case_when(
    k2q33b %in% c(1)                      ~ 1,
    k2q33b %in% c(2) | k2q33a %in% c(1:2) ~ 0
  ))

# Current moderate or severe anxiety
nsch_all <- nsch_all %>% mutate(
  anx_mod_sev = case_when(
    k2q33c %in% c(2:3)                                         ~ 1,
    k2q33c %in% c(1) | k2q33b %in% c(1:2) | k2q33a %in% c(1:2) ~ 0
  ))

# Current ADD/ADHD
nsch_all <- nsch_all %>% mutate(
  adhd = case_when(
    k2q31b %in% c(1)                      ~ 1,
    k2q31b %in% c(2) | k2q31a %in% c(1:2) ~ 0
  ))

# Current moderate or severe ADD/ADHD
nsch_all <- nsch_all %>% mutate(
  adhd_mod_sev = case_when(
    k2q31c %in% c(2:3)                                         ~ 1,
    k2q31c %in% c(1) | k2q31b %in% c(1:2) | k2q31a %in% c(1:2) ~ 0
  ))

# Current behavior problems
nsch_all <- nsch_all %>% mutate(
  behavior = case_when(
    k2q34b %in% c(1)                      ~ 1,
    k2q34b %in% c(2) | k2q34a %in% c(1:2) ~ 0
  ))

# Current moderate or severe behavior problems
nsch_all <- nsch_all %>% mutate(
  beh_mod_sev = case_when(
    k2q34c %in% c(2:3)                                         ~ 1,
    k2q34c %in% c(1) | k2q34b %in% c(1:2) | k2q34a %in% c(1:2) ~ 0
  ))

# Stomach/digestive issues
nsch_all <- nsch_all %>% mutate(
  stomach_r = case_when(
    stomach %in% c(1) ~ 1,
    stomach %in% c(2) ~ 0
  ))

# Unmet health care (any)
nsch_all <- nsch_all %>% mutate(
  unmet_needs = case_when(
    k4q27 %in% c(1) ~ 1,
    k4q27 %in% c(2) ~ 0,
  ))

# Unmet mental health care
nsch_all <- nsch_all %>% mutate(
  unmet_mental = case_when(
    k4q28x04 %in% c(1)   ~ 1,
    k4q27    %in% c(1:2) ~ 0
  ))

# Missed days of school
# Dichotomize is as 0-6 vs 7+ days
nsch_all <- nsch_all %>% mutate(
  missed_school = case_when(
    k7q02r_r %in% c(4:5)   ~ 1,
    k7q02r_r %in% c(1:3,6) ~ 0
  ))

# Child's employment
nsch_all <- nsch_all %>% mutate(
  child_job = case_when(
    k7q38 == 1 ~ 1,
    k7q38 == 2 ~ 0
  ))

# Lifetime minimum wage
# With and without inflation adjustments
nsch_all <- nsch_all %>% mutate(
  wage_life_nom = case_when(
    sc_age_years == 0 ~ effective_min_wage_l,
    sc_age_years == 1 ~ (effective_min_wage_l + lag_by_1)/2,
    sc_age_years == 2 ~ (effective_min_wage_l + lag_by_1 + lag_by_2)/3,
    sc_age_years == 3 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3)/4,
    sc_age_years == 4 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                           lag_by_4)/5,
    sc_age_years == 5 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                           lag_by_4 + lag_by_5)/6,
    sc_age_years == 6 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                           lag_by_4 + lag_by_5 + lag_by_6)/7,
    sc_age_years == 7 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                           lag_by_4 + lag_by_5 + lag_by_6 + lag_by_7)/8,
    sc_age_years == 8 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                           lag_by_4 + lag_by_5 + lag_by_6 + lag_by_7 +
                           lag_by_8)/9,
    sc_age_years == 9 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                           lag_by_4 + lag_by_5 + lag_by_6 + lag_by_7 +
                           lag_by_8 + lag_by_9)/10,
    sc_age_years == 10 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4 + lag_by_5 + lag_by_6 + lag_by_7 +
                            lag_by_8 + lag_by_9 + lag_by_10)/11,
    sc_age_years == 11 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4 + lag_by_5 + lag_by_6  + lag_by_7 +
                            lag_by_8 + lag_by_9 + lag_by_10 + lag_by_11)/12,
    sc_age_years == 12 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4 + lag_by_5 + lag_by_6  + lag_by_7 +
                            lag_by_8 + lag_by_9 + lag_by_10 + lag_by_11 +
                            lag_by_12)/13,
    sc_age_years == 13 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4  + lag_by_5 + lag_by_6  + lag_by_7 +
                            lag_by_8  + lag_by_9 + lag_by_10 + lag_by_11 +
                            lag_by_12 + lag_by_13)/14,
    sc_age_years == 14 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 + 
                            lag_by_4  + lag_by_5  + lag_by_6  + lag_by_7 +
                            lag_by_8  + lag_by_9  + lag_by_10 + lag_by_11 +
                            lag_by_12 + lag_by_13 + lag_by_14)/15,
    sc_age_years == 15 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4  + lag_by_5  + lag_by_6  + lag_by_7 +
                            lag_by_8  + lag_by_9  + lag_by_10 + lag_by_11 +
                            lag_by_12 + lag_by_13 + lag_by_14 + lag_by_15)/16,
    sc_age_years == 16 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4  + lag_by_5 +  lag_by_6 +  lag_by_7 +
                            lag_by_8  + lag_by_9  + lag_by_10 + lag_by_11 +
                            lag_by_12 + lag_by_13 + lag_by_14 + lag_by_15 +
                            lag_by_16)/17,
    sc_age_years == 17 ~ (effective_min_wage_l + lag_by_1 + lag_by_2 + lag_by_3 +
                            lag_by_4  + lag_by_5  + lag_by_6  + lag_by_7 +
                            lag_by_8  + lag_by_9  + lag_by_10 + lag_by_11 +
                            lag_by_12 + lag_by_13 + lag_by_14 + lag_by_15 +
                            lag_by_16 + lag_by_17)/18
  ))
nsch_all <- nsch_all %>% mutate(
  wage_life_inf = case_when(
    sc_age_years == 0 ~ inflation_min_wage_l,
    sc_age_years == 1 ~ (inflation_min_wage_l + lag_by_1_inf)/2,
    sc_age_years == 2 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf)/3,
    sc_age_years == 3 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf)/4,
    sc_age_years == 4 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                           lag_by_4_inf)/5,
    sc_age_years == 5 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                           lag_by_4_inf + lag_by_5_inf)/6,
    sc_age_years == 6 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                           lag_by_4_inf + lag_by_5_inf + lag_by_6_inf)/7,
    sc_age_years == 7 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                           lag_by_4_inf + lag_by_5_inf + lag_by_6_inf + lag_by_7_inf)/8,
    sc_age_years == 8 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                           lag_by_4_inf + lag_by_5_inf + lag_by_6_inf + lag_by_7_inf +
                           lag_by_8_inf)/9,
    sc_age_years == 9 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                           lag_by_4_inf + lag_by_5_inf + lag_by_6_inf + lag_by_7_inf +
                           lag_by_8_inf + lag_by_9_inf)/10,
    sc_age_years == 10 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf + lag_by_5_inf + lag_by_6_inf + lag_by_7_inf +
                            lag_by_8_inf + lag_by_9_inf + lag_by_10_inf)/11,
    sc_age_years == 11 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf + lag_by_5_inf + lag_by_6_inf  + lag_by_7_inf +
                            lag_by_8_inf + lag_by_9_inf + lag_by_10_inf + lag_by_11_inf)/12,
    sc_age_years == 12 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf + lag_by_5_inf + lag_by_6_inf  + lag_by_7_inf +
                            lag_by_8_inf + lag_by_9_inf + lag_by_10_inf + lag_by_11_inf +
                            lag_by_12_inf)/13,
    sc_age_years == 13 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf  + lag_by_5_inf + lag_by_6_inf  + lag_by_7_inf +
                            lag_by_8_inf  + lag_by_9_inf + lag_by_10_inf + lag_by_11_inf +
                            lag_by_12_inf + lag_by_13_inf)/14,
    sc_age_years == 14 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf + 
                            lag_by_4_inf  + lag_by_5_inf  + lag_by_6_inf  + lag_by_7_inf +
                            lag_by_8_inf  + lag_by_9_inf  + lag_by_10_inf + lag_by_11_inf +
                            lag_by_12_inf + lag_by_13_inf + lag_by_14_inf)/15,
    sc_age_years == 15 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf  + lag_by_5_inf  + lag_by_6_inf  + lag_by_7_inf +
                            lag_by_8_inf  + lag_by_9_inf  + lag_by_10_inf + lag_by_11_inf +
                            lag_by_12_inf + lag_by_13_inf + lag_by_14_inf + lag_by_15_inf)/16,
    sc_age_years == 16 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf  + lag_by_5_inf +  lag_by_6_inf +  lag_by_7_inf +
                            lag_by_8_inf  + lag_by_9_inf  + lag_by_10_inf + lag_by_11_inf +
                            lag_by_12_inf + lag_by_13_inf + lag_by_14_inf + lag_by_15_inf +
                            lag_by_16_inf)/17,
    sc_age_years == 17 ~ (inflation_min_wage_l + lag_by_1_inf + lag_by_2_inf + lag_by_3_inf +
                            lag_by_4_inf  + lag_by_5_inf  + lag_by_6_inf  + lag_by_7_inf +
                            lag_by_8_inf  + lag_by_9_inf  + lag_by_10_inf + lag_by_11_inf +
                            lag_by_12_inf + lag_by_13_inf + lag_by_14_inf + lag_by_15_inf +
                            lag_by_16_inf + lag_by_17_inf)/18
  ))

# State has EITC program
nsch_all <- nsch_all %>% mutate(
  has_eitc = case_when(
    federal_pct > 0 ~ 1,
    TRUE ~ 0
  ))

# Generate nested sampling clusters
nsch_all$cluster <- paste0(nsch_all$strata, "-", nsch_all$fipsst)

# Code inclusion/exclusion criteria
# That is, must have age and at least once outcome
nsch_all <- nsch_all %>% mutate(
  included_in_study = case_when(
    !is.na(age) & (!is.na(depression) | !is.na(anxiety) | !is.na(adhd) |
                     !is.na(behavior) | !is.na(stomach_r) | !is.na(unmet_needs) |
                     !is.na(unmet_mental)) ~ 1,
    TRUE ~ 0
  ))

# Assess missingness due to inclusion/exclusion criteria
prop.table(table(nsch_all$included_in_study, useNA="always"))

# Complete cases for models
nsch_all_model <- nsch_all %>%
  
  # Exclude children less than 3
  filter(age %in% c(3:17)) %>%
  
  # Exclude children missing age or at least one outcome
  # Note: All other covariates have no missingness due to "Not provided" options
  filter_at(vars(effective_min_wage_l, included_in_study), all_vars(!is.na(.)))

# Rescale weight so mean is 1
nsch_all_model$weights <- nsch_all_model$fwc/mean(nsch_all_model$fwc, na.rm=T)

# Child characteristics: unweighted
summary(tableby(~ as.numeric(as.character(age)) + sex + race_eth + family_struc + adult_edu + nativity,
                nsch_all_model, digits.pct=1), text=T)

# Child characteristics: weighted
summary(tableby(~ as.numeric(as.character(age)) + sex + race_eth + family_struc + adult_edu + nativity,
                nsch_all_model, digits.pct=1, weights=weights), text=T)

# Mental health outcomes: weighted
summary(tableby(~ factor(depression), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(anxiety), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(adhd), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(behavior), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(stomach_r), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(unmet_needs), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(unmet_mental), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)
summary(tableby(~ factor(missed_school), nsch_all_model, digits.pct=1,
                weights=weights, na.action=na.tableby(TRUE)), text=T)

# Get missingness by outcome
summary(tableby(~ is.na(depression) + is.na(anxiety) + is.na(adhd) + is.na(behavior) +
                   is.na(unmet_mental),
                nsch_all_model, digits.pct=1), text=T)

# Get range of observed minimum wage changes
wage_descriptives <- min_wage_long %>%
  filter(year %in% c(2016:2022), fipsst %in% unique(nsch_all_model$fipsst)) %>%
  group_by(fipsst) %>%
  summarise(min_wage = min(effective_min_wage_l, na.rm=T),
            max_wage = max(effective_min_wage_l, na.rm=T),
            change   = max_wage - min_wage)

# Range of minimum wages
min(wage_descriptives$min_wage); max(wage_descriptives$max_wage)

# Number of states that never changed
table(wage_descriptives$change > 0)

# Range of changes among states that changed
summary(subset(wage_descriptives, change > 0)$change)

######### ANALYSIS CODE #########
state_map <- usmap::us_map(regions="states")
states_sf <- st_as_sf(state_map, coords=c("x","y"), crs = 4326)
# Merge spatial data with survey data
spatial_data <- states_sf %>% left_join(nsch_all_model, by = c("full" = "state.x"))

# create spatial neighborhood matrix
spatial_data$fips <- as.factor(as.numeric((spatial_data$fips)))
coords <- st_coordinates(st_centroid(spatial_data))
coords_unique <- unique(coords)
# create k-nearest neighbors
knn <- knearneigh(coords, k = 5)
nb_knn <- knn2nb(knn)
nb_knn <- make.sym.nb(nb_knn)
lw <- nb2listw(nb_knn, style = "W", zero.policy = TRUE)

adjacency <- matrix(0, nrow = length(levels(spatial_data$fips)),
                    ncol = length(levels(spatial_data$fips)))
for (i in 1:length(nb_knn)) {
  adjacency[i, nb_knn[[i]]] <- 1
}
rownames(adjacency) <- levels(spatial_data$fips)
colnames(adjacency) <- levels(spatial_data$fips)

spatial_data$fips <- as.numeric(spatial_data$fips)
# define CAR model in INLA
spatial_data$age.num <- as.numeric(spatial_data$age)
spatial_data$age.cat <- ifelse(spatial_data$age.num<13, '3-12','13-17')
dep.form <- depression ~ effective_min_wage_l + year + age.cat + sex + fpl_mean +
  f(fips, model="besag", graph = adjacency)
dep.mod <- inla(
  dep.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(dep.mod)

dep.int.form <- depression ~ effective_min_wage_l + year + age.cat + sex + fpl_mean + 
  age.cat*effective_min_wage_l + sex*effective_min_wage_l +
  f(fips, model="besag", graph = adjacency)
dep.int.mod <- inla(
  dep.int.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(dep.int.mod)

# create tables for model output
library(gt)
library(flextable)
library(dplyr)
library(broom)
library(tibble)

variable_labels <- c(
  "(Intercept)" = "Intercept",
  "effective_min_wage_l" = "Minimum Wage",
  "year2017" = "Year 2017",
  "year2018" = "Year 2018",
  "year2019" = "Year 2019",
  "year2020" = "Year 2020",
  "year2021" = "Year 2021",
  "year2022" = "Year 2022",
  "age.cat3-12" = "Age 3-12 (vs. 13-17)",
  "sexFemale" = "Sex: Female (vs. Male)",
  "fpl_mean" = "Mean Federal Poverty Level",
  "effective_min_wage_l:age.cat3-12" = "Min Wage × Age 3-12",
  "effective_min_wage_l:sexFemale" = "Min Wage × Female"
)
extract_inla_results <- function(model, labels) {
  summary_df <- as.data.frame(model$summary.fixed) %>%
    rownames_to_column(var = "Variable") %>%
    rename(
      Estimate = mean,
      Std_Error = sd,
      Lower_95_CI = "0.025quant",
      Upper_95_CI = "0.975quant"
    ) %>%
    select(Variable, Estimate, Std_Error, Lower_95_CI, Upper_95_CI)  %>%
    mutate(Variable = labels[Variable])
  
  return(summary_df)
}
create_gt_table <- function(data, title) {
  gt(data) %>%
    tab_header(
      title = md(title)
    ) %>%
    fmt_number(
      columns = vars(Estimate, Std_Error, Lower_95_CI, Upper_95_CI),
      decimals = 3
    ) %>%
    cols_label(
      Variable = "Variable",
      Estimate = "Estimate",
      Std_Error = "Std. Error",
      Lower_95_CI = "Lower 95% CI",
      Upper_95_CI = "Upper 95% CI"
    ) %>%
    tab_options(
      table.font.size = px(12),
      heading.title.font.size = px(14),
      heading.title.font.weight = "bold"
    )
}
create_flextable <- function(data, title) {
  flextable(data) %>%
    set_header_labels(
      Variable = "Variable",
      Estimate = "Estimate",
      Std_Error = "Std. Error",
      Lower_95_CI = "Lower 95% CI",
      Upper_95_CI = "Upper 95% CI"
    ) %>%
    theme_vanilla() %>%
    autofit() %>%
    add_header_lines(values = title)
}
dep_results <- extract_inla_results(dep.mod, variable_labels)
dep_int_results <- extract_inla_results(dep.int.mod, variable_labels)
dep_mod_table <- create_gt_table(dep_results, "Depression Model Results (No Interaction)")
dep_int_mod_table <- create_gt_table(dep_int_results, "Depression Model Results (With Interaction)")
dep_mod_table
dep_int_mod_table

anx_results <- extract_inla_results(anx.mod, variable_labels)
anx_int_results <- extract_inla_results(anx.int.mod, variable_labels)
anx_mod_table <- create_gt_table(anx_results, "Anxiety Model Results (No Interaction)")
anx_int_mod_table <- create_gt_table(anx_int_results, "Anxiety Model Results (With Interaction)")
anx_mod_table
anx_int_mod_table

adhd_results <- extract_inla_results(adhd.mod, variable_labels)
adhd_int_results <- extract_inla_results(adhd.int.mod, variable_labels)
adhd_mod_table <- create_gt_table(adhd_results, "ADD/ADHD Model Results (No Interaction)")
adhd_int_mod_table <- create_gt_table(adhd_int_results, "ADD/ADHD Model Results (With Interaction)")
adhd_mod_table
adhd_int_mod_table

behav_results <- extract_inla_results(behav.mod, variable_labels)
behav_int_results <- extract_inla_results(behav.int.mod, variable_labels)
behav_mod_table <- create_gt_table(behav_results, "Behavioral Problems Model Results (No Interaction)")
behav_int_mod_table <- create_gt_table(behav_int_results, "Behavioral Problems Model Results (With Interaction)")
behav_mod_table
behav_int_mod_table

unmet_results <- extract_inla_results(unmet.mod, variable_labels)
unmet_int_results <- extract_inla_results(unmet.int.mod, variable_labels)
unmet_mod_table <- create_gt_table(unmet_results, "Unmet Mental Needs Model Results (No Interaction)")
unmet_int_mod_table <- create_gt_table(unmet_int_results, "Unmet Mental Needs Model Results (With Interaction)")
unmet_mod_table
unmet_int_mod_table

# create interaction plots
interaction_effects <- data.frame(
  Outcome = rep(c("Depression", "Anxiety", "ADHD", "Behavioral Problems", "Unmet Mental Health Needs"), each = 2),
  Subgroup = c("Age 3-12", "Sex: Female", "Age 3-12", "Sex: Female",
               "Age 3-12", "Sex: Female", "Age 3-12", "Sex: Female",
               "Age 3-12", "Sex: Female"),
  Effect = c(
    dep.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "mean"],
    dep.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "mean"],
    anx.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "mean"],
    anx.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "mean"],
    adhd.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "mean"],
    adhd.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "mean"],
    behav.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "mean"],
    behav.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "mean"],
    unmet.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "mean"],
    unmet.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "mean"]
  ),
  Lower_CI = c(
    dep.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.025quant"],
    dep.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.025quant"],
    anx.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.025quant"],
    anx.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.025quant"],
    adhd.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.025quant"],
    adhd.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.025quant"],
    behav.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.025quant"],
    behav.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.025quant"],
    unmet.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.025quant"],
    unmet.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.025quant"]
  ),
  Upper_CI = c(
    dep.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.975quant"],
    dep.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.975quant"],
    anx.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.975quant"],
    anx.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.975quant"],
    adhd.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.975quant"],
    adhd.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.975quant"],
    behav.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.975quant"],
    behav.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.975quant"],
    unmet.int.mod$summary.fixed["effective_min_wage_l:age.cat3-12", "0.975quant"],
    unmet.int.mod$summary.fixed["effective_min_wage_l:sexFemale", "0.975quant"]
  )
)

ggplot(interaction_effects, aes(x = Outcome, y = Effect, color = Subgroup, group = Subgroup)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Interaction Effects: Minimum Wage × Age/Sex on Mental Health Outcomes",
       x = "Mental Health Outcome", y = "Effect of Minimum Wage",
       color = "Subgroup") +
  theme_minimal()

# small area estimation
spatial_data$predicted_outcome <- dep.mod$summary.fitted.values$mean

dep_pred_map <- spatial_data %>%
  group_by(fipsst) %>%
  summarise(pred_dep = mean(predicted_outcome, na.rm=TRUE))
ggplot(dep_pred_map) +
  geom_sf(aes(fill = pred_dep), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Predicted Prevalence of Depression by State",
    fill = "Rate"
  )

# anxiety
anx.form <- anxiety ~ effective_min_wage_l + year + age.cat + sex + fpl_mean +
  f(fips, model="besag", graph = adjacency)
anx.mod <- inla(
  anx.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(anx.mod)
predictions <- inla.posterior.sample(n=100, result = anx.mod)
predicted_values <- apply(predictions[[1]]$latent, 1, mean)
spatial_data$predicted_outcome <- anx.mod$summary.fitted.values$mean

anx_pred_map <- spatial_data %>%
  group_by(fipsst) %>%
  summarise(pred_anx = mean(predicted_outcome, na.rm=TRUE))
ggplot(anx_pred_map) +
  geom_sf(aes(fill = pred_anx), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Predicted Prevalence of Anxiety by State",
    fill = "Rate"
  )

anx.int.form <- anxiety ~ effective_min_wage_l + year + age.cat + sex + fpl_mean + 
  age.cat*effective_min_wage_l + sex*effective_min_wage_l +
  f(fips, model="besag", graph = adjacency)
anx.int.mod <- inla(
  anx.int.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(anx.int.mod)

# adhd
adhd.form <- adhd ~ effective_min_wage_l + year + age.cat + sex + fpl_mean +
  f(fips, model="besag", graph = adjacency)
adhd.mod <- inla(
  adhd.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(adhd.mod)

adhd.int.form <- adhd ~ effective_min_wage_l + year + age.cat + sex + fpl_mean + 
  age.cat*effective_min_wage_l + sex*effective_min_wage_l +
  f(fips, model="besag", graph = adjacency)
adhd.int.mod <- inla(
  adhd.int.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(adhd.int.mod)

# behavior
behav.form <- behavior ~ effective_min_wage_l + year + age.cat + sex + fpl_mean +
  f(fips, model="besag", graph = adjacency)
behav.mod <- inla(
  behav.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(behav.mod)

behav.int.form <- behavior ~ effective_min_wage_l + year + age.cat + sex + fpl_mean + 
  age.cat*effective_min_wage_l + sex*effective_min_wage_l +
  f(fips, model="besag", graph = adjacency)
behav.int.mod <- inla(
  behav.int.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(behav.int.mod)

# unmet mental needs
unmet.form <- unmet_mental ~ effective_min_wage_l + year + age.cat + sex + fpl_mean +
  f(fips, model="besag", graph = adjacency)
unmet.mod <- inla(
  unmet.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(unmet.mod)

unmet.int.form <- unmet_mental ~ effective_min_wage_l + year + age.cat + sex + fpl_mean + 
  age.cat*effective_min_wage_l + sex*effective_min_wage_l +
  f(fips, model="besag", graph = adjacency)
unmet.int.mod <- inla(
  unmet.int.form,
  data = as.data.frame(spatial_data),
  family = "gaussian",
  weights = spatial_data$weights,
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.predictor = list(compute =TRUE), verbose = TRUE
)
summary(unmet.int.mod)

# population at risk stratified by age and gender
population_risk <- spatial_data %>%
  group_by(State.or.other.jurisdiction, age, sex) %>%
  summarise(count=n(), .groups = "drop")

## mental health issues by state
anxiety_map <- spatial_data %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.anx = weighted.mean(anxiety, weights, na.rm=TRUE))
ggplot(anxiety_map) +
  geom_sf(aes(fill = wtd.mean.anx), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Anxiety by State",
    fill = "Weighted Mean"
  )
anxiety.sex.map <- spatial_data %>% filter(sex=="Female") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.anx = weighted.mean(anxiety, weights, na.rm=TRUE))
ggplot(anxiety.sex.map) +
  geom_sf(aes(fill = wtd.mean.anx), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Anxiety in Females by State",
    fill = "Weighted Mean"
  )
anxiety.male.map <- spatial_data %>% filter(sex=="Male") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.anx = weighted.mean(anxiety, weights, na.rm=TRUE))
ggplot(anxiety.male.map) +
  geom_sf(aes(fill = wtd.mean.anx), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Anxiety in Males by State",
    fill = "Weighted Mean"
  )
depression_map <- spatial_data %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.dep = weighted.mean(depression, weights, na.rm=TRUE))
ggplot(depression_map) +
  geom_sf(aes(fill = wtd.mean.dep), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Depression by State",
    fill = "Weighted Mean"
  )
dep.fem.map <- spatial_data %>% filter(sex=="Female") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.dep = weighted.mean(depression, weights, na.rm=TRUE))
ggplot(dep.fem.map) +
  geom_sf(aes(fill = wtd.mean.dep), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Depression in Females by State",
    fill = "Weighted Mean"
  )
dep.male.map <- spatial_data %>% filter(sex=="Male") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.dep = weighted.mean(depression, weights, na.rm=TRUE))
ggplot(dep.male.map) +
  geom_sf(aes(fill = wtd.mean.dep), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Depression in Males by State",
    fill = "Weighted Mean"
  )

adhd_map <- spatial_data %>% 
  group_by(fipsst) %>%
  summarise(wtd.mean.adhd = weighted.mean(adhd, weights, na.rm=TRUE))
ggplot(adhd_map) +
  geom_sf(aes(fill = wtd.mean.adhd), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of ADHD by State",
    fill = "Weighted Mean"
  )
adhd.fem.map <- spatial_data %>% filter(sex=="Female") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.adhd = weighted.mean(adhd, weights, na.rm=TRUE))
ggplot(adhd.fem.map) +
  geom_sf(aes(fill = wtd.mean.adhd), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of ADHD in Females by State",
    fill = "Weighted Mean"
  )
adhd.male.map <- spatial_data %>% filter(sex=="Male") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.adhd = weighted.mean(adhd, weights, na.rm=TRUE))
ggplot(adhd.male.map) +
  geom_sf(aes(fill = wtd.mean.adhd), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of ADHD in Males by State",
    fill = "Weighted Mean"
  )

behavior_map <- spatial_data %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.behav = weighted.mean(behavior, weights, na.rm=TRUE))
ggplot(behavior_map) +
  geom_sf(aes(fill = wtd.mean.behav), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Behavioral Issues by State",
    fill = "Weighted Mean"
  )
behav.fem.map <- spatial_data %>% filter(sex=="Female") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.behav = weighted.mean(behavior, weights, na.rm=TRUE))
ggplot(behav.fem.map) +
  geom_sf(aes(fill = wtd.mean.behav), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Behavioral Issues in Females by State",
    fill = "Weighted Mean"
  )
behav.male.map <- spatial_data %>% filter(sex=="Male") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.behav = weighted.mean(behavior, weights, na.rm=TRUE))
ggplot(behav.male.map) +
  geom_sf(aes(fill = wtd.mean.behav), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Behavioral Issues in Males by State",
    fill = "Weighted Mean"
  )

unmet_map <- spatial_data %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.unmet = weighted.mean(unmet_mental, weights, na.rm=TRUE))
ggplot(unmet_map) +
  geom_sf(aes(fill = wtd.mean.unmet), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Unmet Mental Needs by State",
    fill = "Weighted Mean"
  )
unmet.fem.map <- spatial_data %>% filter(sex=="Female") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.unmet = weighted.mean(unmet_mental, weights, na.rm=TRUE))
ggplot(unmet.fem.map) +
  geom_sf(aes(fill = wtd.mean.unmet), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Unmet Mental Needs in Females by State",
    fill = "Weighted Mean"
  )
unmet.male.map <- spatial_data %>% filter(sex=="Male") %>%
  group_by(fipsst) %>%
  summarise(wtd.mean.unmet = weighted.mean(unmet_mental, weights, na.rm=TRUE))
ggplot(unmet.male.map) +
  geom_sf(aes(fill = wtd.mean.unmet), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Weighted Prevalence of Unmet Mental Needs in Males by State",
    fill = "Weighted Mean"
  )

wage_map <- spatial_data %>%
  group_by(fips) %>%
  summarise(mean.min.wage = mean(effective_min_wage_l, na.rm=TRUE))
ggplot(wage_map) +
  geom_sf(aes(fill = mean.min.wage), color="white") +
  scale_fill_viridis_c(option="C", direction = -1) +
  theme_minimal() +
  labs(
    title = "Minimum Wage by State",
    fill = "Wage"
  )

# average prevalence by year
library(gtsummary)
annual.prev.dep <- spatial_data %>%
  group_by(year) %>%
  summarise(wtd.dep = weighted.mean(depression, weights, na.rm=TRUE)) %>% select(c(year, wtd.dep))
annual.prev.anx <- spatial_data %>%
  group_by(year) %>%
  summarise(wtd.anx = weighted.mean(anxiety, weights, na.rm=TRUE)) %>% select(c(year, wtd.anx))
annual.prev.adhd <- spatial_data %>%
  group_by(year) %>%
  summarise(wtd.adhd = weighted.mean(adhd, weights, na.rm=TRUE)) %>% select(c(year, wtd.adhd))
annual.prev.behav <- spatial_data %>%
  group_by(year) %>%
  summarise(wtd.behav = weighted.mean(behavior, weights, na.rm=TRUE)) %>% select(c(year, wtd.behav))
annual.prev.unmet <- spatial_data %>%
  group_by(year) %>%
  summarise(wtd.unmet = weighted.mean(unmet_mental, weights, na.rm=TRUE)) %>% select(c(year, wtd.unmet))
annual.prev <- full_join(as.data.frame(annual.prev.dep), as.data.frame(annual.prev.anx), by="year")
annual.prev <- full_join(annual.prev, as.data.frame(annual.prev.adhd), by="year")
annual.prev <- full_join(annual.prev, as.data.frame(annual.prev.behav), by="year")
annual.prev <- full_join(annual.prev, as.data.frame(annual.prev.unmet), by="year")
annual.prev <- left_join(annual.prev, as.data.frame(spatial_data), by="year")
annual.prev %>% 
  select(c("year","wtd.dep", "wtd.anx", "wtd.adhd", "wtd.behav", "wtd.unmet")) %>%
  gtsummary::tbl_summary(
    by = year,
    include=c(wtd.dep, wtd.anx, wtd.adhd, wtd.behav, wtd.unmet),
    label = list(
      wtd.dep ~ 'Depression',
      wtd.anx ~ 'Anxiety',
      wtd.adhd ~ 'ADD/ADHD',
      wtd.behav ~ 'Behavioral Problems',
      wtd.unmet ~ 'Unmet Mental Needs'
    ),
    missing_text = "Missing",
    type = list(wtd.dep ~ "continuous", wtd.anx ~ "continuous", wtd.adhd ~ "continuous",
                wtd.behav ~ "continuous", wtd.unmet ~ "continuous"),
    statistic = all_continuous() ~ c(
      "{mean}"
    ),
   digits = all_continuous() ~ 2
  ) %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_caption("**Weighted Prevalence by Year**")

# average prevalence by sex
sex.prev.dep <- spatial_data %>%
  group_by(sex) %>%
  summarise(wtd.dep = weighted.mean(depression, weights, na.rm=TRUE)) %>% select(c(sex, wtd.dep))
sex.prev.anx <- spatial_data %>%
  group_by(sex) %>%
  summarise(wtd.anx = weighted.mean(anxiety, weights, na.rm=TRUE)) %>% select(c(sex, wtd.anx))
sex.prev.adhd <- spatial_data %>%
  group_by(sex) %>%
  summarise(wtd.adhd = weighted.mean(adhd, weights, na.rm=TRUE)) %>% select(c(sex, wtd.adhd))
sex.prev.behav <- spatial_data %>%
  group_by(sex) %>%
  summarise(wtd.behav = weighted.mean(behavior, weights, na.rm=TRUE)) %>% select(c(sex, wtd.behav))
sex.prev.unmet <- spatial_data %>%
  group_by(sex) %>%
  summarise(wtd.unmet = weighted.mean(unmet_mental, weights, na.rm=TRUE)) %>% select(c(sex, wtd.unmet))
sex.prev <- full_join(as.data.frame(sex.prev.dep), as.data.frame(sex.prev.anx), by="sex")
sex.prev <- full_join(sex.prev, as.data.frame(sex.prev.adhd), by="sex")
sex.prev <- full_join(sex.prev, as.data.frame(sex.prev.behav), by="sex")
sex.prev <- full_join(sex.prev, as.data.frame(sex.prev.unmet), by="sex")
sex.prev <- left_join(sex.prev, as.data.frame(spatial_data), by="sex")
sex.prev %>% 
  select(c("sex","wtd.dep", "wtd.anx", "wtd.adhd", "wtd.behav", "wtd.unmet")) %>%
  gtsummary::tbl_summary(
    by = sex,
    include=c(wtd.dep, wtd.anx, wtd.adhd, wtd.behav, wtd.unmet),
    label = list(
      wtd.dep ~ 'Depression',
      wtd.anx ~ 'Anxiety',
      wtd.adhd ~ 'ADD/ADHD',
      wtd.behav ~ 'Behavioral Problems',
      wtd.unmet ~ 'Unmet Mental Needs'
    ),
    missing_text = "Missing",
    type = list(wtd.dep ~ "continuous", wtd.anx ~ "continuous", wtd.adhd ~ "continuous",
                wtd.behav ~ "continuous", wtd.unmet ~ "continuous"),
    statistic = all_continuous() ~ c(
      "{mean}"
    ),
     digits = all_continuous() ~ 2
  ) %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_caption("**Weighted Prevalence by Sex**")

# average prevalence by age group
age.prev.dep <- spatial_data %>%
  group_by(age.cat) %>%
  summarise(wtd.dep = weighted.mean(depression, weights, na.rm=TRUE)) %>% select(c(age.cat, wtd.dep))
age.prev.anx <- spatial_data %>%
  group_by(age.cat) %>%
  summarise(wtd.anx = weighted.mean(anxiety, weights, na.rm=TRUE)) %>% select(c(age.cat, wtd.anx))
age.prev.adhd <- spatial_data %>%
  group_by(age.cat) %>%
  summarise(wtd.adhd = weighted.mean(adhd, weights, na.rm=TRUE)) %>% select(c(age.cat, wtd.adhd))
age.prev.behav <- spatial_data %>%
  group_by(age.cat) %>%
  summarise(wtd.behav = weighted.mean(behavior, weights, na.rm=TRUE)) %>% select(c(age.cat, wtd.behav))
age.prev.unmet <- spatial_data %>%
  group_by(age.cat) %>%
  summarise(wtd.unmet = weighted.mean(unmet_mental, weights, na.rm=TRUE)) %>% select(c(age.cat, wtd.unmet))
age.prev <- full_join(as.data.frame(age.prev.dep), as.data.frame(age.prev.anx), by="age.cat")
age.prev <- full_join(age.prev, as.data.frame(age.prev.adhd), by="age.cat")
age.prev <- full_join(age.prev, as.data.frame(age.prev.behav), by="age.cat")
age.prev <- full_join(age.prev, as.data.frame(age.prev.unmet), by="age.cat")
age.prev <- left_join(age.prev, as.data.frame(spatial_data), by="age.cat")
age.prev %>% 
  select(c("age.cat","wtd.dep", "wtd.anx", "wtd.adhd", "wtd.behav", "wtd.unmet")) %>%
  gtsummary::tbl_summary(
    by = age.cat,
    include=c(wtd.dep, wtd.anx, wtd.adhd, wtd.behav, wtd.unmet),
    label = list(
      wtd.dep ~ 'Depression',
      wtd.anx ~ 'Anxiety',
      wtd.adhd ~ 'ADD/ADHD',
      wtd.behav ~ 'Behavioral Problems',
      wtd.unmet ~ 'Unmet Mental Needs'
    ),
    missing_text = "Missing",
    type = list(wtd.dep ~ "continuous", wtd.anx ~ "continuous", wtd.adhd ~ "continuous",
                wtd.behav ~ "continuous", wtd.unmet ~ "continuous"),
    statistic = all_continuous() ~ c(
      "{mean}"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_caption("**Weighted Prevalence by Age Group**")