
# =============================================================================
# Data preparation
# =============================================================================
#
# Purpose:
#   Set up the R environment, define reusable helper functions, import the raw
#   datasets, and carry out cleaning steps required by later analyses.
#
# Note: This script should be run before the analysis scripts.
# =============================================================================


# 1. Load packages ------------------------------------------------------------

# Data manipulation and visualisation
library(dplyr)
library(tidyr)
library(ggplot2)

# Data import and date handling
library(readxl)
library(lubridate)

# Statistical modelling
library(mgcv)
library(INLA)

# Plot formatting
library(phsstyles)  # PHS palette
library(scales)


# 2. Define helper functions --------------------------------------------------

# Summarise the variables in a data frame
summarise_columns <- function(data) {
  
  # Identify the missing value formats used in the PHS datasets
  is_missing <- function(x) {
    if (is.character(x)) {
      is.na(x) | trimws(x) %in% c("", ":")
    } else {
      is.na(x)
    }
  }
  
  data %>%
    summarise(
      across(everything(),
             list(
               type = ~ class(.)[1],
               n_rows = ~ length(.),
               n_missing = ~ sum(is_missing(.)),
               pct_missing = ~ round(mean(is_missing(.)) * 100, 2),
               n_unique = ~ n_distinct(.[!is_missing(.)]),
               options = ~ paste(
                 sort(unique(as.character(.[!is_missing(.)]))),
                 collapse = ", ")
             ),
             .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(cols = everything(),
                 names_to = c("variable", ".value"),
                 names_sep = "__")
}

# Bin single year ages into the same age bands used across the PHS A&E datasets
bin_age <- function(age_year) {
  case_when(
    age_year < 18 ~ "Under 18",
    age_year <= 24 ~ "18-24",
    age_year <= 39 ~ "25-39",
    age_year <= 64 ~ "40-64",
    age_year <= 74 ~ "65-74",
    TRUE ~ "75 plus"
  )
}

# 3. Import raw data ----------------------------------------------------------

# Monthly attendance counts by demographic group
month_demo_data <- read.csv("data/opendata_monthly_ae_demographics_202604.csv")

# Weekly attendance and performance activity
week_activity_data <- read.csv("data/weekly_ae_activity_20260628.csv")

# Monthly attendance and performance activity
month_activity_data <- read.csv("data/monthly_ae_activity_202605.csv")

# Treatment location names, codes and department information
trt_loc <- read.csv("data/ae_hospital_site_list_09_09_2025.csv")

# Monthly attendance counts by referral source
referrals_data <- read.csv("data/opendata_monthly_ae_referral_202604.csv")

# Monthly attendance counts by discharge type
discharges_data <- read.csv("data/opendata_monthly_ae_discharge_202604.csv")

# Multiple attendance counts
mul_att_data <- read.csv("data/opendata_monthly_ae_multiple_attendances_demographics_202604.csv")

# When (day of week / time of day / in-hours vs out-of-hours) 
when_data <- read.csv("data/opendata_monthly_ae_when_202604.csv")

# Population data by health board, age and sex
pop_data_t1 <- read_xlsx("data/data-mid-year-population-estimates-2024.xlsx", 
                               sheet = 4, skip = 3)
# Population density by health board 
pop_data_t4 <- read_xlsx("data/data-mid-year-population-estimates-2024.xlsx", 
                               sheet = 7, skip = 3)

# SIMD main (Data Zone, HB, quintile)
simd_main <- read.csv("data/simd2020v2_22062020.csv")

# SIMD indicators (Data Zone only, population and domain indicators)
# "*" treated as NA
simd_ind <- read_xlsx("data/SIMD_2020v2_indicators.xlsx", sheet = 3, na = "*")


# 4. Initial data checks ------------------------------------------------------

summarise_columns(month_demo_data)
summarise_columns(week_activity_data)
summarise_columns(month_activity_data)
summarise_columns(trt_loc)
summarise_columns(referrals_data)
summarise_columns(discharges_data)
summarise_columns(mul_att_data)
summarise_columns(when_data)
summarise_columns(pop_data_t1)
summarise_columns(pop_data_t4)
summarise_columns(simd_main)
summarise_columns(simd_ind)


# 5. Clean individual datasets ------------------------------------------------

# Monthly demographics: drop QF columns and parse Month into a proper date
month_demo_data <- month_demo_data %>%
  select(-AgeQF, -SexQF, -DeprivationQF) %>%
  mutate(m_date = ym(as.character(Month)),
         year   = format(m_date, "%Y"),
         m_num  = as.integer(format(m_date, "%m")))

# Weekly activity: parse WeekEndingDate into date
week_activity_data <- week_activity_data %>%
  mutate(w_date   = ymd(as.character(WeekEndingDate)),
         year     = format(w_date, "%Y"),
         m_num    = as.integer(format(w_date, "%m")),
         week_num = as.integer(format(w_date, "%U")))

# Monthly activity: same date parsing as demographics and drop "All"
#   rows (since All = Unplanned + New planned)
month_activity_data <- month_activity_data %>%
  filter(AttendanceCategory != "All") %>% 
  mutate(m_date = ym(as.character(Month)),
         year   = format(m_date, "%Y"),
         m_num  = as.integer(format(m_date, "%m")))

# Referrals: drop QF, parse date, recode missing Referral as "Not Known" 
referrals_data <- referrals_data %>%
  select(-AgeQF, -ReferralQF) %>%
  mutate(m_date = ym(as.character(Month)),
         year   = format(m_date, "%Y"),
         m_num  = as.integer(format(m_date, "%m")),
         Referral = case_when(
           is.na(Referral) | trimws(Referral) %in% c("", ":") ~ "Not Known",
           TRUE ~ Referral
         ))

# Discharges: drop QF, parse date, recode missing Discharges as "Not Known"
discharges_data <- discharges_data %>%
  select(-AgeQF, -DischargeQF) %>%
  mutate(m_date = ym(as.character(Month)),
         year   = format(m_date, "%Y"),
         m_num  = as.integer(format(m_date, "%m")),
         Discharge = case_when(
           is.na(Discharge) | trimws(Discharge) %in% c("", ":") ~ "Not Known",
           TRUE ~ Discharge
         ))

# Multiple attendance: drop QF 
mul_att_data <- mul_att_data %>%
  select(-AgeQF, -SexQF, -DeprivationQF)

# When: parse date
when_data <- when_data %>%
  mutate(m_date = ym(as.character(Month)),
         year   = format(m_date, "%Y"),
         m_num  = as.integer(format(m_date, "%m")))

# Population Table 1: filter to health board level, reshape wide age columns
#   to long, and bin into the same age bands used in month_demo_data
pop_hb_age_sex <- pop_data_t1 %>%
  filter(`Area type` == "Health board") %>%
  pivot_longer(cols = `0`:`90 and over`,
               names_to = "age_year", values_to = "population") %>%
  mutate(age_year = as.integer(gsub("[^0-9]", "", age_year)),
         Age = bin_age(age_year)) %>%
  group_by(`Area code`, Sex, Age) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  rename(HB = `Area code`)

# Population Table 4: filter to health board level, keep just HB + density
pop_hb_density <- pop_data_t4 %>%
  filter(`Area type` == "Health board") %>%
  select(HB = `Area code`,
         population = 4,   # "Estimated population 30 June 2024" - 4th column
         area_km2 = 5,     # "Area (square kilometres)" - 5th column
         density = 6)      # "Population density ..." - 6th column

# SIMD: indicators (income, employment, crime, education, housing, etc.)
#   aggregated from data zone level up to HB x deprivation quintile, using a
#   population weighted mean so each data zone contributes proportionally to
#   its actual population rather than being treated as equally sized.

simd_joined <- simd_ind %>%
  left_join(
    simd_main %>% select(DataZone, HB, Quintile = SIMD2020V2CountryQuintile),
    by = c("Data_Zone" = "DataZone")
  )

hb_quintile_indicators <- simd_joined %>%
  group_by(HB, Quintile) %>%
  summarise(
    across(
      where(is.numeric) & !c(Total_population),
      ~ weighted.mean(.x, w = Total_population, na.rm = TRUE)
    ),
    quintile_population = sum(Total_population, na.rm = TRUE),
    .groups = "drop"
  )

