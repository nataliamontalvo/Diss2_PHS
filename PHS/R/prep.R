
# =============================================================================
# Data preparation
# =============================================================================
#
# Purpose:
#   Set up the R environment, define reusable helper functions, import the raw
#   datasets, and carry out cleaning steps required by later analyses.
#     1. Load packages used across the project 
#     2. Define helper functions
#     3. Import raw data
#     4. Initial checks of the datasets
#     5. Cleaning of the datasets
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

# Sum a vector, return NA (not 0) if every value in the group is NA.
sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# Compute a weighted mean, rows where both the value and its weight are non 
# missing and the weight is positive, returning NA if no valid rows remain.
weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  if (!any(valid)) return(NA_real_)
  weighted.mean(x[valid], w[valid])
}

# Consistent plotting theme for report figures
theme_report <- function() {
  theme_phs() +
    theme(
      plot.title = element_text(size = 15, face = "bold",hjust = 0.5,
                                margin = margin(b = 10)),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13),
      strip.text = element_text(size = 11, face = "bold"),
      plot.margin = margin(10, 10, 10, 10))}

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

# Population density by health board
pop_data_t4 <- read_xlsx("data/data-mid-year-population-estimates-2024.xlsx",
                               sheet = 7, skip = 3)

# Population data by data zone, age and sex 
pop_data <- read.csv("data/dz2011-pop-est_24022026.csv")

# SIMD main (Data Zone, HB, quintile)
simd_main <- read.csv("data/simd2020v2_22062020.csv")

# SIMD indicators (Data Zone only, population and domain indicators)
# "*" treated as NA
simd_ind <- read_xlsx("data/SIMD_2020v2_indicators.xlsx", sheet = 3, na = "*")


# 4. Clean individual datasets ------------------------------------------------

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

# Population by data zone, age and sex
pop_dz <- pop_data %>%
  mutate(Sex = trimws(Sex)) %>%
  filter(Sex %in% c("Male", "Female"), DataZone != "S92000003") %>%
  pivot_longer(cols = Age0:Age90plus, names_to = "age_col", values_to = "population") %>%
  mutate(age_year = as.integer(gsub("[^0-9]", "", age_col)),
         Age = bin_age(age_year)) %>%
  group_by(DataZone, Year, Sex, Age) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# Population Table 4: filter to health board level, keep just HB + density
pop_hb_density <- pop_data_t4 %>%
  filter(`Area type` == "Health board") %>%
  select(HB = `Area code`,
         population = 4,   # "Estimated population"
         area_km2 = 5,     # "Area (square km)"
         density = 6)      # "Population density"

# Consistent age-band ordering across all datasets
age_levels <- c("Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus")

month_demo_data <- month_demo_data %>%
  mutate(Age = factor(Age, levels = age_levels))

referrals_data <- referrals_data %>%
  mutate(Age = factor(Age, levels = age_levels))

discharges_data <- discharges_data %>%
  mutate(Age = factor(Age, levels = age_levels))

pop_dz <- pop_dz %>%
  mutate(Age = factor(Age, levels = age_levels))

# HB-level population by age and sex (for EDA plots and age/sex model)
pop_hb_age_sex <- pop_dz %>%
  filter(Year == max(Year)) %>%
  left_join(simd_main %>% select(DataZone, HB), by = "DataZone") %>%
  group_by(HB, Sex, Age) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

mul_att_data <- mul_att_data %>%
  mutate(Age = factor(Age, levels = age_levels))

# Reshape multiple attendance data from wide to long
mul_att_long <- mul_att_data %>%
  pivot_longer(cols = OneAttendance:FivePlusAttendances,
               names_to = "attendance_freq", values_to = "n_patients") %>%
  mutate(
    attendance_freq = factor(attendance_freq,
                             levels = c("OneAttendance", "TwoAttendances", 
                                        "ThreeAttendances","FourAttendances", 
                                        "FivePlusAttendances"),
                             labels = c("1", "2", "3", "4", "5+")))

# HB name mapping 
hb_names <- tribble(~HBT, ~HBName,
                    "S08000015", "Ayrshire and Arran",
                    "S08000016", "Borders",
                    "S08000017", "Dumfries and Galloway",
                    "S08000019", "Forth Valley",
                    "S08000020", "Grampian",
                    "S08000022", "Highland",
                    "S08000024", "Lothian",
                    "S08000025", "Orkney",
                    "S08000026", "Shetland",
                    "S08000028", "Western Isles",
                    "S08000029", "Fife",
                    "S08000030", "Tayside",
                    "S08000031", "Greater Glasgow and Clyde",
                    "S08000032", "Lanarkshire")



# 5. SIMD aggregation ---------------------------------------------------------

# Merge SIMD data sets by Data Zone
simd_joined <- simd_ind %>%
  left_join(simd_main %>% select(DataZone, HB, CA, 
                                 Quintile = SIMD2020V2CountryQuintile),
            by = c("Data_Zone" = "DataZone"))

# Verify whether Income_rate/Employment_rate can be safely recomputed from
# counts, or must stay as weighted averages of the original SIMD-calculated
# rate.

# simd_ind %>%
#   mutate(recomputed = 100 * Income_count / Total_population) %>%
#   summarise(max_diff = max(abs(recomputed - Income_rate), na.rm = TRUE))
# 
# simd_ind %>%
#   mutate(recomputed = 100 * Employment_count / Working_age_population) %>%
#   summarise(max_diff = max(abs(recomputed - Employment_rate), na.rm = TRUE))

# Aggregate by HB and quintile
simd_hb <- simd_joined %>%
  group_by(HB, Quintile) %>%
  summarise(
    # Population totals
    quintile_population = sum(Total_population, na.rm = TRUE),
    quintile_working_age_population = sum(Working_age_population, na.rm = TRUE),
    n_data_zones = n_distinct(Data_Zone),
    
    # Income, employment and housing counts: kept as sums
    Income_count = sum(Income_count, na.rm = TRUE),
    Employment_count = sum(Employment_count, na.rm = TRUE),
    across(c(crime_count, overcrowded_count, nocentralheat_count),
           ~ sum(.x, na.rm = TRUE)),
    
    # Rate, ratio and travel time indicators: population weighted mean
    across(c(Income_rate, Employment_rate, CIF, ALCOHOL, DRUG, SMR, DEPRESS,
             LBWT, EMERG, Attendance, Attainment, no_qualifications,
             not_participating, University, crime_rate,
             starts_with("drive_"), starts_with("PT_"), Broadband),
           ~ weighted.mean(.x, w = Total_population, na.rm = TRUE)),
    .groups = "drop")

# Collapse SIMD indicators to HB level
simd_hb_overall <- simd_hb %>%
  group_by(HB) %>%
  summarise(
    across(c(Income_rate, Employment_rate, crime_rate, ALCOHOL, DRUG, EMERG, 
             drive_GP),
           ~ weighted.mean(.x, w = quintile_population, na.rm = TRUE)),
    total_population = sum(quintile_population, na.rm = TRUE),
    .groups = "drop")

