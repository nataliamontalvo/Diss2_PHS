
# =============================================================================
# Build risk model data
# =============================================================================
#
# Purpose:
#   Build three versions of the attendance-risk model data, reflecting the
#   population-denominator options discussed for this project:
#     1. age_sex_model_data: Age x Sex, exact population (no Deprivation)
#     2. deprivation_model_data: Deprivation, exact population (no Age/Sex)
#     3. combined_model_data: Age x Sex x Deprivation together, using an
#                             exact joint population built from pop_data.
#
# Note: This script sources "R/prep.R" and therefore assumes that all raw data
#   files and paths required by prep.R are available.
# 
# =============================================================================

source("R/prep.R")

# 1. Merged SIMD data ---------------------------------------------------------

# Merge SIMD data sets by Data Zone
simd_joined <- simd_ind %>%
  left_join(simd_main %>% select(DataZone, HB, CA, 
                                 Quintile = SIMD2020V2CountryQuintile),
            by = c("Data_Zone" = "DataZone"))

# Verify whether Income_rate/Employment_rate can be safely recomputed from
# counts, or must stay as weighted averages of the original SIMD-calculated
# rate.

simd_ind %>%
  mutate(recomputed = 100 * Income_count / Total_population) %>%
  summarise(max_diff = max(abs(recomputed - Income_rate), na.rm = TRUE))

simd_ind %>%
  mutate(recomputed = 100 * Employment_count / Working_age_population) %>%
  summarise(max_diff = max(abs(recomputed - Employment_rate), na.rm = TRUE))

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
    
    .groups = "drop"
  )

# Collapse SIMD indicators to HB level
simd_hb_overall <- simd_hb %>%
  group_by(HB) %>%
  summarise(
    across(c(Income_rate, Employment_rate, crime_rate, ALCOHOL, DRUG, EMERG),
           ~ weighted.mean(.x, w = quintile_population, na.rm = TRUE)),
    total_population = sum(quintile_population, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Shared HB-month context --------------------------------------------------

# Referrals data by HB and month
referral_hb_monthly <- referrals_data %>%
  group_by(Month, HBT, Referral) %>%
  summarise(referral_total = sum(NumberOfAttendances, na.rm = TRUE), 
            .groups = "drop") %>%
  group_by(Month, HBT) %>%
  mutate(referral_pct = referral_total / sum(referral_total) * 100) %>%
  ungroup() %>%
  select(Month, HBT, Referral, referral_pct) %>%
  pivot_wider(names_from = Referral, values_from = referral_pct,
              values_fill = 0, names_prefix = "pct_ref_")

# Referrals data by HB, month and age
referral_hb_age_monthly <- referrals_data %>%
  filter(!is.na(Age), Age != "") %>%
  group_by(Month, HBT, Age, Referral) %>%
  summarise(referral_total = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Month, HBT, Age) %>%
  mutate(referral_pct = referral_total / sum(referral_total) * 100) %>%
  ungroup() %>%
  select(Month, HBT, Age, Referral, referral_pct) %>%
  pivot_wider(names_from = Referral, values_from = referral_pct,
              values_fill = 0, names_prefix = "pct_ref_")

# When data by HB and month
when_hb_monthly <- when_data %>%
  group_by(Month, HBT) %>%
  summarise(
    total = sum(NumberOfAttendances, na.rm = TRUE),
    out_of_hours = sum(NumberOfAttendances[InOut == "Out of Hours"], 
                       na.rm = TRUE),
    weekend = sum(NumberOfAttendances[Week == "Weekend"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(oohours_pct = out_of_hours / total * 100,
         weekend_pct = weekend / total * 100) %>%
  select(Month, HBT, oohours_pct, weekend_pct)

# HB level population by age and sex
pop_hb_age_sex <- pop_dz %>%
  filter(Year == max(Year)) %>%
  left_join(simd_main %>% select(DataZone, HB), by = "DataZone") %>%
  group_by(HB, Sex, Age) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# HB name mapping (for plot labels throughout the analysis)
hb_names <- tribble(
  ~HBT,          ~HBName,
  "S08000015",   "Ayrshire and Arran",
  "S08000016",   "Borders",
  "S08000017",   "Dumfries and Galloway",
  "S08000019",   "Forth Valley",
  "S08000020",   "Grampian",
  "S08000022",   "Highland",
  "S08000024",   "Lothian",
  "S08000025",   "Orkney",
  "S08000026",   "Shetland",
  "S08000028",   "Western Isles",
  "S08000029",   "Fife",
  "S08000030",   "Tayside",
  "S08000031",   "Greater Glasgow and Clyde",
  "S08000032",   "Lanarkshire"
)


# 3. Age/sex risk model data --------------------------------------------------

## 3.1 Dataset: Month x HB x Age x Sex
age_sex_model_data <- month_demo_data %>%
  
  # Exclude records with missing age and sex (approximately 2%)
  filter(!is.na(Age), Age != "", Sex %in% c("Male", "Female")) %>%
  
  group_by(Month, m_date, year, m_num, HBT, Age, Sex) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(pop_hb_age_sex, by = c("HBT" = "HB", "Age", "Sex")) %>%
  left_join(simd_hb_overall, by = c("HBT" = "HB")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_age_monthly, by = c("Month", "HBT", "Age"))

## 3.2 Checks

# Rows with no population match
age_sex_model_data %>% filter(is.na(population)) %>% distinct(HBT, Age, Sex)

age_sex_model_data %>%
  summarise(n_rows = n(),
            n_missing_population = sum(is.na(population)),
            pct_missing_population = mean(is.na(population)) * 100)


# 4. Deprivation risk model data ----------------------------------------------

## 4.1 Dataset: Month x HB x Deprivation
deprivation_model_data <- month_demo_data %>%
  filter(Deprivation %in% 1:5) %>%
  group_by(Month, m_date, year, m_num, HBT, Deprivation) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(simd_hb, by = c("HBT" = "HB", "Deprivation" = "Quintile")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_monthly, by = c("Month", "HBT"))


## 4.2 Checks

# Rows with no quintile_population match
deprivation_model_data %>% filter(is.na(quintile_population)) %>% 
  distinct(HBT, Deprivation)

# Which HB-Quintile combinations are genuinely absent from simd_hb?
expected_combos <- expand_grid(HB = sort(unique(month_demo_data$HBT)), 
                               Quintile = 1:5)
missing_combos <- expected_combos %>% anti_join(simd_hb, 
                                                by = c("HB", "Quintile"))
missing_combos


# 5. Combined age/sex/deprivation model data ----------------------------------

## 5.1 Dataset: Month x HB x Age x Sex x Deprivation

# Built directly from pop_dz (Data Zone x Age x Sex population), joined to 
# its SIMD quintile via simd_main, then aggregated up to HB x Quintile x Age x 
# Sex. Population itself varies by year, so each attendance month's count is 
# offset by the correct year's population. For 2025 and 2026 attendance data 
# the most recent available year (2024) is used as a documented fallback.

# Year-varying population by HB x Quintile x Age x Sex
max_pop_year <- max(pop_dz$Year)

pop_hb_age_sex_quintile_year <- pop_dz %>%
  left_join(simd_main %>% select(DataZone, HB, 
                                 Quintile = SIMD2020V2CountryQuintile),
            by = "DataZone") %>%
  group_by(HB, Quintile, Age, Sex, Year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

combined_model_data <- month_demo_data %>%
  filter(Deprivation %in% 1:5, !is.na(Age), Age != "",
         Sex %in% c("Male", "Female")) %>%
  mutate(pop_year = pmin(as.integer(year), max_pop_year)) %>%
  group_by(Month, m_date, year, m_num, HBT, Age, Sex, Deprivation, pop_year) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(pop_hb_age_sex_quintile_year,
            by = c("HBT" = "HB", "Age", "Sex", "Deprivation" = "Quintile",
                   "pop_year" = "Year"),
            relationship = "many-to-one") %>%
  left_join(simd_hb %>% select(HB, Quintile, Income_rate, Employment_rate,
                               crime_rate, ALCOHOL, DRUG, EMERG, drive_GP,
                               quintile_population),
            by = c("HBT" = "HB", "Deprivation" = "Quintile")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_age_monthly, by = c("Month", "HBT", "Age"))%>%
  left_join(hb_names, by = "HBT")

## 5.2 Checks

# Confirm the year-cap logic 
combined_model_data %>% distinct(year, pop_year) %>% arrange(year)

# Rows with no matched population - expect only the known island-board
# quintile gaps (S08000025, S08000026 and S08000028 missing quintile 1/4/5,
# see missing_combos in Section 4.2)
combined_model_data %>% 
  filter(is.na(population)) %>%
  distinct(HBT, Age, Sex, Deprivation)

# Duplicate-row check - each Month x HB x Age x Sex x Deprivation combination
# should appear exactly once; should return zero rows
combined_model_data %>%
  count(Month, HBT, Age, Sex, Deprivation) %>%
  filter(n > 1)

# National population reconciliation for a single year (2020) - compare the
# year-varying table's 2020 slice against simd_hb's quintile_population
# (which reflects SIMD 2020 vintage ~2017 population). These won't match
# exactly (different underlying years), but should be in the same ballpark.
pop_hb_age_sex_quintile_year %>%
  filter(Year == 2020) %>%
  summarise(total = sum(population, na.rm = TRUE))

sum(simd_hb$quintile_population)
