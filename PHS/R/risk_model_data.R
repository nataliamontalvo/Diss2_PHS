
# =============================================================================
# Build Risk Model Data
# =============================================================================
#
# Purpose:
#   Build three versions of the attendance risk model data, reflecting the
#   population denominator options discussed for this project:
#     1. age_sex_model_data: Age x Sex, exact population (no Deprivation)
#     2. deprivation_model_data: Deprivation, exact population (no Age/Sex)
#     3. combined_model_data: Age x Sex x Deprivation together, using an
#                             exact joint population built from pop_data.
#
# Note: This script sources "R/prep.R" and therefore assumes that all raw data
#       files and paths required by prep.R are available.
# 
# =============================================================================

source("R/prep.R")

# 1. Shared HB-month context --------------------------------------------------

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


# 2. Age/sex risk model data --------------------------------------------------

## 2.1 Dataset: Month x HB x Age x Sex
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

## 2.2 Checks

# Rows with no population match
age_sex_model_data %>% filter(is.na(population)) %>% distinct(HBT, Age, Sex)

age_sex_model_data %>%
  summarise(n_rows = n(),
            n_missing_population = sum(is.na(population)),
            pct_missing_population = mean(is.na(population)) * 100)


# 3. Deprivation risk model data ----------------------------------------------

## 3.1 Dataset: Month x HB x Deprivation
deprivation_model_data <- month_demo_data %>%
  filter(Deprivation %in% 1:5) %>%
  group_by(Month, m_date, year, m_num, HBT, Deprivation) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(simd_hb, by = c("HBT" = "HB", "Deprivation" = "Quintile")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_monthly, by = c("Month", "HBT"))


## 3.2 Checks

# Rows with no quintile_population match
deprivation_model_data %>% filter(is.na(quintile_population)) %>% 
  distinct(HBT, Deprivation)

# Which HB-Quintile combinations are genuinely absent from simd_hb?
expected_combos <- expand_grid(HB = sort(unique(month_demo_data$HBT)), 
                               Quintile = 1:5)
missing_combos <- expected_combos %>% anti_join(simd_hb, 
                                                by = c("HB", "Quintile"))
missing_combos


# 4. Combined age/sex/deprivation model data ----------------------------------

## 4.1 Dataset: Month x HB x Age x Sex x Deprivation

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

## 4.2 Checks

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

# do not need this any more??? 
# National population reconciliation for a single year (2020) - compare the
# year-varying table's 2020 slice against simd_hb's quintile_population
# (which reflects SIMD 2020 with 2017 population).
pop_hb_age_sex_quintile_year %>%
  filter(Year == 2020) %>%
  summarise(total = sum(population, na.rm = TRUE))

sum(simd_hb$quintile_population)
