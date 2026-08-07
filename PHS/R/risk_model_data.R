
# =============================================================================
# Build Risk Model Data
# =============================================================================
#
# Purpose:
#   Build three versions of the attendance risk model dataat the
#   Month x HB x Age x Sex x Deprivation grain, with an exact joint
#   population denominator built from NRS Data Zone Population Estimates
#   joined to SIMD 2020 quintile classification.
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


# 2. Combined age/sex/deprivation model data ----------------------------------

## 2.1 Dataset: Month x HB x Age x Sex x Deprivation

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


## 2.2 Checks

# Confirm the year cap logic 
combined_model_data %>% distinct(year, pop_year) %>% arrange(year)

# Rows with no matched population
combined_model_data %>% 
  filter(is.na(population)) %>%
  distinct(HBT, Age, Sex, Deprivation)

# Duplicate row check
combined_model_data %>%
  count(Month, HBT, Age, Sex, Deprivation) %>%
  filter(n > 1)

# National population reconciliation for a single year (2020)
pop_hb_age_sex_quintile_year %>%
  filter(Year == 2020) %>%
  summarise(total = sum(population, na.rm = TRUE))

sum(simd_hb$quintile_population)
