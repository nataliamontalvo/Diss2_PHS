
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

head(referral_hb_monthly)

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


# 3. Age/sex risk model data --------------------------------------------------

## 3.1 Dataset: Month x HB x Age x Sex
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

# Built directly from pop_data (Data Zone x Age x Sex population), joined to 
# its SIMD quintile via simd_main, then aggregated up to HB x Quintile x Age x 
# Sex. Using the data avilable from 2020 as the simd data is from htat year aswell
true_pop_hb_age_sex_quintile <- pop_data %>%
  mutate(Sex = trimws(Sex)) %>%
  filter(Year == 2020, Sex %in% c("Male", "Female"), DataZone != "S92000003") %>%
  left_join(simd_main %>% select(DataZone, HB, Quintile = SIMD2020V2CountryQuintile),
            by = "DataZone") %>%
  pivot_longer(cols = Age0:Age90plus, names_to = "age_col", values_to = "population") %>%
  mutate(age_year = as.integer(gsub("[^0-9]", "", age_col)),
         Age = bin_age(age_year)) %>%
  group_by(HB, Quintile, Age, Sex) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

combined_model_data <- month_demo_data %>%
  filter(Deprivation %in% 1:5, !is.na(Age), Age != "",
         Sex %in% c("Male", "Female")) %>%
  group_by(Month, m_date, year, m_num, HBT, Age, Sex, Deprivation) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(true_pop_hb_age_sex_quintile,
            by = c("HBT" = "HB", "Age", "Sex", "Deprivation" = "Quintile"),
            relationship = "many-to-one") %>%
  left_join(simd_hb %>% select(HB, Quintile, Income_rate, Employment_rate,
                               crime_rate),
            by = c("HBT" = "HB", "Deprivation" = "Quintile")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_age_monthly, by = c("Month", "HBT", "Age"))

## 5.2 Checks

# Rows with no matched population, expect only the known island board quintile
# gaps (see missing_combos in Section 4.2)
combined_model_data %>% filter(is.na(approx_population)) %>% 
  distinct(Age, Sex, Deprivation)

# Sanity check: total approximated population across quintiles for a given
# Age x Sex x HB should sum back to roughly the original pop_hb_age_sex value
approx_population_check <- approx_pop_age_sex_quintile %>%
  group_by(HB, Age, Sex) %>%
  summarise(approx_total = sum(approx_population, na.rm = TRUE), 
            .groups = "drop") %>%
  left_join(pop_hb_age_sex %>%
              mutate(Sex = case_when(Sex == "Males" ~ "Male", 
                                     Sex == "Females" ~ "Female", TRUE ~ Sex)),
            by = c("HB", "Age", "Sex")) %>%
  mutate(difference = approx_total - population)

stopifnot(max(abs(approx_population_check$difference), na.rm = TRUE) < 1e-8)
