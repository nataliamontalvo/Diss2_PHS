
# =============================================================================
# Build discharge model data
# =============================================================================
#
# Purpose:
#   Build Build discharge_model_data, at grain Month x HB x TreatmentLocation x
#   DepartmentType x Age.
#
# Note: This script sources risk_model_data.R (which itself sources prep.R) to 
# reuse simd_hb.
#
# =============================================================================

source("R/risk_model_data.R")

# 1. Site level context -------------------------------------------------------

# Referral data by site, month and age
referral_wide_site_age <- referrals_data %>%
  group_by(Month, HBT, TreatmentLocation, DepartmentType, Age, Referral) %>%
  summarise(referral_total = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Month, HBT, TreatmentLocation, DepartmentType, Age) %>%
  mutate(referral_pct = referral_total / sum(referral_total) * 100) %>%
  ungroup() %>%
  select(Month, HBT, TreatmentLocation, DepartmentType, Age, Referral,
         referral_pct) %>%
  pivot_wider(names_from = Referral, values_from = referral_pct,
              values_fill = 0, names_prefix = "pct_ref_")

# When data by site and month (no Age column in when_data, so this stays at site 
# month resolution and repeats across Age rows when joined to discharges)
when_site_monthly <- when_data %>%
  group_by(Month, HBT, TreatmentLocation, DepartmentType) %>%
  summarise(
    total = sum(NumberOfAttendances, na.rm = TRUE),
    out_of_hours = sum(NumberOfAttendances[InOut == "Out of Hours"],
                       na.rm = TRUE),
    weekend = sum(NumberOfAttendances[Week == "Weekend"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(oohours_pct = out_of_hours / total * 100,
         weekend_pct = weekend / total * 100) %>%
  select(Month, HBT, TreatmentLocation, DepartmentType, oohours_pct,
         weekend_pct)


# HB level SIMD indicators
hb_indicators_board_level <- simd_hb %>%
  group_by(HB) %>%
  summarise(
    across(where(is.numeric) & !c(quintile_population),
           ~ weighted.mean(.x, w = quintile_population, na.rm = TRUE)),
    total_population = sum(quintile_population, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Model 1a - age/sex risk model data ----------------------------------------

# Grain: Month x HB x Age x Sex. Deprivation summed 
age_sex_model_data <- month_demo_data %>%
  group_by(Month, m_date, year, m_num, HBT, Age, Sex) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(
    pop_hb_age_sex %>%
      mutate(Sex = case_when(
        Sex == "Males" ~ "Male",
        Sex == "Females" ~ "Female",
        TRUE ~ Sex
      )),
    by = c("HBT" = "HB", "Age", "Sex")
  ) %>%
  left_join(simd_hb_overall, by = c("HBT" = "HB")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_monthly, by = c("Month", "HBT"))


# 4. Model 1b - deprivation risk model data ------------------------------------

# Grain: Month x HB x Deprivation. Age/Sex summed away. Offset uses
# simd_hb$quintile_population, which exactly matches this grain, and
# quintile-specific SIMD indicators (Income_rate, crime_rate, etc.) can be
# used directly as predictors since they're already at the same resolution.
deprivation_model_data <- month_demo_data %>%
  filter(!is.na(Deprivation)) %>%
  group_by(Month, m_date, year, m_num, HBT, Deprivation) %>%
  summarise(NumberOfAttendances = sum(NumberOfAttendances, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(t = as.numeric(m_date - min(m_date)) / 30) %>%
  left_join(simd_hb, by = c("HBT" = "HB", "Deprivation" = "Quintile")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_monthly, by = c("Month", "HBT"))

# 5. Model 2 - discharge model data -------------------------------------------

# NOTE: confirm admission_labels against distinct(discharges_data, Discharge)
# before trusting this - see project notes on narrow vs broad admission
# definition (same-hospital admission only, vs including transfers)
admission_labels <- c("Admission to same Hospital")

discharge_wide <- discharges_data %>%
  group_by(Month, HBT, TreatmentLocation, DepartmentType, Age) %>%
  summarise(
    total_attendances = sum(NumberOfAttendances, na.rm = TRUE),
    admitted = sum(NumberOfAttendances[Discharge %in% admission_labels],
                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(not_admitted = total_attendances - admitted)

discharge_model_data <- discharge_wide %>%
  left_join(referral_wide_site_age,
            by = c("Month", "HBT", "TreatmentLocation", "DepartmentType",
                   "Age")) %>%
  left_join(when_site_monthly,
            by = c("Month", "HBT", "TreatmentLocation", "DepartmentType")) %>%
  left_join(hb_indicators_board_level, by = c("HBT" = "HB")) %>%
  left_join(trt_loc, by = c("TreatmentLocation" = "TreatmentLocationCode"))


# 6. Quick checks ---------------------------------------------------------------

# Age/sex model: rows with no population match
age_sex_model_data %>% filter(is.na(population)) %>% distinct(Age, Sex)

# Deprivation model: rows with no quintile_population match
deprivation_model_data %>% filter(is.na(quintile_population)) %>% distinct(Deprivation)

# Discharge model: admitted should never exceed total_attendances
discharge_model_data %>% filter(admitted > total_attendances) %>% nrow()


