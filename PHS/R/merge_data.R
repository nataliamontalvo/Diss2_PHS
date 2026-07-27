
# =============================================================================
# Build and merge model datasets
# =============================================================================
#
# Purpose:
#   Build datasets to be used for modelling:
#
# Note: This script should be run after source("R/prep.R")
# =============================================================================

source("R/prep.R")

# Merge SIMD data sets by Data Zone, after agregatTe them by HB

# Select specific columns:

simd_ind <- simd_ind %>%
  select(Data_Zone, , Quintile = SIMD2020V2CountryQuintile)

simd_joined <- simd_ind %>%
  left_join(
    simd_main %>% select(HB, CA, Quintile = SIMD2020V2CountryQuintile),
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



# 1. Risk model data ----------------------------------------------------------

# Referral data by HB month 

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

# When data HB level
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


# Risk model data set
risk_model_data <- month_demo_data %>%
  left_join(
    pop_hb_age_sex %>%
      mutate(Sex = case_when(
        Sex == "Males" ~ "Male",
        Sex == "Females" ~ "Female",
        TRUE ~ Sex
      )),
    by = c("HBT" = "HB", "Age", "Sex")
  ) %>%
  left_join(hb_quintile_indicators, by = c("HBT" = "HB", 
                                           "Deprivation" = "Quintile")) %>%
  left_join(when_hb_monthly, by = c("Month", "HBT")) %>%
  left_join(referral_hb_monthly, by = c("Month", "HBT"))


# 2. Discharge model data -----------------------------------------------------

# HB level SIMD indicators
hb_indicators_board_level <- hb_quintile_indicators %>%
  group_by(HB) %>%
  summarise(
    across( where(is.numeric) & !c(quintile_population),
            ~ weighted.mean(.x, w = quintile_population, na.rm = TRUE)
    ),
    total_population = sum(quintile_population, na.rm = TRUE),
    .groups = "drop"
  )

# Site month age grain 
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

# When data site month
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

# Discharges model dataset
discharge_model_data <- discharge_wide %>%
  left_join(referral_wide_site_age,
            by = c("Month", "HBT", "TreatmentLocation", "DepartmentType", 
                   "Age")) %>%
  left_join(when_site_monthly,
            by = c("Month", "HBT", "TreatmentLocation", "DepartmentType")) %>%
  left_join(hb_indicators_board_level, by = c("HBT" = "HB")) %>%
  left_join(trt_loc, by = c("TreatmentLocation" = "TreatmentLocationCode"))

