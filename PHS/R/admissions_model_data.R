
# =============================================================================
# Build Admissions Model Data
# =============================================================================
#
# Purpose:
#   Build the admission model data, given that someone has already attended A&E,
#   what factors predict whether they are admitted to hospital?
#
#   Two admission definitions are built for sensitivity comparison:
#     - Narrow: "Admission to same Hospital" only
#     - Broad:  also includes "Transferred to Other Hospital/Service"
#
# Note: This script sources "R/prep.R" and therefore assumes that all raw data
#   files and paths required by prep.R are available.
#
# =============================================================================

source("R/prep.R")

# 1. Site level context -------------------------------------------------------

# Referral data by site, month and age
referral_wide_site_age <- referrals_data %>%
  filter(!is.na(Age), Age != "") %>%
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


# 2. Admission outcome --------------------------------------------------------

admission_labels <- c("Admission to same Hospital")

# HB level SIMD indicators
hb_indicators_board_level <- simd_hb %>%
  group_by(HB) %>%
  summarise(
    across(where(is.numeric) & !c(quintile_population),
           ~ weighted.mean(.x, w = quintile_population, na.rm = TRUE)),
    total_population = sum(quintile_population, na.rm = TRUE),
    .groups = "drop"
  )

discharge_wide <- discharges_data %>%
  group_by(Month, m_date, year, m_num, HBT, TreatmentLocation,
           DepartmentType, Age) %>%
  summarise(
    total_attendances = sum(NumberOfAttendances, na.rm = TRUE),
    admitted = sum(NumberOfAttendances[Discharge %in% admission_labels],
                   na.rm = TRUE),
    .groups = "drop") %>%
  mutate(not_admitted = total_attendances - admitted,
         t = as.numeric(m_date - min(m_date)) / 30)

# Broad definition: including transfers
admission_labels_broad <- c("Admission to same Hospital",
                            "Transferred to Other Hospital/Service")

discharge_wide_broad <- discharges_data %>%
  group_by(Month, m_date, year, m_num, HBT, TreatmentLocation,
           DepartmentType, Age) %>%
  summarise(
    total_attendances = sum(NumberOfAttendances, na.rm = TRUE),
    admitted = sum(NumberOfAttendances[Discharge %in% admission_labels_broad],
                   na.rm = TRUE),
    .groups = "drop") %>%
  mutate(not_admitted = total_attendances - admitted,
    t = as.numeric(m_date - min(m_date)) / 30)

# 3. Admission model data -----------------------------------------------------

# Narrow definition
admission_model_data <- discharge_wide %>%
  left_join(referral_wide_site_age,
            by = c("Month", "HBT", "TreatmentLocation", "DepartmentType",
                   "Age")) %>%
  left_join(when_site_monthly, by = c("Month", "HBT", "TreatmentLocation", 
                                      "DepartmentType")) %>%
  left_join(simd_hb_overall, by = c("HBT" = "HB")) %>%
  left_join(trt_loc, by = c("TreatmentLocation" = "TreatmentLocationCode"))

# Broad definition (for sensitivity comparison)
admission_model_data_broad <- discharge_wide_broad %>%
  left_join(referral_wide_site_age, by = c("Month", "HBT", "TreatmentLocation", 
                                           "DepartmentType","Age")) %>%
  left_join(when_site_monthly, by = c("Month", "HBT", "TreatmentLocation", 
                                      "DepartmentType")) %>%
  left_join(simd_hb_overall, by = c("HBT" = "HB")) %>%
  left_join(trt_loc, by = c("TreatmentLocation" = "TreatmentLocationCode"))

# 4. Checks -------------------------------------------------------------------

# admitted should never exceed total_attendances under either definition
admission_model_data %>% filter(admitted > total_attendances) %>% nrow()
admission_model_data_broad %>% filter(admitted > total_attendances) %>% nrow()

# Row counts and missing data summary
admission_model_data %>%
  summarise(n_rows = n(),
            n_missing_referral = sum(is.na(`pct_ref_Self Referral`)),
            n_missing_when = sum(is.na(oohours_pct)),
            n_missing_simd = sum(is.na(Income_rate)))

# Duplicate check - each Month x HBT x TreatmentLocation x DepartmentType x
# Age should appear exactly once
admission_model_data %>%
  count(Month, HBT, TreatmentLocation, DepartmentType, Age) %>%
  filter(n > 1)
                                                                                                                    
# Quick look at admission rates under both definitions
admission_model_data %>%
  summarise(total_attendances = sum(total_attendances),
            admitted_narrow = sum(admitted),
            pct_narrow = admitted_narrow/total_attendances * 100)

admission_model_data_broad %>%
  summarise(total_attendances = sum(total_attendances),
            admitted_broad = sum(admitted),
            pct_broad = admitted_broad/total_attendances * 100)
