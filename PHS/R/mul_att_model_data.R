
# =============================================================================
# Build Multiple Attendance model data
# =============================================================================
#
# Purpose:
#   Build the frequent attender model data among people who
#   attended A&E at least once in a 12 month period, what demographic
#   factors are associated with becoming a frequent attender (5+ visits)?
#
# Note: This script sources "R/prep.R" and therefore assumes that all raw data
#   files and paths required by prep.R are available.
# 
# =============================================================================

source("R/prep.R")


# 1. Merged SIMD data ---------------------------------------------------------

# Multiple attendances data set with all variables pre-factored.
# Outcome: frequent (5+ visits) vs not frequent (1-4 visits)
mul_model_data <- mul_att_data %>%
  filter(!is.na(Age), Sex %in% c("Male", "Female"),
         Deprivation %in% 1:5,
         !is.na(HBT), !is.na(DepartmentType)) %>%
  mutate(
    frequent = FivePlusAttendances,
    not_frequent = OneAttendance + TwoAttendances +
                   ThreeAttendances + FourAttendances,
    total_patients = frequent + not_frequent,
    
    # Factor variables with consistent levels
    Age = factor(Age, levels = age_levels),
    Sex = factor(Sex),
    Deprivation = factor(Deprivation),
    HBT = factor(HBT),
    DepartmentType = factor(DepartmentType)
  ) %>%
  filter(total_patients > 0)


# Alternative outcome: repeat attender (2+ visits) vs single attender (1 visit)

mul_model_data_2 <- mul_att_data %>%
  filter(!is.na(Age), Sex %in% c("Male", "Female"),
         Deprivation %in% 1:5,
         !is.na(HBT), !is.na(DepartmentType)) %>%
  mutate(
    repeat_att = TwoAttendances + ThreeAttendances +
                 FourAttendances + FivePlusAttendances,
    not_repeat_att = OneAttendance,
    total_patients = repeat_att + not_repeat_att,
    
    Age = factor(Age, levels = age_levels),
    Sex = factor(Sex),
    Deprivation = factor(Deprivation),
    HBT = factor(HBT),
    DepartmentType = factor(DepartmentType)
  ) %>%
  filter(total_patients > 0)


# 3. Domain indicators join ---------------------------------------------------

# Join SIMD domain indicators
mul_model_data_simd <- mul_model_data %>%
  mutate(Deprivation_int = as.integer(as.character(Deprivation))) %>%
  left_join(simd_hb %>% select(HB, Quintile, Income_rate, EMERG,
                               crime_rate, drive_GP),
            by = c("HBT" = "HB", "Deprivation_int" = "Quintile"))


# 4. Checks -------------------------------------------------------------------

# Period
cat("Snapshot period:", unique(mul_att_data$YearEnd), "\n")

# Row counts
cat("Primary model data rows:", nrow(mul_model_data), "\n")
cat("Sensitivity (2+) data rows:", nrow(mul_model_data_2), "\n")
cat("SIMD-joined data rows:", nrow(mul_model_data_simd), "\n")

# Confirm row counts match between primary and sensitivity
# (should be identical — same filters, different outcome columns)
stopifnot(nrow(mul_model_data) == nrow(mul_model_data_2))

# Overall frequent attender rate
cat("Total patients:", sum(mul_model_data$total_patients), "\n")
cat("Frequent attenders (5+):", sum(mul_model_data$frequent), "\n")
cat("Frequent rate:",
    round(sum(mul_model_data$frequent)/
            sum(mul_model_data$total_patients) * 100, 1), "%\n")

# Repeat attender rate
cat("Repeat attenders (2+):", sum(mul_model_data_2$repeat_att), "\n")
cat("Repeat rate:",
    round(sum(mul_model_data_2$repeat_att)/
            sum(mul_model_data_2$total_patients) * 100, 1), "%\n")

# SIMD join: rows lost to island-board quintile gaps
cat("Rows with missing SIMD indicators:",
    sum(is.na(mul_model_data_simd$Income_rate)), "\n")

# No zero/negative total_patients (would break cbind)
stopifnot(all(mul_model_data$total_patients > 0))
stopifnot(all(mul_model_data_2$total_patients > 0))
