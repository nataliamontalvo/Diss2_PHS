# Scottish A&E Attendance Analysis

## Project Purpose
 
This project analyses aggregate NHS Scotland Accident and Emergency (A&E)
data published by Public Health Scotland (PHS). It examines how demographic,
socioeconomic, geographic, and service-level factors are associated with
three stages of emergency-care utilisation:

1. **Attendance rates**: which population groups have the highest
   population-level A&E attendance rates?
   - Negative-binomial generalised additive models (GAMs)

2. **Hospital admission**: which factors are associated with hospital
   admission following A&E attendance?
   - Bayesian hierarchical binomial models fitted using INLA

3. **High-frequency attendance**: which groups have the highest probability
   of recording five or more A&E attendances within 12 months?
   - Quasibinomial generalised linear models (GLMs)

All source datasets contain aggregated counts rather than individual patient
records.

## Project Structure
 
### Data preparation
- `R/prep.R`: package loading, helper functions, data import, cleaning,
  SIMD aggregation, HB name mapping, age level standardisation. Sourced
  by all downstream scripts.
- `R/build_risk_data.R`: constructs the combined risk model dataset
  (Month × HB × Age × Sex × Deprivation) with year varying population
  denominator, SIMD domain indicators, and operational context variables.
  Sources `prep.R`.
- `R/build_admission_data.R`: constructs admission model datasets
  (narrow and broad definitions) from discharge and referral data.
  Sources `prep.R`.
- `R/build_mul_att_data.R`: constructs frequent attender model datasets
  (primary 5+ definition and sensitivity 2+ definition) with SIMD join.
  Sources `prep.R`.

### Analysis
- `01_eda.Rmd`: exploratory data analysis (plots covering attendance
  trends, demographic distributions, referral patterns, discharge outcomes,
  SIMD indicators, and multiple attendance composition)
- `02_risk_model.Rmd`: GAM attendance risk model: two final models 
  (A: deprivation quintile, B: domain indicators) 
- `03_admissions_model.Rmd`: GAM diagnostic exploration, INLA admission
  model. 
- `04_mul_att_model.Rmd`: GLM frequent attender model: baseline,
  overdispersion check, HBT and DepartmentType addition,
  Age×Deprivation interaction, sensitivity analyses

### Figures
- `figures/eda/`: EDA plots
- `figures/risk_model/`: risk model plots
- `figures/admissions_model/`: admission model plots
- `figures/mul_attendance/`: frequent-attender plots


## Data Sources

### Public Health Scotland A&E data
- **Monthly demographics**: https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/6abbf8e4-e4e0-4a56-a7b9-f7c7b4171ff3 
- **Monthly activity**: https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/37ba17b1-c323-492c-87d5-e986aae9ab59 
- **Weekly activity**: https://www.opendata.nhs.scot/dataset/weekly-accident-and-emergency-activity-and-waiting-times 
- **Treatment locations**: https://www.opendata.nhs.scot/dataset/nhs-scotland-accident-emergency-sites/resource/1a4e3f48-3d9b-4769-80e9-3ef6d27852fe
- **Referrals**: https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/235407ca-1676-472e-9e4d-6e7230934a95 
- **Discharges**: https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/c4622324-f59c-4011-a67b-83b59c59ca94
- **Multiple attendance**: https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/7f2e9288-5ea7-4d55-819d-dde4d211c72d
- **When data**: https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/022c3b27-6a58-48dc-8038-8f1f93bb0e78

### Population data
- **Data Zone (2011) Population Estimates**: https://www.opendata.nhs.scot/dataset/population-estimates/resource/c505f490-c201-44bd-abd1-1bd7a64285ee
- **Mid-2024 Population Estimates (HB-level density)**: https://www.nrscotland.gov.uk/publications/mid-2024-population-estimates-outdated/

### SIMD data
- **SIMD 2020v2 (quintiles and population weights)**: https://www.opendata.nhs.scot/dataset/78d41fa9-1a62-4f7b-9edb-3e8522a93378/resource/acade396-8430-4b34-895a-b3e757fa346e
- **SIMD 2020v2 (domain indicators)**: https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-indicator-data/

### HB boundaries shapefile
- **NHS Health Boards 2019**: https://maps.gov.scot/ATOM/shapefiles/SG_NHS_HealthBoards_2019.zip


## Running the Analysis
 
Each `.Rmd` file is self-contained, it sources its own data preparation
pipeline, so files can be knitted independently in any order.
 
**Prerequisites:**
1. Place all data files in the `data/` directory
2. The population estimates file must be downloaded separately (107MB,
   not tracked in git) from the URL above
**Note:** The INLA package is required for `03_admissions_model.Rmd` and
the INLA robustness check in `02_risk_model.Rmd`.

## Software

The analysis was conducted using R version 4.5.2 (2025-10-31 ucrt).