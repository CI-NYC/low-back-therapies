# Associations between initial treatments for acute low back pain and opioid use disorder and overdose risk in Medicaid patients

Placeholder for full citation of the paper.

In this study, we examined an opioid-naïve adult Medicaid population with newly diagnosed acute low-back pain and estimated the association between initial treatment modality and subsequent risk of new-onset OUD and overdose diagnosis.

---

## Repository Structure

| Files and Directories | Description |
|-----------|----------|
| [data/](data)     | Codelists (ICD-10, CPT, ATC, NDC) for covariates, exposure, and outcome variables   |
| [scripts/](scripts)  | Cohort creation  |
| [R/](R)        | Reusable functions  |
| [figures/](figures)  | Result figures  |
| [renv.lock](renv.lock)  | List of required package versions |

Raw data not included due to privacy restrictions.

---

## Who is included in the cohort?

Patients included in this cohort have an ICD-10 diagnosis code for low back pain, defined as one of the following codes found in the other services file, excluding those diagnosed in an inpatient or residential setting. See [here](scripts/00_create_cohort/00_filter_diagnosis_claims.R).

Patients must receive pain treatment within 1 month of diagnosis. [Pharmacologic treatments](scripts/00_create_cohort/03_treatment_dts/04_nonopioid_pain_rx.R) include acetaminophen, anti-inflammatories, benzodiazepines, gabapentinoids, duloxetine, muscle relaxants, steroids, and [opioid analgesics](scripts/00_create_cohort/03_treatment_dts/03_cohort_mme_join.R) (excluding opioid formulations indicated for OUD treatment). [Non-pharmacologic treatments](scripts/00_create_cohort/03_treatment_dts/05_non_pharmacologic.R) include chiropractic therapy, physical therapy, massage therapy, and other interventions including ablative techniques, botulism toxin injections, electrical nerve stimulation, intrathecal drug therapies, epidural steroids, and minimally invasive spinal procedures. In the primary analysis, treatments are collected from day 0 (first treatment date) to day 30. Sensitivity analyses are also conducted, re-defining the initial treatment period to be all treatments within 30/7 day gaps, up until a max of 3 months.

Participants must be continuously enrolled in Medicaid for the 6-month washout period ([here](scripts/00_create_cohort/04_01_filter_continuous_enrollment.R) and [here](scripts/00_create_cohort/04_02_filter_continuous_enrollment.R)) and be [opioid-naive](scripts/00_create_cohort/05_opioid_naive_exclusions.R) in the washout period.

Participants are also excluded if they were diagnosed with any other pain condition during the 3 months preceding day 0, or having any history of: OUD diagnosis, overdose, medication for treating OUD, opioid prescription, pregnancy, dual eligibility for Medicaid and Medicare, cancer, or institutionalization.

The final dataset includes [14 exposure columns](scripts/00_create_cohort/05_exposure/09_combine_exposures.R), a binary column for each of the treatments above, with opioid further specified as Opioid <= 7 days supply and <= 50mme, Opioid > 7 days and <= 50mme, or Opioid > 50mme. 

Exposure columns: 


The outcome was [new OUD and overdose diagnosis](scripts/00_create_cohort/06_outcomes/01_oud.R), evaluated during months 2-7 (6 months of follow-up, *period 1*) and 2-13 (12 months of follow-up, *period 2*). Individuals were censored ([here](scripts/00_create_cohort/06_outcomes/05_getting_enrollment_dates.R), [here](scripts/00_create_cohort/06_outcomes/06_censoring_enrollment.R) and [here](scripts/00_create_cohort/06_outcomes/07_censoring_combined.R)) at loss of Medicaid enrollment, Medicare eligibility, death (if recorded), or December 31, 2019.
 
Outcome columns:

[Baseline covariates](scripts/00_create_cohort/08_baseline_covariates.R) are collected during the 6-month washout period for age, sex, race and ethnicity, primary language, marital status, household size, veteran status, income category, whether or not they received Temporary Assistance for Needy Families (TANF) benefits or Disability Insurance (SSDI) benefits. Clinical covariates included alcohol use disorder, other substance use disorder, and any inpatient or outpatient diagnosis of psychiatric disorders including diagnoses of bipolar disorder, anxiety disorder, major depressive disorder, attention-deficit/hyperactivity disorder, or other psychiatric disorder, and whether they received any mental health counseling. Health system utilization measures included the number of inpatient hospitalizations, outpatient visits, and emergency department visits during the washout period.

Covariates:

---

## Reproducing the Analysis

Steps for reproducing the workflow:

1. Clone the repository  
   git clone https://github.com/CI-NYC/low-back-therapies.git

2. Run `renv.init()` in R console. This will download the required packages.

3. Run the cohort creation scripts in "scripts/00_create_cohort" in numbered order *(temporary until file reorganization: this is the cohort for the sensitivity analyses - 30/7 day allowable gap between treatments)*.

4. Here are the details for the different scripts in the analysis directory:

| Script | Modifiable parameters &nbsp;&nbsp; | Description |
|-----------|----------|-----------|
| run_main_on | run_index <- 1 | Estimate relative risk of incident OUD over **6** months of follow-up hypothetically **adding a treatment** to initial treatment combination and no disenrollment |
| run_main_on | run_index <- 2 | Estimate relative risk of incident OUD over **12** months of follow-up hypothetically **adding a treatment** to initial treatment combination and no disenrollment |
| run_main_off | run_index <- 1 | Estimate relative risk of incident OUD over **6** months of follow-up hypothetically **removing a treatment** from initial treatment combination and no disenrollment |
| run_main_off | run_index <- 2 | Estimate relative risk of incident OUD over **12** months of follow-up hypothetically **removing a treatment** from initial treatment combination and no disenrollment |
| no_cens | run_index <- 1 | Estimate risk of incident OUD over **6** months of follow-up **holding treatments as observed** and no disenrollment |
| no_cens | run_index <- 2 | Estimate risk of incident OUD over **12** months of follow-up **holding treatments as observed** and no disenrollment |

Different cohorts will need to be loaded for different analyses. "Version" identifies where the results for the different analyses are saved. These are relevant in `01_lmtp.R` and `04_no_cens.R`.

```
# sensitivity analysis cohort with 30-day gap between treatments
data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final"))
version <- "opioid_categorized"

# sensitivity analysis cohort with 7-day gap between treatments
data <- load_data("pain_cohort_clean_imputed_7day_gap.fst", file.path(drv_root, "final")) 
version <- "sensitivity"

# (primary analysis) cohort with 30 day exposure
data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root_30_day_treatment, "modified_final"))
version <- "30_day_exposure"
```

5. Run 05_results.R, modifying "version" as needed. This corresponds to the *on-vs-off* analysis.

6. Run 05_results_no_cens.R, modifying "version" as needed. This corresponds to the *on-vs-observed* analysis.

7. Repeat steps 4-6 for the sensitivity analysis, except using `06_results_sensitivity.R` and `06_results_sensitivity_no_cens.R` for the figures


