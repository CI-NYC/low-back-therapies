# low-back-therapies

# Research Question
Associations between the use of non-opioid therapies, including spinal injections, for low back pain and opioid prescribing

## Study sample
Patients with low back pain

## Exclusions
The cohort found at `/mnt/general-data/disability/mediation-unsafe-pain-mgmt/analysis_df.rds` already applies all exclusions except for:
- Must have low back pain. "Back pain" category exists in ([ICD pain codes file](https://github.com/CI-NYC/disability/blob/main/projects/create_cohort/input/ICD_codes/chronic_pain_icd10_20230216.csv)). But need a way to subset it to just low back pain. *Perhaps `grepl("low back",...)` will do?*

Additionally, the treatments-risk paper excludes those younger than 35 ([GitHub](https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/05_create_cohort/06mo/01_create_final_analysis_cohort.R)). *Might expand this to include 18-64?*

## Baseline covariates 
Calculate during first 6 months (washout period). These already exist in `/mnt/general-data/disability/mediation-unsafe-pain-mgmt/mediator_df.rds`

-  Age
-  Sex
-  race/ethnicity
-  primary language (English)
-  marital status
-  household size
-  veteran status
-  income > 138% of Federal Poverty Level
-  bipolar disorder
-  anxiety disorder
-  attention deficit hyperactivity disorder
-  depressive disorder
-  other mental disorder
-  mental helth counseling

## Pain/disability group (*is this needed?*)
Use the washout period to group patients into:
1. low back pain alone
2. low back pain and physical disability

## Treatments
Calculate during months 7-12. These may already exist in `/mnt/general-data/disability/mediation-unsafe-pain-mgmt/mediator_df.rds`
- non-opioid therapies
    - spinal injections
    - counseling?
    - non-opioid rx?
    - physical therapy?
    - counseling?
    - acupuncture?
    - anything else?

## Outcomes
- prolonged opioid use
    - Wait to hear back from Lisa on how this should be defined
    - whether they are continuing to use opioids after 3 months? 6 months? 12 months?


