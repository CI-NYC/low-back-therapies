# low-back-therapies

# Research Question
Associations between the use of non-opioid therapies, including spinal injections, for low back pain and opioid prescribing

## Study sample
Patients with low back pain

## Exclusions
The cohort found at `/mnt/general-data/disability/mediation-unsafe-pain-mgmt/analysis_df.rds` already applies all exclusions except for:
- Must have low back pain. "Back pain" category exists in ([ICD pain codes file](https://github.com/CI-NYC/disability/blob/main/projects/create_cohort/input/ICD_codes/chronic_pain_icd10_20230216.csv)). But need a way to subset it to just low back pain. *Perhaps `grepl("low back",...)` will do?*

Adjust age range to 18-65


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

## Treatments
Calculate during months 7-12. These may already exist in `/mnt/general-data/disability/mediation-unsafe-pain-mgmt/mediator_df.rds`
- non-opioid therapies
    - spinal injections and others
    - Other than phyical therapy, currently these are all combined as mediator_has_multimodal_pain_treatment_restrict. *Should spinal injections also be separated?*
- Max daily dose MME `mediator_max_daily_dose_mme`
- Proportion of days covered `mediator_opioid_days_covered`
- Unique prescribers `mediator_prescribers_6mo_sum`
- Tapering `mediator_has_tapering`
- Opioid coprescribed with benzodiazepine `mediator_opioid_benzo_copresc`
- Opioid coprescribed with stimulants `mediator_opioid_stimulant_copresc`
- Opioid coprescribed with muscle relaxants `mediator_opioid_mrelax_copresc`
- Opioid coprescribed with gabapentenoids `mediator_opioid_gaba_copresc`
- Nonopioid presription medications for pain `mediator_nonopioid_pain_rx`
- Physical therapy `mediator_has_physical_therapy`
- Multimodal pain treatment `mediator_has_multimodal_pain_treatment_restrict`


## Outcomes
- prolonged opioid use
    - Wait to hear back from Lisa on how this should be defined
    - whether they are continuing to use opioids after 3 months? 6 months? 12 months?
 
    - Possible:
    - at least 1 opioid prescription fulfillment between 90 and 180 days after the procedure
    - "any opioid use pattern occurring 3 months after surgery. An exception is use of the term chronic opioid use. Based on previous literature,2,11,37-40 we defined chronic opioid use as the receipt of at least 10 opioid prescription fills, at least 90 consecutive days’ supply of opioids, or 120 cumulative days in the first year after surgery, excluding the initial 90 postoperative days" (https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2767637)
    - Other definitions (https://pmc.ncbi.nlm.nih.gov/articles/PMC7058495/#:~:text=More%20than%2090%20days%20of,d%20of%20the%20first%20prescription.&text=Preoperative%20long%2Dterm%20use%20was,during%20the%20year%20before%20surgery.&text=No%20explicit%20definition.)
    - more than 1 year supply of opioids within a 3-year follow up (equivalent to 6 months supply in 1.5 year follow up)
    - Looking for just 1 opioid does not accurately reflect consistent use. Utilizing days supply seems to be effective and prevalent in previous research

## Analysis

- Intervene on non-opioid therapies, multimodal, and spinal injections by comparing the risk of ___ when all patients are set to 1 vs the risk of ___ using observed values
- Opioid treatments might not be intervened on


