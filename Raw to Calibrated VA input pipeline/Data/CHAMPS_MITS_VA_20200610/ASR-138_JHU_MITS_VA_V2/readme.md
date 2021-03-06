

## CHAMPS VA-DeCoDe Data for JHU (version 2)

## Change Log
Number  | Change  |  Date
--|---|--
1.  | Added to **DeCoDe Results**: <br> Hosp_LOS 24h, Hosp LOS 48h, Hosp Gt 48h,Hosp LOS 72h,Hosp LOS Gt 72h, <br> GA Maternal Wks,GA Maternal Months,    GA Maternal Range,<br> GA Child Wks,GA Child Months,GA Child Method, <br> BW Maternal,BW Child, calc_postmortem_hrs, case_type_subcat  |  11-MAY-2020


## 1. 2016VA
File|Description
--|---
**asr114_va2016.csv**  |  Verbal Autopsy data in 2016VA version

Column_Name | Description
--|--
site_name  |  Site Name
champs_deid| De-Identified CHAMPS Case ID
M00024  |  1=MITS performed
case_type_desc  |  Age group of the CHAMPS Case
  -|  Rest of the columns are from the VA2016 survey results

## 2. 2012VA Perinatal Cases
File|Description
--|---
**dst_57_PerinatalVA2012.csv**  |  Perinatal Verbal Autopsy data from 2012VA. <br>*Note:* The survey columns will need to be mapped to the 2012VA variables

Column_Name | Description
--|--
site_name  |  Site Name  |  
champs_deid| De-Identified CHAMPS Case ID
M00024  |  1=MITS performed
case_type_desc  |  Age group of the CHAMPS Case
  -|  Rest of the columns are from the VA2012 survey results.  

## 3. 2012VA Non-PERINATAL Cases
  File|Description
  --|---
  **dst_57_ChildVA2012.csv**  |  Non-Perinatal Verbal Autopsy data from 2012VA. <br>*Note:* The survey columns will need to be mapped to the 2012VA variables

  Column_Name | Description
  --|--
  site_name  |  Site Name
  champs_deid| De-Identified CHAMPS Case ID
  M00024  |  1=MITS performed
  case_type_desc  |  Age group of the CHAMPS Case
  -| Rest of the columns are from the VA2012 survey results.


## 4. DeCoDe Results
File|Description
--|---
**asr_138_decode_results_classification.csv**  |  File contains the Determination of Cause of Death (DeCoDe) results coded in  ICD10 by the site. The Immediate COD, Underlying_Cause, Main_Maternal_Disease_Condition, Morbid conditions ICD10 codes are categorized by the CHAMPS Basic U5 Child Mortality Categorization scheme. Please refer to `CHAMPSICDMappings.csv` for mapping information.  


Column_Name | Description
--|--
site_name  |  Site Name
champs_deid| De-Identified CHAMPS Case ID
Immediate_COD|Immediate Cause of Death. <br> The Immediate Cause of Death determination under the revised rule is:<br> If `Underlying_Cause` is not present and `Morbid_Condition_01` is present Then `Immediate_COD` = `Morbid_Condition_01`
IC_ICD10_desc|Immediate Cause of Death ICD10 Description
IC_champs_group_desc|`Immediate_COD` mapped to CHAMPS Groupings
Underlying_Cause_calc| Calculated Underlying Cause or Factor based on the following rule applied on `Underlying_Cause`. <br>To be able to present perinatal/neonatal causes of death per ICD-10 and ICD-PM guidelines simultaneously the rule that defined the `Underlying_Cause_calc` for these cases was revised:<br> The `Underlying_Cause_calc` for perinatal cases now correctly reflects the main disease or condition in fetus or infant as defined in **ICD-PM** <br>~~for PERINATAL: <br> if Main_Maternal_Disease_Condition exists then  <br> Underlying_Cause_calc = Main_Maternal_Disease_Condition. <br> if Main_Maternal_Disease_Condition is blank and Underlying_Cause exists then  <br> Underlying_Cause_calc = Underlying_Cause <br>  for NON-PERINATAL: <br>  if Underlying_Cause is blank then Underlying_Cause_calc=Immediate_COD  <br> else Underlying_Cause_calc = Underlying_Cause  <br> ELSE  <br> if Underlying_Cause AND Main_Maternal_Disease_Condition is blank  <br> then Underlying_Cause_calc=Immediate_COD~~  |  
UC_ICD10_desc|`Underlying_Cause_calc` ICD10 Description.
UC_champs_group_desc | `Underlying_Cause_calc` mapped to CHAMPS Groups
mannerofdeath|
casetype|Age group of the CHAMPS Case.
Preventable|Could the death of this child have been prevented?
dateofpanelreview| Date when the DeCoDe Panel reviewed the case.
Underlying_Cause|Underlying Cause or Factor as recorded by the site [**No transformation applied**]
Main_Maternal_Disease_Condition|Main maternal disease/condition affecting fetus or infant
main_maternal_icd_pm_m_code| `Main_Maternal_Disease_Condition` mapped to ICD-PM
Morbid_Condition_01|Morbid condition 1 or other condition in fetus or infant
Morbid_Cond_01_champs_group_desc|`Morbid_Condition_01` mapped to CHAMPS Groups
Morbid_Condition_02|Morbid condition 2 or other condition in fetus or infant
Morbid_Cond_02_champs_group_desc|`Morbid_Condition_02` mapped to CHAMPS Groups
Morbid_Condition_03|
Morbid_Cond_03_champs_group_desc|
Morbid_Condition_04|
Morbid_Cond_04_champs_group_desc|
Morbid_Condition_05|
Morbid_Cond_05_champs_group_desc|
Morbid_Condition_06|
Morbid_Cond_06_champs_group_desc|
Morbid_Condition_07|
Morbid_Cond_07_champs_group_desc|
Morbid_Condition_08|
Morbid_Cond_08_champs_group_desc|
Other_Maternal_Condition_01|Other Maternal Condition 1
Other_Maternal_Condition_02|Other Maternal Condition 2
Other_Maternal_Condition_03|
Other_Maternal_Condition_04|
Other_Significant_Condition_01|Other significant conditions contributing to death 1
Other_Significant_Condition_02|Other significant conditions contributing to death 2
Other_Significant_Condition_03|
Other_Significant_Condition_04|
Other_Significant_Condition_05|
Other_Significant_Condition_06|
Other_Significant_Condition_07|
Other_Significant_Condition_08|
Other_Significant_Condition_09|
Other_Significant_Condition_10|
Immediate_Cause_of_Death_etiol1|Etiology/Agent (1) for `Immediate_COD`
Immediate_Cause_of_Death_etiol2|Etiology/Agent (2) for `Immediate_COD`
Immediate_Cause_of_Death_etiol3|Etiology/Agent (3) for `Immediate_COD`
Underlying_Cause_Factor_etiol1|Etiology/Agent (1) for `Underlying_Cause`
Underlying_Cause_Factor_etiol2|Etiology/Agent (2) for `Underlying_Cause`
Underlying_Cause_Factor_etiol3|Etiology/Agent (3) for `Underlying_Cause`
Morbid_Condition_01_etiol1|Etiology/Agent (1) for `Morbid_Condition_01`
Morbid_Condition_01_etiol2|Etiology/Agent (2) for `Morbid_Condition_01`
Morbid_Condition_01_etiol3|
Morbid_Condition_02_etiol1|Etiology/Agent (1) for `Morbid_Condition_02`
Morbid_Condition_02_etiol2|
Morbid_Condition_02_etiol3|
Morbid_Condition_03_etiol1|Etiology/Agent (1) for `Morbid_Condition_03`
Morbid_Condition_03_etiol2|
Morbid_Condition_03_etiol3|
Morbid_Condition_04_etiol1|Etiology/Agent (1) for `Morbid_Condition_04`
Morbid_Condition_04_etiol2|
Morbid_Condition_04_etiol3|
Morbid_Condition_05_etiol1|Etiology/Agent (1) for `Morbid_Condition_05`
Morbid_Condition_05_etiol2|
Morbid_Condition_05_etiol3|
Morbid_Condition_06_etiol1|Etiology/Agent (1) for `Morbid_Condition_06`
Morbid_Condition_06_etiol2|
Morbid_Condition_06_etiol3|
Morbid_Condition_07_etiol1|Etiology/Agent (1) for `Morbid_Condition_07`
Morbid_Condition_07_etiol2|
Morbid_Condition_07_etiol3|
Morbid_Condition_08_etiol1|Etiology/Agent (1) for `Morbid_Condition_08`
Morbid_Condition_08_etiol2|
Morbid_Condition_08_etiol3|
hosp_LOS_24h|Hospital Length of Stay (LOS) <=24 Hours
hosp_LOS_48h|Hospital Length of Stay (LOS) between >24H and <=48H
hosp_LOS_gt_48h  |  Hospital Length of Stay (LOS)  >48h
hosp_LOS_lt_72h  |  Hospital Length of Stay (LOS)  <= 72H
hosp_LOS_gt_72h  |  Hospital Length of Stay (LOS) > 72H
GA_maternal_wks| Gestational age at delivery in weeks (Source: Pregnancy Labor and Delivery)
GA_maternal_months|Gestational age at delivery in months Months (1-10) (Source: Pregnancy Labor and Delivery)
GA_maternal_range  |  Gestational age at delivery as a range (Source: Pregnancy Labor and Delivery) :<br>	CH01161	Preterm (< 37 weeks) <br> CH01162	Term (37-41 completed weeks) <br>CH01163	Post-term (42 weeks or greater)<br>CH00750	Unknown or Unavailable
GA_child_wks  |  Estimated gestational age (EGA) in weeks (Source: Past Medical And Birth History)
GA_child_months  |  If weeks unknown, EGA in months (Source: Past Medical And Birth History)
GA_child_method  |  Specify how EGA was determined (Source: Past Medical And Birth History):<br>CH01078	Last Menstrual Period<br>CH01079	First Trimester Ultrasound<br>CH01080	Second Trimester Ultrasound<br>CH01081	Third Trimester Ultrasound<br>CH01082	Fundal Height<br>CH01083	Mother's Recall<br>CH00750	Unknown or Unavailable
BW_maternal  |  Weight of baby at delivery (g) (Source: Perinatal Outcome)
BW_child  |  Birth weight (g) (Source: Past Medical And Birth History)
calc_postmortem_hrs  |  Calculated interval in hours between date and time of Death to MITS procedure
case_type_subcat  |  Further sub-Categorization of CHAMPS Case Types. (See `casetype`): <br> *neonate_72h* Neonate <=72Hours Age <br> *neonate_>72h* Neonate >72Hours Age <br> *infant_6M* Infant <= 6 Months of Age <br> *infant_>6M* Infant > 6 Months of Age.


## 5. CHAMPS Basic U5 Child Mortality Categorization
File|Description
--|---
**CHAMPSICDMappings.csv**  |  File contains the mapping between ICD10 Code and the CHAMPS Basic U5 Child Mortality Categorization.
