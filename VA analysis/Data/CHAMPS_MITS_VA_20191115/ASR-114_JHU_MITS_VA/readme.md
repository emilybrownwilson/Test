

## CHAMPS VA-DeCoDe Data as of June 2019.
## 1. 2016VA
File|Description
--|---
**dst_87_va2016.csv**  |  Verbal Autopsy data in 2016VA version

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
**dst_87_decode_results_classification.csv**  |  File contains the Determination of Cause of Death (DeCoDe) results coded in  ICD10 by the site. The Immediate COD, Underlying_Cause, Main_Maternal_Disease_Condition, Morbid conditions ICD10 codes are categorized by the CHAMPS Basic U5 Child Mortality Categorization scheme. Please refer to `CHAMPSICDMappings.csv` for mapping information.  


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


## 5. CHAMPS Basic U5 Child Mortality Categorization
File|Description
--|---
**CHAMPSICDMappings.csv**  |  File contains the mapping between ICD10 Code and the CHAMPS Basic U5 Child Mortality Categorization.
