
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CGMProcessing
<!-- badges: start -->
<!-- badges: end -->

The goal of {CGMprocessing} is to provide functions for preparing, cleaning and analysing Continuous Glucose Monitoring (CGM) data aligned with concensus definitions. The functions in the package were developed from  Dexcom G4 data with 5 minute intervals and was specifically developed to enable processing and analysing of data from the 2 separate Randomised Control Trials (RCT) investigating exercise in type 1 diabetes lead by Professor Rob Andrews and Dr Parth Narendran See [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7317834/) for published information about the EXTOD Education program and [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7317834/) for the published EXTOD RCT. 

Glycemic output variables and definitions are aligned with the [International Concensus on Use of Continuous Gucose Monitoring](https://care.diabetesjournals.org/content/40/12/1631).

This code was developed based off work by [T. Vigers](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0216851)


## File Structure
- **/data-raw**: raw data from sensor
- **/data-preprocessed**: Exeter specific preprocessed data from sensor
- **/data-clean**: data ran through cleanCGM() function
- **/CGMupload**: Output csv of glycemic metrics generated from analysedCGM() function  

❗**Important**: As this was developed for analysis of RCT data files preprocessed are outputted with the filename formatted as _ID_timepoint.csv_ based on the excel sheet name. If dealing with RCT data please ensure files are named in this way before running through cleanCGM() and analyseCGM() pipeline. Edit scripts if neccesary if not dealing with RCT data❗



## Pre-processing of CGM files (Relevent to Exeter in house data)
___
**Functionality**: The script **prepare_spitsheets.R** is a rough script pipeline (non-function) that prepares files from EXTOD and EXTOD education for further processing with functions part of {CGMprocessing}. Chunks of traces with non-consecutive dates are separated with **wrongstart** appended to traces prior to the final consecutive chunk. The final chunk with consecutive dates is outputted to **/data-preprocessed**. Manual inpection is recommended at this point to inspect and remove any **wrongstart** files from this folder.
___

- **cgmvariable_dictionary.xlsx** is used to rename variables of interest. This should be updated for variable names of other sensors and is integrated into cleanCGM() function. Final variable names should be **id**, **timestampfp** ,**fingerprickglucose**, **timestamp**, **sensorglucose** defined as below. See [cleanCGM section](#cleancgm) for further detail.

- Files from preprocessing are outputted with changed CGM variable names already and reformatted slightly (conversion of HI/LO etc.) compared to raw data as below:

**Table**: Preprocess script output
| id    | timestampfp      | fingerprickglucose | timestamp        | sensorglucose |
|-------|------------------|--------------------|------------------|---------------|
| 10013 | 14/06/2013 13:24 | 8.88               | 14/06/2013 13:54 | 10.88         |
| 10013 | 14/06/2013 13:50 | 12.49              | 14/06/2013 13:59 | 11.27         |
| 10013 | 14/06/2013 18:18 | 8.32               | 14/06/2013 14:04 | 11.32         |
| 10013 | 15/06/2013 17:42 | 6.22               | 14/06/2013 14:09 | 11.43         |
| 10013 |                  |                    | 14/06/2013 14:14 | 11.54         |
| 10013 |                  |                    | 14/06/2013 14:19 | 11.6          |
| 10013 |                  |                    | 14/06/2013 14:24 | 11.76         |

- Files are named as **ID_timepoint.csv** based on the excel sheet name

❗**Important**: **Glucose readings must be in mmmol/l. Manually change files in raw excel files. More information on conversion found [here](https://www.diabetes.co.uk/blood-sugar-converter.html)❗


## cleanCGM
___
**Functionality:** **cleanCGM()** is a function written to clean CGM data for simpler file outputs and perform (optional) calibration against fingerstick SMBG values developed of Dexcom G4 data. 

❗**Before you begin**:❗
  - ❗Files should be named as _ID_optional.ext_ 
  - ❗Update **cgmvariable_dictionary.xlsx** with names of variables specific to sensor
  - ❗Glucose readings must be in mmmol/l. Manually change files in raw excel files. More information on conversion found [here](https://www.diabetes.co.uk/blood-sugar-converter.html)
  - ❗Inspect raw data for blank rows before row of variables. Within the code at the point of file load in the parametier _skip_ can be added to skip such rows. For example for Libre data we are required to skip 2 rows this is outlined below by _skip=2_.
  
  ```{r eval=FALSE, echo=T}
 table <-  rio::import(files[f], skip =2 ,guess_max = 10000000)
```
 
___
- Function can take raw files from Dexcom, Libre or previosuly preprocessed from an input folder directory. Files can be of any format, csv is preferred. ie in funtion call folder **data-raw**

- **cgmvariable_dictionary.xlsx** is used to rename variables of interest. This should be updated for variable names of other sensors and is integrated into this cleanCGM() function. Final variable names should be **id**, **timestampfp** ,**fingerprickglucose**, **timestamp**, **sensorglucose** defined in Table below.


**Table**: Definitions of the final variables
| variable           | definition                                                  |
|--------------------|-------------------------------------------------------------|
| id                 | Patient ID or sensor ID                                     |
| timstampfp         | Finger stick SMBG timestamp (optional and sensor dependent) |
| fingerprickglucose | Finger stick SMBG value                                     |
| timestamp          | Sensor glucose timestamp                                    |
| sensorglucose      | Sensor glucose value                                        |

**Table**: Dictionary for renaming old variables in raw CGM data files, edit as required
| old_vars                 | new_vars           |
|--------------------------|--------------------|
| id                       | id                 |
| date_and_time_event_4    | timestampfp        |
| date_and_time_event_5    | timestampfp        |
| finger_prick_glucose     | fingerprickglucose |
| date_and_time_event_7    | timestamp          |
| cgms_glucose_reading     | sensorglucose      |
| displaytime              | timestampfp        |
| value                    | fingerprickglucose |
| displaytime3             | timestamp          |
| value4                   | sensorglucose      |
| glucosedisplaytime       | timestamp          |
| glucosevalue             | sensorglucose      |
| meterdisplaytime         | timestampfp        |
| metervalue               | fingerprickglucose |
| serial.number            | id                 |
| meter.timestamp          | timestamp          |
| record.type              | recordtype         |
| historic.glucose.mmol.l. | sensorglucose      |
| Scan.Glucose.mmol.L.     | scanglucose        |


- Text Low/High are in filled with the min/max limits depending on the inputted sensor type

- Calibration is performed againsted logged fingerstick SMBG readings and nearest 15 min later sensor reading. Calibration excluded 1) whole traces if 2 blood glucose calibrations were not completed at the start of the sensor wear, 2) a day of wear if the MARD of the sensor glucose and blood glucose calibration on that day is >20% or if <2 blood glucose calibrations were completes on that day. This can be set to false for CGM. 

- If calibration check is TRUE then the calibration table of fingerstick SMBG matched to nearest 15 min later sensor glucose with the correlation (checking there were 2 fingersticks per day) and the MARD between the sensor and fingerstick with be output to the specified calibrationoutput directory

---
👷‍♀️ **_FOR DEVELOPMENT_**👷‍:_Libre sensors store glucose every 15 mins, in order for analyseCGM to work based consensus CGM analysis [here](https://care.diabetesjournals.org/content/40/12/1631) we must make the 15 min intervaltimeseries data into 5 min interval data. Currently cleanCGM() handles this with the line below, adding dummy 5 min data by adding 2 rows after every original row that is the same as the original row :_ 

```{r eval=FALSE, echo=T}
table<-slice(table,rep(1:n(), each = 3))
```

---

- Each file outputed should look like the below table:

**Table**: Cleaned CGM output
| id   | timestamp        | sensorglucose | Date       | percent_cgm_wear | percentage_expected_wear | percenageexpectedwear_7daycut |
|------|------------------|---------------|------------|------------------|--------------------------|-------------------------------|
| 1010 | 20/09/2018 15:20 | 8.27          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:25 | 8.38          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:30 | 8.1           | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:35 | 8.16          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:40 | 8.38          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:45 | 8.44          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:50 | 7.88          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |
| 1010 | 20/09/2018 15:55 | 7.94          | 20/09/2018 | 92.2418107       | 98.4580026               | 98.4580026                    |

## analyseCGM
___
**Functionality:** **analyseCGM()** is a function written to create concensus glycemic metrics based off definitions outlined [here](https://care.diabetesjournals.org/content/40/12/1631). Fuction takes files from the **data-clean** folder where the output of cleanCGM() is stored.
___

🖋️ **NOTE**: There is an paramater for exercise analysis specified in this function specific to in house Exeter/Liverpool processing of data. This additional option is used to analyse specific files of exercise aligned glucose that were not ran through the cleanCGM() function. Keep parameter as FALSE.🖋️

- For calculation of time spent variables data is checked to be consecutive. If timestamps are >20 min apart a missing row is added to the table to prevent events from runnning on if the time gap is >20 min.

- Time spent variables are created for:
  - Above 10, 13.9, 16
  - Below 3, 
  - Range 3-<3.9, 3.9-10
  - Hyperglycemia (at levels >10 and >13.9)
  - Hypoglycemia

- The original equation of HBGI/LBGI [here](https://pubmed.ncbi.nlm.nih.gov/9353603/) performs symmetrization using the assumption that the minimum glucose value as 1.1 and maximum as 33.3 to derive the coefficients in the equation. However this only correct for the SMBG monitors and if CGM sensor limits are different this violates the assumption of the deriviation and the coefficients that are used as a "standard" are incorrect and caused underesimation of risk. If we want to be able to compare LBGI/HBGI between sensors we therefore have to rederive the a,b y corefficents in the equation based on the specific sensor limits.

- Hyper/hypoglycemia are defined as excursions. The start of an excursion is going above/below the specified value for 15 mins. If this doesn't happpen it is not defined as an excursion and isn't included in the hyper/hypo time spent, but will be included in the general time spent for the defined time spent variables. 

___
🖋️ **NOTE**:  

In this function Hypoglycemia is defined as: 

_Beginning of a CGM event_: readings below 3 mmol/l for 15 min defines a clinically significant hypoglycemic event. Code checks if 4 consecuative rows are below 3 (4 rows in total: row 1 detected below 3mmol/l the next 3 rows = 15 mins with 1 row being assumed as a 5 min reading). If this is the case then the first isantace of dropping below 3 is marked as the start row of a true hypoglycemia event

_End of a CGM event_: readings for 15 min at ≥ 3.9 mmol/l. From the start row of the true hypoglycemic event defined above the code checks if the glucose in consecutive rows is ≥ 3.9 mmol/l and marks these values as hypoglycemia. At the row glucose becomes > 3.9 the code checks if the next 3 rows remain > 3.9 mmol/l (4 rows in total: row 1 detected > 3.9 mmol/l the next 3 rows = 15 mins with 1 row being assumed as a 5 min reading). If glucose dips below 3.9 again within these 4 rows then the event does not end. If the glucose stays above 3.9 for these 4 rows the event ends at the end of the last row ie. including the 15 min of remaining above 3.9 mmol/l in the hypoglycemia time.     

If a missing row is present (inserted because the consecuative timestamps had a gap >20 min) the hypoglycemic event has to end at this point).
🖋️
___

- All CGM variables generated by analyseCGM() are detailed  below: 

**Table**: CGM metrics generated for each individual
| Output variable                                    | Description                                                                                                                                                                                                                                                                                                                                       |
|----------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| subject_id                                         | Patient ID                                                                                                                                                                                                                                                                                                                                        |
| totaltime_mins                                     | Total time in the table, this is caluclted from the number of rows present, assuming each row is 5 min reading NOT max and min as this would not be accurate if there were gaps in the data                                                                                                                                                       |
| start_cgm_analysis                                 | min datetime in table                                                                                                                                                                                                                                                                                                                             |
| end_cgm_analysis                                   | max datetime in table                                                                                                                                                                                                                                                                                                                             |
| interval                                           | Most common interval in the data, for CGM this is 300 seconds (5min) for libre this is put to 900 seconds (15 min)                                                                                                                                                                                                                                |
| num_days_good_data                                 | Total time / (24*60*60))                                                                                                                                                                                                                                                                                                                          |
| num_hrs_good_data                                  | Total time / 3600                                                                                                                                                                                                                                                                                                                                 |
| total_sensor_readings                              | Total number of rows in the table                                                                                                                                                                                                                                                                                                                 |
| percent_cgm_wear                                   | Calculated in cleanCGM() this is the amount of data left post calibration (ie. When dates may have been removed)                                                                                                                                                                                                                                  |
| percentage_expected_wear                           | Calculated in cleanCGM() Before cutting data at 7 days this is the amount of data we have vs what we expected based on expected 7 days of wear (before calibration)                                                                                                                                                                               |
| percentage_expected_wear_aftersensorlifetimecutoff | Calculated in cleanCGM() When we have cut data off at 7 days. This is the amount of data we have vs what we expected based on expected 7 days of wear (before calibration)                                                                                                                                                                        |
| average_sensor                                     | Average sensor glucise                                                                                                                                                                                                                                                                                                                            |
| estimated_a1c                                      | Estimated Hba1C based on hba1c equation and mean glucose                                                                                                                                                                                                                                                                                          |
| gmi                                                | Glucose Management Indicator inndicates the average Hba1C level that would be expected based on mean.  gmi and estimated Hba1C should therefore be similar                                                                                                                                                                                        |
| q1_sensor                                          | Lower quartile sensor glucose                                                                                                                                                                                                                                                                                                                     |
| median_sensor                                      | Median sensor glucose                                                                                                                                                                                                                                                                                                                             |
| q3_sensor                                          | upper quartile sensor glucose                                                                                                                                                                                                                                                                                                                     |
| standard_deviation                                 | Standard deviation of sensor glucose                                                                                                                                                                                                                                                                                                              |
| cv                                                 | Coefficient of variation. NOTE: SD is highly influenced by the mean glucose – someone with a higher mean glucose will have a higher SD. The CV divides the SD/mean x100. This division helps “correct” and normalize glucose variability, allowing us to set a single variability goal that applies to people with different mean glucose levels. |
| min_sensor                                         | Minimum sensor glucose                                                                                                                                                                                                                                                                                                                            |
| max_sensor                                         | Maximum sensor glucose                                                                                                                                                                                                                                                                                                                            |
| excursions_over_10                                 | Count of number of times glucose went above 10 mmol/l for 15 mins (excursion start defined as above  threshold fpr 15 mins)                                                                                                                                                                                                                       |
| min_spent_excursion_over_10                        | Time in mins spent in excursions above 10 mmol/l (excursion start defined as above threshold fpr 15 mins)                                                                                                                                                                                                                                         |
| percent_time_excursion_over_10                     | Percentage of time spent in excursions above 10 mmol/l in relation to total time of the sensor calculated previously   (excursion defined as above  threshold fpr 15 mins)                                                                                                                                                                        |
| excursions_over_13                                 | Count of number of times glucose went above 13.9 mmol/l for 15 mins (excursion start defined as above  threshold fpr 15 mins)                                                                                                                                                                                                                     |
| min_spent_excursion_over_13                        | Time in mins spent in excursions above 13.9 mmol/l (excursion start defined as above threshold fpr 15 mins)                                                                                                                                                                                                                                       |
| percent_time_excursion_over_13                     | Percentage of time spent in excursions above 13.9 mmol/l in relation to total time of the sensor calculated previously   (excursion defined as above  threshold fpr 15 mins)                                                                                                                                                                      |
| excursions_over_16                                 | Count of number of times glucose went above 16 mmol/l for 15 mins (excursion start defined as above  threshold fpr 15 mins)                                                                                                                                                                                                                       |
| min_spent_excursion_over_16                        | Time in mins spent in excursions above 16 mmol/l (excursion start defined as above threshold fpr 15 mins)                                                                                                                                                                                                                                         |
| percent_time_excursion_over_16                     | Percentage of time spent in excursions above 16 mmol/l in relation to total time of the sensor calculated previously  (excursion defined as above  threshold fpr 15 mins)                                                                                                                                                                         |
| hypo_under_3_prolonged                             | Count of number of times hypoglycemic episode (as defined see below) was > 2 hours                                                                                                                                                                                                                                                                |
| hypo_under_3                                       | Count of number of hypoglycemic episodes. Clinically significant hypoglycemic event excursion begins as readings below 3 mmol/l for 15 min, ending when readings for 15 min at ≥ 3.9 mmol/l. See below NOTE for more information.                                                                                                                 |
| min_spent_under_hypo3                              | Time in mins spent in hypoglycemia episode                                                                                                                                                                                                                                                                                                        |
| percent_time_under_hypo3                           | Percentage of time spent in hypoglycemic episodes in relation to total time of the sensor calculated previously                                                                                                                                                                                                                                   |
| min_spent_<3                                       | Time in mins spent below 3 mmol/l                                                                                                                                                                                                                                                                                                                 |
| percent_time_<3                                    | Percentage of time spent below 3 mmol/l in relation in relation to total time of the sensor calculated previously                                                                                                                                                                                                                                 |
| min_spent_3_3.8                                    | Time in mins spent between  3-3.9 mmol/l                                                                                                                                                                                                                                                                                                          |
| percent_time_3_3.8                                 | Percentage of time spent between 3-3.9 mmol/l in relation in relation to total time of the sensor calculated previously                                                                                                                                                                                                                           |
| min_spent_3.9_10                                   | Time in mins spent between  3.9-10 mmol/l                                                                                                                                                                                                                                                                                                         |
| percent_time_3.9_10                                | Percentage of time spent between 3.9-10 mmol/l in relation in relation to total time of the sensor calculated previously                                                                                                                                                                                                                          |
| min_spent_3.9_7.8                                  | Time in mins spent between  3.9-7.8 mmol/l                                                                                                                                                                                                                                                                                                        |
| percent_time_3.9_7.8                               | Percentage of time spent between 3.9-7.8 mmol/l in relation in relation to total time of the sensor calculated previously                                                                                                                                                                                                                         |
| min_spent_over10                                   | Time in mins spent above 10 mmol/l                                                                                                                                                                                                                                                                                                                |
| percent_time_over10                                | Percentage of time spent above 10 mmol/l in relation in relation to total time of the sensor calculated previously                                                                                                                                                                                                                                |
| min_spent_over13                                   | Time in mins spent above 13.9 mmol/l                                                                                                                                                                                                                                                                                                              |
| percent_time_over13                                | Percentage of time spent above 13.9  mmol/l in relation in relation to total time of the sensor calculated previously                                                                                                                                                                                                                             |
| total_auc                                          | Total area under the glucose curve                                                                                                                                                                                                                                                                                                                |
| r_mage                                             | Mean Amplitude Glycemic Excursion. Option to asses average of the differences greater than either  entire dataset SD, 2SD, etc                                                                                                                                                                                                                    |
| j_index                                            | Combination of information from mean and SD of all glucose values doi: 10.1055/s-2007-979906.                                                                                                                                                                                                                                                     |
| conga_1                                            | Continuous overlapping net glycemic action, with _n appended as the indicated number of hours being assessed (in this case 1 hour)                                                                                                                                                                                                                |
| modd                                               | Mean of daily difference                                                                                                                                                                                                                                                                                                                          |
| lbgi                                               | Low Blood Glucose Index, equation is for glucose in mmol/l (dc1386 appendix)                                                                                                                                                                                                                                                                      |
| hbgi                                               | High Blood Glucose Index, equation is for glucose in mmol/l (dc1386 appendix)                                                                                                                                                                                                                                                                     |
| timepoint                                          | The RCT timepoint (specifc to the filenanes given to Exeter inhouse data, comment this out of code if not necessary)                                                                                                                                                                                                                              |
