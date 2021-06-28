---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, echo= FALSE, include = FALSE}

## Installing required packages for this template
required_packages <- c(
                       "rio",            # read in data
                       "dplyr",          # clean/shape data
                       "tidyr",          # clean/shape data
                       "flextable",      # for nice tables
                       "magick"          # saving tables as image 
                       )
  
for (pkg in required_packages) {
  # install packages if not already present
  if (!pkg %in% rownames(installed.packages())) {
    install.packages(pkg)
  }
  # load packages to this current session 
  library(pkg, character.only = TRUE)
}

```

# CGMprocessing

<!-- badges: start -->
<!-- badges: end -->

The goal of {CGMprocessing} is to provide functions for preparing, cleaning and analysing Continuous Glucose Monitoring (CGM) data. The functions in the package were developed from  Dexcom G4 data with 5 minute intervals and was specifically developed to enable processing and analysing of data from the 2 separate EXTOD Randomised Control Trials (RCT) lead by Professor Rob Andrews and Dr Parth Narendran See [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7317834/) for published information about the EXTOD Education program and [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7317834/) for the published EXTOD RCT. 

Glycemic variables and definitions are based off the [International Concensus on Use of Continuous Gucose Monitoring](https://care.diabetesjournals.org/content/40/12/1631).

This code was developed based off work by [T. Vigers](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0216851)



## File Structure
- **/data-raw**: raw data from sensor
- **/data-preprocessed**: Exeter specific preprocessed data from sensor
- **/data-clean**: data ran through cleanCGM() function
- **/CGMupload**: Output csv of analysed CGM data with glycemic metrics generated

**Important** As this was developed for analysis of RCT data files preprocessed are outputted with the filename formatted as **ID_timepoint.csv** based on the excel sheet name. If dealing with RCT data please ensure files are named in this way before running through cleanCGM() and analyseCGM() pipeline. Edit scripts if neccesary if not dealing with RCT data 


## Pre-processing of CGM files (Relavent to Exeter in house data)

The script **prepare_spitsheets.R** is a rough script pipeline (non-function) that prepares files from EXTOD and EXTOD education for further processing with functions part of {CGMprocessing}. 

**Functionality**: Chunks of traces with non-consecutive dates are separated with **wrongstart** appended to traces prior to the final consecutive chunk. The final chunk with consecutive dates is outputted to **/data-preprocessed**. Manual inpection is recommended at this point to inspect and remove any **wrongstart** files from this folder.

- **cgmvariable_dictionary.xlsx** is used to rename variables of interest. This could be updated for variable names of other sensors and integrated into cleanCGM() function


**Table**: Dictionary for renaming old variables in raw CGM data files
```{r cgmvariable_dictionary, echo = FALSE}
rio::import("cgmvariable_dictionary.xlsx") %>% 
  flextable::flextable() %>% 
  flextable::merge_v(j = ~ old_vars + new_vars) %>% 
  flextable::theme_vanilla() %>% 
  flextable::autofit() 

```
- Files from preprocessing are outputted with changed CGM variable names already and reformatted slightly (conversion of HI/LO etc.) compared to raw data as below:

**Table**: Preprocess script output
```{r preprocess, echo = FALSE}

rio::import("dummy_data.xlsx", which = "preprocessoutput") %>% head() %>%
  flextable::flextable() %>% 
  flextable::theme_vanilla() %>% 
  flextable::autofit() 

```

- Files are named as **ID_timepoint.csv** based on the excel sheet name

-**Important**: Glucose readings must be in mmmol/l. Manually change files in raw excel files. More information on conversion found [here](https://www.diabetes.co.uk/blood-sugar-converter.html)


## cleanCGM

**Functionality:** **cleanCGM()** is a function written to clean CGM data for simpler file outputs and perform (optional) calibration against fingerstick SMBG values developed of Dexcom G4 data. 

- Function can take raw files from Dexcom, Libre or previosuly preprocessed from an input folder directory. Files can be of any format, csv is preferred.
**Important** Files should be named as **ID_optional.ext**

- Text Low/High are in filled with the min/max limits depending on the inputted sensor type

- Calibration is performed againsted logged fingerstick SMBG readings and nearest 15 min later sensor reading. Calibration excluded 1) whole traces if 2 blood glucose calibrations were not completed at the start of the sensor wear, 2) a day of wear if the MARD of the sensor glucose and blood glucose calibration on that day is >20% or if <2 blood glucose calibrations were completes on that day. This can be set to false for CGM. 

-If calibration check is TRUE then the calibration table of fingerstick SMBG matched to nearest 15 min later sensor glucose with the correlation (checking there were 2 fingersticks per day) and the MARD between the sensor and fingerstick with be output to the specified calibrationoutput directory

---
**_FOR DEVELOPMENT_**
Libre sensors store glucose every 15 mins, in order for analyseCGM to work based consensus CGM analysis [here](https://care.diabetesjournals.org/content/40/12/1631) we must make the 15 min intervaltimeseries data into 5 min interval data. Currently cleanCGM() handles this with the line below, adding dummy 5 min data by adding 2 rows after every original row that is the same as the original row : 

```{r eval=FALSE, echo=T}
table<-slice(table,rep(1:n(), each = 3))
```

---

- Each file outputed should look like the belwo table:

**Table**: Cleaned CGM output
```{r cleancgm, echo = FALSE}

rio::import("dummy_data.xlsx", which = "cgmcleanoutput") %>% head() %>%
  flextable::flextable() %>% 
  flextable::theme_vanilla() %>% 
  flextable::autofit()

```


## analyseCGM

**Functionality:** **analyseCGM()** is a function written to create Concensus glycemic metrics based off definitions outlined [here](https://care.diabetesjournals.org/content/40/12/1631). Fuction takes files from the **data-clean** folder where the output of cleanCGM() is stored.

- For calculation of time spent variables data is checked to be consecutive. If timestamps are >20 min apart a missing row is added to the table to prevent events from runnning on if the time gap is >20 min.

- Time spent variables are created for: 
                - Above 10, 13.9, 16
                - Below 3, 
                - Range 3-<3.9, 3.9-10
                - Hyperglycemia (at levels >10 and >13.9)
                - Hypoglycemia

- Hyper/hypo glycemia are defined as excursions. The start of an excursion is going above/below the specified value for 15 mins. If this doesnt happpen it is not defined as an excursion and isnt included in the hyper/hypo time spent, but will be included in the general time spent for the defined time spent variables. 

___
**NOTE**
In this function Hypoglycemia is defined as: 

_Beginning of a CGM event_: readings below 3 mmol/l for 15 min defines a clinically significant hypoglycemic event. Code checks if 4 consecuative rows are below 3 (4 rows in total: row 1 detected below 3mmol/l the next 3 rows = 15 mins with 1 row being assumed as a 5 min reading). If this is the case then the first isantace of dropping below 3 is marked as the start row of a true hypoglycemia event

_End of a CGM event_: readings for 15 min at ≥ 3.9 mmol/l. From the start row of the true hypoglycemic event defined above the code checks if the glucose in consecutive rows is ≥ 3.9 mmol/l and marks these values as hypoglycemia. At the row glucose becomes > 3.9 the code checks if the next 3 rows remain > 3.9 mmol/l (4 rows in total: row 1 detected > 3.9 mmol/l the next 3 rows = 15 mins with 1 row being assumed as a 5 min reading). If glucose dips below 3.9 again within these 4 rows then the event does not end. If the glucose stays above 3.9 for these 4 rows the event ends at the end of the last row ie. including the 15 min of remaining above 3.9 mmol/l in the hypoglycemia time.     

If a missing row is present (inserted because the consecuative timestamps had a gap >20 min) the hypoglycemic event has to end at this point)
___

- There is an option for exercise analysis specified in this function specific to in house Exeter/Liverpool processing of data. This additional option was used to analyse specific files of exercise aligned glucose that were not ran through the cleanCGM() function.

- All CGM variables generated are detailed  below: 
**Table**: CGM metrics generated for each individual
```{r analysecgm, echo = FALSE}

rio::import("dummy_data.xlsx", which = "analysecgmoutput") %>% 
  flextable::flextable() %>% 
  flextable::theme_vanilla() %>% 
  flextable::autofit()

```
