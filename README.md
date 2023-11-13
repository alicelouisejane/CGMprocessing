<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

The goal of CGMprocessing is to provide functions for preparing,
cleaning and analyzing Continuous Glucose Monitoring (CGM) data for
clinical diabetes studies. Glycemic variables and definitions are inline
with the International Consensus on Use of Continuous Glucose
Monitoring: [Danne
2017](https://care.diabetesjournals.org/content/40/12/1631). This
package also includes a specific function for use in the context of
diabetes exercise studies, which splits up CGM into specific time
periods post-exercise. Specific outputs inline with exercise
recommendations in type 1 diabetes: [Riddell
2017](https://pubmed.ncbi.nlm.nih.gov/28126459/).

This code was developed based off work by [T.
Vigers](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0216851)

This README is written as a tutorial for CGM analysis, including in the
context of exercise. This an an update and more in depth explanation of
the general pipeline that was developed and used in publishing a [PhD
Thesis](https://www.proquest.com/openview/78089a4b4d1696754e7d2ae2b3e2e922/1?pq-origsite=gscholar&cbl=51922&diss=y)
and [peer reviewed
research](https://academic.oup.com/jes/article/5/10/bvab127/6323320)

### Functions

See parameters for explanations of the arguments per each function:

-   **cleanCGM** [Jump to cleanCGM](#cleancgm)

-   **exercise\_split** [Jump to exercise\_split](#exercise_split)

-   **analyseCGM** [Jump to analyseCGM output](#analysecgm)

### Example File Structures

-   **/data-raw**: raw data from sensor
-   **/data-clean**: data ran through cleanCGM() function or own
    cleaning pipeline to be inline with the format of a cleanCGM()
    output
-   **/CGMupload**: Folder for output csv of glycemic metrics generated
    from analysedCGM() function  
-   **/exerciseoutput**: Folder for the split exercise files generated
    by exercise\_split()

❗**Important**: This package is optimized for raw Dexcom or Libre CGM
data. Raw CGM files usually have an ID number within the filename. If
this is the case leave the filename as is. If raw files don’t have this
then edit the filenames so they are uniqueily identified with the
patients ID number. If you are using already coalesced data from another
study ie. [JAEB](https://public.jaeb.org/datasets/diabetes) then ID will
already be a variable within the data structure, because it is a
pre-processed data file from another study then leave file name as is.
If you data is coalesced then the file looping is redundant in the
cleanCGM function and will the cleaned version of the coalesced file.
However ensure you separate out each ID type in for eg. R list of
dataframes for when you run either **exercise\_split** or
**analyseCGM**. If you are working with JAEB, other pre-processed data,
or other CGM device types ensure you check data structures and edit code
as appropriately to fit your needs❗ \_\_\_

## Introduction to CGM analysis in diabetes clinical research

------------------------------------------------------------------------

…in process…

------------------------------------------------------------------------

## <a id="cleancgm"></a> cleanCGM

**Functionality:** **cleanCGM()** is a function written to clean CGM
data for simpler file outputs for further analysis. Variable names are
standardized, high and low limits (sensor type dependent) converted from
text to values, gaps identified (but not imputed) and percentage wear/
sensor drop out metrics calculated. Option for calibration.

------------------------------------------------------------------------

❗ **Important If you don’t want to use this function and want to clean
your own data, but want to use exercise\_split or analyseCGM then ensure
data is in the the correct format see example at the end of the cleanCGM
section:** [Jump to cleanCGM output](#cleancgm-output)❗

-   Function can take raw files from Dexcom, Libre or other
    (preprocessed data) from an input folder directory. Files can be of
    any format, csv is preferred. ❗ **Important:: Raw files should have
    patient ID within name *ID\_optional.ext* **❗

-   *cgmvariable\_dictionary.xlsx* included as part of this package is
    an editable dictionary used to rename variables of interest (example
    shown in table). This can be updated for variable names of other
    sensors. The output of the cleanCGM will give the final variable
    names should be **id**, **timestamp**, **sensorglucose**, (optional
    **recordtype** which is only required for calibration) defined as
    below. This part of the code works to standardize variables without
    much user input. Alternatively you can manually change your files to
    follow this variable standardization:

**Table**: Definitions of the standardized variables

<table>
<colgroup>
<col style="width: 19%" />
<col style="width: 80%" />
</colgroup>
<thead>
<tr class="header">
<th>Variable</th>
<th>Definition</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>id</td>
<td>Patient ID or sensor ID</td>
</tr>
<tr class="even">
<td>timestamp</td>
<td>Sensor glucose timestamp</td>
</tr>
<tr class="odd">
<td>sensorglucose</td>
<td>Sensor glucose value</td>
</tr>
<tr class="even">
<td>recordtype</td>
<td>To identify if value is calibration or sensor (optional required for
calibration)</td>
</tr>
</tbody>
</table>

**Table**: Example of dictionary for renaming old variables in raw CGM
data files, edit as required. “Type” column in this dictionary is only
for user only for reference, sensor type is specified as an argument in
the function. Example is of some variable names I have come across in
each of the sensors- Always check you data to see what the variable
names are and edit this dictionary.

<table>
<thead>
<tr class="header">
<th>old_vars</th>
<th>new_vars</th>
<th>type</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Serial Number</td>
<td>id</td>
<td>libre</td>
</tr>
<tr class="even">
<td>Device Timestamp</td>
<td>timestamp</td>
<td>libre</td>
</tr>
<tr class="odd">
<td>Historic Glucose mmol/L</td>
<td>sensorglucose</td>
<td>libre</td>
</tr>
<tr class="even">
<td>Scan Glucose mmol/L</td>
<td>scanglucose</td>
<td>libre</td>
</tr>
<tr class="odd">
<td>Timestamp (YYYY-MM-DDThh:mm:ss)</td>
<td>timestamp</td>
<td>dexcomg6</td>
</tr>
<tr class="even">
<td>Glucose Value (mmol/L)</td>
<td>sensorglucose</td>
<td>dexcomg6</td>
</tr>
<tr class="odd">
<td>Source Device ID</td>
<td>device</td>
<td>dexcomg6</td>
</tr>
<tr class="even">
<td>pt_id</td>
<td>id</td>
<td>other</td>
</tr>
<tr class="odd">
<td>device_dt_tm</td>
<td>timestamp</td>
<td>other</td>
</tr>
<tr class="even">
<td>value</td>
<td>sensorglucose</td>
<td>other</td>
</tr>
<tr class="odd">
<td>record_type</td>
<td>recordtype</td>
<td>other</td>
</tr>
</tbody>
</table>

#### Calibration

-   This is an optional argument which is sensor dependent. Also some
    JAEB studies have CGM that had calibration requirements. **This
    requires the device argument to be set to “other” or “dexcomg4” if
    you are know you are using an old type of dexcom**.

**How it works:**

1.  SMBG calibrations are matched to the closest CGM sensor value and
    compares them by absolute relative difference. Multiple calibrations
    are usually performed per day so the mean absolute relative
    difference (MARD) per day is generated.

2.  A day of data should not be used if the MARD exceeds 20%. These days
    are excluded. A MARD&gt;20% has be chosen as many things can impact
    the MARD (ie. sensor glucose can trail behind blood glucose in
    periods of exercise, or it can just be inherent sensitivity of the
    CGM device used) and this is probably the upper limit of MARD that
    we could expect from some old devices [more
    info](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7189145/).

3.  Days that don’t have calibrations (and should have done) are also
    excluded, however many studies that used CGM have the requirement in
    their protocol and hence if a patient didn’t calibrate or meet the
    requirement of number of calibrations per day then their data is
    usually excluded from study.

Newer sensors don’t require calibrations and have internal algorithms
that keep the glucose values “calibrated”.

If your sensor requires calibration the easiest way to handle these are
to preprocess the data like some example studies in JAEB are formatted
ie. [Wireless Innovation for Seniors with Diabetes Mellitus
(WISDM)](https://pubmed.ncbi.nlm.nih.gov/28126459/) like the example
below. Calibrations work on having the calibration value identified in a
record type variable.

👷**Please Note** Preprocessed data structures may all be different, so
this part may not work as expected for device types that are “other”.
Please review code and data structures and edit your code as necessary.
👷

**Table**: Example head of data structures that have calibration values.
All CGM and SMBG calibrations are put into the same column and
identified by a recordtype variable.

<table>
<thead>
<tr class="header">
<th>id</th>
<th>timestamp</th>
<th>recordtype</th>
<th>sensorglucose</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>2000-06-30 14:49:36</td>
<td>CGM</td>
<td>7.17</td>
</tr>
<tr class="even">
<td>1</td>
<td>2000-06-30 14:54:36</td>
<td>CGM</td>
<td>7.28</td>
</tr>
<tr class="odd">
<td>1</td>
<td>2000-06-30 14:59:36</td>
<td>CGM</td>
<td>7.61</td>
</tr>
<tr class="even">
<td>1</td>
<td>2000-06-30 15:04:36</td>
<td>CGM</td>
<td>7.83</td>
</tr>
<tr class="odd">
<td>1</td>
<td>2000-06-30 15:09:36</td>
<td>calibration</td>
<td>7.9</td>
</tr>
</tbody>
</table>

#### <a id="cleancgm-output"></a> cleanCGM output

The function will output:

1.  **Cleaned CGM files** of the structure:

<table>
<thead>
<tr class="header">
<th>id</th>
<th>date</th>
<th>timestamp</th>
<th>sensorglucose</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 14:49:36</td>
<td>7.17</td>
</tr>
<tr class="even">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 14:54:36</td>
<td>7.28</td>
</tr>
<tr class="odd">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 14:59:36</td>
<td>7.61</td>
</tr>
<tr class="even">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 15:04:36</td>
<td>7.83</td>
</tr>
</tbody>
</table>

1.  **gap\_info.xlsx** Which provides information on the gaps in the
    data. It contains the timestamp and length of gap (diff) per ID:

<table>
<thead>
<tr class="header">
<th>timestamp</th>
<th>diff</th>
<th>subject_id</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>2023-05-19 06:12:00</td>
<td>-45</td>
<td>MU-1-004_V1</td>
</tr>
<tr class="even">
<td>2023-06-14 03:37:00</td>
<td>-30</td>
<td>MU-1-008_V1</td>
</tr>
<tr class="odd">
<td>2023-07-28 00:33:00</td>
<td>-30</td>
<td>MU-1-014_V1</td>
</tr>
<tr class="even">
<td>2023-07-26 23:07:00</td>
<td>-30</td>
<td>MU-1-016_V1</td>
</tr>
<tr class="odd">
<td>2023-08-01 09:07:00</td>
<td>-30</td>
<td>MU-1-016_V1</td>
</tr>
<tr class="even">
<td>2023-08-03 01:10:00</td>
<td>-30</td>
<td>MU-1-019_V1</td>
</tr>
</tbody>
</table>

1.  **percentage\_data\_collected\_info.xlsx** Which provides
    information about the quality of the CGM data:

-   *percentage\_expectedwear\_overstudy* -percentage wear that was
    expected over the study (usually expected lifetime of the sensor if
    it is a 10 day wear (dexcom) or 14 day wear (libre), but some
    studies might only require 3 days of wear, you can specify this in
    the function arguments). It evaluates the length of time from the
    maximum to minimum timestamp.

-   *percentage\_datacollected\_overstudy* -percentage of actual data
    collected. This tells us if there was drop out from the sensor,
    counting the amount of time that the CGM actually measured.

-   *percentage\_dropout\_overstudy* -percentage of time that was lost
    to drop out (inverse of the above)

<table>
<colgroup>
<col style="width: 10%" />
<col style="width: 31%" />
<col style="width: 32%" />
<col style="width: 26%" />
</colgroup>
<thead>
<tr class="header">
<th>subject_id</th>
<th>percentage_expectedwear_overstudy</th>
<th>percentage_datacollected_overstudy</th>
<th>percentage_dropout_overstudy</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>MU-1-001_V1</td>
<td>98.2142857</td>
<td>100</td>
<td>0</td>
</tr>
<tr class="even">
<td>MU-1-003_V1</td>
<td>99.4047619</td>
<td>100</td>
<td>0</td>
</tr>
<tr class="odd">
<td>MU-1-004_V1</td>
<td>56.25</td>
<td>95.3125</td>
<td>4.6875</td>
</tr>
<tr class="even">
<td>MU-1-005_V1</td>
<td>64.8809524</td>
<td>100</td>
<td>0</td>
</tr>
<tr class="odd">
<td>MU-1-008_V1</td>
<td>74.4047619</td>
<td>96.875</td>
<td>3.125</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

------------------------------------------------------------------------

## <a id="exercise_split"></a> exercise\_split

**Functionality:** **exercise\_split()** is a function written to split
up a CGM file (cleaned by cleanCGM or another way) into relevant time
periods post exercise. Exercise splitting is based on an exercisefile- a
list of exercise timestamps for every person in study [Jump to
exercisefile - List of exercise time
stamps](#exercisefile-listofexercisetimestamps) for the structure of
this. Function takes files in the same structure as outputted from
cleanCGM() [Jump to cleanCGM output](#cleancgm-output). These split
files can then be individually ran through analyseCGM() [Jump to
analyseCGM](#analysecgm) or taken for further analysis.

------------------------------------------------------------------------

In the diabetes exercise literature it is usual to assess:

-   6 hours post exercise (In this function you can alter the 6 hours to
    be any number of hours)

-   Overnight post exercise (00:00-06:00)

-   24 hours post exercise

-   The time from exercise end up until 00:00

-   The next day post exercise (06:00-24:00 ie. midnight of that next
    day)

![Example of relevant time periods post
exercise](man/figures/exercise_periods.png)

#### <a id="exercisefile-listofexercisetimestamps"></a> exercisefile - List of exercise time stamps

-   File required for exercise splitting, listing the start and end time
    of each exercise. Type column can be changed to whatever type of
    exercise/event/instance etc. it is.

It should be structured as below:

<table>
<thead>
<tr class="header">
<th>pt_id</th>
<th>type</th>
<th>startdatetime</th>
<th>finishdatetime</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>FRC1</td>
<td>1</td>
<td>2022-01-28 8:19:28</td>
<td>2022-01-28 9:34:28</td>
</tr>
<tr class="even">
<td>FRC1</td>
<td>2</td>
<td>2022-03-15 8:40:48</td>
<td>2022-03-15 9:50:48</td>
</tr>
<tr class="odd">
<td>FRC2</td>
<td>1</td>
<td>2022-03-10 7:17:44</td>
<td>2022-03-10 8:32:44</td>
</tr>
<tr class="even">
<td>FRC2</td>
<td>2</td>
<td>2022-04-07 7:17:43</td>
<td>2022-04-07 8:27:43</td>
</tr>
</tbody>
</table>

#### <a id="exercise_splitoutput"></a> exercise\_split output

-   Folders holding each timeperiod split are created automatically in
    your *outputdirectory* as below: ![Exercise
    files](man/figures/exercisefiles.png)

-The files in this folder are named automatically corresponding to the
timeperiod they are in eg. files in **data-after\_24** are named as:
*ID\_exercisetype\_24.csv*, for **data-after\_0000\_0600** are named as:
*ID\_exercisetype\_00\_06.csv* etc.

❗ **Important:** File naming structure is required for the analyseCGM()
function. Creating a unique file for each individual for each time
period allows us to get a global table of CGM metrics for each of these
timeperiods which is easier to then taken on for further analysis.❗

-   For ease the folder data-all are all the files in one folder. You
    can then point ot this one folder when you run analyseCGM().

These files are structured slightly differently to the cleanCGM output
with the addition of the variables:

-   **type**: relating to the type of exercise/event/instance of
    exercise etc which is specified in the exercisefile

-   **start\_split**: Sensor glucose timestamp where the split began ie.
    the closedst within the CGM interval that matched the start of
    exercise

-   **startdatetime**: Exercise start timestamp from exercise file

-   **finishdatetime**: Exercise end timestamp from exercise file

-   **diff**: The raw time difference between the sesnsor glucose
    timestamp **exercise end timestamp**. If negative this is **during**
    exercise.

-   **diff\_disc**: The discrete time interval created from the diff
    variable. During exercise is set to 0 and follows consecutively ie.
    time within the first hour after exercise is set to 1, second hour
    is 2 etc.

❗ **Important:** analyseCGM() when exercise=T relies on the
**diff\_disc** variable to create the time in range during exercise and
post exercise metrics.❗

<table style="width:100%;">
<colgroup>
<col style="width: 13%" />
<col style="width: 12%" />
<col style="width: 6%" />
<col style="width: 5%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 11%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th>timestamp</th>
<th>sensorglucose</th>
<th>pt_id</th>
<th>type</th>
<th>start_split</th>
<th>startdatetime</th>
<th>finishdatetime</th>
<th>diff</th>
<th>diff_disc</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>2022-03-16 00:00:49</td>
<td>4.3</td>
<td>FRC1</td>
<td>cooldown</td>
<td>2022-03-15 09:50:48</td>
<td>2022-03-15 08:40:48</td>
<td>2022-03-15 09:50:48</td>
<td>14.1669444444444</td>
<td>15</td>
</tr>
<tr class="even">
<td>2022-03-16 00:05:49</td>
<td>4.3</td>
<td>FRC1</td>
<td>cooldown</td>
<td>2022-03-15 09:50:48</td>
<td>2022-03-15 08:40:48</td>
<td>2022-03-15 09:50:48</td>
<td>14.2502777777778</td>
<td>15</td>
</tr>
<tr class="odd">
<td>2022-03-16 00:10:49</td>
<td>4.7</td>
<td>FRC1</td>
<td>cooldown</td>
<td>2022-03-15 09:50:48</td>
<td>2022-03-15 08:40:48</td>
<td>2022-03-15 09:50:48</td>
<td>14.3336111111111</td>
<td>15</td>
</tr>
<tr class="even">
<td>2022-03-16 00:15:49</td>
<td>5</td>
<td>FRC1</td>
<td>cooldown</td>
<td>2022-03-15 09:50:48</td>
<td>2022-03-15 08:40:48</td>
<td>2022-03-15 09:50:48</td>
<td>14.4169444444444</td>
<td>15</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

## <a id="analysecgm"></a> analyseCGM

------------------------------------------------------------------------

**Functionality:** **analyseCGM()** is a function written to create
consensus glycemic metrics based off definitions outlined International
Consensus on Use of Continuous Glucose Monitoring:[Danne
2017](https://care.diabetesjournals.org/content/40/12/1631). For
definitions [Jump to analyseCGM output](#analysecgm-output). Function
takes files in the same structure as outputted from cleanCGM() or
exercise\_split().

------------------------------------------------------------------------

-   For calculation of time spent variables data is checked to be
    consecutive. If timestamps are &gt;20 min apart a missing row is
    added to the table to prevent events from running on if the time gap
    is &gt;20 min.

-   General time spent variables are created for:

    -   Above 10, 13.9, 16
    -   Below 3
    -   Range 3-&lt;3.9, 3.9-10

-   In addition to this hypo/hyperglycemia also have defined
    “excursions”:

    -   Hyperglycemia (at levels &gt;10 and &gt;13.9 and 16)
    -   Hypoglycemia (clinical excursion event beginning when &lt;3 for
        15min and ending when ≥3.9 for 15min)

The start of an excursion is when glucose goes above/below the specified
value for 15 mins. If this doesn’t happen it is not defined as an
excursion and isn’t included in the *excursion time spent variables*.
The *general time spent variables* will include any time above/below the
level regardless of if this is &gt;15min.

🖋️ **NOTE** it is my recommendation that the *general time spent
variables* are sufficient and the defined excursions or “clinical
excursion” hypoglcemia are too stringent. You may miss important
information if you just assess events that had to last &gt;15min. This
is particularly relevant if you use the Libre 1, 2 or Libre pro as this
CGM measures glucose every 15min. Using time above/below as your
measures of hypo/hyperglycemia would give you a good indication of the
lived experience of a patient and this, in my opinion, is most
important. Excursion metrics are still required by the International
Consensus on Use of Continuous Glucose Monitoring and hence why this
function still generates them 🖋

#### Hypoglycemia (clinical excursion event beginning &lt;3 for 15min and ending when &gt;3.9 for 15min)

**Beginning of a CGM event**: readings below 3 mmol/l for 15 min defines
a clinically significant hypoglycemic event. Code checks if 4
consecutive rows are below 3 (4 rows in total: row 1 detected below
3mmol/l the next 3 rows = a time of 15 mins as 1 row is assumed as a 5
min reading). If this is the case then the first instance of dropping
below 3 is marked as the start row of the clinical hypoglycemia CGM
event.

**End of a CGM event**: readings for 15 min at ≥ 3.9 mmol/l. From the
start row of the true hypoglycemic event defined above the code checks
if the glucose in consecutive rows is ≥ 3.9 mmol/l and marks these
values as hypoglycemia. At the row glucose becomes ≥ 3.9 the code checks
if the next 3 rows remain ≥ 3.9 mmol/l (4 rows in total: row 1 detected
≥ 3.9 mmol/l the next 3 rows = a time of 15 mins as 1 row is assumed as
a 5 min reading). If glucose dips below 3.9 again within these 4 rows
then the event does not end. If the glucose stays ≥ 3.9 for these 4 rows
the event ends at the first instance of ≥3.9.

If a missing row is present (inserted because the consecutive timestamps
had a gap &gt;20 min) the hypoglycemic event has to end at this point).
🖋️

#### Freestyle libre in the analyseCGM() function

-   If the data you have are from a Libre (or any sensor that has 15min
    sampled data) then ensure your argument libre set to TRUE in the
    analyseCGM() function

-   The analyseCGM() function has a section that pseudo codes thing kind
    of data to be “5min” data ie. Every row is repeated 2 more times (3
    lots of 5 mins are in 15 mins). It doesn’t matter that the
    timestamps aren’t increasing in 5min but having the number of row
    corresponding to 5 min data is the important part, mainly for the
    clinical hypo excursion definition. Code section as below:

<!-- -->

      table <- slice(table, rep(1:n(), each = 3))

#### <a id="analysecgm-output"></a> analyseCGM output

1.  A .csv file called by default *CGMupload* containing CGM variables
    per each individual (file) on each row

All CGM variables generated by analyseCGM() are detailed below:

<table>
<colgroup>
<col style="width: 8%" />
<col style="width: 91%" />
</colgroup>
<thead>
<tr class="header">
<th>Output variable</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>subject_id</td>
<td>Patient ID taken from raw data filename- may require further string
processing</td>
</tr>
<tr class="even">
<td>exercise</td>
<td>Only created when exercise=T. See section on exercise analysis for
more info</td>
</tr>
<tr class="odd">
<td>totaltime_mins</td>
<td>Total time in the table, this is calculated from the number of rows
present, assuming each row is 5 min reading NOT max and min as this
would not be accurate if there were gaps in the data</td>
</tr>
<tr class="even">
<td>start_cgm_analysis</td>
<td>min datetime in table</td>
</tr>
<tr class="odd">
<td>end_cgm_analysis</td>
<td>max datetime in table</td>
</tr>
<tr class="even">
<td>interval</td>
<td>Most common interval in the data, for dexcom this is 300 seconds
(5min) for libre this is put to 900 seconds (15 min)</td>
</tr>
<tr class="odd">
<td>num_days_good_data</td>
<td>Total time / (24*60*60))</td>
</tr>
<tr class="even">
<td>num_hrs_good_data</td>
<td>Total time / 3600</td>
</tr>
<tr class="odd">
<td>total_sensor_readings</td>
<td>Total number of rows in the table</td>
</tr>
<tr class="even">
<td>average_sensor</td>
<td>Average sensor glucose</td>
</tr>
<tr class="odd">
<td>estimated_a1c%</td>
<td>Estimated Hba1C in % based on hba1c equation and mean glucose</td>
</tr>
<tr class="even">
<td>estimated_a1cmmolmol</td>
<td>Estimated Hba1C in mmol/mol based on hba1c equation and mean
glucose</td>
</tr>
<tr class="odd">
<td>gmimmol/mol</td>
<td>Glucose Management Indicator inndicates the average Hba1C level that
would be expected based on mean. gmi and estimated Hba1C should
therefore be similar</td>
</tr>
<tr class="even">
<td>q1_sensor</td>
<td>Lower quartile sensor glucose</td>
</tr>
<tr class="odd">
<td>median_sensor</td>
<td>Median sensor glucose</td>
</tr>
<tr class="even">
<td>q3_sensor</td>
<td>upper quartile sensor glucose</td>
</tr>
<tr class="odd">
<td>standard_deviation</td>
<td>Standard deviation of sensor glucose</td>
</tr>
<tr class="even">
<td>cv</td>
<td>Coefficient of variation. NOTE: SD is highly influenced by the mean
glucose – someone with a higher mean glucose will have a higher SD. The
CV divides the SD/mean x100. This division helps “correct” and normalize
glucose variability, allowing us to set a single variability goal that
applies to people with different mean glucose levels.</td>
</tr>
<tr class="odd">
<td>min_sensor</td>
<td>Minimum sensor glucose</td>
</tr>
<tr class="even">
<td>max_sensor</td>
<td>Maximum sensor glucose</td>
</tr>
<tr class="odd">
<td>excursions_over_10</td>
<td>Count of number of times glucose went above 10 mmol/l for 15 mins
(excursion start defined as above threshold fpr 15 mins)</td>
</tr>
<tr class="even">
<td>min_spent_excursion_over_10</td>
<td>Time in mins spent in excursions above 10 mmol/l (excursion start
defined as above threshold fpr 15 mins)</td>
</tr>
<tr class="odd">
<td>percent_time_excursion_over_10</td>
<td>Percentage of time spent in excursions above 10 mmol/l in relation
to total time of the sensor calculated previously (excursion defined as
above threshold fpr 15 mins)</td>
</tr>
<tr class="even">
<td>excursions_over_13</td>
<td>Count of number of times glucose went above 13.9 mmol/l for 15 mins
(excursion start defined as above threshold fpr 15 mins)</td>
</tr>
<tr class="odd">
<td>min_spent_excursion_over_13</td>
<td>Time in mins spent in excursions above 13.9 mmol/l (excursion start
defined as above threshold fpr 15 mins)</td>
</tr>
<tr class="even">
<td>percent_time_excursion_over_13</td>
<td>Percentage of time spent in excursions above 13.9 mmol/l in relation
to total time of the sensor calculated previously (excursion defined as
above threshold fpr 15 mins)</td>
</tr>
<tr class="odd">
<td>excursions_over_16</td>
<td>Count of number of times glucose went above 16 mmol/l for 15 mins
(excursion start defined as above threshold fpr 15 mins)</td>
</tr>
<tr class="even">
<td>min_spent_excursion_over_16</td>
<td>Time in mins spent in excursions above 16 mmol/l (excursion start
defined as above threshold fpr 15 mins)</td>
</tr>
<tr class="odd">
<td>percent_time_excursion_over_16</td>
<td>Percentage of time spent in excursions above 16 mmol/l in relation
to total time of the sensor calculated previously (excursion defined as
above threshold fpr 15 mins)</td>
</tr>
<tr class="even">
<td>hypo_under_3_prolonged</td>
<td>Count of number of times hypoglycemic episode (as defined see below)
was &gt; 2 hours</td>
</tr>
<tr class="odd">
<td>hypo_under_3</td>
<td>Count of number of hypoglycemic episodes. Clinically significant
hypoglycemic event excursion begins as readings below 3 mmol/l for 15
min, ending when readings for 15 min at ≥ 3.9 mmol/l. See below NOTE for
more information.</td>
</tr>
<tr class="even">
<td>min_spent_under_hypo3</td>
<td>Time in mins spent in hypoglycemia episode</td>
</tr>
<tr class="odd">
<td>percent_time_under_hypo3</td>
<td>Percentage of time spent in hypoglycemic episodes in relation to
total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>min_spent_&lt;3</td>
<td>Time in mins spent below 3 mmol/l</td>
</tr>
<tr class="odd">
<td>percent_time_&lt;3</td>
<td>Percentage of time spent below 3 mmol/l in relation in relation to
total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>min_spent_3_3.8</td>
<td>Time in mins spent between 3-3.9 mmol/l</td>
</tr>
<tr class="odd">
<td>percent_time_3_3.8</td>
<td>Percentage of time spent between 3-3.9 mmol/l in relation in
relation to total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>min_spent_3.9_10</td>
<td>Time in mins spent between 3.9-10 mmol/l</td>
</tr>
<tr class="odd">
<td>percent_time_3.9_10</td>
<td>Percentage of time spent between 3.9-10 mmol/l in relation in
relation to total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>min_spent_3.9_7.8</td>
<td>Time in mins spent between 3.9-7.8 mmol/l</td>
</tr>
<tr class="odd">
<td>percent_time_3.9_7.8</td>
<td>Percentage of time spent between 3.9-7.8 mmol/l in relation in
relation to total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>min_spent_over10</td>
<td>Time in mins spent above 10 mmol/l</td>
</tr>
<tr class="odd">
<td>percent_time_over10</td>
<td>Percentage of time spent above 10 mmol/l in relation in relation to
total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>min_spent_over13</td>
<td>Time in mins spent above 13.9 mmol/l</td>
</tr>
<tr class="odd">
<td>percent_time_over13</td>
<td>Percentage of time spent above 13.9 mmol/l in relation in relation
to total time of the sensor calculated previously</td>
</tr>
<tr class="even">
<td>total_auc</td>
<td>Total area under the glucose curve</td>
</tr>
<tr class="odd">
<td>r_mage</td>
<td>Mean Amplitude Glycemic Excursion. Option to asses average of the
differences greater than either entire dataset SD, 2SD, etc</td>
</tr>
<tr class="even">
<td>j_index</td>
<td>Combination of information from mean and SD of all glucose values
doi: 10.1055/s-2007-979906.</td>
</tr>
<tr class="odd">
<td>conga_1</td>
<td>Continuous overlapping net glycemic action, with _n appended as the
indicated number of hours being assessed (in this case 1 hour)</td>
</tr>
<tr class="even">
<td>modd</td>
<td>Mean of daily difference</td>
</tr>
<tr class="odd">
<td>lbgi</td>
<td>Low Blood Glucose Index, equation is for glucose in mmol/l (dc1386
appendix)</td>
</tr>
<tr class="even">
<td>hbgi</td>
<td>High Blood Glucose Index, equation is for glucose in mmol/l (dc1386
appendix)</td>
</tr>
<tr class="odd">
<td>min_spent_7_15_exercise</td>
<td>Only created when exercise=T. Time in mins spent between 7-15 mmol/l
during exercise (Ridell 2017).</td>
</tr>
<tr class="even">
<td>percent_time_7_15_exercise</td>
<td>Only created when exercise=T. Percentage of time in mins spent
between 7-15 mmol/l during exercise (Ridell 2017).</td>
</tr>
<tr class="odd">
<td>min_spent_5_12_exercise</td>
<td>Only created when exercise=T. Time in mins spent between 5-12 mmol/l
in the time up to 6 hours post exercise (Ridell 2017).</td>
</tr>
<tr class="even">
<td>percent_time_5_12_exercise</td>
<td>Only created when exercise=T. Percentage of time in mins spent
between 5-12 mmol/l in the time up to 6 hours post exercise (Ridell
2017).</td>
</tr>
</tbody>
</table>

#### <a id="exercise"></a> Exercise

Set argument exercise is set to TRUE. Analysis of exercise data is
usually done for time periods post exercise. If you are using this
function on exercise data, structured as periods of CGM data that are a
certain time post exercise, ensure your files are structured as
outputted from the exercise\_split() function [Jump to
exercise\_split](#exercise_split)

-   Additional CGM metrics are created for exercise and are defined in
    the above section [Jump to analyseCGM output](#analysecgm-output)

-   *min\_spent\_7\_15\_exercise*

-   *percent\_time\_7\_15\_exercise*

-   *min\_spent\_5\_12\_exercise*

-   *percent\_time\_5\_12\_exercise*

------------------------------------------------------------------------
