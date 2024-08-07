% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanCGM.R
\name{cleanCGM}
\alias{cleanCGM}
\title{cleanCGM}
\usage{
cleanCGM(
  inputdirectory,
  outputdirectory,
  cgmdictionaryfile = NULL,
  device = "other",
  aggregated = F,
  calibration = F,
  removerow = F,
  nrow = 3,
  expectedwear = "full",
  saveplot = F
)
}
\arguments{
\item{inputdirectory}{path to folder containing raw files. If data is pre-aggregated then use the full file path including file name of this file. Preferred csv format but can read in others.}

\item{outputdirectory}{path to folder where cleaned output files will be stored}

\item{cgmdictionaryfile}{Optional. A string indicating the full path to a custom 'cgmvariable_dictionary.xlsx' file. See README on how to construct.
If NULL, the default file included with the package is used.}

\item{device}{Use "other" if CGM required calibrations. Options are can be any dexcom (g4,g6etc), libre (1,2,pro), or "other." Important to specify what device is being used for the alignment of standardized variable names and to ensure sensor limits, ensure device type is also in the cgm dictionary next to variables.
"Other" is specified if dealing with data that requires calibration or is in an alternative layout. Gap testing and standard percentage wear and droupout measures will not be created with this but number of days worn will be reported. Edit code as necessary if sensor is other and calibration is false}

\item{aggregated}{TRUE/FALSE. Default is FALSE. This aims to handle pre aggregated data where more than one individual/visit etc. is in the file, most likely from external clinical study databases. Before running this code please combine patient ID, visit name and device id into one Id type variable. Still specify all variable names in the CGM dictionary.}

\item{calibration}{only works for device = "other" TRUE/FALSE Default is FALSE. The majority of CGM data will not require a calibration, this is sensor dependant. For guardian sensors, dexcom g4 (and older generations) and medtronic ipro2 blood calibrations were required. The layout of the data may effect how this argument works please ensure calibration blood glucoses are labelled in a recordtype variable in the raw data (ie. in JAEB data)}

\item{removerow}{TRUE/FALSE Default is TRUE. used when raw data has additional first row before variables. Usually encountered in freestyle libre. If sure this doesn't exist use F. Default to T otherwise which will check if row exists and act accordingly}

\item{nrow}{used in conjunction with removerow=T. Number of rows to removed before variable names. Requires inspection of data files}

\item{expectedwear}{Default is full, can acccept a numeric. This indicates if a participant was expected to wear the CGM for the lifetime of the sensor or for a particular number of days in a study. Enter the number of days expected as a numeric if so.}

\item{saveplot}{TRUE/FALSE Default is TRUE. Save the overall CGM plot over the total number of days of wear. Will not generate if device type is "other"}
}
\value{
Returns individual files (or aggregated depending on inital structure) of CGM data that have variable names standardized, missing glucose removed, high and low limits re coded to specific sensor limits. Option of "other" in
argument device is specific to data that is pre-aggregated into one sheet (ie multiple device uploads + multiple people in 1 file) and may use CGM with calibrations (for eg JAEB data).
detailed output of percentage wear and sensor drop out and optional CGM summary graphs created.
Cleaned files can then be run through the analyseCGM and/or exercise_split functions.
}
\description{
This function cleans and standardizes raw Continuous glucose monitoring (CGM) devices used in diabetes management. This function is optimised for raw Dexcom or libre data however has some functionality to handle pre-aggregated trial data bases.
Remember to always check your data structures and follow the guide in the README for more help optimizing code for your specific needs.
}
\seealso{
analyseCGM and exercise_split
}
\author{
Alice Carr
}
