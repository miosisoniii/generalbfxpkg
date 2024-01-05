#' @importFrom tibble tibble
NULL

#' Frequency counts for FLOW01 data
#'
#' Frequency counts from the FLOW01 comparison data, extracted from FlowJo .wsp.
#'
#' @format A data frame with 7 variables:
#' \describe{
#' \item{\code{SID}}{Unique Subject Identifier, anonymized by anonymize() package}
#' \item{\code{VISIT}}{Timepoint \code{x}}
#' \item{\code{STIM}}{Stimulation applied to the data: `ConA`, `DMSO`, `OVA`, `WT`}
#' \item{\code{Population}}{Cell population pre-parsing, with CD4/CD8}
#' \item{\code{Parent}}{Parent population pre-parsing, with CD4/CD8 ranging to 'root'}
#' \item{\code{Frequency}}{Frequency of cell population, \code{Frequency/ParentFrequency}}
#' \item{\code{ParentFrequency}}{Parent frequency of cell population as a decimal 0-100 \code{ParentFrequency/100?}}
#' }
#'
#'
#'
"flow01_count_data"

#' CD4 (CD8?) markers in all Boolean combinations that will be extracted from the .wsp in the "Post-QC" Export
#'
#' FLOW parameters that were extracted from .wsp.
#' This data will be used to extract
#'
#' @format A data frame with 2 variables:
#' \describe{
#' \item{\code{strings}}{parameters as strings for aLGL in every combination: `CD38`, `CD69`, `Gnly`, `GrzA`, `GrzB`, `Ki67`, `LAG3`, `PD1`, `Prf`, `TIM3`}
#' \item{\code{strings_regex}}{\code{strings} enclosed by regex to ensure that order of params is not required}
#' }
#'
#'
#'
"markers_flow"


#' ELISPOT data02
#'
#' Describe the ELISPOT data02 here
#'
#' @format A data frame with 8 variables:
#' \describe{
#' \item{\code{STUDY}}{Study type}
#' \item{\code{ASSAY}}{Assay type}
#' \item{\code{SID}}{Unique Subject Identifier}
#' \item{\code{VISIT}}{Timepoints}
#' \item{\code{STIM}}{Stimulation applied}
#' \item{\code{RESULT}}{Values from assay}
#' \item{\code{RESPONDER}}{Responder in study post-assay, with value of 'YES' or 'NO'}
#' \item{\code{BATCH}}{Batch the SID belongs to}
#' }
#'
"elispot02_data"


#' ELISPOT data01
#'
#' Describe the ELISPOT data01 here
#'
#' @format A data frame with 7 variables:
#' \describe{
#' \item{\code{STUDY}}{Study type}
#' \item{\code{ASSAY}}{Assay type}
#' \item{\code{SID}}{Unique Subject Identifier}
#' \item{\code{VISIT}}{Timepoints}
#' \item{\code{STIM}}{Stimulation applied}
#' \item{\code{RESULT}}{Values from assay}
#' \item{\code{EVALUABLE}}{Determination if data is able to be used}
#' }
#'
"elispot01_data"

#' VISIT Re-code
#'
#' Dataframe that will be used to swap the incorrect string matches to the correct version
#'
#' @format A data frame with 5 variables:
#' \describe{
#' \item{\code{type}}{Type of error/non-complying variable name}
#' \item{\code{full}}{Full name of the correct visit, in Upper to lower case ie. Day0}
#' \item{\code{abbr}}{Preferred/correct visit name}
#' \item{\code{wrong_vis}}{Incorrect visit name}
#' \item{\code{regex_vis}}{\code{wrong_vis} with regular expression at beginning and end}
#' }
#'
"visit_recoding"

#' Flow QC Meeting File input (from proj03)
#'
#' Dataframes that will be used as input to create the final QC meeting file.
#' This will include proj03 for analyst1, proj03 for analyst2, and the CV data from both analysts
#' @name flow_qcmeet_input
#'
#' @keywords datasets flow
#'
#' @format A list object containing three data frames:
#' \itemize{
#'   \item{\code{proj03_analyst1} A data frame with 2520 rows and 3 columns:}{
#'       \item{name}{filename of fcs file, separated by "_"}
#'       \item{PARAM}{Cell markers for CD4 and CD8}
#'       \item{Frequency}{Frequency of cells in the population, 0-100}
#'   }
#'   \item{\code{proj03_analyst2} A data frame with 2520 rows and 3 columns:}{
#'       \item{name}{filename of fcs file, separated by "_"}
#'       \item{PARAM}{Cell markers for CD4 and CD8}
#'       \item{Frequency}{Frequency of cells in the population, 0-100}
#'   }
#'   \item{\code{proj03_qc_compare} A data frame with 2520 rows and 6 columns:}{
#'       \item{name:}{filename of fcs file, separated by "_"}
#'       \item{PARAM:}{Cell markers for CD4 and CD8}
#'       \item{Frequency_a1:}{Frequency of cells in the population, 0-100 for analyst1}
#'       \item{Frequency_a2:}{Frequency of cells in the population, 0-100 for analyst2}
#'       \item{cv:}{Calculation of Coefficient of Variation}
#'       \item{qc_pass:}{PASS for those with CV < 20.0, and FAIL for CV >= 20.0}
#'   }
#' }
#'
#'
#'
"flow_qcmeet_input"

#' FLOW intermediate data
#'
#' Created from Batches x to 14, Data Management (LONG) format to be converted into FINAL format.
#' Original LONG format for Flow Cytometry is in Wide format. This specific dataset was created from Wide and stored in
#' Long format for easier usage within generalbfxpkg() data was also generated from the first 5 unique subjects.
#'
#' @format A data frame with 5 variables:
#' \describe{
#' \item{\code{SID}}{Unique Subject Identifier, anonymized by anonymize() package}
#' \item{\code{VISIT}}{Timepoints}
#' \item{\code{STIM}}{Stimulation applied}
#' \item{\code{SUM_FREQ}}{Population frequency, a value 0-100}
#' \item{\code{PARAM}}{FLOW gating parameters `CD38`, `CD69`, `Gnly`, `GrzA`, `GrzB`, `Ki67`, `LAG3`, `PD1`, `Prf`, `TIM3`}
#' }
#'
"FLOW_intermediate_data"


#' FLOW Markers List for Parameter Calculations
#'
#' Created from FLOW_markerlist.csv, used for parameter calculations using regex for replacement of markers/populations,
#' summing all of the cell population frequencies that match the regex 'Code' column. This only includes biologically relevant
#' cell populations for a Flow Cytometry assay, unlike 'markers_flow' df that contains ALL combinations of cell populations.
#'
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{ParameterCAPS}}{Cell population name, in all capital letters}
#' \item{\code{ParameterLOWER}}{Cell population name, in all lower-case letters}
#' \item{\code{Code}}{Regex for detection of Cell population names, to sum all occurrences of parameter name in each row}
#' }
#'
"flow_markerlist"
