#' Parse Flow Data into Data Management (DM) format
#'
#' @description
#' `Flow_Final_Parse_to_DM` will parse the Subject ID, Visit Name, and Stim Name from fcs file names in preparation in data transfer to DM
#'
#' @details
#' This function inputs a translated flow data frame and returns a data frame appropriately formatted for transfer to Data Management
#'
#' @param flow.translated.data A flow data frame with fcs file names in the first column
#' @param sid.regex A character vector listing the SID formats - default is '[:digit:]{4}-[:digit:]{4}'
#' @param visit.regex A character vector listing the potential visit names to extract from the fcs files.  Default is "Day 0|Screen|Week 26|Week 52"
#' @param stim.regex A character vector listing the potential stim names to extract from the fcs file names.  Default is "R10|OVA|ConA|HPV6|HPV11"
#' @param study.name A character vector with the name of the study to be included in the returned data frame
#' @param assay.name Character string containing the assay type: 'flow
#'
#' @importFrom dplyr mutate select rename
#' @importFrom stringr str_replace str_extract
#' @importFrom tidyselect all_of
#'
#' @examples
#' \dontrun{
#' Flow_Final_Parse_To_DM(cd4.translated,
#'                        study.name = "proj03",
#'                        sid.regex = "[:digit:]{4}-[:digit:]{4}",
#'                        visit.regex = "Day 0|Screen|Week 26|Week 52",
#'                        stim.regex = "R10|OVA|ConA|HPV6|HPV11",
#'                        study.name = "flow02",
#'                        assay.name = "flow")
#'}
#'
#' @export

# Flow_Final_Parse_To_DM <- function(flow.translated.data,
#                                    sid.regex =  "[:digit:]{4}-[:digit:]{4}",
#                                    visit.regex = "Day 0|Screen|Week 26|Week 52",
#                                    stim.regex = "R10|OVA|ConA|HPV6|HPV11",
#                                    study.name = "STUDY_NAME",
#                                    assay.name = "flow"){
#
#   ## check args
#   validate_input_df(flow.translated.data)
#
#   ## add output cols
#   select_outputcols <- c("FCS", "STUDY", "ASSAY", "SID", "VISIT", "STIM", "PARAM", "Frequency")
#
#   flow.translated.data |>
#     mutate(SID = str_extract(.data$FCS, sid.regex),
#            VISIT = str_extract(.data$FCS, visit.regex) |> str_replace(" ", ""),
#            STIM = str_extract(.data$FCS, stim.regex),
#            STUDY = study.name,
#            ASSAY = assay.name
#     ) |>
#     select(all_of(select_outputcols)) |>
#     rename("RESULT" = "Frequency")
# }
