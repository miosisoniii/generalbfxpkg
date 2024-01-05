#' Extract Cell Counts from Flow Data
#'
#' This function extracts cell counts from a data frame produced by the `Flow_PolyF_Extract`
#' function for specified target populations.
#'
#' @param Flow_PolyF_Extract_data A data frame created using the `Flow_PolyF_Extract` function.
#' @param targets A character vector listing the target populations for which to extract cell counts.
#' Defaults to c("/Singlets/Lymphocytes/Live/CD3", "/Singlets/Lymphocytes/Live/CD3/CD8",
#' "/Singlets/Lymphocytes/Live/CD3/CD4z/CD4").
#'
#' @return A data frame with selected columns: FCS, Population, and Count. 'FCS' represents
#' the modified name column where "fcs.*" is replaced by "fcs", 'Population' is the
#' modified Population column where any population named ".*CD3" is replaced by "CD3", and
#' 'Count' represents the cell count for the target population.
#'
#' @details This function queries an open cyto combination gate extract and pulls cell count
#' for population of interest.
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_replace
#' @importFrom rlang is_empty
#' @importFrom tidyselect all_of
#'
#' @examples
#' # Assuming Flow_PolyF_Extract_data is your data frame
#' \dontrun{
#' Extract_Cell_Counts_Flow(Flow_PolyF_Extract_data, targets = c("/Singlets/Lymphocytes/Live/CD3"))
#' }
#' @export

# Extract_Cell_Counts_Flow <- function(Flow_PolyF_Extract_data,
#                                      targets = c("/Singlets/Lymphocytes/Live/CD3",
#                                                  "/Singlets/Lymphocytes/Live/CD3/CD8",
#                                                  "/Singlets/Lymphocytes/Live/CD3/CD4z/CD4")){
#
#   ## check input args:
#   ## Flow_PolyF_Extract_data arg
#   if (is_empty(Flow_PolyF_Extract_data)) {
#     cli_abort(message = paste0("Output from Flow_PolyF_Extract_data is not present"))
#   }
#   validate_input_df(.data = Flow_PolyF_Extract_data)
#
#   ## check argument choices for extracting cell counts
#   targ_choices <- c("/Singlets/Lymphocytes/Live/CD3",
#                     "/Singlets/Lymphocytes/Live/CD3/CD8",
#                     "/Singlets/Lymphocytes/Live/CD3/CD4z/CD4")
#   targets <- match.arg(targets, choices = targ_choices)
#
#   ## create vector for output colnames
#   select_outputcols <- c("FCS", "Population", "Count")
#
#     Flow_PolyF_Extract_data |>
#     filter(.data$Population %in% targets) |>
#       mutate(
#         FCS = str_replace(.data$name, "fcs.*", "fcs"),
#         Population = str_replace(.data$Population, ".*CD3", "CD3")
#       ) |>
#       select(all_of(select_outputcols))
#   }
