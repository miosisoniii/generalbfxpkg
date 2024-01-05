#' Parameter Cleanup After Flow Cytometry Gate Extractions
#'
#' @description
#' `OC_translate` cleans up the parameter names in the result of a flow cytometry polyfunctional gate extraction.
#'
#' @details
#' This function is designed to be used after a FLOW data extraction by
#' the `Flow_PolyF_Extract` function. It processes the extracted data to create a data frame with naming
#' conventions similar to those you would get from a direct flow cytometry data extraction using flowJo software.
#'
#' It modifies the names of parameters in the input data frame, in particular, changing names that contain the "&"
#' character or certain specified parameter names. It also computes a Frequency field as the ratio of Count to ParentCount.
#'
#' @param Flow.PolyF.Extract.data A data frame resulting from an FLOW data extraction by OpenCyto or the `Flow_PolyF_Extract` function.
#'
#' @return A data frame with the modified parameter names and additional Frequency field. The data frame includes the
#' fields: FCS (file names), PARAM (parameters), Count, ParentCount, and Frequency.
#'
#' @importFrom dplyr rename select filter mutate
#' @importFrom stringr str_replace str_replace_all str_remove str_remove_all str_detect
#' @importFrom tidyselect all_of
#'
#' @examples
#' \dontrun{
#' OC_translate(Flow_PolyF_Extract_data)
#' }
#'
#' @export
# OC_Translate <-  function(Flow.PolyF.Extract.data){
#
#   ## validate data input
#   validate_input_df(Flow.PolyF.Extract.data)
#
#   ## select columns in a vector to assign global variables
#   select_cols <- c("FCS", "PARAM", "Count", "ParentCount")
#
#   ## rename columns and drop unneeded ones
#   Flow.PolyF.Extract.data <- Flow.PolyF.Extract.data  |>
#     rename("FCS" = "name",
#            "PARAM" = "Population") |>
#     select(all_of(select_cols)) |>
#     filter(str_detect(.data$PARAM, "&"))
#
#   ## clean up strings with regex
#   Flow.PolyF.Extract.data <- Flow.PolyF.Extract.data |>
#   mutate(FCS = str_replace(.data$FCS, "fcs.*", "fcs"),
#          Frequency = .data$Count/.data$ParentCount,
#          PARAM = .data$PARAM |>
#            str_remove(pattern = ".*CD3/|")  |>
#            str_remove(pattern = ".*CD4z/|") |>
#            str_remove_all(pattern = "CD8:|CD4:") |>
#            str_replace_all(pattern = "&", replacement = "") |>
#                str_replace(pattern = "!CD38", replacement = "CD38-") |>
#                str_replace(pattern = "!CD69", replacement = "CD69-") |>
#                str_replace(pattern = "!Gnly", replacement = "Gnly-") |>
#                str_replace(pattern = "!GrzA", replacement = "GrzA-") |>
#                str_replace(pattern = "!GrzB", replacement = "GrzB-") |>
#                str_replace(pattern = "!KI67", replacement = "KI67-") |>
#                str_replace(pattern = "!LAG3", replacement = "LAG3-") |>
#                str_replace(pattern = "!PD1", replacement = "PD1-") |>
#                str_replace(pattern = "!Prf", replacement = "Prf-") |>
#                str_replace(pattern = "!TIM3", replacement = "TIM3-") |>
#                str_replace(pattern = "!CD137", replacement = "CD137-") |>
#                str_replace(pattern = "CD38(?!-)", replacement = "CD38+") |>
#                str_replace(pattern = "CD69(?!-)", replacement = "CD69+") |>
#                str_replace(pattern = "Gnly(?!-)", replacement = "Gnly+") |>
#                str_replace(pattern = "GrzA(?!-)", replacement = "GrzA+") |>
#                str_replace(pattern = "GrzB(?!-)", replacement = "GrzB+") |>
#                str_replace(pattern = "KI67(?!-)", replacement = "KI67+") |>
#                str_replace(pattern = "LAG3(?!-)", replacement = "LAG3+") |>
#                str_replace(pattern = "PD1(?!-)", replacement = "PD1+") |>
#                str_replace(pattern = "Prf(?!-)", replacement = "Prf+") |>
#                str_replace(pattern = "TIM3(?!-)", replacement = "TIM3+") |>
#                str_replace(pattern = "CD137(?!-)", replacement = "CD137+")
#       )
# }
#
