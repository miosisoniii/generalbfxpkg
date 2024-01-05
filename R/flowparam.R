
#' Extract flow parameters/markers from .wsp export
#'
#'
#' @param wsp_df tibble extracted from flowjo .wsp, containing columns: 'SID', 'VISIT', STIM', 'Population', 'Parent', 'Frequency', and 'ParentFrequency'.
#'
#' @return character vector list containing unique values present in CD4/CD8
#' @export
#'

get_flow_params <- function(wsp_df) {

  wsp_df |>
    pull(.data$Population) |>
    str_remove_all(".*CD8\\/|.*CD4\\/") |>
    str_subset("\\/", negate = TRUE) |>
    unique()

  ## may need a CD4/CD8 argument here. can be built out

}

#' Generate all combinations of parameters
#'
#' This function takes a list of unique parameter values for unique parameters in CD4/CD8 and generates all possible combinations of those parameters. It returns a tibble containing all the combinations of data.
#'
#' @param param.list A character vector list containing unique values for unique parameters in CD4/CD8.
#'
#' @return A tibble containing all the combinations of data.
#' @export
#'
#' @examples
#' paramlist <- c("CD38", "CD69", "Gnly", "GrzA", "GrzB", "Ki67",
#'                "LAG3", "PD1", "Prf", "TIM3", "CD137")
#' gen_flow_combos(paramlist)
#'

gen_flow_combos <- function(param.list) {

  # Create a list of all combinations of the input list
  combo_list <- unlist(
    lapply(1:length(param.list), function(x) combn(param.list, x, simplify = FALSE)),
    recursive = FALSE)

  # Create a dataframe with a column for each combination
  combo_df <- data.frame(
    strings = unlist(lapply(combo_list, paste, collapse = "_")),
    strings_regex = sapply(combo_list,
                           function(x) paste0("(.*", paste0(x, collapse = "\\+.*)(.*"), "\\+.*)")),
    stringsAsFactors = FALSE
  )

  # Add a row for each individual string
  combo_df <- rbind(
    combo_df,
    data.frame(
      strings = param.list,
      strings_regex = paste0(".*", param.list, "\\+.*"),
      stringsAsFactors = FALSE
    )) |>
    # replace the "_" with + for all positive combinations
    mutate(strings = str_replace_all(.data$strings, "\\_", "+"),
           strings = paste0(.data$strings, "+"))

  return(combo_df)

}
