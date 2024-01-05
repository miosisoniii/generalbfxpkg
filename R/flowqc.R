#' Import frequency counts from XML within .wsp
#'
#' This function imports frequency counts from an XML file within a .wsp (FlowJo workspace)
#' file and returns a tibble with columns 'name', 'Population', 'Parent', 'Frequency', and
#' 'ParentFrequency'. The function uses the flowCore package to extract gating information
#' from the .wsp file, convert it into a gating set, and then extract the population frequency
#' counts.
#'
#' @param path.to.wsp A string representing the file path to a single .wsp (FlowJo workspace) file.
#'
#' @return A tibble with columns 'name', 'Population', 'Parent', 'Frequency', and 'ParentFrequency',
#' containing the extracted frequency counts and associated metadata from the .wsp file.
#'
#' @export
#'
#  @importFrom CytoML open_flowjo_xml flowjo_to_gatingset
#  @importFrom flowWorkspace gs_pop_get_count_fast
#' @importFrom tibble as_tibble
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#'   output <- import_flow_freq("path/to/your/workspace.wsp")
#' }
#'

import_flow_freq <- function(path.to.wsp){


  ## testing requireNamespace for movement of BioC pkg deps to Suggests
  if (!rlang::is_installed("CytoML")) {
    stop("Package 'CytoML' needed for this fn to work.\n
         Please install using: BiocManager::install('CytoML').")
  }

  if (!rlang::is_installed("flowWorkspace")) {
    stop("Package 'flowWorkspace' needed for this fn to work.\n
         Please install using: BiocManager::install('flowWorkspace').")
  }


  if (!file.exists(path.to.wsp)) {
    cli_abort(message = paste0("File '", path.to.wsp, "' does not exist."))
  }

  CytoML::open_flowjo_xml(path.to.wsp) |>
    CytoML::flowjo_to_gatingset(name = "All Samples",
                        execute = FALSE) |>
    flowWorkspace::gs_pop_get_count_fast(statistic = "freq",
                          format = "long",
                          xml = TRUE) |>
    as_tibble()
}



#' Convert Numeric Frequencies to Percentages
#'
#' FlowJo-exported output does not come out in percentages, but in frequencies. This changes that
#'
#' @param x A numeric vector of frequencies.
#' @param round Logical value indicating whether to round the percentage values. Defaults to \code{FALSE}.
#'
#' @return A numeric vector of percentages.
#'
#' @details
#' This function multiplies each numeric frequency by 100 to convert it to a percentage. If the \code{round}
#' argument is set to \code{TRUE}, the percentage values will be rounded to three decimal places using the
#' \code{round()} function.
#'
#' @seealso
#' \code{\link{round}}
#'
#' @keywords numeric frequency percentage conversion
#'
#' @export
#'
#' @examples
#' convert_freq_pct(c(0.2, 0.4, 0.3, 0.1))
#' # Returns: 20 40 30 10
#'
#' convert_freq_pct(c(0.2, 0.4, 0.3, 0.1), round = TRUE)
#' # Returns: 20.000 40.000 30.000 10.000
#'
convert_freq_pct <- function(x, round = FALSE) {
  if (round == FALSE)
    return(x * 100)

  if (round == TRUE)
    return(round(x * 100, digits = 3))

}


#' Format Long Frequency Data
#'
#' This function takes a data frame containing long format frequency data and formats it for export. It renames
#' the columns and combines the parent and population columns into a single PARAM column. It also calculates
#' the frequency as a percentage and optionally splits the metadata columns (SID, VISIT, STIM) into a single
#' "name" column.
#'
#' @param df.wsp.export A data frame containing long format frequency data.
#' @param meta.split Logical value indicating whether to split the metadata columns (SID, VISIT, STIM) into
#' a single "name" column. Defaults to \code{FALSE}.
#'
#' @return A formatted data frame with the following columns: PARAM, Frequency, and (optionally) name, with frequency converted to percentage represented as a decimal (0.0-1.00).
#'
#' @examples
#'
#' data(flow01_count_data)
#'
#' ## flow01_count_data has anonymized, split UID into SID, VISIT, STIM col
#' format_long_freq(flow01_count_data, meta.split = TRUE)
#'
#' @details
#' This function performs the following operations on the input data frame:
#' 1. Removes the ParentFrequency column (if present).
#' 2. Removes the "CD8/" or "CD4/" prefix from the Population column.
#' 3. Filters out any rows where the Population column contains a "/".
#' 4. Extracts the Parent column ("CD4" or "CD8") from the Parent column.
#' 5. Combines the Parent and Population columns into a single PARAM column.
#' 6. Calculates the frequency as a percentage using the \code{convert_freq_pct()} function.
#' 7. Optionally splits the metadata columns (SID, VISIT, STIM) into a single "name" column if \code{meta.split = TRUE}.
#'
#' @seealso
#' \code{\link{convert_freq_pct}}, \code{\link{unite}}, \code{\link{select}}, \code{\link{mutate}}, \code{\link{filter}},
#' \code{\link{str_remove_all}}, \code{\link{str_detect}}, \code{\link{str_extract}}
#'
#' @importFrom rlang .data
#'
#' @keywords frequency data format long tidyverse
#'
#' @export
#'
format_long_freq <- function(df.wsp.export, meta.split = FALSE) {
  unite_wsp_cols <- c("Parent", "Population")


  # meta.split <- match.arg(meta.split)
  df.wsp.export <- df.wsp.export |>
    select(-all_of("ParentFrequency")) |>
    mutate(Population = str_remove_all(.data$Population, ".*CD8\\/|.*CD4\\/")) |>
    filter(str_detect(.data$Population, "\\/", negate = TRUE)) |>
    mutate(Parent = str_extract(.data$Parent, "CD4(?<!z)|CD8")) |>
    unite(all_of(unite_wsp_cols),
          col = "PARAM",
          sep = "/")

  if (meta.split == FALSE) ## raw metadata col name from wsp "name"
    df.wsp.export <- df.wsp.export |>
      mutate(name = str_remove(.data$name, "(?<=fcs)_.*"),
             Frequency = convert_freq_pct(.data$Frequency, round = FALSE))
  return(df.wsp.export)

  if (meta.split == TRUE) ## metadata cols include c("SID", "VISIT", "STIM)
    unite_meta_cols <- c("SID", "VISIT", "STIM")
    df.wsp.export <- df.wsp.export |>
      mutate(Frequency = convert_freq_pct(.data$Frequency, round = FALSE)) |>
      unite(all_of(unite_meta_cols),
            col = "name",
            sep = "_")
  return(df.wsp.export)

}


#' Pre-QC Workflow Wrapper for Flow Cytometry Data
#'
#' This function wraps a pre-QC (quality control) workflow for flow cytometry data. It processes
#' data files from a specified directory, extracts frequency information, standardizes the
#' data format, converts frequencies to percentages, and merges the results into an output
#' dataframe.
#'
#' @param path.to.analyst.dir A character string specifying the path to the directory containing
#' the flow cytometry data files.
#' @param path.pattern A character string specifying the regex. Good to use the analyst's initials here.
#' @param meta.split A logical value indicating whether the metadata should be split or not.
#' Default is FALSE.
#'
#' @return A data frame containing the processed flow cytometry data, with frequencies converted
#' to percentages.
#'
#' @examples
#' \dontrun{preqc_data <- flow_preqc_wrap("path/to/analyst/directory")}
#'
#' @importFrom stringr str_detect
#' @importFrom pbapply pblapply
#' @importFrom dplyr bind_rows
#'
#' @export
#'
flow_preqc_wrap <- function(path.to.analyst.dir, path.pattern = "*.wsp$", meta.split = FALSE) {

  path_list <- list.files(path = path.to.analyst.dir,
                          pattern = path.pattern,
                          full.names = TRUE,
                          recursive = TRUE)

  if (length(path_list) == 0 | is.null(path_list)) cli::cli_abort("Path does not exist. Please check the path.")

  ## drop any ref donor occurrences
  path_list <- path_list[str_detect(path_list, "REF|DON|ref|don", negate = TRUE)]

  message("Initializing Pre-QC Workflow:")
  message("Exporting frequencies from FlowJo... (1/3)")
  exports <- pblapply(path_list, import_flow_freq)

  message("Converting frequencies to percent... (2/3)")
  exports_std <- pblapply(exports,
                          function(x) format_long_freq(x, meta.split = meta.split))

  message("Merging frequencies into output dataframe...(3/3)")
  df_out <- bind_rows(exports_std)
  message("Done! Pre-QC Workflow complete.")
  return(df_out)
}


#' Quality Control - Compare Flow Data Between Two Analysts
#'
#' This function compares flow cytometry data from two analysts by calculating the coefficient of variation (CV) and determining if the data pass or fail the quality control criteria.
#'
#' @param analyst1.df A data frame containing the flow cytometry data from the first analyst, with columns "name", "PARAM", and "Frequency".
#' @param analyst2.df A data frame containing the flow cytometry data from the second analyst, with columns "name", "PARAM", and "Frequency".
#'
#' @return A data frame containing the original data and two additional columns, 'rast_cv' for the calculated CV and 'qc_pass' to indicate if the data passed or failed the quality control criteria (PASS or FAIL). If the input data frames have a different number of rows, the function will return an error message.
#'
#' @importFrom rlang .data
#'
#' @examples
#' analyst1_data <- data.frame(name = c("Sample1", "Sample2"),
#' PARAM = c("CD4", "CD8"),
#' Frequency = c(55.0, 30.0))
#' analyst2_data <- data.frame(name = c("Sample1", "Sample2"),
#' PARAM = c("CD4", "CD8"),
#' Frequency = c(54.5, 29.5))
#' comparison_result <- flow_qc_compare(analyst1_data, analyst2_data)
#'
#' @export
flow_qc_compare <- function(analyst1.df, analyst2.df) {

  if (nrow(analyst1.df) != nrow(analyst2.df))
    cli_abort(message = paste0(
      "Differing number of rows in analyst df's.\n",
      nrow(analyst1.df), " rows in analyst1.df.\n",
      nrow(analyst2.df), " rows in analyst2.df"
    ))

  if (nrow(analyst1.df) == nrow(analyst2.df))
    cv_df <- left_join(analyst1.df, analyst2.df,
                    by = c("name", "PARAM"),
                    suffix = c("_a1", "_a2")) |>
      rowwise() |>
      mutate(cv = cv(.data$Frequency_a1, .data$Frequency_a2, aszero = TRUE),
             qc_pass = ifelse(.data$cv < 20.0, "PASS", "FAIL"))
  return(cv_df)
}


