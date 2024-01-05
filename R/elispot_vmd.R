# Optimized elispot_vmd.R code -------------------------------------------------

#' Set Recode Vector
#'
#' This function renames the unique values in a column to the column's name and
#' appends an index to them. It returns a named vector.
#'
#' @param df A dataframe.
#' @param df.var A character string specifying the name of the column to be recoded.
#'
#' @return A named character vector with new names for unique values in the specified column.
#' @export
#'
#' @examples
#' # Example usage:
#' df <- proj01_elispot_data
#' recode_vector <- get_recode_vector(df, "VISIT")
#' str(recode_vector)
get_recode_vector <- function(df, df.var){

  validate_input_df(df)
  validate_input_cols(df, df.var)


  var_names <- unique(df[[df.var]])
  # Create new names (e.g., STIM1, STIM2, ...)
  var_common_names <- paste0(df.var, seq_along(var_names))
  recode_vector <- setNames(var_common_names, var_names)

  if (is.character(recode_vector) && is.vector(recode_vector) && !is.null(names(recode_vector))) {
    return(recode_vector)
  } else {
    cli::cli_abort("The returned object is not a named character vector.")
  }
}

#' Convert to Base Variable
#'
#' This function takes the output from `get_recode_vector()` and changes the
#' values in the supplied dataframe to the values labeled in the returned
#' recode_vector output.
#'
#' @param df A dataframe.
#' @param df.var A character string specifying the name of the column to be recoded.
#'
#' @return A dataframe with the specified column's values recoded based on the recode_vector.
#' @export
#'
#' @examples
#' # Example usage:
#' df <- proj01_elispot_data
#' converted_df <- recode_col_to_base(df, "STIM")
#' converted_df |> head()
recode_col_to_base <- function(df, df.var) {

  validate_input_df(df)
  validate_input_cols(df, df.var)

  ## check NA
  NA_val <- df |> dplyr::filter(is.na(df.var))
  if (nrow(NA_val) > 0) {cli::cli_abort(message = "NA detected. Please omit or escalate.")}

  # if (is.na(df[[df.var]]))

  recode_vector <- get_recode_vector(df = df, df.var = df.var)
  df |> mutate(!!df.var := recode(!!sym(df.var), !!!recode_vector))
}

#' Reverse Base Variable to Original
#'
#' This function takes the output from `recode_col_to_base()` and reverses the
#' "basevar" values back to the original values in the column, using the
#' recode_vector from the original dataframe.
#'
#' @param converted.df A dataframe with the "basevar" values.
#' @param orig.df The original dataframe before conversion.
#' @param df.var A character string specifying the name of the column to be recoded back to the original values.
#'
#' @return A dataframe with the specified column's values reversed to their original state.
#' @export
#'
#' @examples
#' # Example usage:
#' original_df <- proj01_elispot_data |> head()
#' converted_df <- recode_col_to_base(original_df, "STIM")
#' reversed_df <- recode_col_to_orig (converted_df, original_df, "STIM")
#'
#' ## show converted df
#' head(converted_df)
#' ## show 'reversed' converted df
#' head(reversed_df)
recode_col_to_orig <- function(converted.df, orig.df, df.var) {

  validate_input_df(orig.df)
  validate_input_cols(orig.df, df.var)
  validate_input_df(converted.df)
  validate_input_cols(converted.df, df.var)

  orig_recode_vec <- get_recode_vector(df = orig.df, df.var = df.var)
  rev_recode_vec <- setNames(names(orig_recode_vec), orig_recode_vec)

  converted.df |>
    mutate(!!df.var := recode(!!sym(df.var), !!!rev_recode_vec))
}


#' Calculate the sum of specified columns in a wide-format data frame
#'
#' This function calculates the sum of specified columns in a wide-format data frame,
#' using dplyr's `across()` function. It returns a new data frame with an additional
#' column containing the calculated sum.
#'
#' @param df The wide-format data frame to be used
#' @param sum.cols A character vector of column names to be summed
#' @param output.col The name of the column to be created for the calculated sum
#' @param na.rm Logical indicating whether or not to be using na.rm = TRUE. Default is TRUE
#'
#' @return A new data frame with an additional column containing the calculated sum
#'
#' @importFrom dplyr rowwise mutate ungroup
#'
#' @examples
#' ## get proj01 elispot dm
#' dat <- proj01_elispot_data |> head(n = 250)
#' ## make wide
#' dat_wide <- dat |> tidyr::pivot_wider(names_from = "STIM", values_from = "RESULT")
#'
#' ## assign cols for sum in vector obj
#' hpv16cols <- c("HPV16E6", "HPV16E7")
#' hpv18cols <- c("HPV18E6", "HPV18E7")
#' totalhpvcols <- c(hpv16cols, hpv18cols)
#'
#' ## calculate sum for total_hpv16, total_hpv18, total_hpv
#' ## test the fn sum_across_wide
#' sum_wide <- dat_wide |>
#'   sum_across_wide(sum.cols = hpv16cols,
#'                   output.col = "TOTAL_HPV16") |>
#'   sum_across_wide(sum.cols = hpv18cols,
#'                   output.col = "TOTAL_HPV18") |>
#'   sum_across_wide(sum.cols = totalhpvcols,
#'                   output.col = "TOTAL_HPV")
#' ## show the output
#' head(sum_wide)
#'
#' @export
sum_across_wide <- function(df, sum.cols, output.col, na.rm = TRUE) {

  validate_input_df(df)
  validate_input_cols(df, sum.cols)

  df |>
    rowwise() |>
    mutate(!!output.col := sum(across(all_of(sum.cols)),
                               na.rm = na.rm)) |> ## na rm for total
    ungroup() ## rowwise groups the sum cols, ungroup the dataframe here


  # output.col <- match.arg(arg = output.col, choices = names(df))
  #
  # recode_vector <- get_recode_vector(df = df, df.var = df.var)
  # df |> mutate(!!df.var := recode(!!sym(df.var), !!!recode_vector))
}

#' Get Baseline Function
#'
#' This function is used to create a 'BASELINE' column in a data frame based on the contents
#' of 'day0' and 'screen' columns. It identifies the 'day0' and 'screen' columns based on
#' regex patterns. 'day0' takes precedence over 'screen' when both are available. Dataframe
#' passed into argument MUST be in WIDE format. Should also considering adding another argument to deal with
#' 'Unscheduled' or errant values in the VISIT columns...
#'
#' @param df A data frame for which the 'BASELINE' column should be generated. The data frame
#' should have columns that match the 'day0' and 'screen' patterns defined in the function.
#' @param d0.regex character string of regex, default "d0|d 0|day 0|day0" which can be added to depending on df values
#' @param scr.regex character string of regex, default "screen|screening|scr"; can be added to depending on df values
#'
#' @return A data frame identical to the input data frame, but with an additional 'BASELINE'
#' column. This column contains the value from the 'day0' column, if available, otherwise
#' the value from the 'screen' column, if available. If neither are available, the 'BASELINE'
#' value will be set to NA.
#'
#' @importFrom stringr str_which
#' @importFrom dplyr mutate
#'
#' @examples
#' df <- data.frame(
#'   'Day0' = c(1, NA, 3),
#'   'Screen' = c(NA, 2, NA),
#'   'Other' = c(3, 4, 5)
#' )
#' get_baseline(df, d0.regex = "d0|d 0|day 0|day0", scr.regex = "screen|screening|scr")
#'
#' @export
get_baseline <- function(df,
                         d0.regex = "d.*0",
                         scr.regex = "scr.*|re*scr.*") {

  validate_input_df(df)
  # validate_input_cols(df, baseline.col)
  # baseline.col <- match.arg(baseline.col, choices = unique(names(df)))

  ## get strings that are baseline - added in fn argument
  # d0.regex <- "d.*0"
  # scr.regex <- "scr.*|re*scr.*"
  # bline_regex <- paste(d0_regex, scr_regex, sep = "|")

  ## get indices for baseline strings
  d0_col_ind <- str_which(string = tolower(names(df)), pattern = d0.regex)
  scr_col_ind <- str_which(string = tolower(names(df)), pattern = scr.regex)
  d0_col_ind <- names(df)[d0_col_ind]
  scr_col_ind <- names(df)[scr_col_ind]

  df_out <- df |>
    mutate(BASELINE = case_when(
      is.na(.data[[d0_col_ind]]) & is.na(.data[[scr_col_ind]]) ~ NA,
      is.na(.data[[d0_col_ind]]) ~ .data[[scr_col_ind]],
      is.na(.data[[scr_col_ind]]) ~ .data[[d0_col_ind]],
      .default = NA_real_
    )
  )

  return(df_out)
}


#' Delta Calculation Function
#'
#' @description
#' `delta_calc` is a function that calculates the delta or difference between
#' baseline values and corresponding visit values for each subject in a dataset.
#' It requires a dataframe in a wide format with visits specified in the column headers.
#'
#' @details
#' The function uses tidyverse's dplyr and tidyselect packages to apply
#' the calculation across a set of columns determined by the regex pattern.
#' The pattern defaults to "WEEK|WK|Week " which means that it will match columns
#' that have "WEEK", "WK", or "Week " in their names. These columns are assumed to
#' represent different visits. The delta calculation is performed by subtracting
#' the 'BASELINE' values from the corresponding visit values.
#'
#' The resulting dataframe will include new columns for each visit, with column
#' names in the format "{Visit}_DELTA". These columns represent the delta values
#' for each subject and visit.
#'
#' @param dat A dataframe in a wide format with column headers specifying different visits.
#' @param regex A character string containing a regular expression pattern used to match visit columns.
#' The default pattern is "WEEK|WK|Week " which matches columns containing "WEEK", "WK", or "Week ".
#'
#' @return A dataframe with added columns containing delta values for each visit.
#' These columns are named as "{Visit}_DELTA".
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   SID = c("001", "002", "003"),
#'   BASELINE = c(10, 12, 15),
#'   WEEK1 = c(15, 17, 20),
#'   WEEK2 = c(20, 22, 25)
#' )
#'
#' delta_calc(data)
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr across mutate
#' @importFrom tidyselect matches
#'
#' @export
#'

delta_calc <- function(dat, regex = "WEEK|WK|Week ") {
  # targ_var <- "BASELINE"
  dat |>
    mutate(
      across(
        matches(match = regex, ignore.case = TRUE, perl = TRUE),
        ~ .x - .data$BASELINE,
        .names = "{col}_DELTA"
      )
    )
}


# Original elispot_vmd.R code --------------------------------------------------

#' proj01 total stim calculations
#'
#' @param dat dataframe containing DM format cols 'SID', 'STIM', 'VISIT', 'RESULT'
#'
#' @return dataframe in long format containing newly calculated STIMs
#' TOTAL_HPV16, TOTAL_HPV18, TOTAL_HPV

# proj01_stim_calc <- function(dat) {
#   name_from_col  <- "STIM"
#   val_from_col <- "RESULT"
#
#   name_long_col <- c("SID", "VISIT")
#
#   dat |>
#     pivot_wider(names_from = all_of(name_from_col),
#                 values_from = all_of(val_from_col)) |>
#     mutate(TOTAL_HPV16 = .data$HPV16E6 + .data$HPV16E7,
#            TOTAL_HPV18 = .data$HPV18E6 + .data$HPV18E7,
#            TOTAL_HPV = .data$TOTAL_HPV16 + .data$TOTAL_HPV18) |>
#     pivot_longer(cols = -all_of(name_long_col),
#                  names_to = "STIM",
#                  values_to = "RESULT")
#
# }

#' proj02 total stim calculations
#'
#' @param dat DM formatted file for proj02 data
#'
#' @importFrom rlang .data
#'
#' @return dataframe in long format containing newly calculated STIM proj02_TOTAL

# proj02_stim_calc <- function(dat) {
#   name_from_col  <- "STIM"
#   val_from_col <- "RESULT"
#   name_long_col <- c("SID", "VISIT", "BATCH")
#
#   dat |>
#     pivot_wider(names_from = all_of(name_from_col),
#                 values_from = all_of(val_from_col)) |>
#     mutate(TOTAL_proj02 = .data$`proj02-1` + .data$`proj02-2`) |>
#     pivot_longer(cols = -all_of(name_long_col),
#                  names_to = "STIM",
#                  values_to = "RESULT")
#
# }

#' proj03 total stim calculations
#'
#' @param dat dataframe containing DM format cols 'SID', 'STIM', 'VISIT', 'RESULT'
#'
#' @importFrom rlang .data
#'
#' @return dataframe in long format containing newly calculated STIMs
#' TOTAL_HPV6, TOTAL_HPV11, TOTAL_HPV

# proj03_stim_calc <- function(dat) {
#   name_from_col  <- "STIM"
#   val_from_col <- "RESULT"
#
#   name_long_col <- c("SID", "VISIT")
#
#   dat |>
#     pivot_wider(names_from = all_of(name_from_col),
#                 values_from = all_of(val_from_col)) |>
#     mutate(TOTAL_HPV6 = .data$HPV6E6 + .data$HPV6E7,
#            TOTAL_HPV11 = .data$HPV11E6 + .data$HPV11E7,
#            TOTAL_3107 = .data$TOTAL_HPV6 + .data$TOTAL_HPV11) |>
#     pivot_longer(cols = -all_of(name_long_col),
#                  names_to = "STIM",
#                  values_to = "RESULT")
#
# }

#' proj03 total stim calculations
#'
#' @param dat dataframe containing DM format cols 'SID', 'STIM', 'VISIT', 'RESULT'
#'
#' @importFrom rlang .data
#'
#' @return dataframe in long format containing newly calculated STIMs
#' TOTAL_HPV6, TOTAL_HPV11, TOTAL_HPV

# proj03_stim_calc <- function(dat) {
#   name_from_col  <- "STIM"
#   val_from_col <- "RESULT"
#
#   name_long_col <- c("SID", "VISIT")
#
#   dat |>
#     pivot_wider(names_from = all_of(name_from_col),
#                 values_from = all_of(val_from_col)) |>
#     mutate(TOTAL_HPV6 = .data$HPV6E6 + .data$HPV6E7,
#            TOTAL_HPV11 = .data$HPV11E6 + .data$HPV11E7,
#            TOTAL_3107 = .data$TOTAL_HPV6 + .data$TOTAL_HPV11) |>
#     pivot_longer(cols = -all_of(name_long_col),
#                  names_to = "STIM",
#                  values_to = "RESULT")
#
# }





#' Wrapper fn for total stim calculations using if/else for study
#'
#' @param dat dataframe containing DM format cols 'SID', 'STIM', 'VISIT', 'RESULT'
#' @param study character string containing one of 'proj02002', 'proj01', 'proj03' etc.
#'
#' @importFrom rlang .data
#'
#' @return dataframe in long format containing newly calculated STIM values

# stim_calc <- function(dat, study = c("proj01", "proj02002", "proj03")) {
#   study <- match.arg(study)
#
#   study <- match.arg(study, choices = c("proj01", "proj02002"))
#   study <- toupper(study)
#
#   if (study == "proj01") {
#     proj01_cols <- c("SID", "VISIT", "STIM", "RESULT")
#     dat <- dat |> select(all_of(proj01_cols))
#     proj01_stim_calc(dat)
#
#   } else if (study == "proj02002") {
#     proj02002_cols <- c("SID", "VISIT", "STIM", "RESULT", "BATCH")
#     dat <- dat |> select(all_of(proj02002_cols))
#     proj02002_stim_calc(dat)
#
#   } else if (study == "proj03") {
#     dat <- dat |>
#       select("SID", "STIM", "VISIT", "RESULT", "BATCH")
#     proj03_stim_calc(dat)
#   }
#   else {
#     cli_abort(message = "Wrong study entered.\nPlease enter one of the following: 'proj01' or 'proj02002' or 'proj03")
#
#   }
# }




#' Baseline calculations
#'
#' @param dat dataframe in long format containing cols 'SID', 'VISIT', 'STIM', 'RESULT'
#'
#' @importFrom rlang .data
#'
#' @return dataframe in long format with newly calculated BASELINE in 'VISIT' col.

# baseline_calc <- function(dat) {
#   name_from_col <- "VISIT"
#   val_from_col <- "RESULT"
#
#   dat |>
#     mutate(VISIT = toupper(.data$VISIT)) |>
#     mutate(VISIT = str_replace_all(.data$VISIT,
#                                    pattern = "D0|DAY_0|DAY 0|D 0",
#                                    replacement = "DAY0")) |>
#     mutate(VISIT = str_replace_all(.data$VISIT,
#                                    pattern = "SCR$|SCREENING$",
#                                    replacement = "SCREEN")) |>
#     pivot_wider(names_from = all_of(name_from_col),
#                 values_from = all_of(val_from_col)) |>
#     mutate(BASELINE =
#              case_when(
#                is.na(.data$DAY0) == FALSE ~ .data$DAY0,
#                is.na(.data$DAY0) == TRUE ~ .data$SCREEN
#              )
#     )
# }


#' ELISPOT VMD generation function
#'
#' @param dat DM formatted data for ELISPOT
#' @param study The study the data is from. Right now accepts 'proj01' or 'proj02002'
#' @param regex regular expression for the character string containing WEEK to perform delta subtractions
#'
#' @return a data.frame
#'
#'
#'
# elispot_vmd <- function(dat, study, regex = "WEEK|WK|Week"){
#
#     stim_calc(dat = dat, study = study) |>
#     baseline_calc(dat = _) |>
#     delta_calc(dat = _, regex = regex)
# }
#
