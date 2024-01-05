# FN VALIDATION CODE -----------------------------------------------------------

#' Validate input data frame
#'
#' This function checks whether a given input is a data frame.
#'
#' @param .data Input data frame to validate
#' @param call Calling environment for debugging purposes
#'
#' @return The input data frame if it passes validation
#'
#' @export
#'
#' @examples
#' validate_input_df(iris)
validate_input_df <- function(.data, call = rlang::caller_env()) {
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      ## information for the user
      c(
        "Must supply a data frame",
        "x" = "You supplied a {.cls {class(.data)}}."
      ),
      ## class information for developers
      class = "bfxpkg_error_data",
      ## other information for developers
      class_data = class(.data),
      ## tell the user where we are calling (the fn) from (Renvir(), etc)
      call = call
    )

  }

  invisible(.data)
}

#' Validate input columns
#'
#' This function checks whether a given data frame contains all the required columns.
#'
#' @param .data Input data frame to validate
#' @param cols_req A character vector of required column names
#' @param call Calling environment for debugging purposes
#'
#' @return The input data frame if it passes validation
#'
#' @export
#'
#' @examples
#' validate_input_cols(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
validate_input_cols <- function(.data, cols_req, call = rlang::caller_env()) {

  cols_data <- names(.data)
  cols_missing <- cols_req[!(cols_req %in% cols_data)]

  ## predicate
  if (length(cols_missing) > 0) {

    ## helper fn to format quantities
    # - see https://cli.r-lib.org/articles/pluralization.html
    qlen <- function(x) cli::qty(length(x))

    cli::cli_abort(
      ## information for the user
      c(
        "Data frame is missing some required columns",
        "i" = "Required columns: {.field {cols_req}}",
        "x" = "Missing columns: {.field {cols_missing}}"
      ),
      ## class information for developers
      class = "bfxpkg_error_cols",
      ## other information for developers
      cols_req = cols_req,
      cols_data = cols_data,
      ## tell the user where we are calling (the fn) from (Renvir(), etc)
      call = call
    )

  }

  invisible(.data)
}


#' Validate Uncommon Visits Function
#'
#' This function checks for any errant VISIT values in a specified column of a data frame.
#' It validates whether there are any unexpected visit names, such as 'Unscheduled', 'scr',
#' 're-screen', 'rescreen', 'screening', and allows for additional errant visit names to be
#' specified by the user.
#'
#' @param .data A data frame which contains a column with visit names that need to be validated.
#' @param visit.col The name of the column in the data frame which contains the visit names.
#' Default is "VISIT".
#' @param err.visits A character vector of additional errant visit names that should be
#' flagged as errors. Default is c("Unscheduled").
#' @param call The environment from which the function is being called. Default is
#' rlang::caller_env().
#'
#' @return The function primarily performs a validation check. If no errant visit values
#' are found, it returns the original data invisibly and prints a message "No issues with
#' columns. Proceed!". If errant visit values are detected, it throws an error with a
#' detailed message including the names of the errant visit values found.
#'
#' @examples
#'
#' \dontrun{
#' df <- data.frame(
#'   'VISIT' = c('Scheduled', 'Unscheduled', 'Screening'),
#'   'Other' = c(1, 2, 3)
#' )
#' validate_uncommon_visits(df)
#'}
#'
#' @export
validate_uncommon_visits <- function(.data,
                                     visit.col = "VISIT",
                                     err.visits = c("Unscheduled"),
                                     call = rlang::caller_env()){

  validate_input_df(.data)
  validate_input_cols(.data, visit.col)
  visit.col <- match.arg(visit.col, choices = unique(names(.data)))

  ## get unique visit values
  uniq_visits <- tolower(unique(.data[[visit.col]]))
  ## find errant visit columns
  err.visits <- c(tolower(err.visits), "unscheduled", "scr", "re-screen", "rescreen", "screening")
  ## show values that occur
  show_err_visits <- intersect(x = err.visits, y = uniq_visits)

  if (length(show_err_visits) > 0) {

    cli::cli_abort(
      c(
        "Input has errant visit values, please remove using dplyr::filter() :",
        "i" = "Cols in input dataframe: {.field {uniq_visits}}",
        "x" = "Problem  with: {.field {show_err_visits}}"
      ),
      ## class_information for developers
      class = "bfxpkg_error_visit",
      ## other information for developers
      uniq_visits = uniq_visits,
      show_err_visits = show_err_visits,
      ## tell the user where we are calling the fn from (Renvir(), etc.)
      call = call
    )
  }

  if (length(show_err_visits) == 0) {
    message("No issues with columns. Proceed!")
  }

  invisible(.data)
}




