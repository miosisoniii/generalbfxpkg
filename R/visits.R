#' Return VISITs for study-specific
#'
#' This function takes in a character vector specifying the study type and returns a vector of all visits for the given study. If the study type is set to "any", the function returns all possible visits.
#'
#' @param v.study A character vector specifying the study type. Options are "any", "proj01", "proj02", "proj03", or "flow01". Default is "any".
#'
#' @return A character vector of all visits for the specified study type.
#' @export
#'
#' @examples
#' # Returns all possible visits
#' visit_test(v.study = "any")
#'
#' # Returns all visits for the HPV study
#' visit_test(v.study = "proj01")
#'
#' # Returns all visits for the flow01 study
#' visit_test(v.study = "flow01")
#'
#' # Returns an error because the study type is not recognized
#' # visit_test(v.study = "XYZ")
#'
visit_test <- function(v.study = c("any", "proj01", "proj02", "proj03", "flow01")) {

  v.study <- match.arg(v.study)
  scr <- c("SCR", "SCREEN", "SCREENING", "UNSCHEDULED", "BASELINE")
  day <- paste0("DAY", 0:100)
  wk <- paste0("WEEK", 0:100)
  allvisits <- c(scr, day, wk)

  if (v.study == "any") {
    return(allvisits)

  } else if (v.study == "proj01") {
    c('BASELINE', 'DAY0', 'SCREEN', 'WEEK2',
      'WEEK4', 'WEEK6', 'WEEK12', 'WEEK24', 'WEEK48')

  } else if (v.study == "proj02") {
    c('BASELINE', 'SCREEN', 'DAY0', 'WEEK2',
      'WEEK4', 'WEEK6', 'WEEK12', 'WEEK24', 'WEEK48')

  } else if (v.study == "proj03") {
    c('BASELINE', 'SCREEN', 'RESCREEN', 'DAY0',
      'WEEK6', "WEEK9", 'WEEK11', 'WEEK26', 'WEEK52')

  } else if (v.study == "flow01") {
    c('BASELINE', 'PreBD', 'PBD W2', 'PBD W12',
      'PBD W24', 'Early Termination', "WK6", "WK6_DELTA")

  }
}

#' Recode of VISIT strings
#'
#' This function takes in a character vector with a specific format and recodes the visit name in the vector based on the output format specified. It can be used within mutate functions to recode the visit name in a data frame column.
#'
#' @param v.string A character vector with a specific format, e.g. "1235125_WEEK 6_HPV11.fcs"
#' @param output.format A character vector specifying the output format, either "full" or "abbr". Default is "full".
#'
#' @return A character vector with the recoded visit name.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#' ex1 <- "1235125_WEEK 6_HPV11.fcs"
#' ex_df <- data.frame(
#' SID = paste0("0000-000", seq_along(1:4)),
#' VISIT = c("Week 1", "Week1", "Week2", "Week_2"),
#' STIM = paste0("STIM", seq_along(1:2))) |>
#' mutate(FILENAME = paste(SID, VISIT, STIM, sep = "_")) |>
#' relocate(FILENAME, .before = SID)
#'
#' # using on a single string, returns: "Week6"
#' recode_visits(v.string = ex1, output.format = "full")
#'
#' # using on a single string, returns: "WK6" (preferred)
#' recode_visits(v.string = ex1, output.format = "abbr")
#'
#' # use within a mutate for filename column
#' ex_df %>% rowwise() %>% mutate(FILENAME = recode_visits(FILENAME, "full"))
#'
#' # use within a mutate for VISIT column, must use dplyr::rowwise()
#' ex_df %>% rowwise() %>% mutate(VISIT = recode_visits(VISIT, "full"))
#'
#' # use mutate for the FILENAME column (or fcs filename), must use rowwise()
#' ex_df %>% rowwise() %>% mutate(VISIT = recode_visits(VISIT, "abbr"))
#'
recode_visits <- function(v.string, output.format = c("abbr", "full")){

  ## load visit_recoding df
  visit_recoding <- get_dat(data.name = "visit_recoding")

  if ((output.format == "abbr" | output.format == "full")) {
    v.string <- tolower(v.string)
    output.format <- match.arg(output.format)

    str_idx <- str_which(v.string, visit_recoding$regex_vis)
    targ_pattern <- visit_recoding$regex_vis[[str_idx]]
    targ_replace <- visit_recoding[[output.format]][[str_idx]]

    str_replace(string = v.string,
                pattern = targ_pattern,
                replacement = targ_replace)

  } else if (!(output.format == "abbr" | output.format == "full")) {
    stop("Error. Please supply 'abbr' or 'full' to the output.format argument.")

  }
}
