#' Retrieve Available Dataset Names in generalbfxpkg Package
#'
#' This function returns a character vector containing the names of available datasets in the generalbfxpkg package.
#' The dataset names can be used with the get_dat function to load the respective dataset for further analysis.
#'
#' @return A character vector containing the names of available datasets in the generalbfxpkg package.
#'
#' @examples
#' \dontrun{
#' # Get the list of dataset names
#' data_names <- get_generalbfxpkg_data_names()
#'
#' # Load a specific dataset using the get_dat function
#' some_data <- get_dat(data_names[1])
#' }
#'
#' @seealso \code{\link{get_dat}} for loading the datasets using their names.
#'
#' @export
get_generalbfxpkg_data_names <- function() {
  c(
    "visit_recoding",             # Reference VISIT data for conversion to uniform format
    "flow_param_data",            # .wsp import data
    "flowmarkerlist",             # FLOW markerlist translation for FLOW ParameterCalc
    "markers_flow",               # all marker combos for FLOW Parameter Frequency Calculations
    "flow_qcmeet_input",          # input for Calculating CV and QC meeting xlsx
    "FLOW_intermediate_data",     # wide-format DM converted to long before VMD generation
    "elispot01_data",             # proj01 ELISPOT data in long format
    "elispot02_data"              # proj02 ELISPOT data in long format
  )
}


#' Add or drop hyphen in SID (column)
#'
#' This function adds or drops a hyphen in a character string representing an 8-digit subject ID (SID).
#'
#' @param sid.string A character string representing an 8-digit SID, ideally in the format "NNNN-NNNN", where "N" is a digit. If the SID string does not contain a hyphen, the function will add one after the fourth digit.
#' @param add.drop A character string specifying whether to add or drop the hyphen. Must be either "add" or "drop".
#'
#' @return A character string representing the modified SID, with a hyphen added or dropped.
#'
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#'
#' library(dplyr)
#'
#' ## Create example SIDs
#' sid1 <- "0101-0101"
#' sid2 <- "01010101"
#'
#' ## Add the hyphen to SID2
#' add.drop_sid_hyphen(sid.string = sid2, add.drop = "add")
#'
#' ## Drop the hyphen from SID1
#' add.drop_sid_hyphen(sid.string = sid1, add.drop = "drop")
#'
#' ## Use with mutate(), and rowwise()
#' ex_df <- data.frame(SID = paste0("0000-000", seq_along(1:9)))
#' ex_df %>% rowwise() %>% mutate(SID = add.drop_sid_hyphen(sid.string = SID, add.drop = "drop"))
#'
#' @seealso \code{\link{mutate}}
#'
#' @keywords SID, hyphen, mutate
#' @export
add.drop_sid_hyphen <- function(sid.string, add.drop = c("add", "drop")) {
  add.drop <- match.arg(add.drop)

  if (add.drop == "add") {

    if (str_detect(sid.string, "\\-"))
      cli::cli_abort(message = "String contains '-' already. Have a look at the unique() SID's.")
    if (!str_detect(sid.string, "\\-"))
      str_replace(sid.string, "(\\d{4})(\\d+)$", "\\1-\\2")

  } else if (add.drop == "drop") {

    if (!str_detect(sid.string, "\\-"))
      cli::cli_abort(message = "String does not contain '-'. Check out the unique() SID's.")
    if (nchar(sid.string) < 8)
      str_replace(sid.string, "\\-", "")
    if (nchar(sid.string) > 8)
      str_replace(sid.string, "(\\d{4})-(\\d+)", "\\1\\2")

  } else {
    cli::cli_abort("Incorrect argument supplied. Specify 'add' or 'drop'.")
  }
}


#' Get Dataset from the generalbfxpkg Package
#'
#' This function retrieves a dataset from the generalbfxpkg package and makes it available for use in other functions.
#' The get_dat function allows you to easily access the data stored within the package and pass it to other functions.
#'
#' @param data.name A character string specifying the name of the dataset to be retrieved from the generalbfxpkg package.
#' The name should match one of the datasets available in the package.
#'
#' @return The requested dataset from the generalbfxpkg package as an object.
#'
#' @examples
#' \dontrun{
#' # Load the iris dataset from generalbfxpkg package
#' visit_data <- get_dat("visit_recoding")
#'
#' # Perform some analysis on the loaded dataset
#' # summary(iris_data)
#' }
#'
#' @seealso \code{\link[utils]{data}} for more details on how the data is loaded from the package.
#'
#' @export
#'
#' @importFrom utils data
get_dat <- function(data.name = "visit_recoding") {
  data.name <- match.arg(data.name,
                         choices = get_generalbfxpkg_data_names(),
                         )

  ## create new environment to load the data
  e <- new.env()
  name <- utils::data(list = data.name,
                      package = "generalbfxpkg",
                      envir = e)
  e[[name]]
}

# General Data Wrangling Functions ---------------------------------------------

#' Get Today's Date in Custom Format
#'
#' This function retrieves the current date and returns it in the format "ddMonyy".
#' The format consists of a two-digit day, a three-letter abbreviated month, and a two-digit year.
#'
#' @return A character string representing the current date in the format "ddMonyy".
#' @examples
#' # Get today's date in the "ddMonyy" format
#' # produces "26Apr23"
#' get_date()
#'
#' # Best used in the context of writing a filename with a date
#' paste0("proj03_LONG_MS3_", get_date(), ".csv")
#'
#' @export
get_date <- function() {
  today <- Sys.Date()
  formatted_date <- strftime(today, format = "%d%b%y")
  return(formatted_date)
}

#' Get User Initials from Current Username
#'
#' This function retrieves the current username from `Sys.info()["user"]`,
#' converts it to lowercase, and returns the associated initials for a list of predefined users.
#' If the current username is not found in the predefined list, the function returns NULL.
#'
#' @return A character string representing the initials of the current user if found in the list, otherwise NULL.
#' @examples
#' # Get initials for the current user
#' user_initials <- get_initials()
#' if (!is.null(user_initials)) {
#'   cat("The user initials are:", user_initials)
#' } else {
#'   cat("The user is not in the predefined list.")
#' }
#'
#' # best used for pasting together output filename with get_date()
#' paste0("proj03_ELISPOT_DM_", get_initials(), "_", get_date(), ".csv")
#'
#' @export
get_initials <- function(){
  current_user <- tolower(Sys.info()['user'])

  user_initials <- switch(current_user,
                          "artemio.sison" = "MS3",
                          "user01" = "u1",
                          "user02" = "u2",
                          "user03" = "u3",
                          "user04" = "u4",
                          "runner" = "GIT",
                          "runneradmin" = "GIT")

  return(user_initials)
}

#' Get Current User's Username in Lowercase
#'
#' This function retrieves the current user's system username and returns it in lowercase.
#' The function uses `Sys.info()` to gather system information and extracts the 'user' field.
#'
#' @return A character string containing the current user's username in lowercase.
#' @examples
#' # Get current user's username in lowercase
#' get_user()
#' @export
#' @seealso \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.info.html}
get_user <- function() {
  tolower(Sys.info()['user'])
}


# Explicit data column names  ----------------------------------------------
#' Return column names for ELISPOT LONG
#'
#' This function returns a character vector list of the column names for ELISPOT LONG, which include "SID", "STIM", "VISIT", and "RESULT".
#'
#' @return A character vector list of the column names for ELISPOT LONG.
#' @export
#'
#' @examples
#' elispot_LONG_names()

# elispot_LONG_names <- function() {
#   c("SID", "STIM", "VISIT", "RESULT")
# }

#' Return column names for ELISA LONG
#'
#' This function returns a character vector list of the column names for ELISA DM, which include "SID", "STIM", "VISIT", and "RESULT".
#'
#' @return A character vector list of the column names for ELISA LONG.
#' @export
#'
#' @examples
#' elisa_LONG_names()

# elisa_LONG_names <- function() {
#   c("SID", "STIM", "VISIT", "RESULT")
# }

#' Return column names for Flow Cytometry LONG
#'
#' This function returns a character vector list of the column names for Flow Cytometry LONG, which include "STUDY", "ASSAY", "SID", "STIM", "VISIT", "PARAM", and "RESULT".
#'
#' @return A character vector list of the column names for Flow Cytometry LONG.
#' @export
#'
#' @examples
#' flow_DM_names()

# flow_DM_names <- function() {
#   c("SID", "STIM", "VISIT", "PARAM", "RESULT")
# }
