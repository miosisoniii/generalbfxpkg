#' Identify Indices with CV Failure
#'
#' This function takes a wide-format data frame and identifies the cells with values greater than 20.0,
#' which are considered CV failures.
#'
#' @param cv.idf.wide A wide-format data frame containing CV values.
#'
#' @return A data frame containing the row and column indices of cells with values greater than 20.0.
#'
#' @examples
#' data <- data.frame(A = c(19, 21), B = c(22, 18))
#' index_cv_fail(data)
#'
#' @export
#'
index_cv_fail <- function(cv.idf.wide) {

  # Find array indices of values > 20.0
  # Drop the first column for file name/metadata
  qc_fail_ind <- which(cv.idf.wide[, -1] > 20.0, arr.ind = TRUE)

  ## return index array
  return(qc_fail_ind)
}

#' Import CV Data and Create XLSX File
#'
#' This function creates an XLSX file with a "QC" sheet and writes the input wide-format CV data to it.
#'
#' @param cv.idf.wide A wide-format data frame containing CV values.
#'
#' @return A workbook object with the "QC" sheet containing the input CV data.
#'
# @importFrom XLConnect loadWorkbook createSheet createName writeNamedRegion
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' # data <- data.frame(A = c(19, 21), B = c(22, 18))
#' \dontrun{wb <- import_cv_xl(data)}
#'
#' @export
import_cv_xl <- function(cv.idf.wide) {

  if (!rlang::is_installed("XLConnect")) {
    stop("Package 'XLConnect' needed for this fn to work.\n
         Please install using: install.packages('XLConnect').")
  }

  ## create xlsx file to be modified
  output.path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(x = cv.idf.wide, file = output.path)
  message(paste0("XLSX file created at ", output.path))

  # Load the workbook and create an empty worksheet
  wb <- XLConnect::loadWorkbook(filename = output.path, create = TRUE)
  XLConnect::createSheet(wb, name = "QC")
  XLConnect::createName(wb, name = "QC", formula = "QC!$A$1")
  XLConnect::writeNamedRegion(object = wb,
                              data = cv.idf.wide,
                              name = "QC")

  return(list(wb = wb, output.path = output.path))
}

#' Color CV Failure Cells in XLSX File
#'
#' This function colors the cells in the "QC" sheet of the input workbook object that have values greater than 20.0,
#' indicating CV failures. The CV failure cells are colored yellow.
#'
#' @param wb A workbook object containing a "QC" sheet with CV data.
#' @param qc.fail.index A data frame containing the row and column indices of CV failure cells.
#'
#' @importFrom rlang .data
# @importFrom XLConnect createCellStyle setCellStyle setFillPattern setFillForegroundColor
#'
#' @examples
#' # data <- data.frame(A = c(19, 21), B = c(22, 18))
#' # qc_fail_ind <- index_cv_fail(data)
#' # wb <- import_cv_xl(data)
#' \dontrun{color_cv_xl(wb, qc_fail_ind)}
#' @export
color_cv_xl <- function(wb, qc.fail.index) {

  if (!rlang::is_installed("XLConnect")) {
    stop("Package 'XLConnect' needed for this fn to work.\n
         Please install using: install.packages('XLConnect').")
  }

  # Define a style with a fill pattern for the foreground and a foreground color
  qc_fail_style <- XLConnect::createCellStyle(object = wb, name = "QCfail")
  XLConnect::setFillPattern(object = qc_fail_style,
                            fill = XLConnect::XLC$"FILL.SOLID_FOREGROUND")
  XLConnect::setFillForegroundColor(object = qc_fail_style,
                                    color = XLConnect::XLC$"COLOR.YELLOW")

  # Since we are working with a header row, we need to ignore that first row
  qc_fail_arr <- as.data.frame(qc.fail.index) |>
    mutate(row = .data$row + 1,
           col = .data$col + 1)

  # Apply the style to the failed QC cells
  XLConnect::setCellStyle(object = wb,
                          sheet = "QC",
                          row = qc_fail_arr$row,
                          col = qc_fail_arr$col,
                          cellstyle = qc_fail_style)

}

#' Write Highlighted CV XLSX File
#'
#' This function takes a wide-format data frame containing CV values, creates an XLSX file with a "QC" sheet,
#' writes the CV data to the sheet, and colors the cells with values greater than 20.0 (CV failures) yellow.
#'
#' @param cv.idf.wide A wide-format data frame containing CV values.
#' @param output.dir The directory where the output XLSX file should be saved (default is the current working directory).
#'
# @importFrom XLConnect saveWorkbook
#'
#' @examples
#' # data <- data.frame(A = c(19, 21), B = c(22, 18)) #
#' \dontrun{write_cv_xl(data, output.dir = paste0(load_sp(), "data"))}
#'
#' @export
write_cv_xl <- function(cv.idf.wide, output.dir = "./") {

  if (!rlang::is_installed("XLConnect")) {
    stop("Package 'XLConnect' needed for this fn to work.\n
         Please install using: install.packages('XLConnect').")
  }


  output_path <- paste0(output.dir, "STUDY_ASSAY_QC_", get_date(), "_", get_initials(), ".xlsx")

  qc_fail_ind <- index_cv_fail(cv.idf.wide)
  wb_obj <- import_cv_xl(cv.idf.wide = cv.idf.wide)
  color_cv_xl(wb = wb_obj$wb, qc.fail.index = qc_fail_ind)

  # Save the modified workbook to the file path
  XLConnect::saveWorkbook(wb_obj$wb, file = output_path)
}






## OLD FUNCTION BELOW ----------------------------------------------------------
# Apply quality control checks to a data frame and save results to an Excel file.
#
# This function applies quality control checks to a data frame with frequency values from FlowJo workspaces. It then saves the resulting data frame with highlighted cells indicating failed QC checks to an Excel file. The function uses the `tidyr`, `dplyr`, `openxlsx`, and `XLConnect` packages.
#
# @param cv.idf A data frame with Coefficient of Variation (CV) values for multiple parameters.
#   The data frame should contain the columns name, PARAM, and cv (coefficient of variation).
# @param output.path character string that denotes where the output .xlsx file will be saved.
#
# @return The function does not return a value, but saves the resulting data frame with highlighted cells indicating failed QC checks to an Excel file.
#
# @examples
# \dontrun{xl_flow_qc(cv.idf = flow_qcmeet_input[[3]], output.path)}
#
# @importFrom dplyr mutate
# @importFrom openxlsx write.xlsx
# @importFrom tidyr pivot_wider
# @importFrom XLConnect loadWorkbook saveWorkbook setCellStyle setFillForegroundColor setFillPattern XLC
#
# @export

# xl_flow_qc <- function(cv.idf, output.path = "./QC.xlsx") {
#
#   cv_idf_wide <- cv.idf |> pivot_wider(id_cols = c(.data$name),
#                                        names_from = .data$PARAM,
#                                        values_from = .data$cv)
#
#   # Save to an Excel workbook as a temporary file
#   # temp_file <- tempfile(fileext = ".xlsx")
#   temp_file <- output.path
#   openxlsx::write.xlsx(x = cv_idf_wide, file = temp_file)
#
#   # Find array indices of values > 20.0
#   # Drop the first column for file name/metadata
#   qc_fail_ind <- which(cv_idf_wide[, -1] > 20.0, arr.ind = TRUE)
#
#   # Load the workbook and create an empty worksheet
#   wb <- XLConnect::loadWorkbook(filename = temp_file, create = TRUE)
#   XLConnect::createSheet(wb, name = "QC")
#   XLConnect::createName(wb, name = "QC", formula = "QC!$A$1")
#   XLConnect::writeNamedRegion(object = wb,
#                               data = cv_idf_wide,
#                               name = "QC")
#
#   # Define a style with a fill pattern for the foreground and a specified foreground color
#   qc_fail_style <- createCellStyle(object = wb, name = "QCfail")
#   setFillPattern(object = qc_fail_style, fill = XLC$"FILL.SOLID_FOREGROUND")
#   setFillForegroundColor(object = qc_fail_style, color = XLC$"COLOR.YELLOW")
#
#   # Since we are working with a header row, we need to ignore that first row
#   qc_fail_arr <- as.data.frame(qc_fail_ind) |>
#     mutate(row = row + 1,
#            col = col + 1)
#
#   # Apply the style to the failed QC cells
#   XLConnect::setCellStyle(object = wb,
#                           sheet = "QC",
#                           row = qc_fail_arr[["row"]],
#                           col = qc_fail_arr[["col"]],
#                           cellstyle = qc_fail_style)
#
#   # Save the modified workbook to the file path
#   XLConnect::saveWorkbook(wb, file = temp_file)
# }



