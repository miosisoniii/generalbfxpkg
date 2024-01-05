#' Shapiro-Wilk Test Function
#'
#' This function performs the Shapiro-Wilk test on a given vector.
#' It has been adjusted to prevent errors if all inputs are identical.
#' The Shapiro-Wilk test is a tool for checking normality of a vector.
#'
#' @param df A dataframe that contains the column to be tested, containing a 'RESULT' or 'SUM_FREQ' column
#' @param result.colname character vector of resulting values of the dependent variable, often 'RESULT' or 'SUM_FREQ'
#'
#' @details
#' The function checks the unique non-NA values of the result column. If there are more than 3 and less than 5000 unique non-NA values,
#' it applies the `shapiro.test` function from the stats package. If not, it returns NA.
#'
#' @references
#' Stackoverflow thread that inspired this function: \url{https://stackoverflow.com/questions/43748663/shapiro-test-plyr-all-x-values-are-identical}
#'
#' @return
#' If conditions are met, a numeric p-value of the Shapiro-Wilk test result. If conditions are not met, it returns NA.
#'
#' @seealso
#' \code{\link[stats]{shapiro.test}} for the function used to perform the Shapiro-Wilk test.
#'
#' @importFrom stats shapiro.test
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FLOW_intermediate_data |> head(n = 250)
#' shap.w(df = data, result.colname = "SUM_FREQ")
#' }
shap_w <- function(df, result.colname){

  validate_input_df(df)
  validate_input_cols(df, result.colname)

  ## pull result column to check
  result_vec <- df[[result.colname]]
  uniq_result_NA_val <- length(unique((result_vec[!is.na(result_vec)])))

  if (uniq_result_NA_val > 3 & uniq_result_NA_val < 5000 ) {
    p <- shapiro.test(result_vec)$p.value
    return(p)

  } else {
    return(NA_real_)
  }
}

#' Grouped Wilcoxon Test Function
#'
#' This function performs a Wilcoxon test for two groups within a given dataframe.
#' The groups for comparison can be defined by the user.
#'
#' @param df A dataframe where the data is stored.
#' @param subset.cols A vector of column names by which you wish to subset the data.
#'        If you are interested in the difference between means for each parameter and each visit, for example.
#' @param result.col The name of the RESULT column. It should be numeric and is typically "RESULT" or "SUM_FREQ".
#' @param grouping.factor The column name that contains the groups you want to compare between.
#'        For example, "STIM" if you want to compare means between "HPV11" and "HPV16".
#' @param group1 The name of the first group for comparison.
#' @param group2 The name of the second group for comparison.
#' @param paired A logical indicating whether it's a paired test. Defaults to FALSE.
#' @param exact A logical indicating whether it's an exact test. Defaults to TRUE.
#'
#' @details
#' This function utilizes `wilcox.exact.formula` from `exactRankTests` package to perform a grouped Wilcoxon test.
#'
#' @return
#' A dataframe of the results of the Wilcoxon test.
#'
#' @references
#' \url{https://www.rdocumentation.org/packages/exactRankTests/versions/0.8-31/topics/wilcox.exact}
#'
#' @seealso
#' \code{\link[exactRankTests]{wilcox.exact.formula}} for the function used to perform the Wilcoxon test.
#'
#' @importFrom dplyr filter group_by group_modify
#' @importFrom tidyselect all_of
#' @importFrom broom tidy
#' @importFrom exactRankTests wilcox.exact.formula
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   RESULT = c(rnorm(50), rnorm(50, mean = 3)),
#'   STIM = rep(c('group1', 'group2'), each = 50),
#'   PARAM = rep(c('A', 'B'), each = 25, times = 2),
#'   VISIT = rep(1:2, each = 25, times = 2)
# )
#' wilcox_test_grouped(data)
#' }
#'

wilcox_test_grouped <- function(df,
                                subset.cols = c("VISIT", "PARAM"), #groups
                                result.col = "RESULT", #lh
                                grouping.factor = "STIM", #rh
                                group1 = "ConA",
                                group2 = "HPV16",
                                #formula = RESULT ~ VISIT,
                                paired = FALSE,
                                exact = TRUE) {

  ## create list of args for colname validation
  arg_list <- c(subset.cols, result.col, grouping.factor)

  ## validation
  validate_input_df(df)
  if (is.null(result.col) || is.null(grouping.factor)) {
    stop("Error: Column names cannot be NULL.")
  } else {
    validate_input_cols(df, arg_list)
  }

  formula_grp <- paste0(result.col, "~", grouping.factor)

  df |>
    filter(.data[[grouping.factor]] == group1 | .data[[grouping.factor]] == group2) |>
    group_by(.data = _, across(all_of(subset.cols))) |>
    group_modify(
      .data = _,
      ~  tidy(
        wilcox.exact.formula(formula = as.formula(formula_grp),
                             data = .x,
                             paired = paired,
                             exact = exact)
      )
    )
}


