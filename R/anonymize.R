#' Anonymize SID column (using a dataframe)
#'
#' @param dat dataframe containing an 'SID' column.
#' @return dataframe with 'SID' column that has randomized values beginning with "SID" and ending in a random 5 character string.
#'
#' @examples
#' flow_param_data |> anonymize_sid() |> head(n = 5)
#'
#' @export


anonymize_sid <- function(dat){

  if ("SID" %in% names(dat)) {
    dat |>
      mutate(SID = anonymizer::anonymize(.data$SID, .seed = 123),
             SID = toupper(paste0("SID", substr(.data$SID, 1, 5)))) |>
      mutate(SID = factor(.data$SID))
  } else {
    stop("Error: input dataframe must contain column named `SID`. ")
  }
}


#' anonymize re-write to use in a column, so that it takes a string rather than a dataframe
#'
#' @param col.meta character string, likely present in the SID, VISIT, STIM, PARAM, or MIRNA cols
#' @param type character string, one of 'SID', 'VISIT', 'STIM', 'PARAM', or 'MIRNA' cols
#'
#' @return character string, where SID = SID_XXXXXXXX, VISIT = V_XXXXX, STIM = S_###, PARAM = P_XXX, or MIRNA = MIR_XXX
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' orig_sid <- "C300C"
#' anonymize_meta(col.meta = orig_sid, type = "SID")
#' orig_visit <- "WEEK6"
#' anonymize_meta(col.meta = orig_visit, type = "VISIT")
#' orig_stim <- "HPV6E6"
#' anonymize_meta(col.meta = orig_stim, type = "STIM")
#' orig_param <- "CD38"
#' anonymize_meta(col.meta = orig_param, type = "PARAM")
#' orig_mir <- "hsa.mir-295.G.T"
#' anonymize_meta(col.meta = orig_mir, type = "MIRNA")
#'
#' ## usage in a dataframe
#' ex_df <- data.frame(
#' SID = paste0("SID", sample(10002000:3000000, 6)),
#' VISIT = paste0("WEEK", sample(1:10, 6)), STIM = paste0("HPV", sample(1:3, 3)),
#' PARAM = paste0("CD", sample(1:25, 3)), MIRNA = paste0("hsa.mir", sample(1:25, 3)))
#'
#' ## type = "SID"
#' ex_df %>% mutate(SID = anonymize_meta(col.meta = .data$SID, type = "SID"))
#'
#' ## type = "VISIT"
#' ex_df %>% mutate(VISIT = anonymize_meta(col.meta = .data$VISIT, type = "VISIT"))
#'


anonymize_meta <- function(col.meta, type = c("SID", "VISIT", "STIM", "PARAM", "MIRNA")){
  type <- match.arg(type)
  if (type == "SID") {
    paste0("SID_", substr(toupper(anonymizer::anonymize(col.meta, .seed = 000)), 1, 8))

  } else if (type == "VISIT") {
    paste0("V_", substr(toupper(anonymizer::anonymize(col.meta, .seed = 001)), 1, 5))

  } else if (type == "STIM") {
    paste0("STIM_", substr(toupper(anonymizer::anonymize(col.meta, .seed = 002)), 1, 3))

  } else if (type == "MIRNA") {
    paste0("MIR_", substr(toupper(anonymizer::anonymize(col.meta, .seed = 003)), 1, 3))

  } else if (type == "PARAM") {
    paste0("P_", substr(toupper(anonymizer::anonymize(col.meta, .seed = 004)), 1, 3))

  } else {
    stop("Please supply the 'col.meta' arg with one of `SID`, `VISIT`, `STIM`, `MIRNA`, or `PARAM`")

  }
}
