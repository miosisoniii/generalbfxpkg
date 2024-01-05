## flowjo import into R and algl marker list generation ------------------------

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(usethis, warn.conflicts = FALSE)

## flowjo import into R --------------------------------------------------------

## loading wsp from sharepoint - goal is to save the import as an .rda file
sp_path <- load_sp()
dat_path <- paste0(sp_path, "path/to/workspace.wsp")

## anonymize SID so we can continue to use this data internally
flow01_data <- import_flow_freq(dat_path) |>
  mutate(name = str_remove_all(name, pattern = "Specimen_001_"),
                name = str_remove_all(name, " "),
                name = str_remove(name, "(?<=fcs)_.*"),
                name = str_remove(name, "\\.fcs")) |>
  separate(name, into = c("SID", "VISIT", "STIM"), sep = "_") |>
  mutate(SID = str_remove(SID, "-"))

## anonymize SID
## using the function from the package
flow01_count_data <- anonymize_sid(flow01_data)

## Generate param combos -------------------------------------------------------

## get param values from the extracted counts/freq from FlowJo
flow_param_data <- get_flow_params(wsp_df = flow01_count_data)

## use get_flow_params to extract params from the initial dataframe
markers_flow <- gen_flow_combos(flow_param_data) %>%
  ## 21Apr23 - drop the last few regex because artifact generated from fn
  filter(str_detect(strings_regex, "\\(.*\\)"))

## Store data in appropriate package location ----------------------------------

## not necessary to do this? the data can be accessed as a file and written to csv if needed manually
## write data to csv (like hadley does here:https://github.com/hadley/babynames/blob/master/data-raw/applicants.R)
# write_csv(flow01_count_data, "data-raw/flow01_count_data.csv")
# write_csv(markers_flow, "data-raw/CD4CD8_markers_flow.csv")

## store data in data-raw to work with it later
## using internal = TRUE to save data into R/sysdata.rda, to be used for testing fn
usethis::use_data(flow_param_data, overwrite = TRUE,
                  internal = TRUE,  # using internal = TRUE because we only need it for testing
                  compress = "gzip")
usethis::use_data(markers_flow,
                  overwrite = TRUE,
                  compress = "gzip")


















## code that can be useful later -----------------------------------------------
## drop "-" in SID column - NOT WORKING FOR SOME REASON
# remove_hyphen_mutate <- function(column) {
#   pattern <- "\\b\\d{4}-\\d{4}\\b" # Regular expression pattern to match 4 digits-4 digits
#   str_replace_all(column, pattern, function(match) {
#     str_remove(match, "-") # Remove hyphen
#   })
# }

### add/remove "-"
# + good resource here: https://stackoverflow.com/questions/4683405/function-default-arguments-and-named-values

## create test data
# t1 <- data.frame(SID = paste0("SID0000000", seq_along(1:9)))
# t2 <- data.frame(SID = paste0("0000000", seq_along(1:9)))
# t3 <- data.frame(SID = paste0("0000-000", seq_along(1:9)))
#
# addrop_sid_hyphen <- function(sid.string, add.drop = c("add", "drop")) {
#   add.drop <- match.arg(add.drop)
#
#   if (add.drop == "add") {
#     str_replace(sid.string, "(\\d{4})(\\d{4})$", "\\1-\\2")
#   } else if (add.drop == "drop") {
#     str_remove(sid.string, "(\\d{4})-")
#   } else if (nchar(sid.string) < 8) {
#     warning("Number of characters is < 8. Check number of chars in SID.")
#     warning(paste("N chars is:", nchar(sid.string)))
#   } else {
#     stop("Incorrect arg supplied. Specify 'add' or 'drop'.")
#   }
# }
#
# t1 %>%
#   mutate(SID = adddrop_sid_hyphen(SID, "drop"))
