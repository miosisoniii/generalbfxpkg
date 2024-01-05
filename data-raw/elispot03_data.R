## code to prepare `elispot03_data` dataset goes here
## elispot data03 ------------------------------------------------------

library(readr)
library(dplyr)
library(usethis)

## code for resetting the environment?

## get path to SharePoint -  could be easier than storing data in this pacakage?
path <- "path/to/sharepoint"

#anonymize SID so we can continue to use this data internally
elispot03_data <- read_csv(path) |> anonymize_sid()

## using internal = TRUE to save data into R/sysdata.rda, to be used for testing fn
use_data(elispot03_data, overwrite = TRUE, internal = TRUE)

