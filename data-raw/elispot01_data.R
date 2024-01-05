## ELISPOT data01 LONG format data ------------------------------------------------------

library(readr)
library(dplyr)
library(usethis)

## use sharepoint fn
sp_path <- load_sp(type = "prod")
dat_path <- paste0(sp_path, "path/to/data")

## anonymize SID so we can continue to use this data internally
elispot01_data <- read_csv(dat_path) |> anonymize_sid()

## using internal = TRUE to save data into R/sysdata.rda, to be used for testing fn
use_data(elispot01_data,
         overwrite = TRUE,
         internal = TRUE,
         compress = "gzip")
