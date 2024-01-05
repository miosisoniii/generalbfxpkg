## code to prepare `elispot_data1` dataset goes here ---------------------

## use sharepoint path fn
sp_path <- load_sp()
dat_path <- paste0(sp_path, "data/ELISPOT/ELISPOT01_LONG.csv")
elispot01_data <- read.csv(dat_path) %>%
  anonymize_sid()

## use the data
usethis::use_data(elispot_data01,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "gzip")

## code to prepare `elispot_data2` dataset goes here ---------------------

## use sharepoint path fn
dat_path <- paste0(sp_path, "data/ELISPOT/ELISPOT02_LONG.csv")
elispot02_data <- read.csv(dat_path) %>%
  anonymize_sid()

## use data to store it in /data
usethis::use_data(elispot02_data,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "gzip")
