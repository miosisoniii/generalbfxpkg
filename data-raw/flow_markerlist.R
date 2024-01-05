## code to prepare `flowmarkerlist.rda` dataset goes here

## load dependencies
library(dplyr)
library(readr)
library(stringr)

## get path
sp_path <- load_sp(type = "dev")
dat_path <- paste0(sp_path, "data/FLOW/FLOW_markerlist.csv")
andML <- read_csv(dat_path)

## drop parentheses
names(andML) <- str_remove_all(names(andML), "\\(|\\)")
names(andML)[2] <- "ParameterLOWER"

## save to csv like hadley
write_csv(andML, "./data-raw/FLOW_markerlist.csv")

## re-assign name to match the script name, flowmarkerlist.rda
flowmarkerlist <- andML

## use data
usethis::use_data(flowmarkerlist,
                  compress = "gzip",
                  overwrite = TRUE)
