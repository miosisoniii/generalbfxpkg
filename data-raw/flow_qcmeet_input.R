## code to prepare `flow_qcmeet_input` dataset goes here
# + the purpose of this is to create files that will be used to test the QC
#   meeting file generation functions

## proj03 -------------------------------------------------------------------------

## use package functions to create intermediate datafiles

## testing reading of all subjects in data folder - from Production
# + specify regex for analyst
prod_sp_path <- load_sp(type = "prod")
proj03_a1 <- paste0(prod_sp_path, "path/to/analyst1")
proj03_a2 <- paste0(prod_sp_path, "path/to/analyst2")

## use wrapper fn here for import
proj03_wrap_a1 <- flow_preqc_wrap(path.to.analyst.dir = proj03_a1,
                               path.pattern = ".*EG.*.wsp",
                               meta.split = FALSE)
proj03_wrap_a2 <- flow_preqc_wrap(path.to.analyst.dir = proj03_a2,
                               path.pattern = ".*KK.*.wsp",
                               meta.split = FALSE) |>
  ## drop /extra/9022004
  filter(!str_detect(.data$name, "9022-0004"))

## Compare analyst data for QC
proj03_wrap_comp <- flow_qc_compare(proj03_wrap_a1, proj03_wrap_a2)

## merge together
flow_qcmeet_input <- list(proj03_wrap_a1, proj03_wrap_a2, proj03_wrap_comp)
names(flow_qcmeet_input) <- c("proj03_analyst1", "proj03_analyst2", "proj03_qc_compare")

## store proj03 data as single object so that it can be used to write functions
## use_data() to save the file in "data_raw"
usethis::use_data(flow_qcmeet_input, overwrite = TRUE, compress = "gzip")

