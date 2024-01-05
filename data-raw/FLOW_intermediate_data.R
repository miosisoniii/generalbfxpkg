## code to prepare `FLOW_intermediate_data` dataset goes here

## load dependencies
library(dplyr, warn.conflicts =  FALSE)
library(tidyr, warn.conflicts =  FALSE)
library(stringr, warn.conflicts = FALSE)

## get path to sharepoint
path_to_data <- "path/to/data"
## use path to sharepoint fn here
sp <- load_sp(type = "dev")
full_path <- paste0(sp, path_to_data)


### NOTE: this data is in "DM" format, which is actually a wide format with params
## get the first 5 subjects in the DM
dm <- read_csv(full_path) %>%
  anonymize_sid() %>%
  select(-c(1:3)) %>%
  filter(SID %in% unique(.data$SID)[1:5]) %>%
  rename("Count" = 4) # rename col since it's just `count`

## cleanup of colnames
names(dm) <- str_remove_all(names(dm), ".*CD8\\/|.*CD4\\/") # drop head
names(dm) <- str_remove_all(names(dm), " \\| Freq. of Parent \\(\\%\\)") # drop tail
names(dm) <- str_remove_all(names(dm), " \\| Count") # drop tail


#### use param calc code to convert to vmd
## set markers_flow to ml
ml <- markers_flow %>% mutate(strings = toupper(strings))
## set dm to cd4
cd4 <- dm %>% unite(c("SID", "VISIT", "STIM"), col = "UID", sep = "_")
out_df <- c()
for (i in 1:nrow(ml)) {
  param <- ml$strings_regex[i]
  matchind <- str_which(names(cd4), param)
  df <- cd4 %>%
    select(1, matchind) %>%
    mutate(SUM_FREQ = rowSums(across(where(is.numeric))),
           SUM_FREQ = SUM_FREQ * 100,
           PARAM = ml$strings[i]) %>%
    select(UID, SUM_FREQ, PARAM)

  out_df[[i]] <- df
  message(paste0("Parameter: ", param, " complete!"))
}

## bind_rows
FLOW_intermediate_data <- bind_rows(out_df) %>%
  ## drop duplicate artifacts of calculated parameters
  distinct() %>%
  separate(col = UID, into = c("SID", "VISIT", "STIM"), sep = "_")

## write data to package
usethis::use_data(FLOW_intermediate_data,
                  internal = TRUE,
                  overwrite = TRUE,
                  compress = "gzip")


