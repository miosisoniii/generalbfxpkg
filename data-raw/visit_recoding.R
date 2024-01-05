## code to prepare `visit_recoding` dataset goes here

library(dplyr)
library(readr)
library(usethis)
library(stringr)
library(tidyr)

## Code for generating intermediate VISIT data ---------------------------------
v1 <- c("Screen", "Day", "Week")
t1 <- 0:100

df <- expand_grid(vname = v1, vnum = t1)
df$full <- paste0(df$vname, df$vnum)

## short
df_t1 <- df %>%
  mutate(s_nosp = case_when(
    str_detect(full, "Screen|Screening|Rescreen") ~ paste0("scr", vnum),
    str_detect(full, "Day") ~ paste0("d", vnum),
    str_detect(full, "Week") ~ paste0("w", vnum),
    TRUE ~ as.character(full)
  )) %>%
  mutate(s_sp = case_when(
    str_detect(full, "Screen") ~ paste0("scr ", vnum),
    str_detect(full, "Day") ~ paste0("d ", vnum),
    str_detect(full, "Week") ~ paste0("w ", vnum),
    TRUE ~ as.character(full)
  )) %>%
  mutate(s_us = case_when(
    str_detect(full, "Screen") ~ paste0("scr_", vnum),
    str_detect(full, "Day") ~ paste0("d_", vnum),
    str_detect(full, "Week") ~ paste0("w_", vnum),
    TRUE ~ as.character(full)
  ))  %>%
  mutate(f_nosp = case_when(
    str_detect(full, "Screen") ~ paste0("screen", vnum),
    str_detect(full, "Day") ~ paste0("day", vnum),
    str_detect(full, "Week") ~ paste0("week", vnum),
    TRUE ~ as.character(full)
  ))  %>%
  mutate(f_sp = case_when(
    str_detect(full, "Screen") ~ paste0("screen ", vnum),
    str_detect(full, "Day") ~ paste0("day ", vnum),
    str_detect(full, "Week") ~ paste0("week ", vnum),
    TRUE ~ as.character(full)
  ))  %>%
  mutate(f_us = case_when(
    str_detect(full, "Screen") ~ paste0("screen_", vnum),
    str_detect(full, "Day") ~ paste0("day_", vnum),
    str_detect(full, "Week") ~ paste0("week_", vnum),
    TRUE ~ as.character(full)
  )) %>%
  mutate(abb_nosp = case_when(
    str_detect(full, "Screen") ~ paste0("scr", vnum),
    str_detect(full, "Day") ~ paste0("d", vnum),
    str_detect(full, "Week") ~ paste0("wk", vnum),
    TRUE ~ as.character(full)
  )) %>%
  mutate(abb_sp = case_when(
    str_detect(full, "Screen") ~ paste0("scr ", vnum),
    str_detect(full, "Day") ~ paste0("d ", vnum),
    str_detect(full, "Week") ~ paste0("wk ", vnum),
    TRUE ~ as.character(full)
  )) %>%
  mutate(abb_us = case_when(
    str_detect(full, "Screen") ~ paste0("scr_", vnum),
    str_detect(full, "Day") ~ paste0("d_", vnum),
    str_detect(full, "Week") ~ paste0("wk_", vnum),
    TRUE ~ as.character(full)
  ))

## pivot longer to see if we can swap using the targets
visit_recoding <- df_t1 %>%
  pivot_longer(-c(vname:full),
               names_to = "type",
               values_to = "wrong_vis") %>%
  select(-vname, -vnum) %>%
  relocate(type, .before = full) %>%
  mutate(full2 = str_replace(full, "Day", "D")) %>%
  mutate(full2 = str_replace(full2, "Week", "WK")) %>%
  mutate(full2 = str_replace(full2, "Screen", "SCR")) %>%
  ## testing the detection of the underscore
  mutate(regex_vis = str_replace(wrong_vis, "\\_", "\\\\_")) %>%
  ## using (?![:digit:]) end of string to prevent picking up additional numeric chars
  mutate(regex_vis = paste0(regex_vis, "(?![:digit:])")) %>%
  mutate(regex_vis = str_replace(regex_vis, " ", "\\\\\\s")) %>%
  relocate(full2, .after = full) %>%
  rename("abbr" = full2)


## create dataframe for handling screen visits
## Screen/Screening/Scr with a SPACE " "
## Screen/Screening/Scr with UNDERSCORE "_"
visit_scr <- data.frame(
  "type" = c("scr_sp.us", "scr"),
  "full" = c("Screen_", "screen"),
  "abbr" = c("SCR_", "SCR"),
  "wrong_vis" = c("Screen"),
  "regex_vis" = c("(screening|screen|scr)(\\_+|\\s+)",
                  "(screening|screen|scr)")
)

## add screen to visit_recoding df
visit_recoding <- visit_recoding %>%
  filter(!str_detect(full, "Screen")) %>%
  bind_rows(visit_scr, .)

## write to csv
write_csv(visit_recoding, "data-raw/visit_recoding.csv")

## use the data
usethis::use_data(visit_recoding,
                  # internal = TRUE, # we are using this data in the recode_visits() fn, not externally!
                  overwrite = TRUE,
                  compress = "gzip")








## adjusting for SCR -----------------------------------------------------------
## must set visit_recoding with changes to "visit_recoding1"
## must change fn name to "recode_vis" when testing

#
# recode_vis <- function(v.string, output.format = c("abbr", "full")){
#
# if ((output.format == "abbr" | output.format == "full")) {
#   v.string <- tolower(v.string)
#   output.format <- match.arg(output.format)
#
#   str_idx <- str_which(v.string, visit_recoding1$regex_vis)
#   targ_pattern <- visit_recoding1$regex_vis[[str_idx]]
#   targ_replace <- visit_recoding1[[output.format]][[str_idx]]
#
#   str_replace(string = v.string,
#               pattern = targ_pattern,
#               replacement = targ_replace)
#
# } else if (!(output.format == "abbr" | output.format == "full")) {
#   stop("Error. Please supply 'abbr' or 'full' to the output.format argument.")
#
# }
# }
#
# ## example strings for SCREEN
# s1 <- "0001_Screening HPV6.fcs"
# s2 <- "0001_Scr_HPV6.fcs"
# s3 <- "0001_Screening_HPV6.fcs"
# s4 <- "0001_Screening HPV6.fcs"
#
# recode_vis(v.string = s1,
#            output.format = "abbr")
# recode_vis(v.string = s2,
#            output.format = "abbr")
#
# recode_vis(v.string = s3,
#            output.format = "abbr")
# recode_vis(v.string = s4,
#            output.format = "abbr")


## Recode VISITS fn ------------------------------------------------------------

## only work with exact matches
# vstring2 <- c("week181", "week 1", "week18")
# str_which(vstring2, "week\\s1(?![:digit:])") # no digits after this

## example strings
# s1 <- "0001_Day0_HPV6.fcs"
# s2 <- "0001_Day 0_HPV6.fcs"
# s3 <- "0001_Day_0_HPV6.fcs"
# s4 <- "0001_Day 0 HPV6.fcs"
# s5 <- "00001_Week 18_HPV6.fcs"
# s5 <- "00001_Week_18_HPV6.fcs"

## test function - works!
# recode_visits("2345235_Week_99_HPV11.fcs", "full")



## 1 INPUT WHEN WE RECEIVE DATA

## 2 CONVERT TO CLEAN NAMES (SCR, D, WK...)

## 3 CONVERT TO Vnames (V1, V2...) - to be used by FN's in bfxpkg
# fn with additional fn - `enquos(...)` for BASELINE fn

## 4 CONVERT TO NAMES IMMUNO NEEDS

## does not work
# replace_vector <- function(vec, df, target_col, replace_col) {
#
#   # Loop over the elements in the character vector
#   for (i in 1:length(vec)) {
#     print(vec[i])
#     # Check if the element matches the target string
#     if (str_detect(vec[i], df[[target_col]])) {
#
#       # Replace the target string with the replacement string from the data frame
#       idx <- which(df$wrong_vis == vec[i])
#       vec[i] <- df[[replace_col]][idx]
#     }
#   }
#
#   return(vec)
# }

