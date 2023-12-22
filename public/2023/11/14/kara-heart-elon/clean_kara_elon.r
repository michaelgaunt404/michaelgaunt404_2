#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(readxl)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

data_raw = here::here("content/post/2023-11-14-kara-heart-elon", "kara_heart_elon_raw.xlsx") %>%
  read_xlsx(col_names = F) %>%
  set_names("col") %>%
  mutate(col = str_trim(col)) %>%
  filter(!is.na(col)) %>%
  # filter(!(str_detect(col, 'hr') & str_detect(col, "min"))) %>%
  filter(col != "PLAY") %>%
  filter(!(str_detect(col, "[[:digit:]]+ min") & str_count(col) < 30)) %>%
  filter(!(str_detect(col, "[[:digit:]]+ hr") & str_count(col) < 30)) %>%
  unique() %>%
  mutate(test = parse_date(col)) %>%
  mutate(test_1 = case_when(str_detect(col, "([[:digit:]]+\\.)") & str_count(col) < 30 ~col, T~NA_character_)) %>%
  separate(col = test_1, into = c("number", "date"), sep = "\\. ") %>%
  mutate(date = lubridate::mdy(date)) %>%
  fill(c(number, date), .direction  = "down") %>%
  group_by(number, date) %>%
  mutate(index = row_number()) %>%
  pivot_wider(values_from = col, names_from = index) %>%
  select(number, date, `2`, `3`) %>%
  set_names(c("episode", "date", "title", "desc"))

saveRDS(
  data_raw
  ,here::here("content/post/2023-11-14-kara-heart-elon", "kara_heart_elon_pro.rds")
)


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































