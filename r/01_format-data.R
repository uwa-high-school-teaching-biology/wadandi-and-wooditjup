###
# Project: High school teaching
# Data:    BOM Rainfall and Temperature data
# Task:    Format data
# author:  Claude Spencer
# date:    April 2024
##

# Load libraries
library(tidyverse)

library(devtools)
devtools::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM) #does not work for tim!



rain <- read.csv("data/raw/composite rain for Wandering.csv") %>%
  clean_names() %>%
  dplyr::mutate(rainfall_amount_millimetres = if_else(is.na(rainfall_amount_millimetres), 0, rainfall_amount_millimetres)) %>%
  dplyr::mutate(period_over_which_rainfall_was_measured_days = if_else(is.na(period_over_which_rainfall_was_measured_days), 1,
                                                                       period_over_which_rainfall_was_measured_days),
                rainfall_amount_millimetres = rainfall_amount_millimetres/period_over_which_rainfall_was_measured_days) %>%
  dplyr::select(year, month, day, rainfall_amount_millimetres) %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(rainfall = mean(rainfall_amount_millimetres)) %>%
  ungroup() %>%
  glimpse()


#if we make rain.day - we can use for a ggridge - just for makaru
rain.day <- read.csv("data/raw/composite rain for Wandering.csv") %>%
  clean_names() %>%
  dplyr::mutate(rainfall_amount_millimetres = if_else(is.na(rainfall_amount_millimetres), 0, rainfall_amount_millimetres)) %>%
  dplyr::mutate(period_over_which_rainfall_was_measured_days = if_else(is.na(period_over_which_rainfall_was_measured_days), 1,
                                                                       period_over_which_rainfall_was_measured_days),
                rainfall_amount_millimetres = rainfall_amount_millimetres/period_over_which_rainfall_was_measured_days) %>%
  dplyr::select(year, month, day, rainfall_amount_millimetres) %>%
  dplyr::group_by(year, month, day) %>%
  dplyr::summarise(rainfall = mean(rainfall_amount_millimetres)) %>%
  ungroup() %>%
  glimpse()


temp <- read.csv("data/raw/composite temp for Wandering.csv",
                 skip = 1) %>%
  clean_names() %>%
  dplyr::rename(year = row_labels) %>%
  dplyr::select(-c(x_blank, grand_total)) %>%
  pivot_longer(cols = starts_with("x"),
               names_to = "month",
               values_to = "temperature") %>%
  dplyr::filter(!year %in% c("(blank)", "Grand Total")) %>%
  dplyr::mutate(month = as.integer(str_remove_all(month, "x")),
                year = as.integer(year)) %>%
  glimpse()

#if we make temp.day - we can use for a ggridge - just for makaru?



dat <- rain %>%
  left_join(temp) %>%
  glimpse()

write.csv(dat, file = "data/tidy/wandering_rainfall-temp.csv",
          row.names = F)
