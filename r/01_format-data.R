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
# devtools::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)

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


# if we make rain.day - we can use for a ggridge - just for makaru
rain_day <- read.csv("data/raw/composite rain for Wandering.csv") %>%
  clean_names() %>%
  dplyr::mutate(rainfall_amount_millimetres = if_else(is.na(rainfall_amount_millimetres), 0, rainfall_amount_millimetres)) %>%
  dplyr::mutate(period_over_which_rainfall_was_measured_days = if_else(is.na(period_over_which_rainfall_was_measured_days), 1,
                                                                       period_over_which_rainfall_was_measured_days),
                rainfall_amount_millimetres = rainfall_amount_millimetres/period_over_which_rainfall_was_measured_days) %>%
  dplyr::select(year, month, day, rainfall_amount_millimetres) %>%
  dplyr::rename(rainfall = rainfall_amount_millimetres) %>%
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

# Need to download temperature data daily if wanted to use that

dat <- rain %>%
  left_join(temp) %>%
  dplyr::filter(year > 1901) %>%
  glimpse()

# Create mean temperature and rainfall anomaly data - move to format script!!
anomaly_base <- dat %>%
  dplyr::filter(between(year, 1961, 1990)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(rainfall_base = mean(rainfall, na.rm = T),
                   temperature_base = mean(temperature, na.rm = T)) %>%
  glimpse()

anomaly <- dat %>%
  left_join(anomaly_base) %>%
  dplyr::mutate(rain_anom = rainfall - rainfall_base,
                temp_anom = temperature - temperature_base) %>%
  group_by(year) %>%
  summarise(rain_anom = mean(rain_anom, na.rm = T),
            temp_anom = mean(temp_anom, na.rm = T)) %>%
  dplyr::mutate(rain_dir = if_else(rain_anom < 0, "neg", "pos"),
                temp_dir = if_else(temp_anom < 0, "neg", "pos")) %>%
  glimpse()

write.csv(dat, file = "data/tidy/wandering_rainfall-temp_month.csv",
          row.names = F)

write.csv(rain_day, file = "data/tidy/wandering_rainfall_day.csv",
          row.names = F)

write.csv(anomaly, file = "data/tidy/wandering_rainfall-temp_anomaly.csv",
          row.names = F)
