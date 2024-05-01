###
# Project: High school teaching
# Data:    BOM Rainfall and Temperature data
# Task:    Visualise data
# author:  Claude Spencer
# date:    April 2024
##

rm(list = ls())

library(tidyverse)
library(ggridges)

# Load raw rainfall and temperature data
dat <- read.csv("data/tidy/wandering_rainfall-temp_month.csv") %>%
  dplyr::mutate(month = lubridate::month(month, label = T),
                season = case_when(month %in% c("Dec", "Jan") ~ "Birak",
                                   month %in% c("Feb", "Mar") ~ "Bunuru",
                                   month %in% c("Apr", "May") ~ "Djeran",
                                   month %in% c("Jun", "Jul") ~ "Makuru",
                                   month %in% c("Aug", "Sep") ~ "Djilba",
                                   month %in% c("Oct", "Nov") ~ "Kambarang")) %>%
  glimpse()

ggplot() +
  geom_col(data = filter(dat, season %in% "Makuru"), aes(x = year, y = rainfall), fill = "steelblue3") +
  labs(x = "Year", y = "Rainfall (mm)", title = "Rainfall in Makuru (June/July)") +
  theme_classic()

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

# Rainfall anomaly
ggplot() +
  geom_col(data = anomaly, aes(x = year, y = rain_anom, fill = rain_dir), show.legend = F) +
  scale_fill_manual(values = c("neg" = "firebrick2",
                               "pos" = "steelblue3")) +
  labs(x = "Year", y = "Rainfall anomaly (mm)", title = "Yearly rainfall anomaly") +
  theme_classic()

# Temperature anomaly
ggplot() +
  geom_col(data = anomaly, aes(x = year, y = temp_anom, fill = temp_dir), show.legend = F) +
  scale_fill_manual(values = c("pos" = "firebrick2",
                               "neg" = "steelblue3")) +
  labs(x = "Year", y = "Temperature anomaly (°C)", title = "Yearly temperature anomaly") +
  theme_classic()

# Yearly temperature and rainfall data
yearly <- dat %>%
  dplyr::filter(!year == 2024) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(temperature = mean(temperature),
                   rainfall = mean(rainfall))

ggplot(data = yearly, aes(x = year, y = temperature)) +
  geom_line() +
  geom_smooth(method = "lm", colour = "red") +
  labs(x = "Year", y = "Mean temperature (mm)") +
  theme_classic()

ggplot(data = yearly, aes(x = year, y = rainfall)) +
  geom_line() +
  geom_smooth(method = "lm", colour = "red") +
  labs(x = "Year", y = "Mean rainfall (mm)") +
  theme_classic()

# Load daily data for ridgeline plot
rainfall <- read.csv("data/tidy/wandering_rainfall_day.csv") %>%
  dplyr::mutate(month = lubridate::month(month, label = T)) %>%
  glimpse()

# Ridgeline plots
# Daily data - looks like shit
ggplot() +
  geom_ridgeline(data = filter(rainfall, between(year, 2000, 2020)), aes(x = day, y = year, 
                                      group = year, height = rainfall),
                 scale = 0.2) +
  scale_y_continuous(trans = "reverse") +
  theme_classic()

# Monthly data - too many years to plot the whole lot, can't see the trend
ggplot() +
  geom_ridgeline(data = filter(dat, between(year, 2000, 2020)), aes(x = month, y = year, 
                                 group = year, height = rainfall),
                 scale = 0.2) +
  scale_y_continuous(trans = "reverse") +
  theme_classic()

ggplot() +
  geom_ridgeline(data = dat, aes(x = month, y = year, 
                                                                    group = year, height = rainfall),
                 scale = 0.2) +
  scale_y_continuous(trans = "reverse") +
  theme_classic()

# Every 5 years - still can't really see the trend
test <- dat %>%
  dplyr::mutate(decade = if_else(str_detect(year, "0$|5$"), "yes", "no"))

ggplot() +
  geom_ridgeline(data = filter(test, decade %in% "yes"), aes(x = month, y = year, 
                                                                    group = year, height = rainfall),
                 scale = 0.5) +
  scale_y_continuous(trans = "reverse") +
  theme_classic()


