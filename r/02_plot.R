###
# Project: High school teaching
# Data:    BOM Rainfall and Temperature data
# Task:    Visualise data
# author:  Claude Spencer
# date:    April 2024
##

library(tidyverse)
library(ggridges)

dat <- read.csv("data/tidy/wandering_rainfall-temp.csv") %>%
  dplyr::mutate(month = lubridate::month(month, label = T),
                season = case_when(month %in% c("Dec", "Jan") ~ "Birak",
                                   month %in% c("Feb", "Mar") ~ "Bunuru",
                                   month %in% c("Apr", "May") ~ "Djeran",
                                   month %in% c("Jun", "Jul") ~ "Makuru",
                                   month %in% c("Aug", "Sep") ~ "Djilba",
                                   month %in% c("Oct", "Nov") ~ "Kambarang")) %>%
  glimpse()

ggplot() +
  geom_col(data = dat, aes(x = year, y = rainfall)) +
  # scale_x_continuous(trans = "reverse") +
  # coord_flip() +
  facet_wrap(~season)

anomaly <- dat %>%
  dplyr::filter(between(year, 1961, 1990)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(rainfall_base = mean(rainfall, na.rm = T),
                   temperature_base = mean(temperature, na.rm = T)) %>%
  glimpse()

test <- dat %>%
  left_join(anomaly) %>%
  dplyr::mutate(rain_anom = rainfall - rainfall_base,
                temp_anom = temperature - temperature_base) %>%
  group_by(year) %>%
  summarise(rain_anom = mean(rain_anom, na.rm = T),
            temp_anom = mean(temp_anom, na.rm = T)) %>%
  glimpse()

ggplot() +
  geom_col(data = test, aes(x = year, y = temp_anom)) +
  theme_classic() +
  labs(x = "Year", y = "Temperature anomaly")

ggplot() +
  geom_col(data = test, aes(x = year, y = rain_anom))

ggplot() +
  geom_ridgeline(data = dat, aes(x = month, y = year, 
                                      group = year, height = rainfall),
                 scale = 0.2) +
  scale_y_continuous(trans = "reverse") +
  theme_classic()

test <- dat %>%
  dplyr::filter(year > 1990)

ggplot() +
  geom_ridgeline(data = test, aes(x = month, y = year, 
                                 group = year, height = temperature),
                 scale = 0.05) +
  scale_y_continuous(trans = "reverse") +
  theme_classic()

test <- dat %>%
  dplyr::filter(!year == 2024) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(temperature = mean(temperature),
                   rainfall = mean(rainfall))

ggplot(data = test, aes(x = year, y = temperature)) +
  geom_line() +
  geom_smooth(method = "lm", colour = "red") +
  labs(x = "Year", y = "Mean temperature (mm)") +
  theme_classic()

ggplot(data = dplyr::filter(test, year > 1901), aes(x = year, y = rainfall)) +
  geom_line() +
  geom_smooth(method = "lm", colour = "red") +
  labs(x = "Year", y = "Mean rainfall (mm)") +
  theme_classic()
