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
  dplyr::mutate(month = lubridate::month(month, label = T)) %>%
  glimpse()

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
