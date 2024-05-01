###
# Project: High school teaching
# Data:    BOM Rainfall and Temperature data
# Task:    Visualise data
# author:  Claude Spencer
# date:    April 2024
##

# Load libraries ----
library(tidyverse)
library(ggplot)

# Load raw rainfall and temperature data ----
dat <- read.csv("data/tidy/wandering_rainfall-temp_month.csv") %>%
  dplyr::mutate(month = lubridate::month(month, label = T),
                season = case_when(month %in% c("Dec", "Jan") ~ "Birak",
                                   month %in% c("Feb", "Mar") ~ "Bunuru",
                                   month %in% c("Apr", "May") ~ "Djeran",
                                   month %in% c("Jun", "Jul") ~ "Makuru",
                                   month %in% c("Aug", "Sep") ~ "Djilba",
                                   month %in% c("Oct", "Nov") ~ "Kambarang")) %>%
  glimpse()

# Make a plot of rainfall for just the Makuru season ----
ggplot() +
  geom_col(data = filter(dat, season %in% "Makuru"), aes(x = year, y = rainfall), fill = "steelblue3") +
  labs(x = "Year", y = "Rainfall (mm)", title = "Rainfall in Makuru (June/July)") +
  theme_classic()

# Load temperature and rainfall anomaly data ----
anomaly <- read.csv("data/tidy/wandering_rainfall-temp_anomaly.csv")

## Make a plot of rainfall anomaly ----
ggplot() +
  geom_col(data = anomaly, aes(x = year, y = rain_anom, fill = rain_dir), show.legend = F) +
  scale_fill_manual(values = c("neg" = "firebrick2",
                               "pos" = "steelblue3")) +
  labs(x = "Year", y = "Rainfall anomaly (mm)", title = "Yearly rainfall anomaly") +
  theme_classic()

## Make a plot of temperature anomaly ----
ggplot() +
  geom_col(data = anomaly, aes(x = year, y = temp_anom, fill = temp_dir), show.legend = F) +
  scale_fill_manual(values = c("pos" = "firebrick2",
                               "neg" = "steelblue3")) +
  labs(x = "Year", y = "Temperature anomaly (°C)", title = "Yearly temperature anomaly") +
  theme_classic()

# Create yearly temperature and rainfall data ----
yearly <- dat %>%
  dplyr::filter(!year == 2024) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(temperature = mean(temperature),
                   rainfall = mean(rainfall))

## Make a plot of yearly temperature trends ----
ggplot(data = yearly, aes(x = year, y = temperature)) +
  geom_line() +
  geom_smooth(method = "lm", colour = "red") +
  labs(x = "Year", y = "Mean temperature (mm)") +
  theme_classic()

## Make a plot of yearly rainfall trends ----
ggplot(data = yearly, aes(x = year, y = rainfall)) +
  geom_line() +
  geom_smooth(method = "lm", colour = "red") +
  labs(x = "Year", y = "Mean rainfall (mm)") +
  theme_classic()






##### RIDGELINE PLOTS - DON'T RUN ######

# Load daily data for ridgeline plot
# rainfall <- read.csv("data/tidy/wandering_rainfall_day.csv") %>%
#   dplyr::mutate(month = lubridate::month(month, label = T)) %>%
#   glimpse()
# 
# # Ridgeline plots
# # Daily data - looks like shit
# ggplot() +
#   geom_ridgeline(data = filter(rainfall, between(year, 2000, 2020)), aes(x = day, y = year, 
#                                       group = year, height = rainfall),
#                  scale = 0.2) +
#   scale_y_continuous(trans = "reverse") +
#   theme_classic()
# 
# # Monthly data - too many years to plot the whole lot, can't see the trend
# ggplot() +
#   geom_ridgeline(data = filter(dat, between(year, 2000, 2020)), aes(x = month, y = year, 
#                                  group = year, height = rainfall),
#                  scale = 0.2) +
#   scale_y_continuous(trans = "reverse") +
#   theme_classic()
# 
# ggplot() +
#   geom_ridgeline(data = dat, aes(x = month, y = year, 
#                                                                     group = year, height = rainfall),
#                  scale = 0.2) +
#   scale_y_continuous(trans = "reverse") +
#   theme_classic()
# 
# # Every 5 years - still can't really see the trend
# test <- dat %>%
#   dplyr::mutate(decade = if_else(str_detect(year, "0$|5$"), "yes", "no"))
# 
# ggplot() +
#   geom_ridgeline(data = filter(test, decade %in% "yes"), aes(x = month, y = year, 
#                                                                     group = year, height = rainfall),
#                  scale = 0.5) +
#   scale_y_continuous(trans = "reverse") +
#   theme_classic()
# 
# 
