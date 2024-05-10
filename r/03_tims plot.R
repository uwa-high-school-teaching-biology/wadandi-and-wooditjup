###
# Project: High school teaching
# Data:    BOM Rainfall and Temperature data
# Task:    Visualise data
# author:  Claude Spencer
# date:    April 2024
##

# Load libraries ----
library(tidyverse)
library(ggplot2)

# Load raw rainfall data ----
dat <- read.csv("data/raw/swWinterRainfallAnomaly.csv") %>%
  glimpse()

# Make a plot of rainfall anomaly----
ggplot() +
  geom_col(data = dat, aes(x = Year, y = Winter.rainfall.anomaly), fill = "steelblue3") +
  labs(x = "Year", y = "Rainfall (mm)", title = "Rainfall in winter ") +
  theme_classic()


# Load raw  temperature data ----
dat.temp <- read.csv("data/raw/swWinterMeanTemperatureAnomaly.csv") %>%
  glimpse()



# Make a plot of rainfall anomaly----
ggplot() +
  geom_col(data = dat, aes(x = Year, y = Winter.rainfall.anomaly), fill = "steelblue3") +
  labs(x = "Year", y = "Rainfall (mm)", title = "Rainfall in winter ") +
  theme_classic()


# Make a plot of temp anomaly----
ggplot() +
  geom_col(data = dat.temp, aes(x = Year, y = Winter.mean.temperature.anomaly), fill = "steelblue3") +
  labs(x = "Year", y = "temp ()", title = "temp in winter ") +
  theme_classic()

