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
  dplyr::mutate(raindir = ifelse(Winter.rainfall.anomaly > 0, "pos", "neg")) %>%
  glimpse()

# Make a plot of rainfall anomaly----
ggplot() +
  geom_col(data = dat, aes(x = Year, y = Winter.rainfall.anomaly, fill = raindir),
           show.legend = F, colour = "black", linewidth = 3) +
  scale_fill_manual(values = c("neg" = "firebrick2",
                               "pos" = "steelblue3")) +
  geom_smooth(data = dat, aes(x = Year, y = Winter.rainfall.anomaly),
              method = "lm", se = F, colour = "black", linewidth = 5) +
  labs(x = "Year", y = "Rainfall anomaly (mm)", title = "Winter rainfall anomaly\nSouthwestern Australia (1900 to 2023)") +
  theme_classic() +
  theme(text = element_text(size = 50)) +
  scale_y_continuous(sec.axis = sec_axis(~.x)) +
  scale_x_continuous(sec.axis = sec_axis(~.x, labels = NULL)) +
  annotate(geom = "text", size = 15, x = c(1920, 2000, 1912), 
           y = c(-140, -140, 270),
           label = c("Linear trend of -7.76mm/decade",
                     "Based on 30-year climatology (1961-1990)",
                     "Australian Burea of Meteorology"))

ggsave(filename = "plots/sw-rainfall-anomaly_A0.png",
       height = 841, width = 1189, units = "mm", dpi = 300,
       limitsize = F)

# Load raw  temperature data ----
dat.temp <- read.csv("data/raw/swWinterMeanTemperatureAnomaly.csv") %>%
  dplyr::mutate(tempdir = ifelse(Winter.mean.temperature.anomaly > 0, "pos", "neg")) %>%
  glimpse()

# Make a plot of temp anomaly----
ggplot() +
  geom_col(data = dat.temp, aes(x = Year, y = Winter.mean.temperature.anomaly, fill = tempdir),
           show.legend = F, colour = "black", linewidth = 3) +
  scale_fill_manual(values = c("neg" = "firebrick2",
                               "pos" = "steelblue3")) +
  geom_smooth(data = dat.temp, aes(x = Year, y = Winter.mean.temperature.anomaly),
              method = "lm", se = F, colour = "black", linewidth = 5) +
  labs(x = "Year", y = "Mean temperature anomaly (°C)", title = "Winter mean temperature anomaly\nSouthwestern Australia (1900 to 2023)") +
  theme_classic() +
  theme(text = element_text(size = 50)) +
  scale_y_continuous(sec.axis = sec_axis(~.x)) +
  scale_x_continuous(sec.axis = sec_axis(~.x, labels = NULL)) +
  annotate(geom = "text", size = 15, x = c(1920, 2000, 1920), 
           y = c(-1.6, -1.6, 1.3),
           label = c("Linear trend of 0.1 °C/decade",
                     "Based on 30-year climatology (1961-1990)",
                     "Australian Burea of Meteorology"))

ggsave(filename = "plots/sw-temperature-anomaly_A0.png",
       height = 841, width = 1189, units = "mm", dpi = 300,
       limitsize = F)
