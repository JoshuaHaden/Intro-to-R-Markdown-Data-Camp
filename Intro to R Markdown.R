###Chapter 1 Authoring R Markdown Reports

###Prepare the Workspace for Preliminary Analysis
# Load the 'nasaweather' package
library("nasaweather")

# Load the 'dplyr' package
library("dplyr")

# Load the 'ggvis' data set
library("ggvis")

###Prepare Your Data
# The 'nasaweather' and 'dplyr' packages are available in the workspace

# Set the 'year' variable to 1995
year <- 1995

means <- atmos %>% 
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE), 
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

# Inspect the means variable
means

###Experiment With Plot Generation
# The 'nasaweather', dplyr' and 'ggvis' packages are loaded in the workspace.

# Code for the previous exercise - do not change this
means <- atmos %>% 
  filter(year == 1995) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE), 
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

# Change the code to plot the 'temp' variable vs the 'ozone' variable
means %>% 
  ggvis(x = ~temp, y = ~ozone) %>%
  layer_points() 

###Prepare a Model Component
# The 'nasaweather' and dplyr' packages are already at your disposal
means <- atmos %>% 
  filter(year == 1995) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE), 
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

# Change the model: base prediction only on 'temp'
mod <- lm(ozone ~ temp, data = means)

# Generate a model summary and interpret the results
summary(mod)

###Chapter 2 Embedding Code 

###R Code Chunks
library(nasaweather)
library(dplyr)

year <- 1995

means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

###Customize R Code Chunks
library(nasaweather)
library(dplyr)
library(ggvis)

year <- 1995

means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

###Popular Chunk Options

##Cleaning
load(url("http://assets.datacamp.com/course/rmarkdown/atmos.RData")) # working with a subset
library(dplyr)
library(ggvis)

year <- 1995
means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

##Ozone and Temperature
means %>%
  ggvis(~temp, ~ozone) %>%
  layer_points()

means$locale <- "north america"
means$locale[means$lat < 10] <- "south pacific"
means$locale[means$long > -80 & means$lat < 10] <- "south america"
means$locale[means$long > -80 & means$lat > 10] <- "north atlantic"

##Model
lm(ozone ~ temp + locale + temp:locale, data = means)

means %>%
  group_by(locale) %>%
  ggvis(~temp, ~ozone) %>%
  layer_points(fill = ~locale) %>%
  layer_model_predictions(model = "lm", stroke = ~locale) %>%
  hide_legend("stroke") %>%
  scale_nominal("stroke", range = c("darkorange", "darkred", "darkgreen", "darkblue"))

##Diagnostics
mod <- lm(ozone ~ temp, data = means)
mod2 <- lm(ozone ~ temp + locale, data = means)
mod3 <- lm(ozone ~ temp + locale + temp:locale, data = means)

anova(mod, mod2, mod3)


###Labeling and Reusing Code Chunks

## Exploring the mtcars data set
library(dplyr)
library(ggvis)
mtcars %>%
  group_by(factor(cyl)) %>%
  ggvis(~mpg, ~wt, fill = ~cyl) %>%
  layer_points()

###Chapter 3 Compiling Reports
library(nasaweather)
library(dplyr)
library(tidyr)

means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

clouds <- means %>%
  select(-(temp:ozone)) %>%
  gather("altitude", "coverage", 3:5)

