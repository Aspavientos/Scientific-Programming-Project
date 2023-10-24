# Information ----
# Correlation
# Author: Diego Rodríguez Esperante
# Date of creation: 15/10/2023
# Last edited: 23/10/2023

# Loading ----
## Loading packages ----
require(ggplot2)
require(dplyr)
require(ggpubr)
require(rstudioapi)

## Load data ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
data = read.csv("./Data/cleaned_data.csv", stringsAsFactors = TRUE)

data$Start.date = as.Date(data$Start.date)

weekly_data = read.csv('./Data/Weekly diversity.csv')

## Load custom functions ----
source('./R code/customfunctions.R')

## Create groupings ----
data_groupings = createNewGroup('Start.date')

weekly_counts = meltGroup(data_groupings$Start.date, data$Order, date_format = '%Y/%W')

weekly_data$Count = aggregate(weekly_counts$value, by = list(weekly_counts$Start.date), FUN = sum)[,2]

# Tentative plottings ----
weekly_countsVdiversity = ggplot(weekly_data, aes(fill = Season, color = Season)) +
  geom_point(aes(x = Dates, y = max(Count)/max(Diversity)*Diversity)) +
  geom_col(aes(x = Dates, y = Count)) +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(weekly_data$Diversity)/max(weekly_data$Count)), name = 'Diversity'))
