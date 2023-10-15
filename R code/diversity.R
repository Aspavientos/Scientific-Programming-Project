# Information ----
# Descriptive statistics
# Author: Diego Rodríguez Esperante
# Date of creation: 15/10/2023
# Last edited: 15/10/2023

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

data_species = data[data$Taxon.Rank == 'species',]

## Load custom functions ----
source('./R code/customfunctions.R')

## Create groupings ----
data_species = data[data$Taxon.Rank == 'species',]

data_groupings = list()

# createNewGroup defined in customfunctions.R

interestgroups = c('Scientific.name',
                   'Order',
                   'Family',
                   'Start.date',
                   'State.Province')

t = lapply(interestgroups, createNewGroup, dataset = data_species)

rm(interestgroups, t)

# Mappings ----
## Define mapping points ----
lat_points = seq(from = 50, to = 58, by = 0.1)
lon_points = seq(from = -6, to = 2, by = 0.1)


# Calculate diversity ----
weekly_melt = meltGroup(data_groupings$Start.date, data_species$Scientific.name, date_format = '%Y/%W')

weekly_reshaped = reshape(weekly_melt, direction = 'wide', idvar = 'Start.date', timevar = 'variable')

weekly_diver = diversity(weekly_reshaped[,-1])
names(weekly_diver) = weekly_reshaped[,1]
