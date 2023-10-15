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

## Load custom functions ----
source('./R code/customfunctions.R')

# Mappings ----
## Define mapping points ----
lat_points = seq(from = 50, to = 58, by = 0.1)
lon_points = seq(from = -6, to = 2, by = 0.1)

