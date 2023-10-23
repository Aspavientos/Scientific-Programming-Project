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

## Load custom functions ----
source('./R code/customfunctions.R')

# Tentative plottings ----