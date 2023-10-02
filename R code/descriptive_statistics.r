# Information
# Diego Rodríguez Esperante: 02/10/2023

# Load packages
require(ggplot2)
require(dplyr)
require(ggpubr)
require(rstudioapi)

# Load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
data = read.csv("./Data/cleaned_data.csv", stringsAsFactors = TRUE)


# Boxplots
t = group_by(data, Start.date)
