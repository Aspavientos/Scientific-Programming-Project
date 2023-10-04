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

# Supplementary groupings
data_groupings = list()

test = group_by(data, .data[[group]])


createNewGroup = function(group){
  new_grouping = data %>% group_by(.data[[group]])
  # vignette('programming')
  new_grouping_attributes = (new_grouping %>% attributes)$groups
  
}

new_grouping = data %>% group_by(Start.date)
new_grouping_attributes = (new_grouping %>% attributes)$groups
data_groupings = append(data_groupings, list(Start.date = new_grouping_attributes))

new_grouping = data %>% group_by(Start.date)
new_grouping_attributes = (new_grouping %>% attributes)$groups
data_groupings = append(data_groupings, list(Start.date = new_grouping_attributes))


rm(new_grouping, new_grouping_attributes, i)

## Boxplots
t = group_by(data, Start.date)
