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

createNewGroup = function(group){
  new_grouping = data %>% group_by(.data[[group]])
  # vignette('programming')
  new_grouping_attributes = (new_grouping %>% attributes)$groups
  data_groupings <<- append(data_groupings, list(groups = new_grouping_attributes))
  names(data_groupings)[length(data_groupings)] <<- group
}

interestgroups = c('Scientific.name',
                   'Start.date',
                   'State.Province')

lapply(interestgroups, createNewGroup)

rm()

## Boxplots
# Mean observations per day