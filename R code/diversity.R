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
require(vegan)

## Load data ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
data = read.csv("./Data/cleaned_data.csv", stringsAsFactors = TRUE)

data$Start.date = as.Date(data$Start.date)

data_species = data[data$Taxon.Rank == 'species',]

## Load custom functions ----
source('./R code/customfunctions.R')

## Create groupings ----
# createNewGroup defined in customfunctions.R

data_groupings = createNewGroup('Start.date', dataset = data_species)

rm(interestgroups, t)

# Mappings ----
## Define mapping points ----
lat_points = seq(from = 50, to = 58, by = 1)
lon_points = seq(from = -6, to = 2, by = 1)

coord_pairs = expand.grid(lat_points, lon_points)
colnames(coord_pairs) = c('Latitude', 'Longitude')

# Calculate diversity ----
## Country-wide, weekly ----
weekly_melt = meltGroup(data_groupings$Start.date, data_species$Scientific.name, date_format = '%Y/%W')

weekly_reshaped = reshape(weekly_melt, direction = 'wide', idvar = 'Start.date', timevar = 'variable')

weekly_diver = diversity(weekly_reshaped[,-1])
names(weekly_diver) = weekly_reshaped[,1]

## Localized diversity ----
calcLocalDiversity = function(dataset, coordinates, area = 1, date_format){
  distance_vec = sqrt(abs((coordinates$Latitude - dataset$Latitude..WGS84.)^2 + (coordinates$Longitude - dataset$Longitude..WGS84.)^2))
  distance_check = distance_vec < area
  
  local_dataset = dataset[distance_check,]
  
  if (nrow(local_dataset)>0){
    local_grouping = createNewGroup('Start.date', dataset = local_dataset)
    
    local_melt = meltGroup(local_grouping$Start.date, local_dataset$Scientific.name, date_format = '%Y/%W')
    
    local_reshaped = reshape(local_melt, direction = 'wide', idvar = 'Start.date', timevar = 'variable')
    
    local_diver = diversity(local_reshaped[,-1])
    dates = local_reshaped[,1]
    
    local_diverdf = data.frame(Dates = dates,
                               Diversity = local_diver)
    
  }else{
    local_diverdf = data.frame(Dates = week_list,
                               Diversity = rep(NA, length(week_list)))
  }
  return(local_diverdf)
}

### Localized weekly diversity ----
week_list = data_species$Start.date %>% format('%Y/%W') %>% unique %>% sort

all_weeks = array(rep(NA, length(week_list)), dimnames = list(week_list))

weekly_local_diver = array(dim = c(length(lat_points),
                                   length(lon_points),
                                   length(week_list)),
                           dimnames = list(lat_points,
                                        lon_points,
                                        week_list))

for (i in 1:nrow(coord_pairs)){
  loc_diver = calcLocalDiversity(data_species,
                                 coord_pairs[i,],
                                 date_format = '%Y/%W')
  
  loc_diverdf = data.frame(loc_diver$Diversity,
                           row.names = loc_diver$Dates)
  
  weekly_local_diver[as.character(coord_pairs[i,1]), as.character(coord_pairs[i,2]),] = 
    merge(all_weeks, loc_diverdf, by = 'row.names', all = T)[,3]
  
  rm(loc_diver, loc_diverdf)
  }
