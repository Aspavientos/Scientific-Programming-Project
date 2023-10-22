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

# Mappings ----
## Define mapping points ----
lat_points = seq(from = 50, to = 58, by = 0.1)
lon_points = seq(from = -6, to = 2, by = 0.1)

coord_pairs = expand.grid(lat_points, lon_points)
colnames(coord_pairs) = c('Latitude', 'Longitude')

# Calculate diversity ----
## Country-wide ----
### Weekly diversity -----
weekly_melt = meltGroup(data_groupings$Start.date, data_species$Scientific.name, date_format = '%Y/%W')

weekly_reshaped = reshape(weekly_melt, direction = 'wide', idvar = 'Start.date', timevar = 'variable')

weekly_diverdf = data.frame(Dates = weekly_reshaped[,1],
                            Diversity = diversity(weekly_reshaped[,-1]))

rm(weekly_melt, weekly_reshaped)
### Monthly diversity ----
monthly_melt = meltGroup(data_groupings$Start.date, data_species$Scientific.name, date_format = '%Y/%m')

monthly_reshaped = reshape(monthly_melt, direction = 'wide', idvar = 'Start.date', timevar = 'variable')

monthly_diver = diversity(monthly_reshaped[,-1])
names(monthly_diver) = monthly_reshaped[,1]

## Localized diversity ----
calcLocalDiversity = function(dataset, coordinates, area = 0.1, dateFormat){
  distance_vec = sqrt(abs((coordinates$Latitude - dataset$Latitude..WGS84.)^2 + (coordinates$Longitude - dataset$Longitude..WGS84.)^2))
  distance_check = distance_vec < area
  
  local_dataset = dataset[distance_check,]
  
  if (nrow(local_dataset)>0){
    local_grouping = createNewGroup('Start.date', dataset = local_dataset)
    
    local_melt = meltGroup(local_grouping$Start.date, local_dataset$Scientific.name, date_format = dateFormat)
    
    local_reshaped = reshape(local_melt, direction = 'wide', idvar = 'Start.date', timevar = 'variable')
    
    local_diver = diversity(local_reshaped[,-1])
    dates = local_reshaped[,1]
    
    local_diverdf = data.frame(Dates = dates,
                               Diversity = local_diver)
    
  }else{
    date_list = dataset$Start.date %>% format(dateFormat) %>% unique %>% sort
    
    local_diverdf = data.frame(Dates = date_list,
                               Diversity = rep(NA, length(date_list)))
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
                                 dateFormat = '%Y/%W')
  
  loc_diverdf = data.frame(loc_diver$Diversity,
                           row.names = loc_diver$Dates)
  
  weekly_local_diver[as.character(coord_pairs[i,1]), as.character(coord_pairs[i,2]),] = 
    merge(all_weeks, loc_diverdf, by = 'row.names', all = T)[,3]
  
  disp(paste0('Coordinates: ', coord_pairs[i,1], ', ', coord_pairs[i,2]))
  
  rm(loc_diver, loc_diverdf)
  }
rm(all_weeks, week_list)

weekly_local_diverdf = melt(weekly_local_diver)
colnames(weekly_local_diverdf) = c('Latitude', 'Longitude', 'Year/Week', 'Diversity')

### Localized monthly diversity ----
month_list = data_species$Start.date %>% format('%Y/%m') %>% unique %>% sort

all_months = array(rep(NA, length(month_list)), dimnames = list(month_list))

monthly_local_diver = array(dim = c(length(lat_points),
                                    length(lon_points),
                                    length(month_list)),
                           dimnames = list(lat_points,
                                           lon_points,
                                           month_list))

for (i in 5744:nrow(coord_pairs)){
  loc_diver = calcLocalDiversity(data_species,
                                 coord_pairs[i,],
                                 dateFormat = '%Y/%m')
  
  loc_diverdf = data.frame(loc_diver$Diversity,
                           row.names = loc_diver$Dates)
  
  monthly_local_diver[as.character(coord_pairs[i,1]), as.character(coord_pairs[i,2]),] = 
    merge(all_months, loc_diverdf, by = 'row.names', all = T)[,3]
  
  print(paste0('Coordinates: ', coord_pairs[i,1], ', ', coord_pairs[i,2]))
  
  rm(loc_diver, loc_diverdf)
}
rm(all_months, month_list)

monthly_local_diverdf = melt(monthly_local_diver)
colnames(monthly_local_diverdf) = c('Latitude', 'Longitude', 'Year/Month', 'Diversity')

# Plotting ----
seasons = getSeason(data_species$Start.date %>% sort)

seasons_weekly = data.frame(Weeks = data_species$Start.date %>% format('%Y/%W') %>% sort,
                            Season = seasons)

seasons_weekly = distinct(seasons_weekly, Weeks, Season)

seasons_weekly = seasons_weekly[!duplicated(seasons_weekly$Weeks),]

weekly_diverdf$Season = seasons_weekly$Season

weekly_diver_plot = ggplot(weekly_diverdf, aes(x = Dates, y = Diversity)) +
  geom_point(aes(color = Season)) +
  labs(x = 'Week',
       y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  scale_x_discrete(breaks = ISOdate(2010:2023, 1, 1) %>% format('%Y/%W')) +
  ggtitle('Weekly diversity measures across the years') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

season_weekly_diver_violin = ggplot(weekly_diverdf, aes(x = Season, y = Diversity)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              trim = FALSE,
              aes(fill = Season)) +
  scale_x_discrete(limits = custom_colors$seasons$seasons) +
  scale_fill_manual(limits = custom_colors$seasons$seasons,
                    values = custom_colors$seasons$fills,
                    guide = 'none') +
  ggtitle('Weekly diversity measures across seasons') +
  theme(plot.title = element_text(hjust = 0.5))

weekly_diver_plot

# Save data ----
## Write data to csv ----
write.csv(weekly_local_diverdf, './Data/Weekly local diversity.csv', row.names = F)

write.csv(monthly_local_diverdf, './Data/Monthly local diversity.csv', row.names = F)

## Save plots ----
