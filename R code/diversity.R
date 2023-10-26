# Information ----
# Diversity
# Author: Diego Rodríguez Esperante
# Date of creation: 15/10/2023
# Last edited: 26/10/2023

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
lat_points = seq(from = 50, to = 59, by = 0.1)
lon_points = seq(from = -8, to = 2, by = 0.1)

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

monthly_diverdf = data.frame(Dates = monthly_reshaped[,1],
                             Diversity = diversity(monthly_reshaped[,-1]))

rm(monthly_melt, monthly_reshaped)

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

# Plotting ----
## Weekly plots ----
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
  scale_x_discrete(breaks = ISOdate(2010:2023, 1, 1) %>% format('%Y/%W'),
                   labels = (2010:2023)) +
  ggtitle('Weekly diversity measures across the years') +
  theme(plot.title = element_text(hjust = 0.5))

season_weekly_diver_violin = ggplot(weekly_diverdf, aes(x = Season, y = Diversity)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              aes(fill = Season)) +
  scale_x_discrete(limits = custom_colors$seasons$seasons) +
  scale_y_continuous(limits = c(2, 3.5)) +
  scale_fill_manual(limits = custom_colors$seasons$seasons,
                    values = custom_colors$seasons$fills,
                    guide = 'none') +
  ggtitle('Weekly diversity measures across seasons') +
  theme(plot.title = element_text(hjust = 0.5))

## Monthly plots ----
seasons_monthly = data.frame(Months = data_species$Start.date %>% format('%Y/%m') %>% sort,
                            Season = seasons)

seasons_monthly = distinct(seasons_monthly, Months, Season)

seasons_monthly = seasons_monthly[!duplicated(seasons_monthly$Months),]

monthly_diverdf$Season = seasons_monthly$Season

monthly_diver_plot = ggplot(monthly_diverdf, aes(x = Dates, y = Diversity)) +
  geom_point(aes(color = Season)) +
  labs(x = 'Month',
       y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  scale_x_discrete(breaks = ISOdate(2010:2023, 1, 1) %>% format('%Y/%m'),
                   labels = 2010:2023) +
  ggtitle('Monthly diversity measures across the years') +
  theme(plot.title = element_text(hjust = 0.5))

season_monthly_diver_violin = ggplot(monthly_diverdf, aes(x = Season, y = Diversity)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              aes(fill = Season)) +
  scale_x_discrete(limits = custom_colors$seasons$seasons) +
  scale_y_continuous(limits = c(2, 3.5)) +
  scale_fill_manual(limits = custom_colors$seasons$seasons,
                    values = custom_colors$seasons$fills,
                    guide = 'none') +
  ggtitle('Monthly diversity measures across seasons') +
  theme(plot.title = element_text(hjust = 0.5))

# Save data ----
## Write data to csv ----
write.csv(weekly_diverdf, './Data/Weekly diversity.csv', row.names = F)

write.csv(monthly_diverdf, './Data/Monthly diversity.csv', row.names = F)

## Save plots ----
plot_path = '/Diversity'
customggsave(weekly_diver_plot, upscale = 1.5, save_path = plot_path)
customggsave(season_weekly_diver_violin, upscale = 1.5, save_path = plot_path)

customggsave(monthly_diver_plot, upscale = 1.5, save_path = plot_path)
customggsave(season_monthly_diver_violin, upscale = 1.5, save_path = plot_path)

rm(plot_path)