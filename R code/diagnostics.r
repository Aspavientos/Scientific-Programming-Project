# Information ----
# Diagnostics
# Author: Diego Rodríguez Esperante
# Date of creation: 18/09/2023
# Last edited: 10/10/2023

# Loading ----
## Load packages ----
require(ggplot2)
require(dplyr)
require(ggpubr)
require(rstudioapi)
require(maps)

## Load data ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
data = read.csv("./Data/Data resource - National Mammal Atlas Project.csv", stringsAsFactors = TRUE)
pristine_data = data

worldmap = map_data('world')

worldmap = worldmap[worldmap$region == 'UK',]

# Data cleaning ----
## Remove extra columns ----
data = subset(data, select = c(-Vitality,
                               -Country,
                               -Basis.of.record,
                               -Identification.verification.status,
                               -Start.date.day,
                               -Start.date.month,
                               -Start.date.year,
                               -Kingdom,
                               -Phylum,
                               -Class))

## Find and remove nulls ----
nulls = list(rep(NA, nrow(data))) %>%
  rep(ncol(data)) %>%
  setNames(colnames(data)) %>%
  data.frame()

for (i in 1:ncol(data)){
  if (class(data[,i]) == 'factor'){
    data[,i] = factor(data[,i], levels = c(levels(data[,i])[-1], levels(data[,i])[1]))
    levels(data[,i])[levels(data[,i]) == ''] = 'No data'
    nulls[,i] = data[,i] == ''
  }else{
    nulls[,i] = is.na(data[,i])
  }
}

data = data[-c(which(nulls$Scientific.name),
               which(nulls$Start.date),
               which(nulls$Latitude..WGS84.),
               which(nulls$Longitude..WGS84.)),]

## Reformat as Date ----
data$Start.date = as.Date(data$Start.date, format = "%d/%m/%Y")

rm("nulls", "i")

## Load custom functions ----
source('./R code/customfunctions.R')

# Diagnostics ----
## Date diagnostics ----
### Year histograms ----
# All years
year_hist = ggplot(data, aes(x = Start.date %>% format('%Y'))) +
  geom_bar(stat = 'count', fill = "skyblue", colour = 'black') +
  labs(x='Year', y='Frequency') +
  ggtitle('Sighting year histogram', subtitle = 'All years') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = as.character(seq(from = 1900, to = 2023, by = 10)))

# Only after 2010
data_ge2010 = data[-which((data$Start.date %>% format("%Y") %>% as.numeric)<2010),]
removed_ge2010 = round((nrow(data)-nrow(data_ge2010))/nrow(data)*100, 2)

year_hist_ge2010 = ggplot(data_ge2010, aes(x = Start.date %>% format('%Y'))) +
  geom_bar(fill = "skyblue", colour = 'black') +
  labs(x='Year', y='Frequency') +
  ggtitle('Sighting year histogram',
          subtitle = paste('2010 onward:', as.character(removed_ge2010), '% samples removed')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = as.character(seq(from = 1900, to = 2023, by = 5)))

### Month histograms ----
month_hist = ggplot(data, aes(x = Start.date %>% format('%b') %>% factor(levels = format(ISOdate(2004,1:12,1),"%b")))) +
  geom_bar(fill = 'limegreen', colour = 'black') +
  labs(x = 'Month', y  = 'Frequency') +
  ggtitle('Sighting month histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_discrete(breaks = custom_colors$month$months,
                   limits = custom_colors$month$months)

### Day-of-the-month histograms ----
day_hist = ggplot(data, aes(x = Start.date %>% format('%d') %>% as.numeric)) +
  geom_bar(fill = 'magenta', colour = 'black') +
  labs(x = 'Day of the month', y  = 'Frequency') +
  ggtitle('Sighting day histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(1, seq(from = 5, to = 31, by = 5)))

### Day-of-the-week histograms ----
dayweek_hist = ggplot(data, aes(x = Start.date %>% format('%a'))) +
  geom_bar(fill = 'pink', colour = 'black') +
  labs(x = 'Day of the week', y  = 'Frequency') +
  ggtitle('Sighting weekday histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_discrete(breaks = format(ISOdate(2023,5,1:7),"%a"),
                   limits = format(ISOdate(2023,5,1:7),"%a"))

### Date histograms arranged ----
date_hist = ggarrange(year_hist_ge2010, month_hist, day_hist, dayweek_hist,
                      labels = c('Y', 'M', 'D', 'W'),
                      ncol = 2, nrow = 2)

rm(data_ge2010, removed_ge2010)

## Province diagnostics ----
### Standard province histogram ----
province_hist = ggplot(data, aes(x= State.Province)) +
  geom_bar(aes(fill = State.Province,
               color = State.Province)) +
  geom_text(stat='count',
            aes(label=..count..),
            vjust=-0.25) +
  labs(x= 'Countries', y = 'Frequency') +
  ggtitle('Sightings by Country') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none') +
  scale_x_discrete(limits = custom_colors$province$provinces,
                   labels = custom_colors$province$names) +
  scale_fill_manual(values = custom_colors$province$fills) +
  scale_color_manual(values = custom_colors$province$borders)

## Coordiante histograms ----
### Latitude histogram ----
latcoord_hist = ggplot(data, aes(x = Latitude..WGS84.)) +
  geom_histogram(fill = 'blue', color = 'black') +
  labs(x = "Latitude", y = "Count") +
  ggtitle("Latitude histogram") +
  theme(plot.title = element_text(hjust = 0.5))

### Longitude histogram ----
longcoord_hist = ggplot(data, aes(x = Longitude..WGS84.)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "Longitude", y = "Count") +
  ggtitle("Longitude histogram") +
  theme(plot.title = element_text(hjust = 0.5))

### Latitude-longitude histograms arranged ----
latlong_hist = ggarrange(latcoord_hist, longcoord_hist,
                      labels = c('Lat', 'Lon'),
                      ncol = 1, nrow = 2)

### Coordinate uncertainty histogram ----
uncertcoord_hist = ggplot(data, aes(x = Coordinate.uncertainty..m.,
                                    label = Coordinate.uncertainty..m.)) +
  geom_histogram(fill = 'cyan', color = 'black') +
  ggtitle("Uncertainty histogram") +
  theme(plot.title = element_text(hjust = 0.5))

# Remove large uncertainties
data_biguncert = data[-which((data$Coordinate.uncertainty..m.)>10000),]
removed_biguncert = round((nrow(data)-nrow(data_biguncert))/nrow(data)*100, 2)

smalluncertcoord_hist = ggplot(data_biguncert, aes(x = Coordinate.uncertainty..m.)) +
  geom_histogram(fill = 'cyan', color = 'black') +
  labs(x = "Uncertainty (m)", y = "Count") +
  ggtitle("Filtered uncertainties histogram",
          subtitle = paste('Less than 10km:', as.character(removed_biguncert), '% samples removed')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

rm(data_biguncert, removed_biguncert)

## Taxon rank histogram ----
taxonrank_hist = ggplot(data, aes(x = Taxon.Rank)) +
  geom_bar(fill = 'green4', color = 'black') +
  geom_text(stat='count',
            aes(label=..count..),
            vjust=-0.25) +
  labs(x = 'Taxon rank',
       y = 'Count') +
  ggtitle('Taxon rank histogram') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Map visualizations ----
mapping_plot = ggplot(data, aes(x = Longitude..WGS84., y = Latitude..WGS84.)) +
  geom_point(alpha = 0.01,
             size = 0.25) +
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               alpha = 0.5,
               fill = 'gray90', 
               color = 'black') +
  stat_density2d(aes(fill = after_stat(level)), alpha = 0.75,
                 geom = 'polygon', data = data) +
  ggtitle('Density mapping of sightings') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none') +
  labs(x = "Longitude", y = "Latitude") + 
  coord_fixed(ratio = 1.3, 
              xlim = c(-10,2), 
              ylim = c(50, 59))

# Save all plots ----
plot_path = '/Diagnostic'
customggsave(year_hist, save_path = plot_path)
customggsave(year_hist_ge2010, save_path = plot_path)
customggsave(date_hist, upscale = 1.5, save_path = plot_path)
  
customggsave(province_hist, save_path = plot_path)

customggsave(latlong_hist, upscale = 1.5, save_path = plot_path)
customggsave(uncertcoord_hist, save_path = plot_path)
customggsave(smalluncertcoord_hist, save_path = plot_path)

customggsave(taxonrank_hist, upscale = 1.5, save_path = plot_path)

customggsave(mapping_plot, upscale = 1.5, save_path = plot_path)

# Remove all outlier data and save ----
data = data[-which((data$Start.date %>% format("%Y") %>% as.numeric)<2010),]

data = data[-which((data$Coordinate.uncertainty..m.)>10000),]

write.csv(data, './Data/cleaned_data.csv', row.names=FALSE)