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
data = read.csv("./Data/Data resource - National Mammal Atlas Project.csv", stringsAsFactors = TRUE)
pristine_data = data

# Reformat data
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

data$Start.date = as.Date(data$Start.date, format = "%d/%m/%Y")

rm("nulls", "i")

## Initial diagnostics
# Date histograms
year_hist = ggplot(data, aes(x = Start.date %>% format('%Y'))) +
  geom_bar(stat = 'count', fill = "skyblue", colour = 'black') +
  theme_light() +
  labs(x='Year', y='Frequency') +
  ggtitle('Sighting year histogram', subtitle = 'All years') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = as.character(seq(from = 1900, to = 2023, by = 10)))

data_ge2010 = data[-which((data$Start.date %>% format("%Y") %>% as.numeric)<2010),]
removed_ge2010 = round((nrow(data)-nrow(data_ge2010))/nrow(data)*100, 2)

year_hist_ge2010 = ggplot(data_ge2010, aes(x = Start.date %>% format('%Y'))) +
  geom_bar(fill = "skyblue", colour = 'black') +
  theme_light() +
  labs(x='Year', y='Frequency') +
  ggtitle('Sighting year histogram',
          subtitle = paste('2010 onward:', as.character(removed_ge2010), '% samples removed')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = as.character(seq(from = 1900, to = 2023, by = 5)))

month_hist = ggplot(data, aes(x = Start.date %>% format('%b') %>% factor(levels = format(ISOdate(2004,1:12,1),"%b")))) +
  geom_bar(fill = 'limegreen', colour = 'black') +
  theme_light() +
  labs(x = 'Month', y  = 'Frequency') +
  ggtitle('Sighting month histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_discrete(breaks = format(ISOdate(2004,1:12,1),"%b"))

day_hist = ggplot(data, aes(x = Start.date %>% format('%d') %>% as.numeric)) +
  geom_bar(fill = 'magenta', colour = 'black') +
  theme_light() +
  labs(x = 'Day of the month', y  = 'Frequency') +
  ggtitle('Sighting day histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(1, seq(from = 5, to = 31, by = 5)))

dayweek_hist = ggplot(data, aes(x = Start.date %>% format('%a') %>% factor(levels = format(ISOdate(2023,5,1:7),"%a")))) +
  geom_bar(fill = 'pink', colour = 'black') +
  theme_light() +
  labs(x = 'Day of the week', y  = 'Frequency') +
  ggtitle('Sighting weekday histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_discrete(breaks = format(ISOdate(2023,5,1:7),"%a"))

date_hist = ggarrange(year_hist_ge2010, month_hist, day_hist, dayweek_hist,
                      labels = c('Y', 'M', 'D', 'W'),
                      ncol = 2, nrow = 2)

rm(data_ge2010, removed_ge2010)

# Province diagnostics
province_colors = data.frame(Provinces = levels(data$State.Province),
                             Fills = c('#FFFFFF', '#CF142B', '#5E89C2', '#005EB8', '#00B140', '#FF4F00'),
                             Borders = c('#CE1124', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FF4F00'))

province_hist = ggplot(data, aes(x= State.Province)) +
  geom_bar(aes(fill = State.Province,
               color = State.Province)) +
  stat_count(geom = "text",
             size = 3.5,
             aes(label = after_stat(count)),
             vjust=-0.25)+
  labs(x= 'Countries', y = 'Frequency') +
  ggtitle('Sightings by Country') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none') +
  scale_x_discrete(labels = province_colors$Provinces) +
  scale_fill_manual(values = province_colors$Fills) +
  scale_color_manual(values = province_colors$Borders)

#### MISSING
normprovince = data.frame(matrix(nrow = length(levels(data$State.Province)), ncol = 12, dimnames = list(levels(data$State.Province), format(ISOdate(2004,1:12,1),"%b"))))

for (i in nrow(normprovince)){
  provs = which(data$State.Province==levels(data$State.Province)[i])
  for (j in ncol(normprovince)){
    
  }
}

normprovince_hist = ggplot(data, aes(x = Start.date %>% format('%b') %>% factor(levels = format(ISOdate(2004,1:12,1),"%b")))) +
  geom_bar(position = 'dodge',
           aes(fill = State.Province,
               color = State.Province)) +
  labs(x = 'Month',
       y  = 'Frequency') +
  ggtitle('Sighting month histogram') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = format(ISOdate(2004,1:12,1),"%b")) +
  scale_fill_manual(name = 'Countries',
                    labels = province_colors$Provinces,
                    values = province_colors$Fills) +
  scale_color_manual(values = province_colors$Borders,
                     guide = 'none')

normprovince_hist
####

rm(i, j, province_colors)

# Coordiante histograms
latcoord_hist = ggplot(data, aes(x = Latitude..WGS84.)) +
  geom_histogram(fill = 'blue', color = 'black') +
  labs(x = "Latitude", y = "Count") +
  ggtitle("Latitude histogram") +
  theme(plot.title = element_text(hjust = 0.5))

longcoord_hist = ggplot(data, aes(x = Longitude..WGS84.)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "Longitude", y = "Count") +
  ggtitle("Longitude histogram") +
  theme(plot.title = element_text(hjust = 0.5))

latlong_hist = ggarrange(latcoord_hist, longcoord_hist,
                      labels = c('Lat', 'Lon'),
                      ncol = 1, nrow = 2)

uncertcoord_hist = ggplot(data, aes(x = Coordinate.uncertainty..m.)) +
  geom_histogram(fill = 'cyan', color = 'black') +
  labs(x = "Uncertainty (m)", y = "Count") +
  ggtitle("Uncertainty histogram") +
  theme(plot.title = element_text(hjust = 0.5))

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

#

# Save all plots
plotpath = './Plots'

customggsave = function(plot){
  ggsave(paste0(deparse(substitute(plot)),".png"),
         plot = plot,
         device = 'png',
         width = 1920,
         height = 1080,
         units = 'px',
         path = plotpath)
}

customggsave(year_hist)
customggsave(year_hist_ge2010)
customggsave(date_hist)
  
customggsave(province_hist)

customggsave(latlong_hist)
customggsave(uncertcoord_hist)
customggsave(smalluncertcoord_hist)

# Remove all outlier data and save
data = data[-which((data$Start.date %>% format("%Y") %>% as.numeric)<2010),]

data = data[-which((data$Coordinate.uncertainty..m.)>10000),]

write.csv(data, './Data/cleaned_data.csv', row.names=FALSE)