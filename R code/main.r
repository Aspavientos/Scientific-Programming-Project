# Load packages
require(ggplot2)
require(mapview)
require(dplyr)
require(ggpubr)

# Load data
setwd("..")
data = read.csv("Data resource - National Mammal Atlas Project.csv", stringsAsFactors = TRUE)
pristine_data = data

# Reformat data
data = subset(data, select = c(-Vitality, -Country, -Basis.of.record, -Identification.verification.status, -Start.date.day, -Start.date.month, -Start.date.year))

nulls = list(rep(NA, nrow(data))) %>%
  rep(ncol(data)) %>%
  setNames(colnames(data)) %>%
  data.frame()

for (i in 1:ncol(data)){
  if (class(data[,i]) == 'factor'){
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
  geom_histogram(stat = 'count', fill = "skyblue", colour = 'black') +
  labs(x='Year', y='Frequency') +
  ggtitle('Sighting year histogram', subtitle = 'All years') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = as.character(seq(from = 1900, to = 2023, by = 10)))

data_ge2010 = data[-which((data$Start.date %>% format("%Y") %>% as.numeric)<2010),]
removed_ge2010 = round((nrow(data)-nrow(data_ge2010))/nrow(data)*100, 2)

year_hist_ge2010 = ggplot(data_ge2010, aes(x = Start.date %>% format('%Y'))) +
  geom_histogram(stat = 'count', fill = "skyblue", colour = 'black') +
  labs(x='Year', y='Frequency') +
  ggtitle('Sighting year histogram', subtitle = paste('2010 onward:', as.character(removed_ge2010), '% samples removed')) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = as.character(seq(from = 1900, to = 2023, by = 5)))

data = data[-which((data$Start.date %>% format("%Y") %>% as.numeric)<2010),]

month_hist = ggplot(data, aes(x = Start.date %>% format('%b') %>% factor(levels = format(ISOdate(2004,1:12,1),"%b")))) +
  geom_histogram(stat = 'count', fill = 'limegreen', colour = 'black') +
  labs(x = 'Month', y  = 'Frequency') +
  ggtitle('Sighting month histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_discrete(breaks = format(ISOdate(2004,1:12,1),"%b"))

day_hist = ggplot(data, aes(x = Start.date %>% format('%d') %>% as.numeric)) +
  geom_histogram(stat = 'count', fill = 'magenta', colour = 'black') +
  labs(x = 'Day of the month', y  = 'Frequency') +
  ggtitle('Sighting day histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(1, seq(from = 5, to = 31, by = 5)))

dayweek_hist = ggplot(data, aes(x = Start.date %>% format('%a') %>% factor(levels = format(ISOdate(2023,5,1:7),"%a")))) +
  geom_histogram(stat = 'count', fill = 'pink', colour = 'black') +
  labs(x = 'Day of the week', y  = 'Frequency') +
  ggtitle('Sighting weekday histogram') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_x_discrete(breaks = format(ISOdate(2023,5,1:7),"%a"))

date_hist = ggarrange(year_hist_ge2010, month_hist, day_hist, dayweek_hist,
                      labels = c('Y', 'M', 'D', 'W'),
                      ncol = 2, nrow = 2)

rm(data_ge2010, removed_ge2010)

# Plot sightings
mapview(data[data$Common.name == "Roe Deer",], 
        xcol = "Longitude..WGS84.", ycol = "Latitude..WGS84.",
        label = data$Start.date)
