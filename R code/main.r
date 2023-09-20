# Load packages
require(ggplot2)
require(mapview)
require(dplyr)

# Load data
setwd("..")
data = read.csv("Data resource - National Mammal Atlas Project.csv", stringsAsFactors = TRUE)
pristine_data = data

# Reformat data
data = subset(data, select = c(-Vitality, -Country, -Basis.of.record, -Identification.verification.status))

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

# Initial diagnostics
year_hist = barplot(data$Start.date.year,
                 main = "Sighting year histogram, all years",
                 xlab = "Year")

year_hist_ge2010 = hist(data$Start.date.year[data$Start.date.year > 2010],
                 main = "Sighting year histogram, 2010 onward",
                 xlab = "Year")

mtext(paste('Removed information:', as.character(round(sum(data$Start.date.year<2010)/nrow(data)*100, 2)), '%'),
      side = 3)

data = data[-which(data$Start.date.year<2010),]

month_hist = hist(data$Start.date.month,
                  main = "Sighting month histogram",
                  xlab = "Month")

# Plot sightings
mapview(data[data$Common.name == "Roe Deer",], 
        xcol = "Longitude..WGS84.", ycol = "Latitude..WGS84.",
        label = data$Start.date)
