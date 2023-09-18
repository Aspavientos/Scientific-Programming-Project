# Load packages
require(ggplot2)
require(mapview)

# Load data
setwd("..")
data = read.csv("Data resource - National Mammal Atlas Project.csv")
data = subset(data, select = c(-Vitality))
data = data[-126566,]


# Initial diagnostics
year_hist = hist(data$Start.date.year,
                 main = "Sighting year histogram",
                 xlab = "Year")

# Plot sightings
mapview(data[data$Common.name == "Roe Deer",], 
        xcol = "Longitude..WGS84.", ycol = "Latitude..WGS84.",
        label = data$Start.date)
