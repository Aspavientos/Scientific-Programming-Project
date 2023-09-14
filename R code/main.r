setwd("..")
data = read.csv("Data resource - National Mammal Atlas Project.csv")
data = subset(data, select = c(-Start.date, -Vitality))
