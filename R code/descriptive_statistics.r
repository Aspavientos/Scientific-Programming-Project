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

data$Start.date = as.Date(data$Start.date)

# Load custom functions
source('./R code/customfunctions.R')

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
                   'Order',
                   'Start.date',
                   'State.Province')

lapply(interestgroups, createNewGroup)

rm()

## Boxplots
# Mean observations per day
daily_peryear = ggplot(data_groupings$Start.date, aes(x = Start.date %>% format('%Y'),
                                                      y = .rows %>% sapply(length))) +
  geom_boxplot(fill = custom_colors$year$fills) +
  labs(x = 'Year',
       y  = 'Observations per day') +
  ggtitle('Number of daily observations, over the years') +
  theme(plot.title = element_text(hjust = 0.5))

daily_permonth = ggplot(data_groupings$Start.date, aes(x = Start.date %>% format('%b'),
                                                       y = .rows %>% sapply(length))) +
  geom_boxplot(fill = custom_colors$month$fills) +
  scale_x_discrete(limits = custom_colors$month$months) +
  labs(x = 'Month',
       y  = 'Observations per day') +
  ggtitle('Number of daily observations, over the months') +
  theme(plot.title = element_text(hjust = 0.5))


# Mean observations per order
daily_perorder = ggplot(data_groupings$Order, aes(x = Order,
                                                  y = .rows %>% sapply(length))) +
  geom_boxplot()

