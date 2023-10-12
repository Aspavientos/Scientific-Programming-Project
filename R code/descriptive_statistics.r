# Information ----
# Descriptive statistics
# Author: Diego Rodríguez Esperante
# Date of creation: 05/10/2023
# Last edited: 10/10/2023

# Loading  ----
## Load packacges ----
require(ggplot2)
require(dplyr)
require(ggpubr)
require(rstudioapi)
require(reshape2)

## Load data ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
data = read.csv("./Data/cleaned_data.csv", stringsAsFactors = TRUE)

data$Start.date = as.Date(data$Start.date)

## Load custom functions ----
source('./R code/customfunctions.R')

# Function definition ----
## Supplementary groupings ----
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
                   'Family',
                   'Start.date',
                   'State.Province')

lapply(interestgroups, createNewGroup)

rm(interestgroups)

## Melting to make ggplotable ----
meltGroup = function(grouping_list, data_column, date_format = NULL){
  listed = lapply(grouping_list$.rows, function(x){data_column[x]})
  
  names(listed) = grouping_list[[1]]
  
  dframed = data.frame(matrix(NA,
                              nrow = length(grouping_list[[1]]),
                              ncol = length(levels(data_column)) + 1))
  
  colnames(dframed) = c(colnames(grouping_list[1]), levels(data_column))
  
  if (is.null(date_format)){
    dframed[,1] = grouping_list[[1]]
  }else{
    dframed[,1] = grouping_list[[1]] %>% as.Date %>% format(date_format)
  }
  
  for (i in 1:nrow(dframed)){
    dframed[i,-1] = table(listed[[i]])
  }
  
  melted = melt(dframed, colnames(grouping_list[1]))
  
  if(!is.null(date_format)){
    melted_names = colnames(melted)
    melted = aggregate(melted[,3], list(melted[,1], melted[,2]), FUN = sum)
    colnames(melted) = melted_names
  }
  
  return(melted)
}


# Descriptive statistics ----
## Boxplots ----
### Observations per date ----
#### Daily observations per year ----
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

### Observations per order ----
#### Daily observations per order ----
melt_dayorder = meltGroup(data_groupings$Start.date, data$Order)

med_dayorder = aggregate(value ~ variable, data = melt_dayorder, median)
med_dayorder$value = round(med_dayorder$value, 2)

daily_perorder_nobounds = ggplot(melt_dayorder, aes(x = variable,
                                                     y = value)) +
  geom_boxplot(fill = custom_colors$order$fills) +
  labs(x = 'Order',
       y = 'Daily sightings') +
  ggtitle('Number of daily observations per Order',
          subtitle = paste0('All observations')) +
  scale_x_discrete(limits = custom_colors$order$orders) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


daily_perorder = ggplot(melt_dayorder, aes(x = variable,
                                  y = value)) +
  geom_boxplot(fill = custom_colors$order$fills) +
  geom_label(data = med_dayorder,
            aes(label = value, y = value)) +
  labs(x = 'Order',
       y = 'Daily sightings') +
  ggtitle('Number of daily observations per Order',
          subtitle = paste0('Observations out of bounds: ',
                            sum(melt_dayorder$value>50), ' (',
                            signif(sum(melt_dayorder$value>50)/length(melt_dayorder$value), 2), '%)')) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limits = custom_colors$order$orders) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Monthly observations per order ----
melt_monthorder = meltGroup(data_groupings$Start.date, data$Order, date_format = '%b')

med_monthorder = aggregate(value ~ variable, data = melt_monthorder, median)
med_monthorder$value = round(med_monthorder$value, 2)

monthly_perorder = ggplot(melt_monthorder, aes(x = variable,
                                               y = value)) +
  geom_boxplot(fill = custom_colors$order$fills) +
  geom_label(data = med_monthorder,
             aes(label = value, y = value)) +
  labs(x = 'Order',
       y = 'Monthly sightings') +
  ggtitle('Number of monthly observations per Order',
          subtitle = paste0('Observations out of bounds: ',
                            sum(melt_monthorder$value>7000), ' (',
                            signif(sum(melt_monthorder$value>7000)/length(melt_monthorder$value), 2), '%)')) +
  scale_y_continuous(limits = c(0, 7000)) +
  scale_x_discrete(limits = custom_colors$order$orders) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Observations per family ----
#### Daily observations per family ----
melt_dayfamily = meltGroup(data_groupings$Start.date, data$Family)

med_dayfamily = aggregate(value ~ variable, data = melt_dayfamily, median)
med_dayfamily$value = round(med_dayfamily$value, 2)

daily_perfamily = ggplot(melt_dayfamily, aes(x = variable,
                                           y = value)) +
  geom_boxplot(fill = custom_colors$family$fills) +
  geom_label(data = med_dayfamily,
             aes(label = value, y = value),
             size = 3) +
  labs(x = 'Order',
       y = 'Daily sightings') +
  ggtitle('Number of daily observations per Order',
          subtitle = paste0('Observations out of bounds: ',
                            sum(melt_dayfamily$value>50), ' (',
                            signif(sum(melt_dayfamily$value>50)/sum(melt_dayfamily$value), 2), '%)')) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limits = custom_colors$family$families) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Monthly observations per family ----
melt_monthfamily = meltGroup(data_groupings$Start.date, data$Family, date_format = '%b')

med_monthfamily = aggregate(value ~ variable, data = melt_monthfamily, median)
med_monthfamily$value = round(med_monthfamily$value, 2)

monthly_perfamily = ggplot(melt_monthfamily, aes(x = variable,
                                               y = value)) +
  geom_boxplot(fill = custom_colors$family$fills) +
  geom_label(data = med_monthfamily,
             aes(label = value, y = value),
             size = 3) +
  labs(x = 'Family',
       y = 'Monthly sightings') +
  ggtitle('Number of monthly observations per Family',
          subtitle = paste0('Observations out of bounds: ',
                            sum(melt_monthfamily$value>7000), ' (',
                            signif(sum(melt_monthfamily$value>7000)/length(melt_monthfamily$value), 2), '%)')) +
  scale_y_continuous(limits = c(0, 7000)) +
  scale_x_discrete(limits = custom_colors$family$families) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
