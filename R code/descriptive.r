# Information ----
# Descriptive statistics
# Author: Diego Rodríguez Esperante
# Date of creation: 05/10/2023
# Last edited: 15/10/2023

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

## Create groupings ----
data_groupings = list()

# createNewGroup defined in customfunctions.R

interestgroups = c('Scientific.name',
                   'Order',
                   'Family',
                   'Start.date',
                   'State.Province')

for (i in 1:length(interestgroups)){
  data_groupings = append(data_groupings, createNewGroup(interestgroups[i]))
}

rm(interestgroups)

# Descriptive statistics ----
## Boxplots ----
### Observations per date ----
#### Daily observations per year ----
daily_peryear_nobounds = ggplot(data_groupings$Start.date, aes(x = Start.date %>% format('%Y'),
                                                               y = .rows %>% sapply(length))) +
  geom_boxplot(fill = custom_colors$year$fills) +
  labs(x = 'Year',
       y  = 'Observations per day') +
  ggtitle('Number of daily observations, over the years',
          subtitle = paste0('All observations')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

melt_yeardaily = data.frame(Start.date = data_groupings$Start.date$Start.date %>% format('%Y'),
                             Count = data_groupings$Start.date$.rows %>% sapply(length))

med_yeardaily = aggregate(Count ~ Start.date, data = melt_yeardaily, median)
med_yeardaily$Start.date = ISOdate(2010:2023, 1,1)

daily_peryear = ggplot(data_groupings$Start.date, aes(x = Start.date %>% format('%Y'),
                                                      y = .rows %>% sapply(length))) +
  geom_boxplot(fill = custom_colors$year$fills) +
  geom_label(data = med_yeardaily,
             aes(label = Count, y = Count)) +
  scale_y_continuous(limits = c(0,200)) +
  labs(x = 'Year',
       y  = 'Observations per day') +
  ggtitle('Number of daily observations, over the years',
          subtitle = paste0('Observations out of bounds: ',
                            sum((data_groupings$Start.date$.rows %>% sapply(length))>200), ' (',
                            signif(sum((data_groupings$Start.date$.rows %>% sapply(length))>200)/sum(data_groupings$Start.date$.rows %>% sapply(length)), 2), '%)')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

rm(melt_yeardaily, med_yeardaily)

#### Daily observations per month ----
daily_permonth_nobounds = ggplot(data_groupings$Start.date, aes(x = Start.date %>% format('%b'),
                                                       y = .rows %>% sapply(length))) +
  geom_boxplot(fill = custom_colors$month$fills) +
  scale_x_discrete(limits = custom_colors$month$months) +
  labs(x = 'Month',
       y  = 'Observations per day') +
  ggtitle('Number of daily observations, over the months',
          subtitle = paste0('All observations')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

melt_monthdaily = data.frame(Start.date = data_groupings$Start.date$Start.date %>% format('%b'),
                             Count = data_groupings$Start.date$.rows %>% sapply(length))

med_monthdaily = aggregate(Count ~ Start.date, data = melt_monthdaily, median)
med_monthdaily = med_monthdaily[match(custom_colors$month$months, med_monthdaily$Start.date),]
med_monthdaily$Start.date = ISOdate(2010, 1:12,1)

daily_permonth = ggplot(data_groupings$Start.date, aes(x = Start.date %>% format('%b'),
                                                       y = .rows %>% sapply(length))) +
  geom_boxplot(fill = custom_colors$month$fills) +
  geom_label(data = med_monthdaily,
             aes(label = Count, y = Count)) +
  scale_x_discrete(limits = custom_colors$month$months) +
  scale_y_continuous(limits = c(0,200)) +
  labs(x = 'Month',
       y  = 'Observations per day') +
  ggtitle('Number of daily observations, over the months',
          subtitle = paste0('Observations out of bounds: ',
                            sum((data_groupings$Start.date$.rows %>% sapply(length))>200), ' (',
                            signif(sum((data_groupings$Start.date$.rows %>% sapply(length))>200)/sum(data_groupings$Start.date$.rows %>% sapply(length)), 2), '%)')) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

rm(melt_monthdaily, med_monthdaily)

### Observations per order ----
#### Daily observations per order ----
# meltGroup defined in customfunctions.R
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

rm(melt_dayorder, med_dayorder)

#### Weekly observations per order ----
melt_weekorder = meltGroup(data_groupings$Start.date, data$Order, date_format = '%W')

med_weekorder = aggregate(value ~ variable, data = melt_weekorder, median)
med_weekorder$value = round(med_weekorder$value, 2)

weekly_perorder = ggplot(melt_weekorder, aes(x = variable,
                                               y = value)) +
  geom_boxplot(fill = custom_colors$order$fills) +
  geom_label(data = med_weekorder,
             aes(label = value, y = value)) +
  labs(x = 'Order',
       y = 'Weekly sightings') +
  ggtitle('Number of weekly observations per Order',
          subtitle = paste0('Observations out of bounds: ',
                            sum(melt_weekorder$value>7000), ' (',
                            signif(sum(melt_weekorder$value>7000)/length(melt_weekorder$value), 2), '%)')) +
  scale_x_discrete(limits = custom_colors$order$orders) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

rm(melt_weekorder, med_weekorder)

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

rm(melt_monthorder, med_monthorder)

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

rm(melt_dayfamily, med_dayfamily)

#### Weekly observations per family ----
melt_weekfamily = meltGroup(data_groupings$Start.date, data$Family, date_format = '%W')

med_weekfamily = aggregate(value ~ variable, data = melt_weekfamily, median)
med_weekfamily$value = round(med_weekfamily$value, 2)

weekly_perfamily = ggplot(melt_weekfamily, aes(x = variable,
                                                 y = value)) +
  geom_boxplot(fill = custom_colors$family$fills) +
  geom_label(data = med_weekfamily,
             aes(label = value, y = value),
             size = 3) +
  labs(x = 'Family',
       y = 'Weekly sightings') +
  ggtitle('Number of weekly observations per Family',
          subtitle = paste0('Observations out of bounds: ',
                            sum(melt_weekfamily$value>7000), ' (',
                            signif(sum(melt_weekfamily$value>7000)/length(melt_weekfamily$value), 2), '%)')) +
  scale_x_discrete(limits = custom_colors$family$families) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

rm(melt_weekfamily, med_weekfamily)

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

rm(melt_monthfamily, med_monthfamily)

# Save all plots ----
plot_path = '/Descriptive'
customggsave(daily_peryear_nobounds, save_path = plot_path)
customggsave(daily_peryear, save_path = plot_path)

customggsave(daily_permonth_nobounds, save_path = plot_path)
customggsave(daily_permonth, save_path = plot_path)

customggsave(daily_perorder_nobounds, upscale = 1.5, save_path = plot_path)
customggsave(daily_perorder, upscale = 1.5, save_path = plot_path)
customggsave(weekly_perorder, upscale = 1.5, save_path = plot_path)
customggsave(monthly_perorder, upscale = 1.5, save_path = plot_path)

customggsave(daily_perfamily, upscale = 1.5, save_path = plot_path)
customggsave(weekly_perorder, upscale = 1.5, save_path = plot_path)
customggsave(monthly_perfamily, upscale = 1.5, save_path = plot_path)

rm(plot_path)