# Information ----
# Correlation
# Author: Diego Rodríguez Esperante
# Date of creation: 15/10/2023
# Last edited: 25/10/2023

# Loading ----
## Loading packages ----
require(ggplot2)
require(dplyr)
require(ggpubr)
require(rstudioapi)

## Load data ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
data = read.csv("./Data/cleaned_data.csv", stringsAsFactors = TRUE)

data$Start.date = as.Date(data$Start.date)

weekly_data = read.csv('./Data/Weekly diversity.csv')

## Load custom functions ----
source('./R code/customfunctions.R')

## Create groupings ----
data_groupings = createNewGroup('Start.date')

weekly_counts = meltGroup(data_groupings$Start.date, data$Order, date_format = '%Y/%W')

weekly_data$Count = aggregate(weekly_counts$value, by = list(weekly_counts$Start.date), FUN = sum)[,2]

rm(weekly_counts)
# Illustrative plottings ----
weekly_countsNdiversity = ggplot(weekly_data, aes(fill = Season, color = Season)) +
  geom_point(aes(x = Dates, y = max(Count)/max(Diversity)*Diversity)) +
  geom_col(aes(x = Dates, y = Count)) +
  labs(x = 'Week', y = 'Sightings') +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(weekly_data$Diversity)/max(weekly_data$Count)), name = 'Diversity')) +
  scale_x_discrete(breaks = ISOdate(2010:2023, 1, 1) %>% format('%Y/%W'),
                   labels = (2010:2023)) +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  scale_fill_manual(limits = custom_colors$seasons$seasons,
                    values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings across the years') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

weekly_countsNdiversity_end = ggplot(weekly_data[-(1:213),], aes(fill = Season, color = Season)) +
  geom_point(aes(x = Dates, y = max(Count)/max(Diversity)*Diversity)) +
  geom_col(aes(x = Dates, y = Count)) +
  labs(x = 'Week', y = 'Sightings') +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(weekly_data$Diversity)/max(weekly_data$Count)), name = 'Diversity')) +
  scale_x_discrete(breaks = ISOdate(2010:2023, 1, 1) %>% format('%Y/%W'),
                   labels = (2010:2023)) +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                    values = custom_colors$seasons$fills) +
  scale_fill_manual(limits = custom_colors$seasons$seasons,
                    values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings after 2014') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

weekly_countsVdiversity_beginning = ggplot(weekly_data[1:213,], aes(x = Count, y = Diversity)) +
  geom_point(aes(color = Season)) +
  geom_point(data = subset(weekly_data, Dates == '2013/21'),
             pch = 21,
             size = 4,
             colour = "purple") +
  geom_text(data = subset(weekly_data, Dates == '2013/21'),
            aes(x = Count, y = Diversity,
                label = 'Selective \n enthusiasts'),
            nudge_y = -0.1, nudge_x = -50,
            color = 'purple') +
  labs(x = 'Sightings', y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings before 2014') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

weekly_countsVdiversity_beginningoutlier = ggplot(weekly_data[c(1:181, 183:213),], aes(x = Count, y = Diversity)) +
  geom_point(aes(color = Season)) +
  labs(x = 'Sightings', y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings before 2014 (no enthusiasts)') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

weekly_countsVlogdiversity_beginning = ggplot(weekly_data[c(1:181, 183:213),], aes(x = log(Count), y = Diversity)) +
  geom_point(aes(color = Season)) +
  stat_smooth(method = "lm", formula = y ~ log(x)) +
  stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")),
                        formula = y ~ log(x),
                        geom = 'label') +
  labs(x = 'log(Sightings)', y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings before 2014') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

weekly_countsVlogdiversity_end = ggplot(weekly_data[-(1:213),], aes(x = log(Count), y = Diversity)) +
  geom_point(aes(color = Season)) +
  geom_point(data = subset(weekly_data, Dates == '2023/15'),
             pch = 21,
             size = 4,
             colour = "purple") +
  geom_text(data = subset(weekly_data, Dates == '2023/15'),
             aes(x = log(Count), y = Diversity,
                 label = 'Last day'),
             nudge_y = 0.1, nudge_x = 0.2,
            color = 'purple') +
  stat_smooth(method = "lm", formula = y ~ log(x)) +
  stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")),
                        formula = y ~ log(x),
                        geom = 'label') +
  labs(x = 'log(Sightings)', y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings after 2014') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

weekly_countsVlogdiversity_endnolast = ggplot(weekly_data[-c(1:213, nrow(weekly_data)),], aes(x = log(Count), y = Diversity)) +
  geom_point(aes(color = Season)) +
  stat_smooth(method = "lm", formula = y ~ log(x)) +
  stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")),
                        formula = y ~ log(x),
                        geom = 'label') +
  labs(x = 'log(Sightings)', y = 'Diversity') +
  scale_color_manual(limits = custom_colors$seasons$seasons,
                     values = custom_colors$seasons$fills) +
  ggtitle('Weekly diversity measures and sightings after 2014 (no last)') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

# Correlation calculation ----
cor(weekly_data$Diversity[c(1:181, 183:213)], weekly_data$Count[c(1:181, 183:213)], method = 'spearman')

cor(weekly_data$Diversity[-c(1:213, nrow(weekly_data))], weekly_data$Count[-c(1:213, nrow(weekly_data))], method = 'spearman')

# Save data ----
plot_path = '/Correlations'
customggsave(weekly_countsNdiversity, save_path = plot_path)
customggsave(weekly_countsNdiversity_end, save_path = plot_path)
customggsave(weekly_countsVdiversity_beginning, save_path = plot_path)
customggsave(weekly_countsVdiversity_beginningoutlier, save_path = plot_path)
customggsave(weekly_countsVlogdiversity_beginning, save_path = plot_path)
customggsave(weekly_countsVlogdiversity_end, save_path = plot_path)
customggsave(weekly_countsVlogdiversity_endnolast, save_path = plot_path)
