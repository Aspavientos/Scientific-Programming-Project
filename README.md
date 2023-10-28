# Introduction and provenance
This programming project is based on the National Mammal Atlas project, originally retreived through [Kaggle](https://www.kaggle.com/datasets/scarfsman/data-resource-national-mammal-atlas-project) in September 2023. The original provenance for this dataset can be found [here](https://doi.org/10.15468/i2eosa).

# List of files and their usage
For appropriate functioning of the code, the files should be run in this order. That being the case, the only true dependencies are that [`diagnostics.R`](Rcode/diagnostics.r) should be run before anything, and [`diversity.R`](Rcode/diversity.R) should be run before [`correlation.R`](Rcode/correlation.R). All files create plots that will be stored in a Plots folder in the working directory.

It is recommended you use RStudio. If you aren't, please remember to set your working directory to wherever these files are kept.

## [`diagnostics.R`](Rcode/diagnostics.r)
This file creates histograms of all relevant files, cleans up irrelevant variables, and removes samples that fail certain checks. The cleaned data is stored back into [`cleaned_data.csv`](Data/cleaned_data.csv)

## [`descriptive.R`](Rcode/descriptive.r)
This file creates box plots of daily, weekly, and monthly sightings per month, year, and Order. These statistics inform later decisions on time and taxonomic resultion. From here on, weekly sightings and order sightings are the standard resolution.

## [`diversity.R`](Rcode/diversity.R)
This file calculates and plots diversity scores according to the Shannon-Wiener index, and plots this evolution across the weeks, colored by the season of the year. It also creates violin plots that show the distribution of diversity depending on the season.

## [`correlation.R`](Rcode/correlation.R)
This file calculates several correlations: between diversity and amount of sightings, between sightings of different orders, and between the sightings of each order and the diversity scores. Scatter and bar plots, heatmaps, and dendograms are drawn, as well as a logarithmic regression between sightings and diversity score.

## [`customfunctions.R`](Rcode/customfunctions.R)
This file is never meant to be run by itself, but it is called in all other scripts. It contains useful functions for plotting, coloring, and calculating different metrics.

# List of packages
All packages required in each file are called using `require`, which means that once you open them in RStudio a prompt should appear to install any that are missing. Just in case, in order to guarantee that you have all required packages up to date, you can run this snippet:

```
install.packages(c('ggplot2', 'dplyr', 'ggpubr', 'rstudioapi', 'vegan', 'grDevices', 'ggdendro', 'reshape2', 'maps'))
```
