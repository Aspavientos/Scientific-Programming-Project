# Introduction
This programming project is based on the National Mammal Atlas project, originally retreived through [Kaggle](https://www.kaggle.com/datasets/scarfsman/data-resource-national-mammal-atlas-project) in September 2023. The original provenance for this dataset can be found [here](https://doi.org/10.15468/i2eosa).

# Use
## List of files and their usage
For appropriate functioning of the code, the files should be run in this order. That being the case, the only true dependencies are that [`diagnostics.R`](Rcode/diagnostics.r) should be run before anything, and [`diversity.R`](Rcode/diversity.R) should be run before [`correlation.R`](Rcode/correlation.R).
### [`diagnostics.R`](Rcode/diagnostics.r)

### [`descriptive.R`](Rcode/descriptive.r)

### [`diversity.R`](Rcode/diversity.R)

### [`correlation.R`](Rcode/correlation.R)

### [`customfunctions.R`](Rcode/customfunctions.R)

# List of packages
All packages required in each file are called using `require`, which means that once you open them in RStudio a prompt should appear to install all that are missing. Just in case, in order to guarantee that you have all required packages up to date, you can run this snippet:

```
install.packages(c('ggplot2', 'dplyr', 'ggpubr', 'rstudioapi', 'vegan', 'grDevices', 'ggdendro', 'reshape2', 'maps'))
```
