# Supplementary functions
# Author: Diego Rodríguez Esperante
# Date of creation: 05/10/2023
# Last edited: 10/10/2023

# Packages ----
if(!require('grDevices', quietly = TRUE)){
  install.packages('grDevices')
}if(!require('dplyr', quietly = TRUE)){
  install.packages('dplyr')
}if(!require('reshape2', quietly = TRUE)){
  install.packages('reshape2')
}

require(grDevices)
require(dplyr)
require(reshape2)

Sys.setlocale("LC_TIME", "English")

# Colors ----
## Lengths ----
getlength = function(column){
  if (class(column) == 'Date'){
    leng = c(column %>% unique %>% length,
             column %>% format('%a') %>% unique %>% length,
             column %>% format('%b') %>% unique %>% length,
             column %>% format('%d') %>% unique %>% length,
             column %>% format('%Y') %>% unique %>% length)
  }else{
    leng = length(unique(column))
  }
  return(leng)
}

lengthlist = lapply(data, getlength)

names(lengthlist$Start.date) = c('FullDate',
                                 'Weekday',
                                 'Month',
                                 'MonthDay',
                                 'Year')

## Define colors ----
province_colors = data.frame(provinces = c('England', 'Isle of Man', 'Northern Ireland', 'Scotland', 'Wales', ''),
                             names = c('England', 'Isle of Man', 'Northern Ireland', 'Scotland', 'Wales', 'No data'),
                             fills = c('#FFFFFF', '#CF142B', '#5E89C2', '#005EB8', '#00B140', '#FF4F00'),
                             borders = c('#CE1124', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FF4F00'))

month_colors = data.frame(months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         fills = colorRampPalette(c('#AAB34C', '#4CAAB3', '#B34CAA'))(lengthlist$Start.date[3]))

season_colors = data.frame(seasons = c('Winter', 'Spring', 'Summer', 'Fall'),
                           fills = c('lightskyblue', 'olivedrab', 'plum', 'orange2'))

year_colors = data.frame(years = 2010:2023,
                         fills = colorRampPalette(c('#8E37C8','#C88E37', '#37C88E'))(length(2010:2023)))

order_colors = data.frame(orders = levels(data$Order),
                          fills = colorRampPalette(c('#455ABA','#BA455A', '#5ABA45'))(lengthlist$Order))

family_colors = data.frame(families = levels(data$Family),
                           fills = colorRampPalette(c('#BC6943','#43BC69', '#6943BC'))(lengthlist$Family))

custom_colors = list(province = province_colors,
                     month = month_colors,
                     year = year_colors,
                     order = order_colors,
                     family = family_colors,
                     seasons = season_colors)


# Groupings ----
## Create groupings ----
createNewGroup = function(group, dataset = data){
  new_grouping = dataset %>% group_by(.data[[group]])
  # vignette('programming')
  new_grouping_attributes = (new_grouping %>% attributes)$groups
  
  grouping_list = list(new_grouping_attributes)
  names(grouping_list) = group
  
  return(grouping_list)
}

## Melt groupings ----
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

# Custom ggsave ----
customggsave = function(plot, upscale=1.5, save_path = ''){
  save_path = paste0('./Plots', save_path)
  ggsave(paste0(deparse(substitute(plot)),".png"),
         plot = plot,
         device = 'png',
         width = round(1920*upscale),
         height = round(1080*upscale),
         units = 'px',
         path = save_path)
}

# Find seasons ----
# Credit: https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
getSeason <- function(DATES, date_format = "%Y-%m-%d") {
  WS <- as.Date("2012-12-15", format = date_format) # Winter Solstice
  SE <- as.Date("2012-3-15",  format = date_format) # Spring Equinox
  SS <- as.Date("2012-6-15",  format = date_format) # Summer Solstice
  FE <- as.Date("2012-9-15",  format = date_format) # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

# Remove extraneous ----
rm(province_colors,
   month_colors,
   year_colors,
   order_colors,
   family_colors)

rm(lengthlist, getlength)