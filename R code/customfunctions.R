# Supplementary functions
# Author: Diego Rodríguez Esperante
# Date of creation: 05/10/2023
# Last edited: 10/10/2023

# Packages ----
require(grDevices)

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
                     family = family_colors)


# Custom ggsave ----
customggsave = function(plot){
  ggsave(paste0(deparse(substitute(plot)),".png"),
         plot = plot,
         device = 'png',
         width = 1920,
         height = 1080,
         units = 'px',
         path = './Plots')
}

# Remove extraneous ----
rm(province_colors,
   month_colors,
   year_colors,
   order_colors,
   family_colors)

rm(lengthlist, getlength)