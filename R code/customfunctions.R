Sys.setlocale("LC_TIME", "English")

# lengths

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

# Colors
province_colors = data.frame(provinces = c('England', 'Isle of Man', 'Northern Ireland', 'Scotland', 'Wales', ''),
                             names = c('England', 'Isle of Man', 'Northern Ireland', 'Scotland', 'Wales', 'No data'),
                             fills = c('#FFFFFF', '#CF142B', '#5E89C2', '#005EB8', '#00B140', '#FF4F00'),
                             borders = c('#CE1124', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FF4F00'))


month_colors = data.frame(months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         fills = colorRampPalette(c('#E4FF00', '#00E4FF', '#FF00E4'))(lengthlist$Start.date[3]))



year_colors = data.frame(years = 2010:2023,
                         fills = colorRampPalette(c('#6500FF','#FF6500', '#00FF65'))(length(2010:2023)))

order_colors = data.frame(orders = levels(data$Order),
                          fills = colorRampPalette(c('#0F00FF','#FF0F00', '#00FF0F'))(lengthlist$Order))

custom_colors = list(province = province_colors,
                     month = month_colors,
                     year = year_colors,
                     order = order_colors)


customggsave = function(plot){
  ggsave(paste0(deparse(substitute(plot)),".png"),
         plot = plot,
         device = 'png',
         width = 1920,
         height = 1080,
         units = 'px',
         path = './Plots')
}

# remove extraneous
rm(province_colors,
   month_colors,
   year_colors,
   order_colors)

rm(lengthlist, getlength)