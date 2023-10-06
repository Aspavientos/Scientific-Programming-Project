
# Colors
province_colors = data.frame(provinces = c('England', 'Isle of Man', 'Northern Ireland', 'Scotland', 'Wales', 'No data'),
                             fills = c('#FFFFFF', '#CF142B', '#5E89C2', '#005EB8', '#00B140', '#FF4F00'),
                             borders = c('#CE1124', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FF4F00'))

month_colors = data.frame(months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         fills = c('#fafa6e','#cdef72','#a4e27a','#7dd382','#58c389','#35b28e','#0ea18f','#008f8c','#007d85','#146b79','#23596a','#2a4858'))

year_colors = data.frame(years = 2010:2023,
                         fills = c('#d667ff','#ad80ff','#7295ff','#00a8ff','#00b7ff','#00c3ff','#00ceff','#00d7ff','#00deff','#00e4f2','#00e9d9','#00edc1','#22f0ab','#73f297'))
#custom_colors = list(provinces = province_colors,
#                     months = month_colors)

# Paths


customggsave = function(plot){
  ggsave(paste0(deparse(substitute(plot)),".png"),
         plot = plot,
         device = 'png',
         width = 1920,
         height = 1080,
         units = 'px',
         path = './Plots')
}

