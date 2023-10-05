province_colors = data.frame(provinces = levels(data$State.Province),
                             fills = c('#FFFFFF', '#CF142B', '#5E89C2', '#005EB8', '#00B140', '#FF4F00'),
                             borders = c('#CE1124', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FF4F00'))

month_colors = data.frame(months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         fills = c('#fafa6e','#cdef72','#a4e27a','#7dd382','#58c389','#35b28e','#0ea18f','#008f8c','#007d85','#146b79','#23596a','#2a4858'))

customggsave = function(plot){
  ggsave(paste0(deparse(substitute(plot)),".png"),
         plot = plot,
         device = 'png',
         width = 1920,
         height = 1080,
         units = 'px',
         path = plotpath)
}