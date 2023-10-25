# Scrapped code
# Localized weekly diversity ----
week_list = data_species$Start.date %>% format('%Y/%W') %>% unique %>% sort

all_weeks = array(rep(NA, length(week_list)), dimnames = list(week_list))

weekly_local_diver = array(dim = c(length(lat_points),
                                   length(lon_points),
                                   length(week_list)),
                           dimnames = list(lat_points,
                                           lon_points,
                                           week_list))

for (i in 1:nrow(coord_pairs)){
  loc_diver = calcLocalDiversity(data_species,
                                 coord_pairs[i,],
                                 dateFormat = '%Y/%W')
  
  loc_diverdf = data.frame(loc_diver$Diversity,
                           row.names = loc_diver$Dates)
  
  weekly_local_diver[as.character(coord_pairs[i,1]), as.character(coord_pairs[i,2]),] = 
    merge(all_weeks, loc_diverdf, by = 'row.names', all = T)[,3]
  
  disp(paste0('Coordinates: ', coord_pairs[i,1], ', ', coord_pairs[i,2]))
  
  rm(loc_diver, loc_diverdf)
}
rm(all_weeks, week_list)

weekly_local_diverdf = melt(weekly_local_diver)
colnames(weekly_local_diverdf) = c('Latitude', 'Longitude', 'Year/Week', 'Diversity')

# Localized monthly diversity ----
month_list = data_species$Start.date %>% format('%Y/%m') %>% unique %>% sort

all_months = array(rep(NA, length(month_list)), dimnames = list(month_list))

monthly_local_diver = array(dim = c(length(lat_points),
                                    length(lon_points),
                                    length(month_list)),
                            dimnames = list(lat_points,
                                            lon_points,
                                            month_list))

for (i in 5744:nrow(coord_pairs)){
  loc_diver = calcLocalDiversity(data_species,
                                 coord_pairs[i,],
                                 dateFormat = '%Y/%m')
  
  loc_diverdf = data.frame(loc_diver$Diversity,
                           row.names = loc_diver$Dates)
  
  monthly_local_diver[as.character(coord_pairs[i,1]), as.character(coord_pairs[i,2]),] = 
    merge(all_months, loc_diverdf, by = 'row.names', all = T)[,3]
  
  print(paste0('Coordinates: ', coord_pairs[i,1], ', ', coord_pairs[i,2]))
  
  rm(loc_diver, loc_diverdf)
}
rm(all_months, month_list)

monthly_local_diverdf = melt(monthly_local_diver)
colnames(monthly_local_diverdf) = c('Latitude', 'Longitude', 'Year/Month', 'Diversity')