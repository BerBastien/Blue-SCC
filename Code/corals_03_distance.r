# Claculate distance between each coral area and each country
coral_areas_keys_single_ISO <- st_read("Data/corals/coral_area_with_mean_t2coeff_covariates.shp")
filtered_sf_coral <- sf_coral[st_dimension(sf_coral) == 2, ] #out
filtered_sf_coral <- filtered_sf_coral[-c(932),]
fff <- filtered_sf_coral
iso_distance <- read.csv("iso_distance.csv")
iso_distance_xy <- read.csv("iso_distance.csv")
fff$id
glimpse(fff[,c(1:24)])
#st_write(fff[,c(1:24)],"Data/corals/coral_area_with_mean_t2coeff_filtered.shp")


df_list <- lapply(4:length(iso_distance), function(j) {
  iso_j <- substr(names(iso_distance)[j], 9, 11)
  print(paste(j, iso_j, "beginning"))

  iso_distance_xy_1 <- iso_distance_xy[, c(2, 3, j)]
  dist_raster <- rasterFromXYZ(iso_distance_xy_1)
  
  dist_raster_extended <- raster(new_extent, res = res(dist_raster))
  for (i in -repeat_count:(repeat_count)) {
    dist_raster_shifted <- raster::shift(dist_raster, dx = i * (nmax - nmin))
    dist_raster_extended <- merge(dist_raster_extended, dist_raster_shifted)
  }
  
  print(paste(j, "just did the raster extension"))
  
  dist_spat_raster <- rast(dist_raster_extended)
  
  dist_touching <- t(sapply(seq_len(nrow(fff)), function(i) {
    colMeans(raster::extract(dist_spat_raster, fff[i,], method = "simple"), na.rm = TRUE)[2]
  }))
  
  print(paste(j, "just calculated the distance"))
  
  df_corals <- data.frame(iso = substr(names(iso_distance)[j], 9, 11),
                           distance = dist_touching[1, ], id = fff$id)
  write.csv(df_corals, paste0("Data/corals/distance_", j, "_", iso_j, ".csv"))
  
  return(df_corals)
})

distance_corals <- do.call(rbind, df_list)
#write.csv(distance_corals,"Data/corals/distance_corals.csv")
glimpse(distance_corals)
min(distance_corals$distance)
#glimpse(df_list)



