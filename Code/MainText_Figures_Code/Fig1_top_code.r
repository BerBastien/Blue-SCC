## Load Geospatial Data 
    # Corals 
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals\\geospatial_coral_polygons.rdat") 
    # Mangroves
    geospatial_mangroves_polygons <- st_read("Data\\input_modules\\mangroves\\gmw\\gmw_v3_2020_vec.shp")
    # Ports
    geospatial_ports_polygons <- st_read("Data\\input_modules\\ports\\nodes_maritime.gpkg")
    # Fisheries
    geospatial_fish_raster <- readRDS("Data\\input_modules\\fish\\species_richness_yr0.Rds")
    eez_boundaries <- st_read("Data/other/eez_boundaries_v11.gpkg")

    crs_map_corals <- st_crs(geospatial_coral_polygons)
    crs_map_mangroves <- st_crs(geospatial_mangroves_polygons)
    crs_map_ports <- st_crs(geospatial_ports_polygons)
    
    crs_map_corals == crs_map_mangroves
    crs_map_ports == crs_map_corals
    crs_map_mangroves == crs_map_ports

    geospatial_coral_polygons <- st_transform(geospatial_coral_polygons, crs = crs_map_mangroves)

    
    new_crs <- "EPSG:4326"
    # Reproject the raster data to WGS84
    library("terra")
    glimpse(geospatial_fish_raster)
    class(geospatial_fish_raster)
    geospatial_fish_raster <- as.data.frame(geospatial_fish_raster)
    geospatial_fish_raster2 <- terra::rast(geospatial_fish_raster, type="xyz",crs=new_crs)
    crs_map_corals == st_crs(geospatial_fish_raster2)
    fish_raster_df <- as.data.frame(geospatial_fish_raster2, xy = TRUE)

    glimpse(geospatial_fish_raster2)
    
    # ggplot()+ 
    #     #geom_sf(data = geospatial_fish_raster_sf, aes(color = nspp)) +
    #     geom_tile(data = fish_raster_df, aes(x = x, y = y, fill = nspp))+
    #     geom_sf(data = geospatial_coral_polygons%>% filter(id %in% c(1:100)), fill = col_cor,color=col_cor)+
    #     geom_sf(data = geospatial_mangroves_polygons[c(1:1000),],fill=col_man,color=col_man)+

        
    #     scale_color_manual(name = "Features",
    #                  values = c(col_cor, col_man, col_por),
    #                  breaks = c("Corals", "Mangroves", "Ports"),
    #                  labels = c("Corals", "Mangroves", "Ports")) +

    #     geom_sf(data = geospatial_ports_polygons %>% filter(infra=="port"),size=0.5,color=col_por)+
    #     scale_fill_gradient(low = col_fish_low, high = col_fish_high) +
    #     #scale_fill_viridis_c() + # 
    #     theme_minimal() +
    #     guides(fill = guide_colorbar(title = "Fish Species"),
    #      color = guide_legend(title = "Features", override.aes = list(fill = c(col_cor, col_man, NA), color = c(col_cor, col_man, col_por))))
    
    # ggsave("Figures/AllData_Mapv1.png",dpi=300)
    

# Create a named vector for the colors used in the features
lighter_shades <- lighten(col_fish_low, amount = c(0.8,0.7,0.6,0.5))
feature_colors <- c("Corals" = col_cor, "Mangroves" = "#4fe34f", "Ports" = "#0aa3a3", "Fisheries" = col_fish_low, "1"=lighter_shades[1], "2"=lighter_shades[2],"3"=lighter_shades[3],"4"=lighter_shades[4])
glimpse(fish_raster_df)
table(fish_raster_df$nspp_quantile)
fish_raster_df <- fish_raster_df %>%
  mutate(nspp_quantile = ntile(nspp, 4))
world <- ne_countries(scale = "medium", returnclass = "sf")

# Reproject the SpatRaster to Robinson projection
geospatial_fish_raster2_robin <- project(geospatial_fish_raster2, "+proj=robin")
    
    
    eez_boundaries <- st_read("Data/other/eez_v11.gpkg")
    eez_boundaries_terra <- vect(eez_boundaries)

    # Crop the raster to the extent of the EEZ boundaries
    cropped_raster <- crop(geospatial_fish_raster2, eez_boundaries_terra)
    plot(cropped_raster)
  
    # Mask the raster using the EEZ boundaries
    
    masked_raster <- mask(cropped_raster, eez_boundaries_terra)
    masked_raster  <- project(masked_raster , "+proj=robin")
    plot(masked_raster)
    masked_raster_df <- as.data.frame(masked_raster, xy = TRUE, na.rm = TRUE)%>% mutate(nspp_quantile = ntile(nspp, 4))
    plot(masked_raster_df)

# Convert reprojected SpatRaster to data frame
fish_raster_df_robin <- as.data.frame(geospatial_fish_raster2_robin, xy = TRUE)
fish_raster_df_robin <- fish_raster_df_robin %>% mutate(nspp_quantile = ntile(nspp, 4))


ggplot() +
  # Plot raster data
  #geom_tile(data = fish_raster_df_robin, aes(x = x, y = y, fill = factor(nspp_quantile)), show.legend = TRUE) +
    geom_tile(data = masked_raster_df, aes(x = x, y = y, fill = factor(nspp_quantile)), show.legend = TRUE) +

  # Add EEZ boundaries
  #geom_sf(data = eez_boundaries, color="black", alpha=0.2,size = 0.5, show.legend = TRUE) +
  
  # Add black map of the continents
  geom_sf(data = world, fill = "black", color = NA, show.legend = FALSE) +

  # Plot coral polygons and assign them to the "Features" group
  geom_sf(data = geospatial_coral_polygons, # %>% filter(id %in% c(1:100)),
          aes(color = "Corals", fill = "Corals"), show.legend = TRUE) +
  
  # Plot mangroves polygons and assign them to the "Features" group
  geom_sf(data = geospatial_mangroves_polygons, #[c(1:1000),],
          aes(color = "Mangroves", fill = "Mangroves"), show.legend = TRUE) +
  
  # Plot ports polygons and assign them to the "Features" group
  geom_sf(data = geospatial_ports_polygons %>% filter(infra == "port"),
          aes(color = "Ports", fill="Ports"), size = 0.01, show.legend = TRUE) +
  
  # Grayscale fill gradient for fish species
  scale_fill_gradient(low = col_fish_low, high = col_fish_high, name = "Fish Species") +
  
  # Manually set color scale for the polygons
  scale_color_manual(name = "Features", values = feature_colors) +
  # Manually set color scale for the polygons
  scale_fill_manual(name = "Features", values = feature_colors) +
  
  theme_minimal() +
  
  # Combine legends for fill and color
  guides(fill = "none",
         color = "none") +
    coord_sf(crs = "+proj=robin") 

ggsave("Figures/Main/Panels/Fig1_top.png")



