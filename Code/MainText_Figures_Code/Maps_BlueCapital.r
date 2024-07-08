## Load Geospatial Data 
    # Corals
    load(geospatial_coral_polygons, file="Data/output_modules_input_rice50x/output_modules/corals/geospatial_coral_polygons.rdat") 
    # Mangroves
    geospatial_mangroves_polygons <- st_read("Data\\input_modules\\mangroves\\gmw\\gmw_v3_2020_vec.shp")
    # Ports
    geospatial_ports_polygons <- st_read("Data\\input_modules\\ports\\nodes_maritime.gpkg")
    # Fisheries
    geospatial_fish_raster <- readRDS("Data\\input_modules\\fish\\species_richness_yr0.Rds")

    glimpse(geospatial_mangroves_polygons)
    glimpse(geospatial_coral_polygons)
    glimpse(geospatial_ports_polygons) # infra = 'port'
    glimpse(geospatial_fish_raster)
    
    ggplot()+
        geom_sf(data = geospatial_coral_polygons%>% filter(id %in% c(1:10)), fill = "red",color="red")+
        geom_sf(data = geospatial_mangroves_polygons[c(1:10),] , fill="blue",color="blue") +
        geom_sf(data = geospatial_ports_polygons %>% filter(infra=="port"),size=0.5) +
        geom_

    ggplot(data = geospatial_coral_polygons, aes(fill = damage_category)) +
        geom_sf(aes(color = damage_category), size = 2) +
        scale_color_manual(values = custom_colors, name = "Cover damage (% per Degree C)") +
        scale_fill_manual(values = custom_colors, name = "Cover damage (% per Degree C)") +
        geom_sf(data = continents, fill = "gray", color = NA, alpha = 0.2) +
        labs(title = "") +
        my_theme() + 
        theme(
            legend.position = "bottom",
            legend.key.height = unit(1, "cm"), 
            legend.key.width = unit(1.5, "cm")  
        ) +
        guides(
            fill = guide_legend(title.position = "top", title.hjust = 0.5),
            color = guide_legend(title.position = "top", title.hjust = 0.5)
        )
