#setup 

    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales')
    lapply(x, require, character.only = TRUE)

    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\'
    
#setup 


    load(file="Data/modules/corals/corals_area_coeff_sf.Rds") #corals_area_coeff_sf
    load(file="Data/modules/corals/floodedArea_corals.Rds") 
    load(file="Data/modules/corals/corals_pop_ssp1_5.Rds") 
    glimpse(corals_area_coeff_sf)
    glimpse(floodedArea_corals)
    glimpse(corals_pop_ssp)

    c_coef <- corals_area_coeff_sf %>% st_drop_geometry()
    c_pop <- corals_pop_ssp %>% st_drop_geometry()
    c_flood <- floodedArea_corals %>% st_drop_geometry()

    glimpse(c_pop)
    corals_df <- merge(c_pop,c_coef,by="id",all=T)
    corals_df <- merge(corals_df,c_flood,by="id",all=T)
    glimpse(corals_df)

    living_corals <- corals_df %>% filter(year==2020 & ssp=="SSP1") %>%
    summarise(sum(area_km2 * mean_cover))

    coral_reefs <- corals_df %>% filter(year==2020 & ssp=="SSP1") %>%
    summarise(sum(area_km2))
