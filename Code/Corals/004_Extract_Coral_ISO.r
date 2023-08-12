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
    summarise(sum(area_km2)) #Result =  455806.9 --- Comparable with data from https://biodiversitya-z.org/content/warm-water-coral-reef that estimates:  coral reefs occupy an area of only 260,000 - 600,000 km2, less than 0.1% of the Earth’s surface, or 0.2% of the ocean’s surface



    v_c <- 352915 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/ha/year)
    coral_reefs_value <- v_c * coral_reefs *100
    coral_reefs_value_trill <- coral_reefs_value / 10^12  # Result=16 trillion, 6 times higher than the reported in Summary for Policymakers – Status of Coral Reefs of the World: 2020

    living_coral_value_trill <- v_c * living_corals *100 / 10^12 # Result = 6 trill, 2 times higher than the reported in Summary for Policymakers – Status of Coral Reefs of the World: 2020


    #Join with countries 

    dir_wcmc <- "C:\\Users\\basti\\Box\\Data\\Oceans\\coral_extent\\14_001_WCMC008_CoralReefs2021_v4_1\\01_Data"
    v4_coral_py <- sf::st_read(dsn = file.path(dir_wcmc), layer = "WCMC008_CoralReef2021_Py_v4_1")
    v4_coral_py <- st_make_valid(v4_coral_py)
    v4_coral_py$id <- seq(1:dim(v4_coral_py)[1])
    glimpse(v4_coral_py)
    #coral_countries <- v4_coral_py %>% st_drop_geometry()
    #glimpse(coral_countries)

    fp <- 'Data\\all_data\\eez_v11.gpkg'
    eez_gpkg <- st_read(fp)
    eez_gpkg <- st_make_valid(eez_gpkg)
    eez.spatial <- as(eez_gpkg, 'Spatial')
    eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
    class(eez_gpkg)
    glimpse(eez_gpkg)

    coral_country <- v4_coral_py %>%
        st_join(eez_gpkg) %>%
         mutate(countrycode = ISO_SOV1)
         
    glimpse(coral_country)
    save(coral_country,file="Data/modules/corals/coral_country.Rds")

