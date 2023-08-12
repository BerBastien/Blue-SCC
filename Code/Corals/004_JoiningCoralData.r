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
    load(file="Data/modules/corals/coral_country.Rds") 
    glimpse(corals_area_coeff_sf)
    glimpse(floodedArea_corals)
    glimpse(corals_pop_ssp)

    c_coef <- corals_area_coeff_sf %>% st_drop_geometry()
    c_pop <- corals_pop_ssp %>% st_drop_geometry()
    c_flood <- floodedArea_corals %>% ungroup() %>% st_drop_geometry() %>% st_drop_geometry() %>% dplyr::select(-geometry)
    c_country <- coral_country  %>% st_drop_geometry()
    c_country <- c_country %>% dplyr::select(id,countrycode) %>% group_by(id) %>% slice(1) %>% ungroup() 


    glimpse(c_country)
    corals_df <- merge(c_pop,c_coef,by="id",all=T)
    corals_df <- merge(corals_df,c_flood,by="id",all=T)
    corals_df <- merge(corals_df,c_country,by="id",all.x=T,all.y=F)
    glimpse(corals_df)

    #save(corals_df,file="Data/intermediate_output/corals_df.Rds")
    #write.csv(corals_df,file="Data/intermediate_output/corals_df.csv")
    
    #corals_df <- read.csv(file="Data/intermediate_output/corals_df.csv")

    corals_df_iso <- corals_df %>% filter(ssp=="SSP1" & year==2020) %>%
                    group_by(countrycode) %>%
                    summarise(sum_area_km2=sum(area_km2),
                    weighted_mean_coeff = sum(mean_coef * area_km2) / sum(area_km2),
                    sum_area_cover_km2=sum(area_km2*mean_cover),
                    weighted_mean_coeff_cover = sum(mean_coef * area_km2*mean_cover) / sum(area_km2*mean_cover))

    glimpse(corals_df_iso)
    corals_df_iso$provisioning_value_perkm2year <- 55724*100 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    corals_df_iso$regulating_value_perkm2year <- 171478*100 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    corals_df_iso$habitat_value_perkm2year <- 16210*100 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    corals_df_iso$cultural_value_perkm2year <- 108837*100 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)

    save(corals_df,file="Data/intermediate_output/corals_df.Rds")
    write.csv(corals_df,file="Data/intermediate_output/corals_df.csv")

    corals_df_iso <- corals_df_iso[,-c(2,3)]
    glimpse(corals_df_iso)
    names(corals_df_iso)[c(2,3)] <- c("area_km2_t0","DamCoef_changeperC")
    corals_df_iso[,3] <- corals_df_iso[,3]*0.01
    write.csv(corals_df_iso,file="Data/intermediate_output/corals_area_damage_value.csv")
    # living_corals <- corals_df %>% filter(year==2020 & ssp=="SSP1") %>%
    # summarise(sum(area_km2 * mean_cover))

    # coral_reefs <- corals_df %>% filter(year==2020 & ssp=="SSP1") %>%
    # summarise(sum(area_km2)) #Result =  455806.9 --- Comparable with data from https://biodiversitya-z.org/content/warm-water-coral-reef that estimates:  coral reefs occupy an area of only 260,000 - 600,000 km2, less than 0.1% of the Earth’s surface, or 0.2% of the ocean’s surface



    # v_c <- 352915 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/ha/year)
    # coral_reefs_value <- v_c * coral_reefs *100
    # coral_reefs_value_trill <- coral_reefs_value / 10^12  # Result=16 trillion, 6 times higher than the reported in Summary for Policymakers – Status of Coral Reefs of the World: 2020

    # living_coral_value_trill <- v_c * living_corals *100 / 10^12 # Result = 6 trill, 2 times higher than the reported in Summary for Policymakers – Status of Coral Reefs of the World: 2020


