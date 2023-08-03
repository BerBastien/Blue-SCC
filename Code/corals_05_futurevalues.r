
distance_corals <- read.csv("Data/corals/distance_corals.csv")
oc_ssp<- read.csv("Data/oc_ssp_allcountries.csv")
        Yblue <- read.csv("Data/Yblue_growth_v0_2.csv")
        Temp_ssps <- read_excel("Data/Temp_ssps_ndc.xlsx", sheet = "data")
        names(Temp_ssps)[1] <- "year" 

        income_elas <- 0.646
        value_km_year <- 2 #change to value per km per year per person?
        WTP <- 0.7290176915
        glimpse(Yblue)
        glimpse(oc_ssp)
        mean_GDPpc <- mean(oc_ssp$gdp[which(oc_ssp$t==2020)]/oc_ssp$pop[which(oc_ssp$t==2020)],na.rm=TRUE)
        iso_distance<-unique(oc_ssp$countrycode)
        
        coral_sf <- st_read("Data/corals/coral_area_with_mean_t2coeff_filtered.shp")
        glimpse(distance_corals)
        iso_distance<-unique(distance_corals$iso)

        max()
        
        for (j in 1:length(iso_distance)){
                    iso_distance_xy_1 <- distance_corals[which(distance_corals$iso==iso_distance[j]),]
                    dist_iso <- iso_distance[,j]
                    dist_iso_exp <- exp(-(5*dist_iso/max(dist_iso)))
                    dist_iso_lin <- 1- dist_iso/max(dist_iso)
                    iso_j <- substr(names(iso_distance)[j],9,11)
                    print(paste(j,iso_j))# Specify the desired ISO_TER1 value
                    #dist_iso <- iso_distance[,j]
                    #dist_iso_exp <- exp(-(5*dist_iso/max(dist_iso)))
                    #dist_iso_lin <- 1- dist_iso/max(dist_iso)
                    #iso_j <- j
                    #glimpse(pb_df)[j]
                    gdppc_iso <- oc_ssp$gdp[which(oc_ssp$countrycode==iso_j & oc_ssp$t==2020)][1] / oc_ssp$pop[which(oc_ssp$countrycode==iso_j & oc_ssp$t==2020)] [1]
                    
    
                    #glimpse(oc_ssp)
                    decade <- (2010+seq(1:9)*10)
                    ssp_gdp <- oc_ssp$gdp[which(oc_ssp$countrycode==iso_j & oc_ssp$SSP=="SSP2" & oc_ssp$t %in% decade)]
                    ssp_pop <- oc_ssp$pop[which(oc_ssp$countrycode==iso_j & oc_ssp$SSP=="SSP2" & oc_ssp$t %in% decade)]
                    ssp_temp <- Temp_ssps$RCP6[which(Temp_ssps$year%in% decade)]
                    
                    iso_ter1_value <- iso_j
        

                    # Filter the eez_gpkg dataframe by the desired ISO_TER1 value
                    #glimpse(eez_gpkg)
                    # filtered_eez_gpkg <- eez_gpkg %>%
                    # filter(ISO_TER1 == iso_ter1_value)
                    # # Function to make geometry valid only if needed
                    # if (!all(st_is_valid(filtered_eez_gpkg))) {
                    
                    #     filtered_eez_gpkg <- st_make_valid(filtered_eez_gpkg)
                        
                    # } else {
                    #     filtered_eez_gpkg <- (filtered_eez_gpkg)
                    # }



                    # Create a raster from the distance dataframe
                    glimpse(iso_distance_xy_1)
                    dist_raster <- rasterFromXYZ(iso_distance_xy_1)
                    sf_coral <- coral_areas_keys_single2

                    for (n in 1:9){
                        
                            temp <- ssp_temp[n]
                            log_value_km_year_adjusted_income <- log(value_km_year) + (log(ssp_gdp[n]/ssp_pop[n]) - log(mean_GDPpc))*income_elas
                            coral_areas_keys_single_ISO$distance_to_ISO_decay <-exp(-(5*coral_areas_keys_single_ISO$distance_to_ISO/(max(coral_areas_keys_single_ISO$distance_to_ISO, na.rm=TRUE)))) 
                            coral_areas_keys_single_ISO$distance_to_ISO_decay_lin <- 1 -
                                (coral_areas_keys_single_ISO$distance_to_ISO/(max(coral_areas_keys_single_ISO$distance_to_ISO, na.rm=TRUE))) 
                            
                            coral_areas_keys_single_ISO$value_distance_decay <- exp(log_value_km_year_adjusted_income)*coral_areas_keys_single_ISO$distance_to_ISO_decay
                            coral_areas_keys_single_ISO$value_distance_decay_lin <- exp(log_value_km_year_adjusted_income)*coral_areas_keys_single_ISO$distance_to_ISO_decay_lin
                            coral_areas_keys_single_ISO$damaged_area <- coral_areas_keys_single_ISO$area_km2*
                                (100+coral_areas_keys_single_ISO$mean_coef*temp^2)/100
                            
                            coral_areas_keys_single_ISO$value_per_person <- coral_areas_keys_single_ISO$value_distance_decay* coral_areas_keys_single_ISO$damaged_area
                            coral_areas_keys_single_ISO$value <- coral_areas_keys_single_ISO$value_per_person * ssp_pop[n]
                            value_ISO <- sum(coral_areas_keys_single_ISO$value)

                            coral_areas_keys_single_ISO$value_per_person_lin <- coral_areas_keys_single_ISO$value_distance_decay_lin* coral_areas_keys_single_ISO$damaged_area
                            coral_areas_keys_single_ISO$value_lin <- coral_areas_keys_single_ISO$value_per_person_lin * ssp_pop[n]
                            value_ISO_lin <- sum(coral_areas_keys_single_ISO$value_lin)

                            coral_areas_keys_single_ISO$nodamage_area <- coral_areas_keys_single_ISO$area_km2
                            coral_areas_keys_single_ISO$nodamage_value_per_person <- coral_areas_keys_single_ISO$value_distance_decay* coral_areas_keys_single_ISO$nodamage_area
                            coral_areas_keys_single_ISO$nodamage_value <- coral_areas_keys_single_ISO$nodamage_value_per_person * ssp_pop[n]
                            nodamage_value_ISO <- sum(coral_areas_keys_single_ISO$nodamage_value)

                            coral_areas_keys_single_ISO$nodamage_value_per_person_lin <- coral_areas_keys_single_ISO$value_distance_decay_lin* coral_areas_keys_single_ISO$nodamage_area
                            coral_areas_keys_single_ISO$nodamage_value_lin <- coral_areas_keys_single_ISO$nodamage_value_per_person_lin * ssp_pop[n]
                            nodamage_value_ISO_lin <- sum(coral_areas_keys_single_ISO$nodamage_value_lin)

                            cor_val <- data.frame(mean_distance=mean(coral_areas_keys_single_ISO$distance_to_ISO, na.rm=TRUE),
                                value_per_person=mean(coral_areas_keys_single_ISO$value_per_person),
                                total_value=value_ISO,total_value_nodamage=nodamage_value_ISO,
                                total_value_lin=value_ISO_lin,total_value_nodamage_lin=nodamage_value_ISO_lin,exp="RCP6")
                            if(n==1){
                                coral_value_iso <- cor_val

                            }else{
                                coral_value_iso <- rbind(coral_value_iso,cor_val)
                        }


                        
                        
                    }
                    
                                coral_value_iso$iso <- j
                                coral_value_iso$temp <- ssp_temp
                                coral_value_iso$class <- Yblue$ocean_classification[which(Yblue$countrycode==iso_j & Yblue$year==2018)] [1]
                                coral_value_iso$decade = decade
                                coral_value_iso$ssp_gdp <- ssp_gdp
                                coral_value_iso$ssp_pop <- ssp_pop
                                



                    
                    if (j==iso_distance[1]){
                        df_coralvalues2 <- coral_value_iso
                    }else{
                        df_coralvalues2 <- rbind(df_coralvalues2,coral_value_iso)
                    }
                }