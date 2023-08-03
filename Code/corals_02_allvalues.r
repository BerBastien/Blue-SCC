
        coral_areas_gulf <- (coral_areas_wgs84)

        coral_temp_gulf <- (corals_t2coeff_sf)
    
 
        coral_areas_keys <- (coral_areas_gulf)


        coral_temp_keys <- (corals_t2coeff_sf)
       
        coral_areas_keys<-st_as_sf(coral_areas_keys)
        
        coral_areas_keys_single <- coral_areas_keys %>%
        st_cast("POLYGON") %>%
        mutate(geometry = as.list(geometry)) %>%
        unnest(geometry) %>%
        st_as_sf()

        coral_areas_keys_single <- coral_areas_keys

       
        coral_areas_keys_single <- coral_areas_keys_single %>%
        mutate(area = st_area(geometry))
        coral_areas_keys_single$area_km2 <- as.double(coral_areas_keys_single$area)/ 1e6


        coral_areas_keys_single$ID <- seq(1:dim(coral_areas_keys_single)[1])
        
        # Perform spatial join and group by original polygon identifiers
        coral_areas_keys_single_joined <- coral_areas_keys_single %>%
        st_join(coral_temp_keys) %>%
        group_by(ID) %>%
        summarise(surveys = ifelse(!is.na(t2coeff),n(),0),
                    mean_coef = mean(t2coeff, na.rm = TRUE),
                    geometry = geometry)

        coral_areas_keys_single_joined <- coral_areas_keys_single_joined[!duplicated(coral_areas_keys_single_joined$ID), ]

        # Identify polygons with no surveys
        no_surveys_polygons <- coral_areas_keys_single_joined %>%
        filter(surveys == 0)

        # For polygons with no surveys, find the closest three points
        # and calculate the mean coefficient
        closest_mean_coef <- no_surveys_polygons %>%
        st_distance(coral_temp_keys) %>%
        apply(1, function(x) {
            idx <- order(x)[1:3]
            return(mean(coral_temp_keys$t2coeff[idx]))
        })

        # Update the mean_coef variable for polygons with no surveys
        coral_areas_keys_single_joined[which(coral_areas_keys_single_joined$surveys == 0), "mean_coef"] <- closest_mean_coef

        # Check the resulting sf dataframe
        coral_areas_keys_single_joined <- as.data.frame(coral_areas_keys_single_joined)
        coral_areas_keys_single_joined <- st_as_sf(coral_areas_keys_single_joined)
        coral_areas_keys_single_joined$surveys <- as.factor(coral_areas_keys_single_joined$surveys)

        glimpse(coral_areas_keys_single_joined)
        st_write(coral_areas_keys_single_joined,"Data/corals/coral_area_with_mean_t2coeff.shp")


        
    ## Calculate benefits (start)
    
        coral_areas_keys_single2 <- merge(as.data.frame(coral_areas_keys_single),as.data.frame(coral_areas_keys_single_joined[c(1,3)]),by="ID",all.x=TRUE)
        coral_areas_keys_single2$geometry <- coral_areas_keys_single2$geometry.x
        coral_areas_keys_single2 <- coral_areas_keys_single2[,-which(names(coral_areas_keys_single2) %in% c("geometry.x","geometry.y"))]
        coral_areas_keys_single2 <- st_as_sf(coral_areas_keys_single2)
        
        iso_distance <- read.csv("iso_distance.csv")
        fp <- 'Data\\eez_v11.gpkg'
        eez_gpkg <- st_read(fp)
        eez.spatial <- as(eez_gpkg, 'Spatial')
        eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
        oc_ssp<- read.csv("Data/oc_ssp_allcountries.csv")
        Yblue <- read.csv("Data/Yblue_growth_v0_2.csv")
        Temp_ssps <- read_excel("Data/Temp_ssps_ndc.xlsx", sheet = "data")
        names(Temp_ssps)[1] <- "year" 

        glimpse(Temp_ssps)

        library(sf)
        library(dplyr)

        income_elas <- 0.646
        value_km_year <- 2 #change to value per km per year per person?
        WTP <- 0.7290176915
        glimpse(Yblue)
        glimpse(oc_ssp)
        mean_GDPpc <- mean(oc_ssp$gdp[which(oc_ssp$t==2020)]/oc_ssp$pop[which(oc_ssp$t==2020)],na.rm=TRUE)
        iso_distance<-unique(oc_ssp$countrycode)
        
        coral_areas_keys_single_ISO <- coral_areas_keys_single2
        glimpse(coral_areas_keys_single_ISO)
        st_write(coral_areas_keys_single_ISO,"Data/corals/coral_area_with_mean_t2coeff_covariates.shp")

        iso_distance<-unique(oc_ssp$countrycode)
        iso_distance %in% unique(eez_gpkg$ISO_TER1)
        for (j in iso_distance){
                    print(j)# Specify the desired ISO_TER1 value
                    #dist_iso <- iso_distance[,j]
                    #dist_iso_exp <- exp(-(5*dist_iso/max(dist_iso)))
                    #dist_iso_lin <- 1- dist_iso/max(dist_iso)
                    iso_j <- j
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
                    filtered_eez_gpkg <- eez_gpkg %>%
                    filter(ISO_TER1 == iso_ter1_value)
                    # Function to make geometry valid only if needed
                    if (!all(st_is_valid(filtered_eez_gpkg))) {
                    
                        filtered_eez_gpkg <- st_make_valid(filtered_eez_gpkg)
                        
                    } else {
                        filtered_eez_gpkg <- (filtered_eez_gpkg)
                    }
                    

                    
                    filtered_eez_gpkg <- st_union(filtered_eez_gpkg)

                    # Calculate the pairwise distances between the filtered geometries and the polygons in coral_areas_keys_single2
                    distance_st <- st_distance(coral_areas_keys_single2, filtered_eez_gpkg)
                    #glimpse(distance_st)
                    #glimpse(coral_areas_keys_single2)
                    #glimpse(filtered_eez_gpkg)
                    #plot(filtered_eez_gpkg)
                    coral_areas_keys_single_ISO$distance_to_ISO <- as.double(distance_st)


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

                write.csv(df_coralvalues2,"Data/corals/df_coralvalues3.csv")


    ## Calculate benefits (end)


    ## Plots all values (start)
            df_coralvalues <- df_coralvalues2
            df_coralvalues$total_value_nodamage
            
            glimpse(df_coralvalues)
            ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso),color="seagreen3")+
                geom_line(aes(x=decade,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso),color="indianred")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso,label=iso),color="indianred")+
                theme_bw()+
                #scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

            #ggsave("Figures/Coral_Florida_losses.png",dpi=600)

            glimpse(df_coralvalues)
                
            loss_plot <-ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso),color="seagreen3",alpha=0.5)+
                geom_line(aes(x=decade,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso),color="indianred",alpha=0.5)+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso,label=iso),color="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

            valueplot <- ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=(total_value)/ssp_pop,group=iso),color="seagreen3",alpha=0.5)+
                geom_line(aes(x=decade,y=(total_value_lin)/ssp_pop,group=iso),color="indianred",alpha=0.5)+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value)/ssp_pop,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value_lin)/ssp_pop,group=iso,label=iso),color="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Non-use value \nof Florida corals ($/person)")

                ggarrange(valueplot,loss_plot)        
                #ggsave("Figures/corals/value_corals_loss.png",dpi=600) 


            ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=(total_value)/ssp_gdp,group=iso),color="seagreen3",alpha=0.5)+
                geom_line(aes(x=decade,y=(total_value_lin)/ssp_gdp,group=iso),color="indianred",alpha=0.5)+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value)/ssp_gdp,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value_lin)/ssp_gdp,group=iso,label=iso),color="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

            ggplot(data=df_coralvalues[which(df_coralvalues$decade==2100),])+
                geom_histogram(aes(x=(total_value)/ssp_gdp),fill="seagreen3")+
                geom_histogram(aes(x=(total_value_lin)/ssp_gdp),fill="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

                ggplot(data=df_coralvalues[which(df_coralvalues$decade==2100),])+
                geom_point(aes(x=mean_distance,y=(total_value)/ssp_pop),color="seagreen3")+
                geom_point(aes(x=mean_distance,y=(total_value_lin)/ssp_pop),color="indianred")



    ## Plots all values (end)


### Using distance maps instead
    ## Crop BBOX (start)
       
        

        # Crop the coral_areas_wgs84 spatial object to the bounding box of the Gulf of Mexico
        coral_areas_gulf <- (coral_areas_wgs84)

        


        coral_temp_gulf <- (corals_t2coeff_sf)
    
    ## Crop BBOX (start)

        
        


        # Crop the coral_areas_wgs84 spatial object to the bounding box of the Gulf of Mexico
        coral_areas_keys <- (coral_areas_gulf)

        


        coral_temp_keys <- (corals_t2coeff_sf)
        
        

        # Split multipolygons into individual polygons
        glimpse(coral_areas_keys)

        coral_areas_keys<-st_as_sf(coral_areas_keys)
        
        coral_areas_keys_single <- coral_areas_keys %>%
        st_cast("POLYGON") %>%
        mutate(geometry = as.list(geometry)) %>%
        unnest(geometry) %>%
        st_as_sf()

        coral_areas_keys_single <- coral_areas_keys

       
        coral_areas_keys_single <- coral_areas_keys_single %>%
        mutate(area = st_area(geometry))
        coral_areas_keys_single$area_km2 <- as.double(coral_areas_keys_single$area)/ 1e6


        coral_areas_keys_single$ID <- seq(1:dim(coral_areas_keys_single)[1])
        
        # Perform spatial join and group by original polygon identifiers
        coral_areas_keys_single_joined <- coral_areas_keys_single %>%
        st_join(coral_temp_keys) %>%
        group_by(ID) %>%
        summarise(surveys = ifelse(!is.na(t2coeff),n(),0),
                    mean_coef = mean(t2coeff, na.rm = TRUE),
                    geometry = geometry)

        coral_areas_keys_single_joined <- coral_areas_keys_single_joined[!duplicated(coral_areas_keys_single_joined$ID), ]

        # Identify polygons with no surveys
        no_surveys_polygons <- coral_areas_keys_single_joined %>%
        filter(surveys == 0)

        # For polygons with no surveys, find the closest three points
        # and calculate the mean coefficient
        closest_mean_coef <- no_surveys_polygons %>%
        st_distance(coral_temp_keys) %>%
        apply(1, function(x) {
            idx <- order(x)[1:3]
            return(mean(coral_temp_keys$t2coeff[idx]))
        })

        # Update the mean_coef variable for polygons with no surveys
        coral_areas_keys_single_joined[which(coral_areas_keys_single_joined$surveys == 0), "mean_coef"] <- closest_mean_coef

        # Check the resulting sf dataframe
        coral_areas_keys_single_joined <- as.data.frame(coral_areas_keys_single_joined)
        coral_areas_keys_single_joined <- st_as_sf(coral_areas_keys_single_joined)
        coral_areas_keys_single_joined$surveys <- as.factor(coral_areas_keys_single_joined$surveys)

        glimpse(coral_areas_keys_single_joined)
        st_write(coral_areas_keys_single_joined,"Data/corals/coral_area_with_mean_t2coeff.shp")



        

    ## Plot super zoom
        
    ## Calculate benefits (start)
    
        iso_distance_xy <- read.csv("iso_distance.csv")
        fp <- 'Data\\eez_v11.gpkg'
        eez_gpkg <- st_read(fp)
        eez.spatial <- as(eez_gpkg, 'Spatial')
        eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
        oc_ssp<- read.csv("Data/oc_ssp_allcountries.csv")
        Yblue <- read.csv("Data/Yblue_growth_v0_2.csv")
        Temp_ssps <- read_excel("Data/Temp_ssps_ndc.xlsx", sheet = "data")
        names(Temp_ssps)[1] <- "year" 

        glimpse(Temp_ssps)

        library(sf)
        library(dplyr)

        income_elas <- 0.646
        value_km_year <- 2 #change to value per km per year per person?
        WTP <- 0.7290176915
        glimpse(Yblue)
        glimpse(oc_ssp)
        mean_GDPpc <- mean(oc_ssp$gdp[which(oc_ssp$t==2020)]/oc_ssp$pop[which(oc_ssp$t==2020)],na.rm=TRUE)
        iso_distance<-unique(oc_ssp$countrycode)
        
        #coral_areas_keys_single_ISO <- coral_areas_keys_single2
        #glimpse(coral_areas_keys_single_ISO)
        #st_write(coral_areas_keys_single_ISO,"Data/corals/coral_area_with_mean_t2coeff_covariates.shp")
        coral_areas_keys_single_ISO <- st_read("Data/corals/coral_area_with_mean_t2coeff_covariates.shp")
        coral_areas_keys_single2 <- coral_areas_keys_single_ISO
        glimpse(coral_areas_keys_single2)
        
        iso_distance<-unique(oc_ssp$countrycode)
        iso_distance %in% unique(eez_gpkg$ISO_TER1)

        install.packages("exactextractr")
        
        library(exactextractr)
        
        iso_distance<-unique(oc_ssp$countrycode)
        for (j in 3:length(iso_distance)){
                    iso_distance_xy_1 <- iso_distance_xy[,c(2,3,j)]
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

                write.csv(df_coralvalues2,"Data/corals/df_coralvalues3.csv")


    ## Calculate benefits (end)


    ## Plots all values (start)
            df_coralvalues <- df_coralvalues2
            df_coralvalues$total_value_nodamage
            
            glimpse(df_coralvalues)
            ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso),color="seagreen3")+
                geom_line(aes(x=decade,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso),color="indianred")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso,label=iso),color="indianred")+
                theme_bw()+
                #scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

            #ggsave("Figures/Coral_Florida_losses.png",dpi=600)

            glimpse(df_coralvalues)
                
            loss_plot <-ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso),color="seagreen3",alpha=0.5)+
                geom_line(aes(x=decade,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso),color="indianred",alpha=0.5)+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value-total_value_nodamage)/ssp_pop,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=-(total_value_lin-total_value_nodamage_lin)/ssp_pop,group=iso,label=iso),color="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

            valueplot <- ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=(total_value)/ssp_pop,group=iso),color="seagreen3",alpha=0.5)+
                geom_line(aes(x=decade,y=(total_value_lin)/ssp_pop,group=iso),color="indianred",alpha=0.5)+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value)/ssp_pop,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value_lin)/ssp_pop,group=iso,label=iso),color="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Non-use value \nof Florida corals ($/person)")

                ggarrange(valueplot,loss_plot)        
                #ggsave("Figures/corals/value_corals_loss.png",dpi=600) 


            ggplot(df_coralvalues)+
                geom_line(aes(x=decade,y=(total_value)/ssp_gdp,group=iso),color="seagreen3",alpha=0.5)+
                geom_line(aes(x=decade,y=(total_value_lin)/ssp_gdp,group=iso),color="indianred",alpha=0.5)+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value)/ssp_gdp,group=iso,label=iso),color="seagreen3")+
                geom_text_repel(data=df_coralvalues[which(df_coralvalues$decade==2100),],aes(x=decade+5,y=(total_value_lin)/ssp_gdp,group=iso,label=iso),color="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

            ggplot(data=df_coralvalues[which(df_coralvalues$decade==2100),])+
                geom_histogram(aes(x=(total_value)/ssp_gdp),fill="seagreen3")+
                geom_histogram(aes(x=(total_value_lin)/ssp_gdp),fill="indianred")+
                theme_bw()+
                scale_y_continuous(trans="log2")+
                ylab("Climate losses on non-use value \nof Florida corals ($/person)")

                ggplot(data=df_coralvalues[which(df_coralvalues$decade==2100),])+
                geom_point(aes(x=mean_distance,y=(total_value)/ssp_pop),color="seagreen3")+
                geom_point(aes(x=mean_distance,y=(total_value_lin)/ssp_pop),color="indianred")



    ## Plots all values (end)



### Using distance maps instead