#setup 

    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    dir1 <- paste0(getwd(),'\\Data\\all_data\\corals\\')
    
#setup 

# Merge future coral cover with GMST (start)
    # Input Data (start)
        corals <- read.csv(paste0(dir1,"data_futurecorals.csv")) #Dataset from Sully et al.
        T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
        T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
    # Input Data (end)
    
    ## Arranging Coral Projections (start)
        corals$uniqueplace <- paste0(corals$Latitude.Degrees,corals$Longitude.Degrees)
        coral_latlon <- corals[,which(names(corals) %in% c("Longitude.Degrees","Latitude.Degrees","uniqueplace"))]
        
        corals2 <- corals[,which(names(corals) %in% c("Y_future_RCP85_yr_2100_change",
        "Y_future_RCP85_yr_2050_change","uniqueplace",
        "Y_future_RCP45_yr_2100_change","Y_future_RCP45_yr_2050_change"))]  # Y_future_RCP45_yr_2050_change is the coral cover percent in 2050 minus the coral cover percentage in 2018

        corals_long <- corals2 %>%
        pivot_longer(cols = -uniqueplace, names_to = "variable", values_to = "cover_change") %>%
            mutate(scenario = paste0("RCP",str_extract(variable, "(?<=RCP)\\d+")),
            year = as.numeric(str_extract(variable, "(?<=yr_)\\d{4}")))
        
        corals3 <- corals[,which(names(corals) %in% c("Y_New","uniqueplace"))] 
        glimpse(corals3)    
        corals_long2 <- merge(corals_long,corals3,by="uniqueplace")

        corals_long2$cover_change_perc <- 100*corals_long2$cover_change / corals_long2$Y_New
    ## Arranging Coral Projections (start)

    ## Reading SSPs Temp (start)

        temp <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
        temp$year <- as.integer(sub('X', '', temp$year))
        names(temp)[1] <- "temp"
        temp$scenario <- "RCP45"

        temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
        temp2$year <- as.integer(sub('X', '', temp2$year))
        names(temp2)[1] <- "temp"
        temp2$scenario <- "RCP85"

        temp <- rbind(temp,temp2)

        temp <-temp %>% group_by(scenario) %>%
        filter(year > 1996, year<2019) %>%
        mutate(t_96_18 = mean(temp)) %>%
        filter(year==1997) %>%
        dplyr::select(t_96_18,scenario) %>%
        inner_join(temp, by = c("scenario"))

        temp$tdif <- temp$temp - temp$t_96_18

    ## Reading SSPs Temp (start)

    ## Merging coral projections and GMST (start)
    
        corals_temp <- merge(corals_long2,temp,by=c("year","scenario"),all=FALSE)

        # create a new dataframe with the desired observations
        new_data <- data.frame(
        year = 2018,
        scenario = c("RCP45", "RCP85"),
        uniqueplace = unique(corals_temp$uniqueplace),
        variable = paste0("Y_future__yr_2018_change"),
        cover_change = 0,
        t_96_18 = 0,
        temp = mean(corals_temp$temp[corals_temp$year == 2018]), # use the mean temperature for 2018
        tdif = 0
        )

        coral_latlon <- coral_latlon[!duplicated(coral_latlon$uniqueplace), ]
        ct <- merge(corals_temp,coral_latlon,by="uniqueplace")
        corals_temp <- ct
        
        corals_temp_unique <- aggregate(cover_change_perc~uniqueplace+scenario+tdif+year+Latitude.Degrees+Longitude.Degrees,data=corals_temp,FUN="mean")
        corals_temp_unique_cover <- aggregate(Y_New~uniqueplace+scenario+tdif+year+Latitude.Degrees+Longitude.Degrees,data=corals_temp,FUN="mean")
        corals_temp_unique$cover <- corals_temp_unique_cover$Y_New

    ## Merging coral projections and GMST (end)

    ## Output Files (start)    
        save(corals_temp_unique,file="Data/Modules/Corals/corals_temp_unique.Rds")
        write.csv(corals_temp,"Data/corals/corals_temp.csv")
        write.csv(corals_temp_unique,"Data/corals/corals_temp_unique.csv")

    ## Output Files (start)




# Merge future coral cover with GMST (end)

## Estimate Temperature Coefficient (start)
    # Input (start)
        load(file="Data/Modules/Corals/corals_temp_unique.Rds")
    # Input (start)

    # Read coral spatial areas
        v4_coral_py <- sf::st_read(dsn = file.path(dir_wcmc), layer = "WCMC008_CoralReef2021_Py_v4_1")
        v4_coral_py <- st_make_valid(v4_coral_py)
        v4_coral_py$id <- seq(1:dim(v4_coral_py)[1])
        
        coral_sf <- st_as_sf(v4_coral_py)
        
        coral_areas_wgs84 <- st_transform(coral_sf, crs = st_crs(4326))
        continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
            st_transform(st_crs(coral_areas_wgs84))  
        
        na_vals_id <- 0
        for(i in 1:dim(coral_areas_wgs84)[1]){
            xmin <- st_bbox(coral_areas_wgs84$geometry[i])[1]
            if(is.na(xmin)){
                na_vals_id <- c(na_vals_id,i)
            }
        }

        coral_areas_wgs84 <- coral_areas_wgs84[-na_vals_id,]
    
        corals_temp_unique_sf <- st_as_sf(corals_temp_unique, coords = c("Longitude.Degrees", "Latitude.Degrees"))
       

        # Set the CRS for corals_temp_unique_sf
        corals_temp_unique_sf <- st_set_crs(corals_temp_unique_sf, st_crs(coral_sf))
        
        # Convert corals_temp_unique_sf to WGS84 geographic coordinate system
        corals_temp_unique_sf_wgs84 <- st_transform(corals_temp_unique_sf, crs = st_crs(4326))
    # Read coral spatial areas

    
    # Estimate GMST Coefficient (start)
        
        # Group by uniqueplace and estimate the t2coeff
        corals_tcoeff <- corals_temp_unique_sf_wgs84 %>%
        group_by(uniqueplace) %>%
        nest() %>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(cover_change_perc ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(cover_change_perc ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(cover_change_perc ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
            }), 

            living_coral_cover = map_dbl(data, ~{
            mod <- lm(cover ~1, .x)
            summary(mod)$coef[1]
            })
        )

        corals_tcoeff <- corals_tcoeff %>%
        unnest(data)


        
        # Output (start)
            save(corals_tcoeff,file="Data/Modules/Corals/corals_tcoeff.Rds")
        # Output (end)
    # Estimate GMST Coefficient (end)

        load(file="Data/Modules/Corals/corals_tcoeff.Rds")

    # Plot estimated coefficients against values (start) 
        
        corals_tcoeff_sf <- corals_tcoeff %>%
        slice(1) %>% select(uniqueplace, tcoeff, geometry, se, pval,cover)  %>% ungroup()%>%
        st_as_sf()


        all_places <- unique(corals_temp_unique_sf_wgs84$uniqueplace)
        i <- 1
        for (place in all_places){
            
            place_data <- corals_temp_unique_sf_wgs84 %>% filter(uniqueplace==place)
            coef <- corals_tcoeff%>% filter(uniqueplace==place)
            #glimpse(place_data)
            plot <- ggplot() + 
            geom_point(data=place_data,aes(x=tdif,y=cover_change_perc)) +
            geom_line(aes(x=(seq(0,40)*0.1),y=(seq(0,40)*0.1*coef$tcoeff[1])))+
            geom_ribbon(aes(x=(seq(0,40)*0.1),ymin=(seq(0,40)*0.1*(coef$tcoeff[1])-coef$se*1.645),
                ymax=(seq(0,40)*0.1*(coef$tcoeff[1])+coef$se*1.645)),alpha=0.1)+
            theme_minimal()+
            xlab("Temperature Increase") + 
            ylab("Percent Cover Change")
            plot
            ggsave(filename = paste0("Figures/all_figures/corals/place_Area_Damage//place_", i, "_area_damage.jpg"), plot = plot, width = 8, height = 4)
            i <- i +1
        }
        
    # Plot estimated coefficients against values (end)


    # Squared coefficients

        # corals_tcoeff <- corals_temp_unique_sf_wgs84 %>%
        # group_by(uniqueplace) %>%
        # nest() %>%
        # mutate(
        #     tcoeff = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0 + I(tdif^2) | 0 | 0 | 0, .x)
        #     coef(mod)[1]
        #     }),

        #     se = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0 + I(tdif^2) | 0 | 0 | 0, .x)
        #     summary(mod)$coef[2]
        #     }),

        #     pval = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0 + I(tdif^2) | 0 | 0 | 0, .x)
        #     summary(mod)$coef[4]
        #     }), 

        #     living_coral_cover = map_dbl(data, ~{
        #     mod <- lm(cover ~1, .x)
        #     summary(mod)$coef[1]
        #     })
        # )

        # corals_tcoeff <- corals_tcoeff %>%
        # unnest(data)

        # corals_tcoeff_sf <- corals_tcoeff %>%
        # slice(1) %>% select(uniqueplace, tcoeff, geometry, se, pval,cover)  %>% ungroup()%>%
        # st_as_sf()


        # all_places <- unique(corals_temp_unique_sf_wgs84$uniqueplace)
        # i <- 1
        # for (place in all_places){
            
        #     place_data <- corals_temp_unique_sf_wgs84 %>% filter(uniqueplace==place)
        #     coef <- corals_tcoeff%>% filter(uniqueplace==place)
        #     #glimpse(place_data)
        #     plot <- ggplot() + 
        #     geom_point(data=place_data,aes(x=tdif,y=cover_change_perc)) +
        #     geom_line(aes(x=(seq(0,40)*0.1),y=(((seq(0,40)*0.1)^2)*coef$tcoeff[1])))+
        #     geom_ribbon(aes(x=(seq(0,40)*0.1),ymin=(((seq(0,40)*0.1)^2)*(coef$tcoeff[1]-coef$se*1.645)),
        #         ymax=(((seq(0,40)*0.1)^2)*(coef$tcoeff[1]+coef$se*1.645))),alpha=0.1)+
        #     theme_minimal()+
        #     xlab("Temperature Increase") + 
        #     ylab("Percent Cover Change")
        #     plot
        #     ggsave(filename = paste0("Figures/all_figures/corals/place_Area_Damage//Only_Squared//place_", i, "_area_damage.jpg"), plot = plot, width = 8, height = 4)
        #     i <- i +1
        # }
        
        # coral_t_coeff_sq <- corals_temp_unique_sf_wgs84 %>%
        # group_by(uniqueplace) %>%
        # nest() %>%
        # mutate(
        #     tcoeff = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0+ tdif + I(tdif^2)| 0 | 0 | 0, .x)
        #     coef(mod)[1]
        #     }),

        #     tcoeff2 = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0+ tdif + I(tdif^2)| 0 | 0 | 0, .x)
        #     coef(mod)[2]
        #     }),

        #     se = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0  +tdif +  I(tdif^2)| 0 | 0 | 0, .x)
        #     summary(mod)$coef[3]
        #     }),

        #     se2 = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0  +tdif +  I(tdif^2)| 0 | 0 | 0, .x)
        #     summary(mod)$coef[4]
        #     }),

        #     pval = map_dbl(data, ~{
        #     mod <- felm(cover_change_perc ~ 0 +tdif +  I(tdif^2) | 0 | 0 | 0, .x)
        #     summary(mod)$coef[7]
        #     }), 

        #     living_coral_cover = map_dbl(data, ~{
        #     mod <- lm(cover ~1, .x)
        #     summary(mod)$coef[1]
        #     })
        # )

        # glimpse(coral_t_coeff_sq)

        # all_places <- unique(corals_temp_unique_sf_wgs84$uniqueplace)
        # i <- 1
        # for (place in all_places){
            
        #     place_data <- corals_temp_unique_sf_wgs84 %>% filter(uniqueplace==place)
        #     coef <- coral_t_coeff_sq%>% filter(uniqueplace==place)
        #     #glimpse(place_data)
        #     plot <- ggplot() + 
        #     geom_point(data=place_data,aes(x=tdif,y=cover_change_perc)) +
        #     geom_line(aes(x=(seq(0,40)*0.1),y= ((seq(0,40)*0.1)*coef$tcoeff[1]+ ((seq(0,40)*0.1)^2)*coef$tcoeff2[1]))) +

        #     geom_ribbon(    aes(x=(seq(0,40)*0.1),ymin=((seq(0,40)*0.1)*(coef$tcoeff[1]-coef$se*1.645)+((seq(0,40)*0.1)^2)*(coef$tcoeff2[1]-coef$se2*1.645)),
        #         ymax=((seq(0,40)*0.1)*(coef$tcoeff[1]+coef$se*1.645)+((seq(0,40)*0.1)^2)*(coef$tcoeff2[1]+coef$se2*1.645))),alpha=0.1)+
        #     theme_minimal()+
        #     xlab("Temperature Increase") + 
        #     ylab("Percent Cover Change")
        #     plot
        #     ggsave(filename =paste0("Figures/all_figures/corals/place_Area_Damage//sq//place_", i, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)
        #     i <- i +1
        # }
        
    # Squared coefficients
## Estimate Temperature Coefficient (end)

## Example of the Methodology using a subset of the data (start)

    ## Zoom in the Gulf of Mexico (start)
        
        # Define the coordinates of the Gulf of Mexico polygon
        gulf_coords <- matrix(c(
        -90, 18,
        -78, 18,
        -78, 28,
        -90, 28,
        -90, 18
        ), ncol = 2, byrow = TRUE)

        library(sf)
        caribe_coords <- matrix(c(
            -90, 18,
            -78, 18,
            -78, 28,
            -90, 28,
            -90, 18), ncol = 2, byrow = TRUE)
        

        # Create a simple feature with a polygon representing the Gulf of Mexico region
        gulf_polygon <- st_polygon(list(gulf_coords))

        # Get the bounding box of the Gulf of Mexico polygon
        gulf_bbox <- st_bbox(gulf_polygon)




        # Crop the coral_areas_wgs84 spatial object to the bounding box of the Gulf of Mexico
        coral_areas_gulf <- st_crop(coral_areas_wgs84, gulf_bbox)

        


        coral_temp_gulf <- st_crop(corals_tcoeff_sf, gulf_bbox)# Define breaks for the histogram
        library(classInt)
        breaks <- classIntervals(coral_areas_gulf$GIS_AREA_K, n=50, style="jenks")$brks

        breaks <- seq(min(log10(coral_areas_gulf$GIS_AREA_K)), max(log10(coral_areas_gulf$GIS_AREA_K)))
        coral_areas_gulf$area_group <- cut(log10(coral_areas_gulf$GIS_AREA_K), breaks = breaks, labels = FALSE)

        breaks <- seq(min((coral_temp_gulf$tcoeff*0.01)), max((coral_temp_gulf$tcoeff*0.01)),length.out=10)
        coral_temp_gulf$coef_group <- cut((coral_temp_gulf$tcoeff*0.01), breaks = breaks, labels = FALSE)
        save(coral_areas_gulf,file="Data/Modules/Corals/coral_areas_gulf.Rds")
        save(coral_temp_gulf,file="Data/Modules/Corals/coral_temp_gulf.Rds")
    ## Zoom in the Gulf of Mexico (end)

    ## Zoom in Florida Keys (start)
        keys_coords <- matrix(c(
            -80.5, 24.9,
            -80.2, 24.9,
            -80.2, 25.3,
            -80.5, 25.3,
            -80.5, 24.9), ncol = 2, byrow = TRUE)
        

        # Create a simple feature with a polygon representing the Gulf of Mexico region
        keys_polygon <- st_polygon(list(keys_coords))

        # Get the bounding box of the Gulf of Mexico polygon
        keys_bbox <- st_bbox(keys_polygon)




        # Crop the coral_areas_wgs84 spatial object to the bounding box of the Gulf of Mexico
        coral_areas_keys <- st_crop(coral_areas_gulf, keys_bbox)

        


        coral_temp_keys <- st_crop(corals_tcoeff_sf, keys_bbox)
        
        
        # Split multipolygons into individual polygons
        coral_areas_keys_single <- coral_areas_keys %>%
        st_cast("POLYGON") %>%
        mutate(geometry = as.list(geometry)) %>%
        unnest(geometry) %>%
        st_as_sf()

        
        coral_areas_keys_single <- coral_areas_keys_single %>%
        mutate(area = st_area(geometry))
        coral_areas_keys_single$area_km2 <- as.double(coral_areas_keys_single$area)/ 1e6

        
        coral_areas_keys_single$ID <- seq(1:dim(coral_areas_keys_single)[1])
        
        # Perform spatial join and group by original polygon identifiers
        coral_areas_keys_single_joined <- coral_areas_keys_single %>%
        st_join(coral_temp_keys) %>%
        group_by(ID) %>%
        summarise(surveys = ifelse(!is.na(tcoeff),n(),0),
                    mean_coef = mean(tcoeff, na.rm = TRUE),
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
            return(mean(coral_temp_keys$tcoeff[idx]))
        })

        # Update the mean_coef variable for polygons with no surveys
        coral_areas_keys_single_joined[which(coral_areas_keys_single_joined$surveys == 0), "mean_coef"] <- closest_mean_coef

        coral_areas_keys_single_joined <- as.data.frame(coral_areas_keys_single_joined)
        coral_areas_keys_single_joined <- st_as_sf(coral_areas_keys_single_joined)
        
        coral_areas_keys_single_joined$surveys <- as.factor(coral_areas_keys_single_joined$surveys)



        save(coral_areas_keys,file="Data/Modules/Corals/coral_areas_keys.Rds")
        save(coral_areas_keys_single,file="Data/Modules/Corals/coral_areas_keys_single.Rds")
        save(coral_areas_keys_single_joined,file="Data/Modules/Corals/coral_areas_keys_single_joined.Rds")
        save(coral_temp_keys,file="Data/Modules/Corals/coral_temp_keys.Rds")

    ## Zoom in Florida Keys (end)

## Example of the Methodology using a subset of the data (end)

## Assign coefficients to area polygons (start)
        
        coral_areas_wgs84_2 <- coral_areas_wgs84 %>%
        mutate(area = st_area(geometry))
        coral_areas_wgs84_2$area_km2 <- as.double(coral_areas_wgs84_2$area)/ 1e6

        # Perform spatial join and group by original polygon identifiers
        coral_areas_wgs84_2_joined <- coral_areas_wgs84_2 %>%
        st_join(corals_tcoeff_sf) %>%
        group_by(id) %>%
        summarise(surveys = ifelse(!is.na(tcoeff),n(),0),
                    mean_coef = mean(tcoeff, na.rm = TRUE),
                    mean_se = mean(se, na.rm = TRUE),
                    mean_cover = mean(cover, na.rm = TRUE),
                    area_km2 = mean(area_km2,na.rm=TRUE),
                    geometry = geometry)
        
        
        coral_areas_wgs84_2_joined <- coral_areas_wgs84_2_joined[!duplicated(coral_areas_wgs84_2_joined$id), ]

        # Identify polygons with no surveys
        no_surveys_polygons <- coral_areas_wgs84_2_joined %>%
        filter(surveys == 0)

        # For polygons with no surveys, find the closest three points
        # and calculate the mean values
        closest_mean_coef <- no_surveys_polygons %>%
        st_distance(corals_tcoeff_sf) %>%
        apply(1, function(x) {
            idx <- order(x)[1:3]
            mean_coef <- mean(corals_tcoeff_sf$tcoeff[idx])
            mean_se <- mean(corals_tcoeff_sf$se[idx])
            mean_cover <- mean(corals_tcoeff_sf$cover[idx])
            return(c(mean_coef, mean_se,mean_cover))
        })  %>%
        t() %>%
        as.data.frame() %>%
        setNames(c("mean_tcoeff", "mean_se","mean_cover"))

        
        # Update the mean_coef variable for polygons with no surveys
        coral_areas_wgs84_2_joined[which(coral_areas_wgs84_2_joined$surveys == 0), c(3,4,5)] <- closest_mean_coef

        # Check the resulting sf dataframe
        glimpse(coral_areas_wgs84_2_joined)
        corals_area_coeff_df <- as.data.frame(coral_areas_wgs84_2_joined)
        corals_area_coeff_sf <- st_as_sf(corals_area_coeff_df)
        
        ## Output (start)
            #uncomment if data is ot there
            #save(corals_area_coeff_sf,file="Data/modules/corals/corals_area_coeff_sf.Rds")
            #save(corals_area_coeff_df,file="Data/modules/corals/corals_area_coeff_df.Rds")
        ## Output (end)

## Assign coefficients to area polygons (start)