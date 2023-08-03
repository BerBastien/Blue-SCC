#setup 

    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales')
    lapply(x, require, character.only = TRUE)

    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\'
    
#setup 

# Read future coral cover (start)
    corals <- read.csv(paste0(dir1,"data_futurecorals.csv"))
    glimpse(corals)
    corals$uniqueplace <- paste0(corals$Latitude.Degrees,corals$Longitude.Degrees)

    coral_latlon <- corals[,which(names(corals) %in% c("Longitude.Degrees","Latitude.Degrees","uniqueplace"))]
    glimpse(coral_latlon)
    
    corals2 <- corals[,which(names(corals) %in% c("Y_future_RCP85_yr_2100_change",
    "Y_future_RCP85_yr_2050_change","uniqueplace",
    "Y_future_RCP45_yr_2100_change","Y_future_RCP45_yr_2050_change"))]  #Y_future_RCP45_yr_2050_change is the coral cover percent in 2050 minus the coral cover percentage in 2018

    corals_long <- corals2 %>%
    pivot_longer(cols = -uniqueplace, names_to = "variable", values_to = "cover_change")

    corals_long2 <- corals_long %>%
        mutate(scenario = paste0("RCP",str_extract(variable, "(?<=RCP)\\d+")),
        year = as.numeric(str_extract(variable, "(?<=yr_)\\d{4}")))
    #corals_long2$cover_change <- 100*corals_long2$cover_change #Is this the change in percentual points?

    corals3 <- corals[,which(names(corals) %in% c("Y_New","uniqueplace"))] 
    glimpse(corals3)
    corals_long2 <- merge(corals_long2,corals3,by="uniqueplace")

    corals_long2$cover_change_perc <- 100*corals_long2$cover_change / corals_long2$Y_New
    
    ggplot(corals_long2)+
    geom_line(aes(x=year,y=cover_change_perc,color=scenario,group=interaction(uniqueplace,scenario)),alpha=0.05)+
    theme_bw()+
    ylab("Coral cover change (%)")

    T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
    T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
    temp <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
    temp$year <- as.integer(sub('X', '', temp$year))
    names(temp)[1] <- "temp"
    temp$scenario <- "RCP45"

    temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
    temp2$year <- as.integer(sub('X', '', temp2$year))
    names(temp2)[1] <- "temp"
    temp2$scenario <- "RCP85"

    temp <- rbind(temp,temp2)
    glimpse(temp)

    temp <-temp %>% group_by(scenario) %>%
    filter(year > 1996, year<2019) %>%
    mutate(t_96_18 = mean(temp)) %>%
    filter(year==1997) %>%
    select(t_96_18,scenario) %>%
    inner_join(temp, by = c("scenario"))

    glimpse(temp)

    temp$tdif <- temp$temp - temp$t_96_18
    ggplot(temp)+
    geom_line(aes(x= year , y=tdif, color=scenario))

    glimpse(corals_long2)
    glimpse(temp)
    
    corals_temp <- merge(corals_long2,temp,by=c("year","scenario"),all=FALSE)
    glimpse(corals_temp)

    ggplot(corals_temp)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)), alpha = 0.05)

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

    glimpse(corals_temp)
    unique(corals_latlon$uniqueplace)
    unique(coral_latlon$uniqueplace)
    coral_latlon <- coral_latlon[!duplicated(coral_latlon$uniqueplace), ]
    ct <- merge(corals_temp,coral_latlon,by="uniqueplace")
    glimpse(ct)
    corals_temp <- ct
    
    ggplot(corals_temp)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)), alpha = 0.05)+
    theme_bw()+
    scale_color_manual(values=c("#0072B2", "#D55E00"))+
    geom_text(aes(x=1,y=1.5,label="RCP4.5 - 2050"),color="#0072B2",size=3) +
    geom_text(aes(x=1.5,y=0.35,label="RCP8.5 - 2050"),color="#D55E00",size=3)+
    geom_text(aes(x=2.1,y=-0.2,label="RCP4.5 - 2100"),color="#0072B2",size=3)+
    geom_text(aes(x=3.8,y=-1,label="RCP8.5 - 2100"),color="#D55E00",size=3) +
    geom_text(aes(x=0.2,y=1.5,label="Baseline\n(1997-2018)"),color="black",size=3) # "#0072B2", "#D55E00"
    #ggsave("Figures/corals/coralcover_by_temp.png",dpi=600)


    glimpse(corals_temp)
    corals_temp_unique <- aggregate(cover_change_perc~uniqueplace+scenario+tdif+year+Latitude.Degrees+Longitude.Degrees,data=corals_temp,FUN="mean")
    corals_temp_unique_cover <- aggregate(Y_New~uniqueplace+scenario+tdif+year+Latitude.Degrees+Longitude.Degrees,data=corals_temp,FUN="mean")
    glimpse(corals_temp_unique)
    glimpse(corals_temp_unique_cover)
    corals_temp_unique$cover <- corals_temp_unique_cover$Y_New

    glimpse(corals_temp_unique)
    
    save(corals_temp_unique,file="Data/Modules/Corals/corals_temp_unique.Rds")
    
    write.csv(corals_temp,"Data/corals/corals_temp.csv")
    write.csv(corals_temp_unique,"Data/corals/corals_temp_unique.csv")





# Read future coral cover (end)

## Estimate Temperature Coefficient (start)
    library(rnaturalearth)
    dir_wcmc <- "C:\\Users\\basti\\Box\\Data\\Oceans\\coral_extent\\14_001_WCMC008_CoralReefs2021_v4_1\\01_Data"
    v4_coral_py <- sf::st_read(dsn = file.path(dir_wcmc), layer = "WCMC008_CoralReef2021_Py_v4_1")
    v4_coral_py <- st_make_valid(v4_coral_py)
    v4_coral_py$id <- seq(1:dim(v4_coral_py)[1])
    
    coral_sf <- st_as_sf(v4_coral_py)
    # Convert coral_areas to WGS84 geographic coordinate system
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
        

        save(corals_tcoeff,file="Data/Modules/Corals/corals_tcoeff.Rds")
        
        corals_tcoeff_sf <- corals_tcoeff%>%
        slice(1) %>% select(uniqueplace, tcoeff, geometry, se, pval,cover)  %>% ungroup()%>%
        st_as_sf()

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

        breaks <- seq(min((coral_temp_gulf$tcoeff)), max((coral_temp_gulf$tcoeff)),length.out=10)
        coral_temp_gulf$coef_group <- cut((coral_temp_gulf$tcoeff), breaks = breaks, labels = FALSE)
        save(coral_areas_gulf,file="Data/Modules/Corals/coral_areas_gulf.Rds")
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
        glimpse(corals_tcoeff_sf)
        glimpse(coral_areas_wgs84_2)
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
        glimpse(corals_tcoeff_sf)
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
        
        save(corals_area_coeff_sf,file="Data/modules/corals/corals_area_coeff_sf.Rds")
        save(corals_area_coeff_df,file="Data/modules/corals/corals_area_coeff_df.Rds")
## Assign coefficients to area polygons (start)

