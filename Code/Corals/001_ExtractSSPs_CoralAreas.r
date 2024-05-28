## Extract GDP and Pop for coral Areas


## Load Corals
    continents <- ne_countries(scale = "medium", returnclass = "sf") #%>%st_transform(st_crs(coral_areas_wgs84))  
    dir_wcmc <- "C:\\Users\\basti\\Box\\Data\\Oceans\\coral_extent\\14_001_WCMC008_CoralReefs2021_v4_1\\01_Data"
    v4_coral_py <- sf::st_read(dsn = file.path(dir_wcmc), layer = "WCMC008_CoralReef2021_Py_v4_1")
    v4_coral_py <- st_make_valid(v4_coral_py)
    v4_coral_py$id <- seq(1:dim(v4_coral_py)[1])
    
    coral_sf <- st_as_sf(v4_coral_py)
    # Convert coral_areas to WGS84 geographic coordinate system
    coral_areas_wgs84 <- st_transform(coral_sf, crs = st_crs(4326))
    na_vals_id <- 0
    for(i in 1:dim(coral_areas_wgs84)[1]){
        xmin <- st_bbox(coral_areas_wgs84$geometry[i])[1]
        if(is.na(xmin)){
            na_vals_id <- c(na_vals_id,i)
        }
    }

    coral_areas_wgs84 <- coral_areas_wgs84[-na_vals_id,]
    glimpse(coral_areas_wgs84)
   

    print(st_crs(coral_areas_wgs84))
    coral_areas_wgs84$geometry_0 <- coral_areas_wgs84$geometry
    coral_areas_wgs84$geometry_100 <- st_buffer(coral_areas_wgs84$geometry, dist = 100000)
    coral_areas_wgs84$geometry_50 <- st_buffer(coral_areas_wgs84$geometry, dist = 50000)
    coral_areas_wgs84$geometry_500 <- st_buffer(coral_areas_wgs84$geometry, dist = 500000)
    glimpse(coral_areas_wgs84)
    plot(coral_areas_wgs84$geometry_50)
    class(coral_areas_wgs84)
    
    # Save the st_geometry object to a shapefile
    save(coral_areas_wgs84, file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\coral_buff.Rds")
    ## Loop through SSPs - Population
    
    ssps <- paste0("SSP",seq(1:5))
    years <- 2015 + seq(1:17)*5
    buffers <- c(0,50,100)
    first=1
    for(ssp_i in 1:5){
        dir_pop <- paste0(dir_ssps,ssps[ssp_i],"\\",ssps[ssp_i],"\\")
        for (t in years){
            pop_raster_name <- paste0(dir_pop,ssps[ssp_i],"_",t,".tif")
            pop_raster <- raster::raster(pop_raster_name)
            masked_raster <- mask(rast(pop_raster), coral_vect)
            
           
                for(buffer_i in buffers){
                    
                    coral_geom <- coral_areas_wgs84[[paste0("geometry_",buffer_i)]]
                
                        extracted_values <- exact_extract(pop_raster, coral_geom, fun = "sum")
                        ev <- data.frame(id=coral_areas_wgs84$id,pop=unlist(extracted_values))
                        #glimpse(ev)
                        ev$year <- t
                        ev$ssp <- ssps[ssp_i]
                        ev$buffer <- buffer_i
                        
                        if(first==1){
                                corals_pop_ssp <- ev
                                first <- 0
                            } else{
                                corals_pop_ssp <- bind_rows(corals_pop_ssp,ev)
                                #glimpse(corals_pop_ssp)
                            }

                        print(paste(ssps[ssp_i]," Buffer=",buffer_i," Year=",t))

                }
            }
    }
    corals_pop_ssp[which(corals_pop_ssp$id==6),]
    glimpse(corals_pop_ssp)
    #write.csv(corals_pop_ssp,"coral_pop_ssp1.csv")
    library(ggplot2)

    glimpse(coral_geom)
    
    ggplot(corals_pop_ssp)+
    geom_boxplot(aes(x=factor(buffer),y=log(pop)))
    aggregate(pop~buffer,data=corals_pop_ssp,FUN="sum",na.rm=TRUE)

    unique(corals_pop_ssp$id[which(corals_pop_ssp$pop!=0)])

    #plot(coral_areas_wgs84,add=TRUE)
    st_bbox(coral_areas_wgs84$geometry_50[5])
    e=extent(st_bbox(coral_areas_wgs84$geometry_50[5]))
    pop_crop=crop(pop_raster,e)
    plot(pop_crop)
    plot(coral_areas_wgs84$geometry_0[5],col="red",add=TRUE)
    plot(coral_areas_wgs84$geometry_50[5],add=TRUE)

    
    exact_extract(pop_crop, coral_areas_wgs84$geometry_0[5], fun = "sum")
    exact_extract(pop_raster, coral_areas_wgs84$geometry_50[5], fun = "sum")
    exact_extract(pop_raster, coral_areas_wgs84$geometry_0[5], fun = "sum")
    exact_extract(pop_crop, coral_areas_wgs84$geometry_50[5], fun = "sum")

    
                coral_geom <- coral_areas_wgs84 %>% select(paste0("geometry_",buffer_i))
                glimpse(coral_geom)
                coral_geom <- coral_geom[,-c(2)]
                coral_geom <- coral_geom[[paste0("geometry_",buffer_i)]]
                plot(coral_geom)
                coral_geom$geometry <- sf::st_geometry(coral_geom[,1])
                plot(coral_geom$geometry_100[5])
                exact_extract(pop_raster, coral_geom, fun = "sum")

                glimpse(coral_areas_wgs84)
    
                #plot(pop_raster,add=TRUE)




    corals_temp_unique_sf <- st_as_sf(corals_temp_unique, coords = c("Longitude.Degrees", "Latitude.Degrees"))

    # Set the CRS for corals_temp_unique_sf
    corals_temp_unique_sf <- st_set_crs(corals_temp_unique_sf, st_crs(coral_sf))
    
    # Convert corals_temp_unique_sf to WGS84 geographic coordinate system
    corals_temp_unique_sf_wgs84 <- st_transform(corals_temp_unique_sf, crs = st_crs(4326))
    
    class(corals_temp_unique_sf_wgs84)
    glimpse(corals_temp_unique_sf_wgs84)

    glimpse(coral_areas_wgs84)
    coral_areas_wgs84$id <- seq(1:dim(coral_areas_wgs84)[1])

    ggplot() +
    geom_sf(data = coral_areas_wgs84, 
        #aes(fill = log(GIS_AREA_K)),
        color="red",size=3) +
    #scale_fill_gradient(low = "black", high = "red") +
    #geom_sf(data = corals_temp_unique_sf_wgs84, aes(color= cover_change)) +
    #scale_fill_gradient(low = "white", high = "red")+
    labs(title = "Global distribution of coral reefs")+
    geom_sf(data = continents, fill = "black", alpha = 0.5)+theme_void()
    #ggsave("Figures/GlobalDistributionCoral.png",dpi=600)


    
