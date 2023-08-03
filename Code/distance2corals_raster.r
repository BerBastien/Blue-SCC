#install.packages(c("tidyverse", "sf", "rgeos", "raster"))
        library(tidyverse)
        library(sf)
        library(rgeos)
        library(raster)

        library(raster)

        library(raster)
        library(sf)

        library(raster)
        library(rgdal)
        install.packages("geosphere")

        

        # Read coastline shapefile
        coastlines <- readOGR("C:/Users/basti/Box/Data/Geography/coastlines/ne_50m_admin_0_countries.shp")
        glimpse(coastlines)
        # Create raster grid
        globe_res <- 5 # Resolution in degrees
        globe_extent <- extent(-180, 180, -90, 90)
        raster_grid <- raster(globe_extent, res = globe_res)
        iso_coastline <- coastlines[coastlines$ISO_A3 == "USA", ]
        # Extract the CRS from the USA coastline
        usa_crs_sf <- st_crs(iso_coastline)

        # Extract the proj4string from the sf CRS
        usa_proj4string <- usa_crs_sf$proj4string

        # Convert the CRS to sp format
        usa_crs_sp <- CRS(usa_proj4string)
        projection(raster_grid) <- usa_crs_sp
        raster_points <- rasterToPoints(raster_grid, spatial = TRUE)
            

        # Initialize an empty raster stack
        raster_stack <- stack()

        # Initialize an empty dataframe to store the results
        result_df <- data.frame()
        isos <- unique(coastlines$ISO_A3)
        for(j in 1:length(isos)){
            iso_coastline <- coastlines[coastlines$ISO_A3 == isos[j], ]
            distance_values <- dist2Line(raster_points, geometry(iso_coastline))

            distance_raster <- raster_grid
            distance_raster[] <- distance_values
            #plot(distance_raster)
            

            # Add the distance raster to the raster stack
            raster_stack <- stack(raster_stack, distance_raster)
            names(raster_stack)[nlayers(raster_stack)] <- isos[j]
            
            # Create a dataframe with the distance values and ISO code
            temp_df <- data.frame(
                Longitude = raster_points@coords[, 1],
                Latitude = raster_points@coords[, 2],
                Distance = distance_values,
                ISO = isos[j]
            )
            
            # Append the temporary dataframe to the master dataframe
            result_df <- rbind(result_df, temp_df)
            
            print(j)
            
        }

        # Save the raster stack to a file
        writeRaster(raster_stack, "distance_rasters.tif", format = "GTiff", overwrite = TRUE)

        # Save the dataframe to a CSV file
        write.csv(result_df, "distance_data.csv", row.names = FALSE)

        

        eez_sf <- st_as_sf(eez_gpkg)
        glimpse(eez_sf)
        class(eez_gpkg)

        coast_sp_df <- ne_coastline(scale = 110, returnclass = c("sf"))
        glimpse(coast_sp_df)
        class(coast_sp_df)
        class(eez_sf)
        glimpse(eez_sf)
        unique(eez_sf$ISO_TER1)

        
        
        # Set up an empty raster with desired resolution and extent
        globe_res <- 0.5 # Resolution in degrees
        globe_res <- 2 # Resolution in degrees
        globe_extent <- extent(-180, 180, -90, 90)
        eez_raster <- raster(globe_extent, res = globe_res)

        eez_sf$ISO_TER1_factor <- factor( eez_sf$ISO_TER1)

        # Rasterize the EEZ polygons into the empty raster
        eez_raster <- rasterize(eez_sf, eez_raster, field = "ISO_TER1_factor") #this worked

        # Create a decay function for the USA
        usa_decay <- function(dist) {
        ifelse(dist <= 0, 1, ifelse(dist <= 1000000, 1 - dist/1000000, 0))
        }

        plot(eez_raster)
            #Filter the country and hten getting the distance (start)
                # Rasterize USA only
                eez_sf_usa <- eez_sf[eez_sf$ISO_TER1_factor == "USA", ]
                # Filter out empty geometries
                eez_sf_usa <- eez_sf_usa[!is_empty(eez_sf_usa),]
                usa_raster <- rasterize(eez_sf_usa, globe_extent, mask = TRUE, field = "ISO_TER1_factor")

                # Set areas outside USA to NA
                usa_raster[usa_raster != 1] <- NA

                # Compute distance to USA
                usa_distance <- distance(usa_raster, units = "m")
            #Filter the country and hten getting the distance (end)

            # Rasterize based on numbers to then distinguish between countries (start)
                eez_raster <- rasterize(eez_sf, eez_raster, field = "UN_SOV1") #trying this to get numbers instead of names
                eez_raster2 <- eez_raster
                eez_raster2[eez_raster2 != 840] <- NA #UN_SOV1=840 for USA
                plot(eez_raster2)
                
                glimpse(eez_sf)
                unsov <- unique(eez_sf$UN_SOV1)
                isosov <- unique(eez_sf$ISO_SOV1)
                sov <- unique(eez_sf$SOVEREIGN1)

                for (i in 45:length(unsov)){
                    print(paste(isosov[i],i))

                
                    eez_raster2 <- eez_raster
                    eez_raster2[eez_raster2 != unsov[i]] <- NA #UN_SOV1=840 for USA
                    print("Done: set as NA")
                    plot(eez_raster2)
                    if(sum(is.na(values(eez_raster2)))==16200){
                        print(paste("no values for ", isosov[i]))
                        next
                    }

                    # Calculate the distance to the USA's economic zone
                    usa_distance <- distance(eez_raster2 == unsov[i], units = "m")
                    print("Done: distance calculated")
                    #plot(usa_distance)
                
                    #usa_distance_df <- as.data.frame(usa_distance, xy = TRUE)
                    #eez_raster2_df <- as.data.frame(eez_raster2, xy = TRUE)
                    #glimpse(eez_raster2_df)
                    #is.na(eez_raster2_df$layer)
                    #class(usa_distance_df)
                    #class(eez_raster2_df)


                    eez_raster2_df <- eez_raster2_df[which(!is.na(eez_raster2_df$layer)),]
                    linear_USA <- ggplot()+
                        geom_tile(data=usa_distance_df,
                            aes(x=x,y=y,fill=1-layer/max(usa_distance_df$layer)))+     
                        geom_tile(data=eez_raster2_df,
                            aes(x=x,y=y),fill="aquamarine", na.rm = TRUE) + #+scale_fill_continuous(na.value = "transparent")
                        theme_void()+ 
                        guides(fill = guide_legend(title = "Value multiplier"))+
                        scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
                        ggtitle(paste("Linear decay of value - USA"))+
                        borders("world",fill="gray24",colour="transparent")

                    exp_USA <- ggplot()+
                    #    ggplot()+
                        geom_tile(data=usa_distance_df,
                            aes(x=x,y=y,fill=exp(-(5*layer/max(usa_distance_df$layer)))))+     
                        geom_tile(data=eez_raster2_df,
                            aes(x=x,y=y),fill="aquamarine", na.rm = TRUE) + #+scale_fill_continuous(na.value = "transparent")
                        theme_void()+ 
                        guides(fill = guide_legend(title = "Value multiplier"))+
                        scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
                        ggtitle(paste("Exponential decay of value - USA"))+
                        borders("world",fill="gray24",colour="transparent")

                    fig_decay <- ggarrange(linear_USA,exp_USA,common.legend=TRUE,legend="bottom")
                    #png(paste0("Figures/DeepSea/DecayMaps/ValueDecay",isosov[i],".jpg"))
                    fig_decay
                    #dev.off()
                    ggsave(paste0("Figures/DeepSea/DecayMaps/ValueDecayUSA.png"),dpi=600)


                        #     glimpse(usa_distance_df)
                            
                        # dpft_df <- as.data.frame(dpft, xy = TRUE)
                        # glimpse(dpft_df)
                        
                        # plot((1-usa_distance/maxValue(usa_distance)))
                        
                    usa_distance2 <- resample(usa_distance, dpft)
                    print("Done: resampled")
                    usa_distance2_df <- as.data.frame(usa_distance2, xy = TRUE)
                    names(usa_distance2_df)[3] <- paste0("distance",isosov[i])
                    if(i==1){
                        iso_distance <- usa_distance2_df
                    } else{
                        iso_distance <- cbind(iso_distance ,usa_distance2_df[,3])
                        names(iso_distance)[length(iso_distance)] <- paste0("distance",isosov[i])
                    }
                }
        # Rasterize based on numbers to then distinguish between countries (start)

                #write.csv(iso_distance,"iso_distance.csv")
                iso_distance<-read.csv("iso_distance.csv")
                glimpse(iso_distance)
