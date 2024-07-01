#setup#

    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales')
        library(tidyverse)
        library(sf)
        library(rgeos)
        library(raster)

        library(raster)

        library(raster)
        library(sf)
        lapply(x, install.packages, character.only = TRUE)
        lapply(x, require, character.only = TRUE)
    #install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))

    ## Function
        sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(1:end1),c(1:end1)]
            beta.hat <- coef(model)[c(1:end1)]
            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(x, x^2)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x))
        }
    ## Function

    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\all_data\\benthos\\'
    
    ## Retreive biomass (start)
        fish_models <- c("dbpm")   #dbpm_ipsl-cm5a-lr_nobc_rcp85_wo-diaz_no-fishing_no-oa_b30cm-bendet_global_monthly_2006_2100.nc4
        #clim_models <- c("hadgem2-es","gfdl-esm4","ipsl-cm5a-lr","miroc5")
        clim_models <- c("ipsl-cm5a-lr")
        clim_scen <- c("rcp85")
        sceni <- 1
        climi <- 1
        fishi <- 1
        c <- 1

        for (sceni in 1:2){
            for(fishi in 1:length(fish_models)){
                for(climi in 1:length(clim_models)){

                    ncname <- paste(dir1,
                        fish_models[fishi],"_",clim_models[climi],
                        "_nobc_",clim_scen[sceni],
                        #"_wo-diaz_no-fishing_no-oa_b30cm-bendet_global_monthly_2006_2100.nc4",sep="")
                        "_wo-diaz_no-fishing_no-oa_b10cm-bendet_global_monthly_2006_2100.nc4",sep="")
                        #dbpm_ipsl-cm5a-lr_nobc_rcp85_wo-diaz_no-fishing_no-oa_b30cm-bendet_global_monthly_2006_2100.nc4
                        #dbpm_ipsl-cm5a-lr_nobc_rcp85_wo-diaz_no-fishing_no-oa_b10cm-bendet_global_monthly_2006_2100
                    ncin <- nc_open(ncname)

                    print(ncin)    
                    #get units
                    lon <- ncvar_get(ncin,"lon")
                    lat <- ncvar_get(ncin,"lat")
                    time<- ncvar_get(ncin,"time")
                    tunits <- ncatt_get(ncin,"time","units")
                    dname <- "b10cm-bendet"
                    #dname <- "b30cm-bendet"
                
                    var_array <- ncvar_get(ncin,dname, start = c(1,1,1), count=c(-1,-1,-1)) #start, number in dimension to start. count: hopw many in that dimension
                    yearly_totalcatch <- array(numeric(),c(360,180,95)) 
                    for (d in (1:(dim(var_array)[3]/12))){ 
                        yearly_array <- var_array[,,(1+(d-1)*12):(12*d)] #decade d of data
                        yearly_sum <- rowMeans(yearly_array, dims = 2, na.rm = TRUE)
                        yearly_totalcatch[,,d] <- yearly_sum
                    }

                
                    time_y <- 2005+ seq(1: ( dim(yearly_totalcatch)[3])) #months since 1601
                    #numperiods <- 4
                    numperiods <- 9
                    periods <- array(numeric(),c(360,180,numperiods)) 
                    dif_periods <- array(numeric(),c(180,360,numperiods-1)) 
                    #periods_minyear <- c(1,8,27,66) #years 2015,2022,2041,2081
                    #periods_maxyear <- c(11,26,46,87) #years 2025,2040,2060,2099
                    periods_minyear <- c(1,16,26,36,46,56,66,76,86) #years 2015,2031,2041,2051,2061,2071,2081,2091
                    periods_maxyear <- c(15,25,35,45,55,65,75,85,95) #years 2025,2040,2060,2099
                    for (d in (1:numperiods)){ 
                        period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
                        period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
                        period_mean[period_mean>1000] <- NaN 
                        periods[,,d] <- as.numeric(period_mean)
                        if (d>1){
                            dif_periods[,,d-1] <- t((periods[,,d] - periods[,,1])/periods[,,1])
                        }
                    }
                    periods_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2015-2099
                    #assign(paste("decadal_",fish_models[fishi],"_brick_",clim_models[climi],"_rcp6",sep=""), periods_brick)
                    dif_periods_brick <- brick(dif_periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
                    #assign(paste("dif_periods_",fish_models[fishi],"_brick_rcp6",sep=""), dif_periods_brick)
                    #years_veg[16:85]
                    plot(t(subset(periods_brick,1)), main = "Biomass Density of Dentric Detritivores")
                    dpft <- (t(subset(periods_brick,9)) - t(subset(periods_brick,1)))/ t(subset(periods_brick,1))
                    dpft <- (t(subset(periods_brick,9)) - t(subset(periods_brick,1)))
                    dpft <- (t(subset(periods_brick,9)) / t(subset(periods_brick,1)))-1
                    plot( dpft[dpft<0], main = "Change in Biomass Density of Dentric Detritivores \n under RCP85 (2090's-2010's)")
                    plot( dpft, main = "Change in Biomass Density of Dentric Detritivores \n under RCP85 (2090's-2010's)")
                    #plot( (subset(dif_periods_brick,8)), main = "Change in Biomass Density of Dentric Detritivores \n under RCP85 (2090's-2010's)")

                    #yearly_veg_brick_10 <- brick(var_array[,,1:10], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
                    save(periods_brick,file=paste("Data\\periods_Benthos10cm",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],".Rdata",sep=""))
                    save(dif_periods_brick,file=paste("Data\\dif_periods_Benthos10cm",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],".Rdata",sep=""))
                    
                }
            }
        }

    
    ## Retreive biomass (end)

    fp <- 'Data\\eez_v11.gpkg'
    eez_gpkg <- st_read(fp)
    eez.spatial <- as(eez_gpkg, 'Spatial')
    eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
    class(eez_gpkg)
    glimpse(eez_gpkg)

    ## Get the raster decay

        #install.packages(c("tidyverse", "sf", "rgeos", "raster"))
        

        eez_sf <- st_as_sf(eez_gpkg)
        glimpse(eez_sf)
        
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

                #glimpse(iso_distance)


                #load("Data\\periods_Benthos10cmdbpm_brick_ipsl-cm5a-lr_rcp85.Rdata")
        # Country-level non-use values (start)        
                periods_brick_b10 <- periods_brick
                glimpse(periods_brick)

                apb <- raster::area(t(periods_brick))
                grams <- apb * t(periods_brick)
                plot(grams)
                
                pb_df <- as.data.frame(grams, xy = TRUE)
                bbox <- extent(-48, -43.5, 46, 49)
                r_crop <- crop(t(subset(periods_brick,1)), bbox)
                r_area <- values(area(r_crop))
                r_values <- values(r_crop)
                mean(r_values) * sum(r_area)

                WTP <- 0.7290176915 # See here: https://docs.google.com/spreadsheets/d/1RzfiUROkpS14czG-7DaBumV4CRUgFRwWC3ltPvb0OMY/edit#gid=0 
                GDPpc_Canada <- Yblue$GDP[which(Yblue$countrycode=="CAN" & Yblue$year==2018)] / Yblue$Population[which(Yblue$countrycode=="CAN" & Yblue$year==2018)] 
                income_elas <- 0.646

                exp(log(WTP) + (log(GDPpc_Canada-1500) - log(GDPpc_Canada))*income_elas)

                
                exp(log(WTP) + (log(GDPpc_Canada-1500) - log(GDPpc_Canada))*income_elas)

                dim(iso_distance)
                for (j in 3:length(iso_distance)){
                    print(j)
                    dist_iso <- iso_distance[,j]
                    dist_iso_exp <- exp(-(5*dist_iso/max(dist_iso)))
                    dist_iso_lin <- 1- dist_iso/max(dist_iso)
                    iso_j <- substr(names(iso_distance)[j],9,11)
                    #glimpse(pb_df)[j]
                    gdppc_iso <- Yblue$GDP[which(Yblue$countrycode==iso_j & Yblue$year==2018)][1] / Yblue$Population[which(Yblue$countrycode==iso_j & Yblue$year==2018)] [1]
                    
    
                    #glimpse(oc_ssp)
                    decade <- (2000+seq(1:9)*10)
                    ssp_gdp <- oc_ssp$gdp[which(oc_ssp$countrycode==iso_j & oc_ssp$SSP=="SSP2" & oc_ssp$t %in% decade)]
                    ssp_pop <- oc_ssp$pop[which(oc_ssp$countrycode==iso_j & oc_ssp$SSP=="SSP2" & oc_ssp$t %in% decade)]
                    glimpse(pb_df)
                    for (n in 1:9){
                        if(n==1){
                            value <- as.data.frame(pb_df[,(n+2)]*exp(log(WTP) + (log(ssp_gdp/ssp_pop) - log(GDPpc_Canada))*income_elas)*ssp_pop)
                        }else{
                            value <- cbind(value,pb_df[,(n+2)]*exp(log(WTP) + (log(ssp_gdp/ssp_pop) - log(GDPpc_Canada))*income_elas)*ssp_pop)
                        }

                        
                        
                    }
                    names(value) <- decade
                    
                    #glimpse(value)
                    if(dim(value)[1]==0){next}
                    value_decay_exp <- value * dist_iso_exp
                    value_decay_lin <- value * dist_iso_lin
                    tot_val_exp <- colSums(value_decay_exp,na.rm=TRUE)
                    tot_val_lin <- colSums(value_decay_lin,na.rm=TRUE)

                    df <- data.frame(countrycode=iso_j,tot_val=c(tot_val_exp,tot_val_lin),
                        decay=c(rep("exp",9),rep("lin",9)),class=Yblue$ocean_classification[which(Yblue$countrycode==iso_j & Yblue$year==2018)] [1],
                        decade=decade,ssp_gdp = c(gdppc_iso[1],ssp_gdp),ssp_pop=c(Yblue$Population[which(Yblue$countrycode==iso_j & Yblue$year==2018)][1] ,ssp_pop))
                    if (j==3){
                        df_deepvalues <- df
                    }else{
                        df_deepvalues <- rbind(df_deepvalues,df)
                    }
                }

                #write.csv(df_deepvalues,"DeepSeaValues.csv")
                df_deepvalues <- read.csv("DeepSeaValues.csv")

                glimpse(df_deepvalues)
                val_percapita <- ggplot(df_deepvalues[which(df_deepvalues$decade!=2010),])+
                #geom_point(aes(x=decade,y=(tot_val/ssp_pop)/(ssp_gdp/ssp_pop),shape=decay,color=class,alpha=decade))+
                #geom_point(aes(x=decade,y=(tot_val/ssp_pop)/(ssp_gdp/ssp_pop),shape=decay,color=class))+
                #geom_line(aes(x=decade,y=(tot_val/ssp_pop)/(ssp_gdp/ssp_pop),color=class,group=interaction(countrycode,decay)))+
                geom_point(aes(x=decade,y=(tot_val/ssp_pop),shape=decay,color=class,alpha=decade))+
                geom_point(aes(x=decade,y=(tot_val/ssp_pop),shape=decay,color=class))+
                geom_line(aes(x=decade,y=(tot_val/ssp_pop),color=class,group=interaction(countrycode,decay)))+
                #geom_point(aes(x=decade,y=tot_val,shape=decay,color=class))+
                xlim(2020,2100)+
                theme_bw()+
                #scale_y_continuous(trans="log")+
                ylab("Non-use value of Deep-sea \n(as % of GDP)")
                val_percapita

                summary(felm(I(tot_val/ssp_pop)~decade|countrycode+decay|0|countrycode,data=df_deepvalues[which(df_deepvalues$decade!=2010),]))
                
                a <- ggplot(df_deepvalues[which(df_deepvalues$decade!=2010),])+
                geom_point(aes(x=decade,y=100*tot_val/ssp_gdp,shape=decay,color=class,alpha=decade))+
                geom_point(aes(x=decade,y=100*tot_val/ssp_gdp,shape=decay,color=class))+
                geom_line(aes(x=decade,y=100*tot_val/ssp_gdp,color=class,group=interaction(countrycode,decay)))+
                #geom_point(aes(x=decade,y=tot_val,shape=decay,color=class))+
                xlim(2020,2100)+
                theme_bw()+
                scale_y_continuous(trans="log")+
                ylab("Non-use value of Deep-sea \n(as % of GDP)")
                a
                
                #ggsave("Figures/DeepSeaValues.png",dpi=600)

                b <- ggplot(df_deepvalues[which(df_deepvalues$decade!=2010),])+
                geom_point(aes(x=ssp_gdp/ssp_pop,y=100*tot_val/ssp_gdp,shape=decay,color=class,alpha=decade))+
                #geom_line(aes(x=ssp_gdp/ssp_pop,y=100*tot_val/ssp_gdp,color=class,group=interaction(countrycode,decay)))+
                #geom_point(aes(x=decade,y=tot_val,shape=decay,color=class))+
                #xlim(2020,2100)+
                theme_bw()+
                scale_y_continuous(trans="log")+
                scale_x_continuous(trans="log")+
                ylab("")+
                xlab("GDP per capita")
                b
                #ggsave("Figures/DeepSeaValues_GDP.png",dpi=600)

                projections_plot <- ggarrange(a,b, common.legend=TRUE,legend="right")
                #ggsave("Figures/DeepSeaValues_GDP.png",dpi=600)

                
                ggarrange(fig_decay, projections_plot, ncol=1, common.legend=TRUE,legend="bottom")
                #ggsave("Figures/DeepSeaValues_GDP.jpg",dpi=600)
                #glimpse(Yblue)

        # Country-level non-use values (end)

        # Cuntry-level damage functions (start)
        
                df_deepvalues <- read.csv("DeepSeaValues.csv")
                ssp585 <- read.csv("Data/SSP585_magicc_202303141113.csv")
                glimpse(ssp585[17,c(13:118)])
                temp <- as.data.frame(t(ssp585[17,c(13:118)]))
                glimpse(temp)
                temp$year <- c(1995:2100)
                glimpse(temp)
                names(temp)[1] <- "temp"               
                glimpse(df_deepvalues)
                val_temp <- merge(df_deepvalues,temp,by.x="decade",by.y="year",all=FALSE)
                glimpse(val_temp)
                
                val_temp$temp_dif <- val_temp$temp - val_temp$temp[which(val_temp$decade==2010)][1]

                val_temp <- val_temp %>%
                group_by(countrycode,decay) %>%
                filter(decade == 2020) %>%
                select(countrycode,decay, tot_val,ssp_pop) %>%
                rename(tot_val2020 = tot_val) %>%
                rename(ssp_pop2020 = ssp_pop) %>%
                inner_join(val_temp, by = c("countrycode","decay"))

                glimpse(val_temp)

                val_temp$omega <- (val_temp$tot_val/val_temp$ssp_pop)/(val_temp$tot_val2020/val_temp$ssp_pop2020)

                library("ggrepel")
                
                Omega_plot <- ggplot(val_temp[val_temp$decade!=2010,])+
                theme_bw()+
                xlab("Temperature change")+
                ylab("Normalized non-use values")+
                geom_point(aes(x=temp_dif,y=omega,shape=decay,color=class,group=interaction(countrycode,decay)))+
                geom_line(aes(x=temp_dif,y=omega,color=class,group=interaction(countrycode,decay)))+
                xlim(0,4)+
                geom_text_repel(data=val_temp[which(val_temp$decade==2090 & val_temp$decay=="lin"),],
                    aes(x=temp_dif+0.2,y=omega,color=class,label=countrycode),max.overlaps =40,size=2)
                
                Omega_hist <- ggplot(data=val_temp[val_temp$decade==2090,], aes(x=omega)) + 
                geom_histogram(aes(fill=class))+
                theme_bw()+xlab("")+
                coord_flip()

                ggarrange(Omega_plot, Omega_hist,common.legend=TRUE,legend="bottom", widths = c(2, 0.5))
                ggsave("Figures/DeepSea_Omega2.png",dpi=600)

                val_temp <- as.data.frame(val_temp)

                for(i in 1:length(unique(val_temp$countrycode))){
                    dat_1 <- val_temp[which(val_temp$countrycode==unique(val_temp$countrycode)[i] & val_temp$decay=="exp"),]
                    coeft2<-summary(felm(I(omega-1)~0+I(temp_dif)^2|0|0|0,data=dat_1))$coef[1]
                    deepval_2020=dat_1$tot_val2020[1]/dat_1$ssp_pop2020[1]
                    if(i==1){
                        omega_coef <- data.frame(countrycode=unique(val_temp$countrycode)[i],coeft2=coeft2,deepval_2020=deepval_2020)
                    }else{
                        omega_coef <- rbind(omega_coef,data.frame(countrycode=unique(val_temp$countrycode)[i],coeft2=coeft2,deepval_2020=deepval_2020))
                    }
                }

                glimpse(omega_coef)
                write.csv(omega_coef,"Data/omega_coef_deepsea.csv")


                read temp 
                merge with temp
                loop over countries to get coefficients




        # Country-level damage functions (end)     




                glimpse(pb_df)

                plot(periods_brick_b10)
                
                dif_periods_brick <- load(paste("Data\\dif_periods_Benthos10cm",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],".Rdata",sep=""))
                
                #load Benthic iodiversity loss data


                    #glimpse(usa_distance2_df)
                
                        # plot(usa_distance2)
                        # plot(dpft)
                        # plot(dpft * (exp(-(5*usa_distance2/maxValue(usa_distance2)))))


          

                
                plot(1-usa_distance/maxValue(usa_distance))
                plot(eez_raster2, add=TRUE)
                

                # Apply the decay function to the distance raster
                usa_decay_raster <- usa_decay(usa_distance)
                plot(usa_decay_raster)

                # Create the final raster map
                usa_map <- eez_raster == "USA" # USA economic zone raster
                usa_decay_map <- usa_decay_raster * usa_map # Decay function applied to USA raster
            # Rasterize based on numbers to then distinguish between countries (end) 


        
        
        # Calculate the distance to the USA's economic zone
        usa_distance <- distance(eez_raster == "USA", units = "m")

        # Apply the decay function to the distance raster
        usa_decay_raster <- usa_decay(usa_distance)

        # Create the final raster map
        usa_map <- eez_raster == "USA" # USA economic zone raster
        usa_decay_map <- usa_decay_raster * usa_map # Decay function applied to USA raster









    ## Get the raster decay



    df_fish = data.frame(name = character(0),decade=double(),catch=double(),dif_catch=double(),scen=character(0),fisheries_model=character(0),gcm_model=character(0))
    
    for (sceni in 1:2){
        for(fishi in 1:length(fish_models)){
            for(climi in 1:length(clim_models)){
                print(paste(clim_scen[sceni],fish_models[fishi],clim_models[climi]))

                load(file=paste("Data\\periods_",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],".Rdata",sep=""))
                load(file=paste("Data\\dif_periods_",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],".Rdata",sep=""))
                #assign(paste("periods_",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],sep=""), periods_brick)
                #dif_periods_brick <- eval(parse(text=paste("dif_periods_",fish_models[fishi],"_brick_",clim_models[climi],"_",clim_scen[sceni],sep="")))

                for (c in 1:length(eez_countries)){
                    for (deci in 1:dim(periods_brick)[3]){
                        geom_iso <- st_geometry(eez_gpkg[eez_gpkg$TERRITORY1==eez_countries[c] | eez_gpkg$TERRITORY2==eez_countries[c],])

                    
                        fishdata <- t(subset(periods_brick,deci))
                        
                        #geom_iso <- world$geom[world$iso_a2==isos[j]]
                        geom_iso <- st_cast(geom_iso, "POLYGON")
                        geom_iso <-as_Spatial(geom_iso)
                        #plot(geom_iso)

                        fish_in_c <- crop(fishdata,geom_iso)
                        fic <- mask(fish_in_c, geom_iso)
                        catch <- sum(values(fic), na.rm=TRUE)

                        if (deci ==1){
                            dif_catch <- 'NA'
                        }else{
                            fishdata_dif <-  t(subset(dif_periods_brick,(deci-1)))
                            fish_in_c <- crop(fishdata_dif,geom_iso)
                            fic <- mask(fish_in_c, geom_iso)
                            dif_catch <- sum(values(fic), na.rm=TRUE)

                        }
                        

                        df2 <- data.frame(name = eez_countries[c],decade=2010+(deci*10),catch=catch,dif_catch=dif_catch,scen=clim_scen[sceni],fisheries_model=fish_models[fishi],gcm_model=clim_models[climi])
                        df_fish <-  rbind(df_fish,df2)
                    }
                }
            }
        }
    }

    #write.csv(df_fish,"Data/fisheries.csv")
    

    df_fish <- read.csv("Data/fisheries.csv")
    glimpse(df_fish)

    df_fish$id <- paste0(df_fish$name,df_fish$scen, df_fish$fisheries_model, df_fish$gcm_model)
    baseline <- df_fish[which(df_fish$decade==2020),]
    glimpse(baseline)

    df_fish_baseline <- merge(df_fish,baseline[,which(names(baseline) %in% c("id","catch"))],by="id",all=TRUE)
    glimpse(df_fish_baseline)

    df_fish$dif_catch <- 100*(df_fish_baseline$catch.x - df_fish_baseline$catch.y)/df_fish_baseline$catch.x

    ggplot(df_fish[which(df_fish$scen=="ssp585" & df_fish$fisheries_model=="ecoocean" & df_fish$gcm_model =="gfdl-esm4"),])+
    geom_line(aes(x=decade,y=dif_catch,color=name)) + 
    guides(color="none")


    tas_gfdl <- read.table("Data/gfdl_ssp585_tas.txt", header = FALSE, sep = "", dec = ".")
    glimpse(tas_gfdl)
    
    
    tas_df[c(which(tas_df$V1==2015):which(tas_df$V1==2025)),]


    df_tas = data.frame(decade=double(),scen=character(0),gcm_model=character(0),dif_t=double())
    for (sceni in 1:2){
        
        for(climi in 1:length(clim_models)){

            tas_df <- read.table(paste0("Data/",clim_models[climi],"_",clim_scen[sceni],"_tas.txt"), header = FALSE, sep = "", dec = ".")

            for (i in 1:7){
                
                #meant <- tas_df$V2[c(which(tas_df$V1==(2020+(i-1)*10)):which(tas_df$V1==(2020+(i)*10)))]
                
                    base_t <- tas_df$V2[c(which(tas_df$V1==(2020)):which(tas_df$V1==(2030)))]
                    mean_t <- tas_df$V2[c(which(tas_df$V1==(2020+(i-1)*10)):which(tas_df$V1==(2020+(i)*10)))]
                    dif_t <- mean_t - base_t
                
                df_tas2 <- data.frame(decade=2020+(i-1)*10,scen=clim_scen[sceni],gcm_model=clim_models[climi],dif_t=dif_t)
                df_tas <- rbind(df_tas,df_tas2)


            }
        }
    }

    glimpse(df_tas)
    write.csv(df_tas,"Data/dif_tas.csv")

    
    df_tas <- read.csv("Data/dif_tas.csv")


    glimpse(df_fish)

    df_fish$id_clim <- paste0(df_fish$decade,df_fish$scen,df_fish$gcm_model)
    df_tas$id_clim <- paste0(df_tas$decade,df_tas$scen,df_tas$gcm_model)

    df_fish <- merge(df_fish,df_tas[which(names(df_tas)%in% c("id_clim","dif_t"))],by="id_clim",all.x=TRUE)
    glimpse(df_fish)

    ggplot(df_fish[which(df_fish$scen=="ssp585" & df_fish$fisheries_model=="ecoocean" & df_fish$gcm_model =="gfdl-esm4"),])+
    geom_line(aes(x=dif_t,y=dif_catch,color=name)) + 
    guides(color="none")

    ggplot(df_fish[which(df_fish$scen=="ssp585" & df_fish$fisheries_model=="ecoocean" & df_fish$gcm_model =="gfdl-esm4"),])+
    geom_line(aes(x=decade,y=catch,color=name)) + 
    guides(color="none")+
    geom_text(data=df_fish[which(df_fish$decade==2050 & df_fish$scen=="ssp585" & df_fish$fisheries_model=="ecoocean" & df_fish$gcm_model =="gfdl-esm4"),],
            aes(x=decade,y=catch,color=name,label=name))

    d1 <- df_fish[which(df_fish$decade==2050 & df_fish$scen=="ssp585" & df_fish$fisheries_model=="ecoocean" & df_fish$gcm_model =="gfdl-esm4"),]
    glimpse(d1)

    df_fish2 <- df_fish[which(df_fish$dif_catch <0),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + scen + gcm_model + fisheries_model| name  | 0 | name  , data=df_fish)
    summary(model1)
    df_estimates <- sqest(df_fish,model1,"dif_t","all")

    
    df_fish2 <- df_fish[which(df_fish$fisheries_model =="ecoocean"),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + scen + gcm_model | name  | 0 | name  , data=df_fish2)
    df_estimates <- rbind(df_estimates,sqest(df_fish,model1,"dif_t","ecoocean"))

    df_fish2 <- df_fish[which(df_fish$fisheries_model =="boats"),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + scen + gcm_model | name  | 0 | name  , data=df_fish2)
    df_estimates <- rbind(df_estimates,sqest(df_fish,model1,"dif_t","boats"))

    comp_fish <- ggplot(df_estimates[which(df_estimates$exp %in% c("ecoocean","boats")),],aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Fisheries (%)")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impacts on fisheries")

    df_fish2 <- df_fish[which(df_fish$gcm_model =="gfdl-esm4"),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + scen + fisheries_model | name  | 0 | name  , data=df_fish2)
    df_estimates <- rbind(df_estimates,sqest(df_fish,model1,"dif_t","gfdl-esm4"))

    df_fish2 <- df_fish[which(df_fish$gcm_model =="ipsl-cm6a-lr"),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + scen + fisheries_model | name  | 0 | name  , data=df_fish2)
    df_estimates <- rbind(df_estimates,sqest(df_fish,model1,"dif_t","ipsl-cm6a-lr"))

    comp_gcms <- ggplot(df_estimates[which(df_estimates$exp %in% c("gfdl-esm4","ipsl-cm6a-lr")),],aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Fisheries (%)")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impacts on fisheries")

    df_fish2 <- df_fish[which(df_fish$scen =="ssp585"),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + gcm_model + fisheries_model | name  | 0 | name  , data=df_fish2)
    df_estimates <- rbind(df_estimates,sqest(df_fish,model1,"dif_t","ssp585"))

    df_fish2 <- df_fish[which(df_fish$scen =="ssp126"),]
    model1 <- felm(dif_catch ~ dif_t + I(dif_t^2) + gcm_model + fisheries_model | name  | 0 | name  , data=df_fish2)
    df_estimates <- rbind(df_estimates,sqest(df_fish2,model1,"dif_t","ssp126"))

    comp_scen <- ggplot(df_estimates[which(df_estimates$exp %in% c("ssp126","ssp585")),],aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Fisheries (%)")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impacts on fisheries")

                    glimpse(df_estimates)

    ggarrange(comp_scen,comp_fish,comp_gcms)
    
    ggsave("Figures/Fisheries_Impact.png",dpi=600)
    getwd()



    
    dataset <- df_fish
    model <- model1
    Sigma <- vcov(model)
    coefT <- "dif_t"
    start1 <- which(names(coef(model))==coefT)
    end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
    
    sigma = Sigma[c(1:end1),c(1:end1)]
    beta.hat <- coef(model)[c(1:end1)]
    x <- seq(from=min(dataset$dif_t,na.rm=TRUE),to=max(dataset$dif_t,na.rm=TRUE), length=100)
    xmat <- cbind(x, x^2)
    gestimated <- colSums(beta.hat*t(xmat)) 
    ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
    ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))



    df_estimates<-data.frame(temp=x,estimated = gestimated, ci12 =ci12, ci22=ci22,exp="all")


                    a=ggplot(df_estimates,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci22,ymax=ci12,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Fisheries (%)")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impacts on fisheries") #+ ylim(-10,5)
                    a

    

    glimpse(dataset)
    dataset$gestimated2 <- gestimated
    dataset$ci12 <- ci12
    dataset$ci22 <- ci22
    dataset$x <- xmat[,2]




    


      
      
      for (sceni in 1:2){
        
                for (c in 1:length(eez_countries)){
                        
                        

                        df_fish[which(df_fish$name == eez_countries[c]) & ,]
                    }
                }


    model1 <- felm(catch ~ decade + scen | name  | 0 | name , data=df_fish)
    summary(model1)





    #Read NetCDF of TAS (start)

   
        tas_iso_year <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    year=integer())
    
        annual <- array(numeric(),c(720,360,100)) 
        for (i in 1:10){ #NOTE i starts in 2 (i.e. from the year 2021)
        dname <- "tas"
        if (i==1){
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_20060101-20101231.nc4",sep="")
        } else if (i <10){
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_",toString(2001+((i-1)*10)),"0101-",
            toString(2010+((i-1)*10)),"1231.nc4",sep="")
        } else{
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_20910101-20991231.nc4",sep="")
        }
        
        ncin <- nc_open(ncname)
        #print(ncin)    
        #get units
        lon <- ncvar_get(ncin,"lon")
        lat <- ncvar_get(ncin,"lat")
        time_tas <- ncvar_get(ncin,"time")
        tunits_tas <- ncatt_get(ncin,"time","units") #"days since 2006-1-1 00:00:00"
        

        numyears <- floor((length(time_tas)/(365)))
        years_tas <- 2006 + (time_tas[time_tas%%365==0])/365
        
        for (d in (1:numyears)){ 
            annual_array <- ncvar_get(ncin,dname, start = c(1,1,(1+(365)*(d-1))), count=c(-1,-1,365)) #start, number in dimension to start. count: how many in that dimension
            annual_mean <- rowMeans(annual_array, dims = 2, na.rm = TRUE)
            annual[,,years_tas[d]-2005] <- annual_mean
            print(d)
        }
        print(i)
    }
    yearly_tas_brick <- brick(annual, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    save(yearly_tas_brick,file="yearly_tas_brick.Rdata")
#Read NetCDF of TAS (close)   
    #load("yearly_tas_brick.Rdata")

#decadal average TAS (start)

    glimpse(yearly_tas_brick)
    dim(yearly_tas_brick)
    var_array <- as.array(yearly_tas_brick)
    numperiods <- 4
    periods <- array(numeric(),c(720,360,numperiods)) 
    dif_periods <- array(numeric(),c(720,360,numperiods-1)) 
    periods_minyear <- c(1,16,36,76) #years 2016,2021,2041,2081
    periods_maxyear <- c(15,35,55,94) #years 2020,2040,2060,2099
        for (d in (1:numperiods)){ 
            period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
            period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
            periods[,,d] <- period_mean
            if (d>1){
                dif_periods[,,d-1] <- periods[,,d] - periods[,,1]
            }
        }
    periods_tas_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    
    dif_periods_tas_brick <- brick(dif_periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    save(periods_tas_brick,file="periods_tas_brick.Rdata")    
    save(dif_periods_tas_brick,file="dif_periods_tas_brick.Rdata")    
    plot(t(dif_periods_tas_brick))


#decadal average TAS (end)



    hist(table(df_fish$decade))
    hist(table(df_fish$decade))
    factor(df_fish$decade)

    ggplot(df_fish[])+
    geom_point(aes(x=decade,y=dif_catch))+ theme(legend.position="none")

    ggplot(data=df_fish,group=name)+
    geom_line(aes(x=decade,y=log(catch),color=name))+
    
        
                    


                
                
                raster::extract(fic)


                     g2 <- as(g, 'Spatial')
                     plot(crop(fishdata,g2))


                    read_stars(fishdata)
                    pnt = st_sample(st_as_sfc(st_bbox(fishdata)), 10)
                    st_extract(g, pnt)
                    
                    plot(geom_iso)
                    crs(geom_iso)
                    crs(periods_brick)
                    stgeom_iso <- st_combine(geom_iso)
                    st_extract(g,fishdata)
                    
                    
                    g<- st_cast(stgeom_iso,"POLYGON")
                    plot(g)
                    class(g)
                    extent(g)
                    st_intersection(fishdata,g)
                    intersect(fishdata,g)
                    
                    
                    st_rasterize(g)
                    st_sf(t(subset(periods_brick,1)))
                    st_crop(t(subset(periods_brick,1)),g)
                    
                    class(stgeom_iso)
                    length(stgeom_iso)
                    plot(stgeom_iso)
                    extent(stgeom_iso)
                    #stgeom_iso <- st_union(geom_iso)
                    
                    crop(t(subset(periods_brick,1)),g)

                    class(geom_iso)
                    
                }
                
                
            }
        }
    }

for (c in 1:length(eez_countries)){
        
    plot(st_geometry(eez_gpkg[eez_gpkg$TERRITORY1=="Italy" | eez_gpkg$TERRITORY2=="Italy",]))

    }

    fp <- 'Data\\eez_v11.gpkg'
    eez_gpkg <- st_read(fp)
    names(eez_gpkg)    
    summary(eez_gpkg)
    head(eez_gpkg)

    plot(st_geometry(eez_gpkg))
    plot(eez_gpkg['TERRITORY1'])



    





    
    plot(eez_gpkg[eez_gpkg$TERRITORY1=="Italy" | eez_gpkg$TERRITORY2=="Italy" ,])
    
    plot(eez_gpkg[eez_gpkg$TERRITORY1=="Italy"  ,])
    plot(eez_gpkg[eez_gpkg$TERRITORY2=="Italy"  ,])



    eez.spatial <- as(eez_gpkg, 'Spatial')
    class(eez.spatial)
    summary(eez.spatial)
    names(eez.spatial)

    plot(eez.spatial[1])

    glimpse(eez.spatial)

    plot(eez_gpkg$geom[5])

    glimpse(eez_gpkg$geom)

    ncname <- paste(dir1,"//ecoocean_gfdl-esm4_nobasd_ssp585_2015soc_default_tc_global_monthly_2015_2100.nc",sep="")
    ncin <- nc_open(ncname)
    #last one is time
    periods_tc_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    #assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), periods_veg_brick)
    plot(t(periods_tc_brick))

    
    
    dif_periods_tc_brick <- brick(dif_periods, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    plot((dif_periods_tc_brick))

    test_spdf <- as( subset(dif_periods_tc_brick,3), "SpatialPixelsDataFrame")
    test_df <- as.data.frame(test_spdf)
    colnames(test_df) <- c("value", "x", "y")

    myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414","#D278E4","#9914B3")
    
    myColors <- c("#a85c41","#bd8c79","#ddcbc4","#f2f1f1","#bdcbd2","#7f9baa","#5c7e91")
    
    #myColors <- c("#ddcbc4","#bd8c79","#a85c41","#080808","#5c7e91","#7f9baa","#bdcbd2")
    
world_coordinates <- map_data("world")

theme_map <- theme(
    #text = element_text(family = "Helvetica", color = "#22211d"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.position="bottom", 
    legend.box = "horizontal",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,-10,-10),
    legend.key.height= unit(0.25, 'cm'),
    legend.key.width= unit(0.75, 'cm') )



    ggplot() +  
    #geom_raster(data=subset(dif_periods_tc_brick,3), aes(x=lon, y=lat, fill=value))+
    geom_tile(data=test_df, aes(x=x, y=y, fill=value*100))+
  ggtitle("Fisheries long-term change (2080-2100 wrt present)")+
  theme()+
  scale_fill_gradientn(guide = guide_legend(title="Total catch change with fixed effort (%)", 
        direction = "horizontal", title.position = "top", 
        title.hjust = 0.5), colours=(myColors), na.value='transparent',
        limits=c(-50, 50) ,oob=squish)+
  coord_equal() +
  theme_map+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))+
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region)
  )

  ggsave("Figures/Fisheries_EcoOceans_totalcatch_longterm.png",dpi=300)


    presentcatch_spdf <- as( t(subset(periods_tc_brick,1)), "SpatialPixelsDataFrame")
    presentcatch_spdf <- as.data.frame(presentcatch_spdf)
    colnames(presentcatch_spdf) <- c("value", "x", "y")

    ggplot() +  
    #geom_raster(data=subset(dif_periods_tc_brick,3), aes(x=lon, y=lat, fill=value))+
    geom_tile(data=presentcatch_spdf, aes(x=x, y=y, fill=value))+
    ggtitle("Fisheries present catch (EcoOceans model)")+
    theme()+
    scale_fill_gradient(guide = guide_legend(title="Total catch (g per m^2)", 
        title.position = "top", 
        title.hjust = 0.5), na.value='transparent',trans="log")+
        #limits=c(-50, 50) ,oob=squish)+
    coord_equal() +
    theme_map+
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))+
    geom_map(
        data = world_coordinates, map = world_coordinates,
        aes(long, lat, map_id = region)
    )
    ggsave("Figures/fisheries_ecoocean_present_totalcatch.png",dpi=300)



dev.new()
  


    
    tc_layer <- as.data.frame(t(subset(dif_periods_tc_brick,3)),xy=TRUE,na.rm=T)
    colnames(tc_layer) <- c("Lon","Lat","Val")
    
#    tc_layer$Val <- sign(tc_layer$Val)*log((abs(tc_layer$Val)+1))
    #myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414","#D278E4","#9914B3")
    
    theme_map <- theme(
    #text = element_text(family = "Helvetica", color = "#22211d"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.position="bottom", 
    legend.box = "horizontal",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-10,-10,-10),
    legend.key.height= unit(0.25, 'cm'),
    legend.key.width= unit(0.75, 'cm') )
    
    worldMap <- shapefile("Data/LandMass Polygon.shp") #Data from: https://github.com/jorgeassis/dataVisualization/tree/master/Projects/seaSurfaceOxygen/Data


    projection <- CRS("+proj=robin +over")

    bb <- sf::st_union(sf::st_make_grid(
    st_bbox(c(xmin = -180,
                xmax = 180,
                ymax = 90,
                ymin = -90), crs = st_crs(4326)), n = 100))
    bb <- st_transform(bb, projection)
    tc_layer1 <- projectRaster(subset(dif_periods_tc_brick,3), crs = projection)
    tc_layer <- mask(tc_layer, as(bb, "Spatial"))
    tc_layer <- as.data.frame(tc_layer,xy=TRUE,na.rm=T)
    colnames(tc_layer) <- c("Lon","Lat","Val")
    worldMap <- spTransform(worldMap, CRSobj = projection)
    worldMap <- gBuffer(worldMap, byid=TRUE, width=0.001)
    worldMap <- crop(worldMap, as(bb, "Spatial"))

    
    writeOGR(worldMap, dsn = '.', layer = 'worldMap_v2', driver = "ESRI Shapefile")
    
    plot <- ggplot() +
    geom_tile(data = tc_layer, aes(x=Lon,y=Lat,fill=Val)) +
    scale_fill_gradientn(guide = guide_legend(title="total catch", 
        direction = "horizontal", title.position = "top", 
        title.hjust = 0.5), colours=rev(myColors), na.value='transparent') +
    geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#A1A1A1", colour = "#A1A1A1" , size=0.25 ) +
    geom_sf(data = bb,fill=NA, colour = "white" , linetype='solid', size=2 ) +
    theme_map
plot



    par(mfrow = c(1, 1))
    









    ###################

    hist(periods_tc_brick)


    
    plot(t(raster(yearly_totalcatch[,,1])))

    
    
    glimpse(yearly)



            glimpse(var_array)
    plot(raster(var_array[,,1]))
            
            
            #years_veg <- 1661 + time_veg



            


    library(RColorBrewer)
    my.palette <- brewer.pal(n = 10, name = "BrBG")

#Read NetCDF of PFTs (start)

    veg_iso_year <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    year=integer())
    pfts <- c("bne","bine","bns","tebs","ibs","tebe","trbe","tribe","trbr","c3g","c4g") 
    clim_models <- c("hadgem2-es","gfdl-esm2m","ipsl-cm5a-lr","miroc5")
    pfts_names <- c("Boreal needleleaved evergreen","Boreal shade intolerant needleleaved evergreen", 
    "Boreal needleleved summergreen (BNS)", 
    "Temperate broadleaved summergreen (TeBS)", 
    "shade intolerant broadleaved summergreen (IBS)", 
    "Temperate broadleved evergreen (TeBE)",
    "Tropical broadleaved evergreen (TrBE)", 
    "Tropical shade intolerant broadleaved evergreen (TrIBE)", 
    "Tropical broadleaved raingreen (TrBR)",
    "C3 grass (C3G)", "C4 grass (C4G)")
    #Boreal needleleaved evergreen (BNE); 
    #Boreal shade intolerant needleleaved evergreen (BINE); 
    #Boreal needleleved summergreen (BNS); 
    #Temperate broadleaved summergreen (TeBS); 
    #shade intolerant broadleaved summergreen (IBS); 
    #Temperate broadleved evergreen (TeBE); 
    #Tropical broadleaved evergreen (TrBE); 
    #Tropical shade intolerant broadleaved evergreen (TrIBE); 
    #Tropical broadleaved raingreen (TrBR); 
    #C3 grass (C3G); C4 grass (C4G); 
    #C3 agricultural grass (C3G_agr); C4 agricultural grass (C4G_agr); 
    #* The last two PFTs are physiologically identical to the previous two but output separately*
    for (climi in 1:length(clim_models)){
        for (p in 1:length(pfts)){
            dname <- paste("pft-",pfts[p],sep="")
            ncname <- paste(dir1,"PFTs_LPJ/lpj-guess_",clim_models[climi],"_ewembi_rcp60_2005soc_co2_pft-",pfts[p],"_global_annual_2006_2099.nc4",sep="")
            ncin <- nc_open(ncname)
            #print(ncin)    
            #get units
            lon <- ncvar_get(ncin,"lon")
            lat <- ncvar_get(ncin,"lat")
            time_veg <- ncvar_get(ncin,"time")
            tunits_veg <- ncatt_get(ncin,"time","units")
            years_veg <- 1661 + time_veg

            
            var_array <- ncvar_get(ncin,dname, start = c(1,1,1), count=c(-1,-1,-1)) #start, number in dimension to start. count: hopw many in that dimension
            plot(var_array)
            #glimpse(var_array)
            #var_array <- var_array[,,16:85] #2021 to 2090
            #numdecades <- floor(dim(var_array)[3]/10)
            #decadal <- array(numeric(),c(720,360,numdecades)) 
            #dif_decadal <- array(numeric(),c(720,360,numdecades-1)) 
            numperiods <- 4
            periods <- array(numeric(),c(720,360,numperiods)) 
            dif_periods <- array(numeric(),c(720,360,numperiods-1)) 
            periods_minyear <- c(1,16,36,76) #years 2016,2021,2041,2081
            periods_maxyear <- c(15,35,55,94) #years 2020,2040,2060,2099
            for (d in (1:numperiods)){ 
                period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
                period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
                periods[,,d] <- period_mean
                if (d>1){
                    dif_periods[,,d-1] <- periods[,,d] - periods[,,1]
                }
            }
            #last one is time
            periods_veg_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
            assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), periods_veg_brick)
            #dif_periods_veg_brick <- brick(dif_periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
            #assign(paste("dif_periods_",pfts[p],"_brick_rcp6",sep=""), dif_periods_veg_brick)
            #years_veg[16:85]
            #plot(t(subset(yearly_veg_brick,1)), main = "Boreal needleleaved evergreen 2021 (% in gridcell)")
            #dpft <- t(subset(decadal_veg_brick,7)) - t(subset(decadal_veg_brick,1))
            #plot( dpft, main = "% change in Boreal needleleaved evergreen \n under rcp6 (2090's-2020's)")

            #yearly_veg_brick_10 <- brick(var_array[,,1:10], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
            save(periods_veg_brick,file=paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6.Rdata",sep=""))
            #save(periods_veg_brick,file=paste("dif_periods_",pfts[p],"_brick_rcp6.Rdata",sep=""))
            print(pfts[p])
        } 
    }   
#Read NetCDF of PFTs (close)

#Read NetCDF of TAS (start)

   
    tas_iso_year <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    year=integer())
    
        annual <- array(numeric(),c(720,360,100)) 
        for (i in 1:10){ #NOTE i starts in 2 (i.e. from the year 2021)
        dname <- "tas"
        if (i==1){
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_20060101-20101231.nc4",sep="")
        } else if (i <10){
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_",toString(2001+((i-1)*10)),"0101-",
            toString(2010+((i-1)*10)),"1231.nc4",sep="")
        } else{
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_20910101-20991231.nc4",sep="")
        }
        
        ncin <- nc_open(ncname)
        #print(ncin)    
        #get units
        lon <- ncvar_get(ncin,"lon")
        lat <- ncvar_get(ncin,"lat")
        time_tas <- ncvar_get(ncin,"time")
        tunits_tas <- ncatt_get(ncin,"time","units") #"days since 2006-1-1 00:00:00"
        

        numyears <- floor((length(time_tas)/(365)))
        years_tas <- 2006 + (time_tas[time_tas%%365==0])/365
        
        for (d in (1:numyears)){ 
            annual_array <- ncvar_get(ncin,dname, start = c(1,1,(1+(365)*(d-1))), count=c(-1,-1,365)) #start, number in dimension to start. count: how many in that dimension
            annual_mean <- rowMeans(annual_array, dims = 2, na.rm = TRUE)
            annual[,,years_tas[d]-2005] <- annual_mean
            print(d)
        }
        print(i)
    }
    yearly_tas_brick <- brick(annual, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    save(yearly_tas_brick,file="yearly_tas_brick.Rdata")
#Read NetCDF of TAS (close)   
    #load("yearly_tas_brick.Rdata")

#decadal average TAS (start)

    glimpse(yearly_tas_brick)
    dim(yearly_tas_brick)
    var_array <- as.array(yearly_tas_brick)
    numperiods <- 4
    periods <- array(numeric(),c(720,360,numperiods)) 
    dif_periods <- array(numeric(),c(720,360,numperiods-1)) 
    periods_minyear <- c(1,16,36,76) #years 2016,2021,2041,2081
    periods_maxyear <- c(15,35,55,94) #years 2020,2040,2060,2099
        for (d in (1:numperiods)){ 
            period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
            period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
            periods[,,d] <- period_mean
            if (d>1){
                dif_periods[,,d-1] <- periods[,,d] - periods[,,1]
            }
        }
    periods_tas_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    
    dif_periods_tas_brick <- brick(dif_periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    save(periods_tas_brick,file="periods_tas_brick.Rdata")    
    save(dif_periods_tas_brick,file="dif_periods_tas_brick.Rdata")    
    plot(t(dif_periods_tas_brick))


#decadal average TAS (end)

#load("periods_tas_brick.Rdata")
#load("dif_periods_tas_brick.Rdata")

# Computing damage in PFTs (start)

        world <- spData::world
        isos <- levels(factor(world$iso_a2))
        df_v <- as.data.frame(periods_tas_brick)
        t <- dif_periods_tas_brick
        t <- t(t)
        plot(mean(t))
        
            for (p in 1:length(pfts)){
                load(paste("dif_periods_",pfts[p],"_brick_rcp6.Rdata",sep=""))
                v1 <- eval(parse(text =paste("dif_periods_",pfts[p],"_brick_rcp6",sep="")))
                
            if (p>1){
                pft_sum <- pft_sum + v1} else {
                    pft_sum <-  v1
                }
                print(p)
            }

            pft_sum <- t(pft_sum)
            a_iso <- area(pft_sum)
            pft_sum_or <- pft_sum
# Computing damage in PFTs (end)

#Plotting maps at different time horizons (start)
            
            crs(pft_sum) <- "+init=EPSG:4326"
            pft_sum <- projectRaster(pft_sum, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            
            
            
            pft_sum_poly_rob1 <- 
            raster::subset(pft_sum,1) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob2 <- 
            raster::subset(pft_sum,2) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob3 <- 
            raster::subset(pft_sum,3) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 
            
            
            world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
            library(wesanderson)
            pal <- palette(brewer.pal(n = 3, name = "Spectral"))

            a <- ggplot() +
            theme_void() +
             geom_sf(data = pft_sum_poly_rob1,aes(fill = layer.1), color = NA)+
            scale_fill_gradientn(colours = pal,limits=c(-20,20),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Near-term (2021-2040)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            b <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob2,aes(fill = layer.2), color = NA) +
            scale_fill_gradientn(colours = pal,limits=c(-20,20),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            #high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Mid-term (2041-2060)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            geom_sf(data = world_coast)+
            #scale_fill_gradient2(midpoint=0, low ="red" , mid = "white",high = "green",na.value="transparent",name="Land cover change (%)")+
            scale_fill_gradientn(colours = pal,limits=c(-20,20),na.value="transparent",name="Land cover change (%)", labels=c("<-20","-10","0","10",">20"),oob = scales::squish)+
            #scale_fill_distiller(palette = "Spectral", direction = 1,name="Land cover change (%)")+
            #scale_fill_npg()+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            leg <- get_legend(c)

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            scale_fill_gradientn(colours = pal,,na.value="transparent",name="Land cover change (%)")+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            ggarrange(a,b,c,ncol=1,nrow=3 ,legend.grob = leg,common.legend = TRUE, legend="bottom")
            ggsave("landcover_change.png",dpi=300)
#plotting maps at different time horizons (end)

#Getting country-level estimate (start)
            world <- spData::world
            world$coefDamage <- NA
            world$meanDamage <- NA
            isos <- world$iso_a2

            all_t <- as.data.frame(t)
            all_a <- as.data.frame(area(t))
            sum(all_t[,1]*all_a[,1])/sum(all_a[,1])
            sum(all_t[,2]*all_a[,1])/sum(all_a[,1])
            sum(all_t[,3]*all_a[,1])/sum(all_a[,1])



            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                if(is.na(isos[j])){next}
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)


                #plot(crop(pft_sum,geom_iso))
                


                veg_iso <- raster::extract(pft_sum_or,geom_iso,metdod='simple', na.rm=TRUE)
                merged_veg_iso <- veg_iso[[1]]
                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                area_iso <- raster::extract(a_iso,geom_iso,metdod='simple', na.rm=TRUE)
                merged_area_iso <- area_iso[[1]]
                    
                        if (length(veg_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_veg_iso <- rbind(merged_veg_iso,veg_iso[[l]])
                                merged_area_iso <- c(merged_area_iso,area_iso[[l]])
                            }
                        }
                veg_iso <- as.data.frame(merged_veg_iso)
                temp_iso <- as.data.frame(merged_temp_iso)
                area_iso <- as.data.frame(merged_area_iso)
                if (sum(complete.cases(veg_iso))<3){next}
                

                num_pix <- dim(veg_iso)[1]
                numyears <- length(veg_iso)
                colnames(veg_iso) <-  c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                veg_iso <- stack(veg_iso)
                veg_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(veg_iso) <- c("pftchange","period","id")

                num_pix <- dim(temp_iso)[1]
                colnames(temp_iso) <-   c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                temp_iso <- stack(temp_iso)
                temp_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(temp_iso) <- c("temp","period","id")

                

                cveg_temp <- cbind(temp_iso,veg_iso[,1])
                colnames(cveg_temp)[4] <- c("pfts_damage")
                cveg_temp$area <- c(area_iso[,1],area_iso[,1],area_iso[,1])
                #glimpse(cveg_temp)
                weightvar <- cveg_temp$area
                model <- felm(pfts_damage~temp|id|0|0,data=cveg_temp, weights = weightvar)
                #library('stargazer')
                #stargazer(model, type = "html", out="Figures/model_mex.html")


                

                ggplot(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period))+
                theme_bw()+
                geom_boxplot(aes(group=period),outlier.shape = NA)+
                geom_point(alpha=0.05)+
                #coord_trans(y="log10") +
                xlab("Gridcell temperature change (C)") +
                ylab("Land cover change (%)") + 
                ggtitle(paste("PFTs change in",world$name_long[j],"under RCP6.0"))
                ggsave(paste("Figures/BoxplotsChange/",isos[j],"_temp_landcover_boxplot.png",sep=""),dpi=300)

                #aggregate(id~period, cveg_temp, function(i) weighted.mean(cveg_temp$temp[i], cveg_temp$area[i]))

                
                #sapply(split(cveg_temp, cveg_temp$period), function(d) weighted.mean(cveg_temp$temp, w = cveg_temp$area))
                
                # library('Hmisc')
                # library('dplyr')
                # wt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(temp, w=area))
                # wsdt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(temp, w=area))
                # wp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                # wsdp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(pfts_damage, w=area))

                # w_mean <- cbind(wt,wsdt[,2]^0.5,wp[,2],wsdp[,2]^0.5)
                # colnames(w_mean) <- c("period","meanT","sdT","meanPFTs","sdPFTs")


                meanP <- plyr::summarize(cveg_temp, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                meanT <- plyr::summarize(cveg_temp, weighted.mean(temp, w=area, na.rm = TRUE))
                
                
                
                # ggplot()+
                # theme_bw()+
                # geom_point(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period),alpha=0.05)+
                # #coord_trans(y="log10") +
                # xlab("Gridcell temperature change (C)") +
                # ylab("Land cover change (%)") + 
                # ggtitle("PFTs change in Mexico under RCP6.0")+
                # geom_point(data=w_mean, aes(x=meanT,y=meanPFTs, color = period))+
                # geom_errorbar(data=w_mean,aes(x = meanT, ymin=meanPFTs-sdPFTs, ymax=meanPFTs+sdPFTs), width=.2,
                #  position=position_dodge(.9))

                world$coefDamage[j] <- summary(model)$coef[1]
                world$meanDamage[j] <- as.double(unlist(meanP/meanT)[1])

            }
            world_damageestimates_pfts <- world
            save(world_damageestimates_pfts,file="world_damageestimates_pfts.Rdata")    
#Getting country-level estimate (end)

#Getting Warming Pattern (start)

            load("world_damageestimates_pfts.Rdata")    
            all_t <- as.data.frame(t)
            all_a <- as.data.frame(area(t))
            Tshort <- sum(all_t[,1]*all_a[,1])/sum(all_a[,1])
            Tmed <- sum(all_t[,2]*all_a[,1])/sum(all_a[,1])
            Tlong <- sum(all_t[,3]*all_a[,1])/sum(all_a[,1])
            isos <- unique(world_damageestimates_pfts$iso_a2)

        for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                if(is.na(isos[j])){next}
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)

                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                area_iso <- raster::extract(a_iso,geom_iso,metdod='simple', na.rm=TRUE)
                merged_area_iso <- area_iso[[1]]
                    
                        if (length(temp_iso)>1){
                            for(l in 2:length(temp_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_area_iso <- c(merged_area_iso,area_iso[[l]])
                            }
                        }
                temp_iso <- as.data.frame(merged_temp_iso)
                area_iso <- as.data.frame(merged_area_iso)
                if (sum(complete.cases(temp_iso))<3){next}

                patternshort <- weighted.mean(x=(temp_iso[,1] / Tshort),w=area_iso[,1])
                patternmed <- weighted.mean(x=(temp_iso[,2] / Tmed),w=area_iso[,1])
                patternlong <- weighted.mean(x=(temp_iso[,3] / Tlong),w=area_iso[,1])

                Tpattern <- mean(patternlong,patternshort,patternmed)

                world_damageestimates_pfts$Tpattern[j] <- Tpattern

            }
            save(world_damageestimates_pfts,file="world_damageestimates_pfts.Rdata")    
#Getting Warming Pattern (start)

# Plot map of estimates 

    ggplot(world_damageestimates_pfts) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = Tpattern)) +
    scale_fill_gradient(name="Temperature pattern (C/C)")+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Country-level temperature pattern by global degree watming")
    ggsave("TempPattern.png",dpi=300)


    ggplot(world_damageestimates_pfts) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = coefDamage)) +
    scale_fill_gradientn(colours = pal,limits=c(-3,3),na.value="transparent",name="Land cover change (%)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Estimated damages based on simulated PFTs")

    ggplot(world_damageestimates_pfts) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = coefDamage*Tpattern)) +
    scale_fill_gradientn(colours = pal,limits=c(-3,3),na.value="transparent",name="Land cover change (%)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Estimated damages based on simulated PFTs")



    w <- st_transform(world_damageestimates_pfts, sp::CRS("+proj=robin +over"))
    ggplot(w) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = coefDamage)) +
    scale_fill_gradientn(colours = pal,limits=c(-2,2),na.value="transparent",name="Estimated change per Global degree C (pp)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Temperature effect on natural land cover area")

    ggsave("Figures/map_estimates_areachange_LocalC.png")

    library("writexl")
    write_xlsx(world_damageestimates_pfts,"world_damageestimates_pfts.xlsx")
# Plot map of estimates



            
            coords <- xyFromCell(pft_sum, seq_len(ncell(pft_sum)))
            pftchange <- stack(as.data.frame(getValues(pft_sum)))
            names(pftchange) <- c('percentage', 'period')
            pftchange <- cbind(coords, pftchange)
            levels(pftchange$period) <- c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")

            ggplot(pftchange) + 
            geom_tile(aes(x, y, fill = percentage)) +
            facet_wrap(~ period) + 
            theme_void()+
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)") +
            coord_equal()+
            ggtitle("PFTs disturbance")+
            guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom"))+
            theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
            
            #ggsave("Figures/PFTs_disturbance_global.png",dpi=300)

            
            library("rnaturalearth")
            library("rnaturalearthdata")
            #world <- ne_countries(scale = "medium", returnclass = "sf", continent != "Antarctica")
            world_coast <- ne_coastline(scale = "small", returnclass = "sf")

            #box = c(xmin = -180, ymin = -60, xmax = 180, ymax = 90)
            #world_coast <- st_crop(world_coast, box)
            
            # pft_sum_poly1 <- 
            # raster::subset(pft_sum,1) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 
            # pft_sum_poly1 <- rotate_sf(pft_sum_poly1)
            # world1 <- rotate_sf(world)

            # pft_sum_poly2 <- 
            # raster::subset(pft_sum,2) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 
            # pft_sum_poly2 <- rotate_sf(pft_sum_poly2,y_add=165)
            # world2 <- rotate_sf(world,y_add=165)

            # pft_sum_poly3 <- 
            # raster::subset(pft_sum,3) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 
            # pft_sum_poly3 <- rotate_sf(pft_sum_poly3,y_add=330)
            # world3 <- rotate_sf(world,y_add=330)
            # # https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/
            
            # ggplot() +
            # theme_void() +
            # geom_sf(data = pft_sum_poly1,aes(fill = layer.1), color = NA) +
            # geom_sf(data = world1, fill=NA,alpha = 0.5)+
            # #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE)+
            
            # geom_sf(data = pft_sum_poly2,aes(fill = layer.2), color = NA) +
            # geom_sf(data = world2, fill=NA,alpha = 0.5)+
            # #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE) +
            # geom_sf(data = pft_sum_poly3,aes(fill = layer.3), color = NA) +
            # geom_sf(data = world3, fill=NA,alpha = 0.5)+
            # scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                 high = "black",na.value="transparent",name="Land cover change (%)") +
            # #scale_fill_distiller(palette = "Reds",direction = 1, title= "PFTs") +
            # annotate(geom="text", x=350, y=-120, label="Near-term (2021-2040)")+
            # annotate(geom="text", x=350, y=60, label="Mid-term (2041-2060)")+
            # annotate(geom="text", x=350, y=220, label="Long-term (2081-2099)") +
            # xlim(c(-260,440)) + 
            # ggtitle("PFTs disturbance under RCP6.0") +
            # theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            # ggsave("Figures/global_pft_change.png",dpi=300)

            

            #Other projections
            #https://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot
            library(rgdal)
            crs(pft_sum) <- "+init=EPSG:4326"
            pft_sum <- projectRaster(pft_sum, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            
            
            
            pft_sum_poly_rob1 <- 
            raster::subset(pft_sum,1) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob2 <- 
            raster::subset(pft_sum,2) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob3 <- 
            raster::subset(pft_sum,3) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 
            #pft_sum_poly1 <- rotate_sf(pft_sum_poly1)
            
            
            world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
            #world_coast <- st_transform(world_coast, crs = st_crs(pft_sum))

            #st_crs(world_coast) <- "+init=EPSG:4326"
            #pft_sum <- projectRaster(pft_sum, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            

            a <- ggplot() +
            theme_void() +
             geom_sf(data = pft_sum_poly_rob1,aes(fill = layer.1), color = NA)+
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Near-term (2021-2040)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            b <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob2,aes(fill = layer.2), color = NA) +
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Mid-term (2041-2060)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            geom_sf(data = world_coast)+scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)")+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            leg <- get_legend(c)

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            library('ggpubr')
            ggarrange(a,b,c,ncol=1,nrow=3 ,legend.grob = leg,common.legend = TRUE, legend="bottom")
            #ggsave("Figures/global_pftchange_temp.png",dpi=300)


            #geom_sf(data = world1, fill=NA,alpha = 0.5)+
            #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE)+
            
            #geom_sf(data = pft_sum_poly_rob2,aes(fill = layer.2), color = NA) +
            #geom_sf(data = world2, fill=NA,alpha = 0.5)+
            #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE) +
            #geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            #geom_sf(data = world3, fill=NA,alpha = 0.5)+
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
             #               high = "black",na.value="transparent",name="Land cover change (%)") +
            #scale_fill_distiller(palette = "Reds",direction = 1, title= "PFTs") +
            #annotate(geom="text", x=350, y=-120, label="Near-term (2021-2040)")+
            #annotate(geom="text", x=350, y=60, label="Mid-term (2041-2060)")+
            #annotate(geom="text", x=350, y=220, label="Long-term (2081-2099)") +
            #xlim(c(-260,440)) + 
            #ggtitle("PFTs disturbance under RCP6.0") +
            #theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            
            
            
            world <- spData::world
            world$coefDamage <- NA
            world$meanDamage <- NA
            #a_iso <- area(pft_sum)
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)


                #plot(crop(pft_sum,geom_iso))
                


                veg_iso <- raster::extract(pft_sum,geom_iso,metdod='simple', na.rm=TRUE)
                merged_veg_iso <- veg_iso[[1]]
                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                area_iso <- raster::extract(a_iso,geom_iso,metdod='simple', na.rm=TRUE)
                merged_area_iso <- area_iso[[1]]
                    
                        if (length(veg_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_veg_iso <- rbind(merged_veg_iso,veg_iso[[l]])
                                merged_area_iso <- rbind(merged_area_iso,area_iso[[l]])
                            }
                        }
                veg_iso <- as.data.frame(merged_veg_iso)
                temp_iso <- as.data.frame(merged_temp_iso)
                area_iso <- as.data.frame(merged_area_iso)
                if (sum(complete.cases(veg_iso))<3){next}
                

                num_pix <- dim(veg_iso)[1]
                numyears <- length(veg_iso)
                colnames(veg_iso) <-  c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                veg_iso <- stack(veg_iso)
                veg_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(veg_iso) <- c("pftchange","period","id")

                
                colnames(temp_iso) <-   c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                temp_iso <- stack(temp_iso)
                temp_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(temp_iso) <- c("temp","period","id")

                

                cveg_temp <- cbind(temp_iso,veg_iso[,1])
                colnames(cveg_temp)[4] <- c("pfts_damage")
                cveg_temp$area <- c(area_iso[,1],area_iso[,1],area_iso[,1])
                #glimpse(cveg_temp)
                weightvar <- cveg_temp$area
                model <- felm(pfts_damage~temp|id|0|0,data=cveg_temp, weights = weightvar)
                #library('stargazer')
                #stargazer(model, type = "html", out="Figures/model_mex.html")




                


                

                # ggplot(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period))+
                # theme_bw()+
                # geom_boxplot(aes(group=period),outlier.shape = NA)+
                # geom_point(alpha=0.2)+
                # coord_trans(y="log10") +
                # xlab("Gridcell temperature change (C)") +
                # ylab("Land cover change (%)") + 
                # ggtitle("PFTs change in Mexico under RCP6.0")
                #ggsave("Figures/mex_temp_landcover_boxplot.png",dpi=300)

                #aggregate(id~period, cveg_temp, function(i) weighted.mean(cveg_temp$temp[i], cveg_temp$area[i]))

                
                #sapply(split(cveg_temp, cveg_temp$period), function(d) weighted.mean(cveg_temp$temp, w = cveg_temp$area))
                
                # library('Hmisc')
                # library('dplyr')
                # wt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(temp, w=area))
                # wsdt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(temp, w=area))
                # wp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                # wsdp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(pfts_damage, w=area))

                # w_mean <- cbind(wt,wsdt[,2]^0.5,wp[,2],wsdp[,2]^0.5)
                # colnames(w_mean) <- c("period","meanT","sdT","meanPFTs","sdPFTs")


                meanP <- plyr::summarize(cveg_temp, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                meanT <- plyr::summarize(cveg_temp, weighted.mean(temp, w=area, na.rm = TRUE))
                
                
                
                # ggplot()+
                # theme_bw()+
                # geom_point(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period),alpha=0.05)+
                # #coord_trans(y="log10") +
                # xlab("Gridcell temperature change (C)") +
                # ylab("Land cover change (%)") + 
                # ggtitle("PFTs change in Mexico under RCP6.0")+
                # geom_point(data=w_mean, aes(x=meanT,y=meanPFTs, color = period))+
                # geom_errorbar(data=w_mean,aes(x = meanT, ymin=meanPFTs-sdPFTs, ymax=meanPFTs+sdPFTs), width=.2,
                #  position=position_dodge(.9))

                world$coefDamage[j] <- summary(model)$coef[1]
                world$meanDamage[j] <- meanP/meanT
                

            }

            
                
            library(ggspatial)
            

            plot(t(pft_sum))
            
            isimip_betas <- data.frame(iso_a2=factor(),coef_y=double())
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                for (p in 1:length(pfts)){
                    
                    v1 <- eval(parse(text =paste("dif_decadal_",pfts[p],"_brick_rcp6",sep="")))
                    iso_v <- crop(v1,geom_iso) # Cropping the gridcells for each country
                    if (p>1){
                        veg_iso_sum <- veg_iso_sum + iso_v} else {
                            veg_iso_sum <- iso_v
                        }

                
                    plot(iso_v)                    
                    
                    plot(crop(v1,geom_iso))
                    
                    

                    plot(v1)
                    plot(crop(v1,geom_iso))
                    dim(crop(v1,geom_iso))
                    c <- crop(v1,geom_iso) + crop(v1,geom_iso)
                    plot(c)
                    #plot(crop(v1_6,geom_iso))
                    v1 <- t(v1)
                    #plot(geom_iso)
                    #plot(v1, add=T)
                    veg_iso <- raster::extract(v1,geom_iso,metdod='simple', na.rm=TRUE)
                    merged_veg_iso <- veg_iso[[1]]
                    
                        if (length(veg_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_veg_iso <- rbind(merged_veg_iso,veg_iso[[l]])
                            }
                        }
                    veg_iso <- abs(as.data.frame(merged_veg_iso))
                    
                    veg_iso[is.na(veg_iso)] = 0
                    if (p>1){
                        veg_iso_sum <- veg_iso_sum + veg_iso} else {
                            veg_iso_sum <- veg_iso
                        }
            }

                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                if (length(temp_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                
                            }
                        }
                
                temp_iso <- as.data.frame(merged_temp_iso)
                if (sum(complete.cases(veg_iso_sum))<3){next}
                

                #glimpse(veg_iso)
                num_pix <- dim(veg_iso_sum)[1]
                numyears <- length(veg_iso_sum)
                colnames(veg_iso_sum) <- c(1:numyears)*10 + 2020
                veg_iso_sum <- stack(veg_iso_sum)
                veg_iso_sum$id <- rep(seq(1:num_pix),numyears)
                colnames(veg_iso_sum) <- c("cveg","year","id")
                #veg_iso$cveg_pct <- veg_iso$cveg / veg_iso$cveg[1:num_pix]

                colnames(temp_iso) <-  c(1:numyears)
                temp_iso <- stack(temp_iso)
                temp_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(temp_iso) <- c("temp","year","id")

                cveg_temp <- cbind(temp_iso,veg_iso_sum[,1]/2) #divided by two to avoid double counting replacement of PFTs in a gridcell
                colnames(cveg_temp)[4] <- c("pfts_damage")
                glimpse(cveg_temp)


                

                ggplot(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id))+
                geom_line()

                #cveg_temp_c <- cveg_temp[is.finite(cveg_temp$cveg_pct),]
                #weightvar <- cveg_temp_c$cveg
                #m <- felm(cveg_pct~temp|id|0|0,data=cveg_temp_c, weights = weightvar)
                #coef_T <- summary(m)$coefficients[1]

                newdata <- data.frame(iso_a2=isos[j],coef_T=coef_T)

        isimip_betas <- rbind(isimip_betas,newdata)
        print(j)
    }
    
    world_betas <- merge(world,isimip_betas)
    
    my.palette <- brewer.pal(n = 10, name = "BrBG")
    library(wesanderson)
    pal <- wes_palette("Zissou1", 100, type = "continuous")
    ggplot(data = world_betas) + 
    theme_bw()+
    geom_sf(aes(fill = coef_T)) +
    ggtitle("Effect of 1C warming on biomass change  (pixel f.e.)")+ 
    scale_fill_continuous_diverging(palette = "Red-Green", 
    l1 = 10, l2 = 100, p1 = 0.1, p2 = 0.1,mid=0#,limits=c(-0.3,0.3)
    )
    ggsave("Figures/tempcoef_cveg_fepix.png",dpi=300)

    scale_fill_gradientn(colors = pal,limits=c(-0.05,0.05))
    
    
    
    
    







# Regression between two rasters (start)
    df_v <- as.data.frame(yearly_veg_brick)
    #for(i in 1:10){
    #  df_v[,10+i]<-df_v[,i] /df_v[,1] #get change w.r.t. year 1
    #}
    #df_v <- df_v[,-c(1:10)]
    colnames(df_v) <- c(2021:2090)
    ddf_v <- stack(df_v)
    ddf_v$id <- rep(seq(1:259200),length(df_v))
    colnames(ddf_v) <- c("cveg","year","id")

    df_t <- as.data.frame(yearly_tas_brick)
    colnames(df_t) <- c(2021:2090)
    ddf_t <- stack(df_t)
    ddf_t$id <- rep(seq(1:259200),length(df_t))
    colnames(ddf_t) <- c("temp","year","id")

    cveg_temp <- cbind(ddf_t,ddf_v$cveg)
    colnames(cveg_temp)[4] <- "cveg"
    #
    glimpse(cveg_temp)
    cveg_temp[c(1:100),]
    cveg_temp$cveg_pct <- cveg_temp$cveg / cveg_temp$cveg[1:259200]
    cveg_temp$temp_inc <- cveg_temp$temp - cveg_temp$temp[1:259200]
    cveg_temp <- cveg_temp[order(cveg_temp$id,cveg_temp$year),]
    cveg_temp$cveg_growth <- (cveg_temp$cveg-lag(cveg_temp$cveg)) / lag(cveg_temp$cveg)
    cveg_temp$temp_inc <- cveg_temp$temp-lag(cveg_temp$temp) 
    cveg_temp <- cveg_temp[which(cveg_temp$year!=2021),]
    #Calculating a global effect of temp on carbon in vegetation 
        cveg_temp_c <- cveg_temp[is.finite(cveg_temp$cveg_growth),]
        
    cveg_temp_c[c(1:100),]
        # Global temp coefficient
        #library('lfe')
        weightvar <- cveg_temp_c$cveg
        m <- felm(cveg_growth~temp_inc|id+year|0|0,data=cveg_temp_c)
        #, weights = weightvar)
        summary(m)    
        # fatal_fe_mod <- plm(cveg ~ temp + I(temp^2), 
        #             data = cveg_temp_c,
        #             index = c("id", "year"), 
        #             model = "within")
        # plot_cme(m,effect="temp",condition="temp")

            #plot quad 
            model <- m
            dataset <- cveg_temp_c
                    Sigma <- vcov(model)
                    coefT <- "temp"
                    start1 <- which(names(coef(model))==coefT)
                    end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
                    
                    sigma = Sigma[c(1:end1),c(1:end1)]
                    beta.hat <- coef(model)[c(1:end1)]
                    x <- seq(from=min(dataset$temp),to=max(dataset$temp), length=20)
                    xmat <- cbind(x, x^2)
                    gestimated <- colSums(beta.hat*t(xmat)) 
                    ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
                    ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))

                    glimpse(dataset)
                    dataset <- data.frame(temp=x,cveg=gestimated,ci12,ci22)
                    
                    cols=c("#7375a4")
                    ggplot(data=dataset, aes(x=temp,y=cveg))+
                    theme_bw()+
                    geom_line(col=cols[1])+
                    geom_ribbon(aes(ymin=ci22,ymax=ci12,x=temp),alpha=0.2,fill=cols[1])+
                    xlab("Temperature")+
                    ylab("Carbon in vegeation (kg/m^2)") + #xlim(-1.2,1.2)+
                    ggtitle("Temperature effect in within-gridcell carbon vegetation")
                    ggsave("Figures/global_temp_cveg_fepix.png",dpi=300)

    #Calculating a global effect of temp on carbon in vegetation 

    # One estimate per pixel
        f <- factor(cveg_temp_c$id)
        length(f) #588900
        #for (j in 1:length(f)){
        for (j in 1:length(f)){
            pix <- cveg_temp_c[which(cveg_temp_c$id==f[j]),]      
            cveg_temp_c$coeff_T[which(cveg_temp_c$id==f[j])]   <- summary(lm(cveg_pct~temp,data=pix))$coefficients[2]
            print(j)
        }

        glimpse(cveg_temp_c)

        #merge back with complete dataset
        cveg_coeffT <- cveg_temp[1:259200,]
        glimpse(cveg_coeffT)
        coef <- cveg_temp_c[cveg_temp_c$year==2006,which(colnames(cveg_temp_c)%in%c("id","coeff_T"))]
        merged_coef <- merge(cveg_coeffT,coef,by="id",all.x=TRUE)
        
    # One estimate per pixel

    # Raster of coefficients (start)


        coord_r <- as.data.frame(yearly_veg_brick,xy=TRUE)[,1:2] #get coordinates
        spg <- cbind(coord_r,merged_coef$coeff_T)
        coordinates(spg) <- ~x+y
        gridded(spg) <- TRUE
        # coerce to raster
        rasterDF <- raster(spg)
        rasterDF
        plot(rasterDF)
    # Raster of coefficients (end)

    





                if(class(veg_iso)=="list"){veg_iso <- unlist(veg_iso)}
                
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                



        colnames(df_v) <- c(2006:2015)
        ddf_v <- stack(df_v)
        ddf_v$id <- rep(seq(1:259200),10)
        colnames(ddf_v) <- c("cveg","year","id")

        df_t <- as.data.frame(yearly_tas_brick)
        colnames(df_t) <- c(2006:2015)
        ddf_t <- stack(df_t)
        ddf_t$id <- rep(seq(1:259200),10)
        colnames(ddf_t) <- c("temp","year","id")

        cveg_temp <- cbind(ddf_t,ddf_v$cveg)
        colnames(cveg_temp)[4] <- "cveg"
        #
        glimpse(cveg_temp)
        cveg_temp$cveg_pct <- cveg_temp$cveg / cveg_temp$cveg[1:259200]
        
        #Calculating a global effect of temp on carbon in vegetation 
            cveg_temp_c <- cveg_temp[is.finite(cveg_temp$cveg_pct),]
            # Global temp coefficient
            #library('lfe')
            m <- felm(cveg_pct~temp|id+year|0|0,data=cveg_temp_c)
            summary(m)    
        #Calculating a global effect of temp on carbon in vegetation 
        numdecades <- 10
        for (ii in(1:numdecades)){
            r <- subset(decadal_brick,ii)
                r <- t(r)
                #r <- flip(r,'y')
                #r <-rotate(r)

                #plot(r)
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                #plot(geom_iso, add = T)               
                
                mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                
                var_iso_decade <- rbind(var_iso_decade,c(isos[j],vars[i],mean_var_iso,2015+(ii*10)))
                
            #print(paste("country",j,isos[j]))

            }
            print(paste("decade",ii))

        }
    # One regression per country (end)


# Get country estimates

    yvb <- yearly_veg_brick <- brick(var_array[,,(1:10)], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    dyveg <- as.data.frame(yvb)
    colnames(dyveg) <- c(2006:2015)
    dyveg$id <- seq(1:259200)
    dd <- stack(dyveg)
    glimpse(dd)
    s <- stack(yearly_tas_brick, yvb)
    v <- data.frame(na.omit(values(s)))
    names(v) <- c('T', 'V')
    m <- lm(V ~ T, data=v)
    summary(m)
# Regression between two rasters (end)
   
    #Extract country-level data
        data(world)
        isos <- levels(factor(world$iso_a2))


        numdecades <- 94
        for (ii in(1:numdecades)){
            r <- subset(decadal_brick,ii)
                r <- t(r)
                #r <- flip(r,'y')
                #r <-rotate(r)

                #plot(r)
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                #plot(geom_iso, add = T)               
                
                mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                
                var_iso_decade <- rbind(var_iso_decade,c(isos[j],vars[i],mean_var_iso,2015+(ii*10)))
                
            #print(paste("country",j,isos[j]))

            }
            print(paste("decade",ii))

        }
        
            
glimpse(var_iso_decade)
names(var_iso_decade) <- c("iso2","var","pct","year")
var_iso_decade$pct <- as.numeric(var_iso_decade$pct)

var_iso_decade$year <- as.numeric(var_iso_decade$year)
#var_iso_decade <- var_iso_decade[which(!is.na(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(!is.nan(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(var_iso_decade$value!=0),]
glimpse(var_iso_decade)
save(var_iso_decade,file="cveg_2005co2_iso_year_jpl.Rdata")
#load("lai_total_iso_year_jpl.Rdata")
ggplot(data=var_iso_decade, aes(x = year, y = pct, group = iso2,color = iso2) )+
geom_line()
#library('lfe')
summary(felm(pct~year|iso2|0|0,data = var_iso_decade))


data(world)
    isos <- levels(factor(world$iso_a2))
    isimip_betas <- data.frame(iso_a2=factor(),
                coef_y=double())
    for (j in 1:length(isos)){
        dat_iso <- var_iso_decade[which(var_iso_decade$iso2==isos[j]),]
        if(sum(is.nan(dat_iso$pct))==dim(dat_iso)[1]){next}
        dat_iso$pct <- 100*dat_iso$pct / dat_iso$pct[1]
        coef_y <- felm(pct~year|0|0|0,data = dat_iso)$coefficients[2]
        
        newdata <- data.frame(iso_a2=isos[j],
                coef_y=coef_y)

        isimip_betas <- rbind(isimip_betas,newdata)
    }
    world_betas <- merge(world,isimip_betas)
    glimpse(world)
    glimpse(world_betas)
    
    ggplot(data = world_betas) + 
    geom_sf(aes(fill = coef_T)) +
    scale_fill_gradientn(colors = my.palette,limits=c(-0.1,0.1))




#for (j in 1:length(isos)){
#    cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
#pftplot <- ggplot(data = var_iso_decade[which(var_iso_decade$iso2==isos[j]),],
#            aes(y =pct, x = year)) +
#             geom_area(alpha=0.6 , size=1, colour="black")+
#             ggtitle(cc)
#    ggsave(pftplot, file=paste("pft/pft_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
#}

#forest <- data.frame(pft = pfts , forest = c(rep("forest",8),rep("other",7)))

#var_iso_decade <- merge(var_iso_decade,forest,by="pft")


#  forest_iso <- aggregate(x = var_iso_decade[c("pct")], 
#                                by =  var_iso_decade[c("iso2", "decade","forest")], 
#                                 function(y){
#                                sum(y)
#                              })
#                              glimpse(forest_iso)

# for (j in 1:length(isos)){
#     cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
# pftplot <- ggplot(data = forest_iso[which(forest_iso$iso2==isos[j]),],
#             aes(y =pct, x = decade, group = forest, fill = forest)) +
#              geom_area(alpha=0.6 , size=1, colour="black")+
#              ggtitle(cc)
#     ggsave(pftplot, file=paste("pft/forest/forest_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
# }


########### Per PIXEL


glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,15] - decadal[lon,lat,pfti,1]
        }
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti <- brick(delta_pfti)
delta_pfti <- flip(t(delta_pfti),'y')
names(delta_pfti) <-  c("NET Temperate", "NET Boreal", "NDT Boreal", "BET Tropical", "BET Temperate",
    "BDT Tropical", "BDT Temperate", "BDT Boreal", "BES Temperate", "BDS Temperate", "BDS Boreal", "c3 arctic gras", "c3 grass", "c4 grass","crop")
plot(delta_pfti)    





glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        losses <- which(forest_loss<0)
        forest_loss <-  forest_loss[losses] 
        delta_forest_abs[lon,lat] <- 100* sum(abs(forest_loss)) / sum(abs(decadal[lon,lat,losses,1]))
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_forest)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "PFT percent change") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of Forest Loss (2015-2100)")
#####



### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- sum( delta_forest[lon,lat,1:8])
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of pixel)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent",limits=c(-80,80)) +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of pixel)")
#####

### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of initial coverage)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of initial coverage)")
#####

### RMSE
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:15){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        rmse<- (sum((delta_forest[lon,lat,1:8])^2))/15
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        #change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- rmse
    }
    print(paste("lon",lon,"/288"))
}
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("RMSE of change in PFTs coverage per gridcell (2020 - 2100)")
#####

#### Changes forest per pixel
glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
delta_pfti_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1]
        }
        delta_pfti_abs[lon,lat] <- sum(abs(delta_pfti[lon,lat,1:dim(decadal)[3]]))/2
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti_abs <- raster(delta_pfti_abs)
delta_pfti_abs <- flip(t(delta_pfti_abs),'y')
plot(delta_pfti_abs, main = "PFT percent change") 
gplot(delta_pfti_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = cbp1, values = c(1.0, 0.05,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of disruption (2015-2100)")

#######





####### Read Land Unit


x <- c('raster', 'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf')
    lapply(x, require, character.only = TRUE)
setwd('C:\\Users\\bastien\\Documents\\GitHub\\vegetation')
dir1 <- "C:/Users/bastien/Box/VegetationData/"
#clm50_r270_1deg_GSWP3V1_iso_SSP3-7_Smooth.clm2.h0.NPP.201501-228212
simname1 <- "clm50_r270_1deg_GSWP3V1_iso_SSP3-7_Smooth.clm2.h0."
simname2 <- ".201501-228212.nc"
vars <- c("PCT_LANDUNIT","TSA","RAIN")
var_iso_decade <- data.frame(iso2=factor(),
                pft = character(),
                var = character(),
                 value=double(),
                 decade=integer())
    i <- 1
    dname <- vars[i]
    ncname <- paste(dir1,simname1,dname,simname2,sep="")
    ncin <- nc_open(ncname)
    print(ncin)    
    #get units
    lon <- ncvar_get(ncin,"lon")
    lat <- ncvar_get(ncin,"lat")
    time <- ncvar_get(ncin,"time")
    tunits <- ncatt_get(ncin,"time","units")

    # days since 2015 01 01
    #365 * 10
    #get variable
    #var_array <- ncvar_get(ncin,dname, start = c(1,1,1,1), count=c(-1,-1,-1,365)) #start, number in dimension to start. count: hopw many in that dimension
    #remove(var_array)
    #last one is time
    dim(var_array)
    numdecades <- 15
    decadal <- array(numeric(),c(288,192,9,numdecades)) 
    for (d in (1:numdecades)){ 
        decade_array <- ncvar_get(ncin,dname, start = c(1,1,1,(120*(d-1)+1)), count=c(-1,-1,-1,(120)))
        decadal_mean <- rowMeans(decade_array, dims = 3, na.rm = TRUE)
        decadal[,,,d] <- decadal_mean
        print(d)
    }
    glimpse(decadal)
    save(decadal,'decadal_pct_landtype.Rdata')
    #load('decadal_pct_pft.Rdata')
    ltypes <- c("Vegetated", "Crop", "Unused", "Ice", "Lake",
    "Wetland", "Urban TBD", "Urban HD", "Urban MD")

 var_iso_decade <- data.frame(iso2=factor(),
                ltype= character(),
                var = character(),
                 value=double(),
                 decade=integer())
    numdecades <- 10
for (jj in 1:9){
    dec <- decadal[,,jj,]
    decadal_brick <- brick(dec, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    #decadal_brick <- brick(dec, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    #decadal_brick <- flip(t((subset(decadal_brick,1))),direction='y')


    #Extract country-level data
        data(world)
        isos <- levels(factor(world$iso_a2))
        
        
        for (ii in(1:numdecades)){
            r <- subset(decadal_brick,ii)
                r <- t(r)
                r <- flip(r,'y')
                r <-raster::rotate(r)

                plot(r)
             for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                #plot(geom_iso, add = T)               
                
                mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                
                var_iso_decade <- rbind(var_iso_decade,c(isos[j],vars[i],ltypes[jj],mean_var_iso,2015+(ii*10)))
                
            #print(paste("country",j,isos[j]))

            }
            print(paste("decade",ii))

        }
        
            print(paste("pft",jj))
    }
glimpse(var_iso_decade)
names(var_iso_decade) <- c("iso2","var","ltype","pct","decade")
var_iso_decade$pct <- as.numeric(var_iso_decade$pct)
var_iso_decade$decade <- as.numeric(var_iso_decade$decade)
#var_iso_decade <- var_iso_decade[which(!is.na(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(!is.nan(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(var_iso_decade$value!=0),]
glimpse(var_iso_decade)
save(var_iso_decade,file="ltype_iso_decade.Rdata")
#var_iso_decade = load("pft_iso_decade.Rdata")
#ggplot(data=var_iso_decade, aes(x = decade, y = value, group = iso2,color = iso2) )+
#geom_line()

for (j in 1:length(isos)){
    cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
pftplot <- ggplot(data = var_iso_decade[which(var_iso_decade$iso2==isos[j]),],
            aes(y =pct, x = decade, group = ltype, fill = ltype)) +
             geom_area(alpha=0.6 , size=1, colour="black")+
             ggtitle(cc)
    ggsave(pftplot, file=paste("ltype/ltype_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
}

forest <- data.frame(pft = pfts , forest = c(rep("forest",8),rep("other",7)))

var_iso_decade <- merge(var_iso_decade,forest,by="pft")


 forest_iso <- aggregate(x = var_iso_decade[c("pct")], 
                               by =  var_iso_decade[c("iso2", "decade","forest")], 
                                function(y){
                               sum(y)
                             })
                             glimpse(forest_iso)

for (j in 1:length(isos)){
    cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
pftplot <- ggplot(data = forest_iso[which(forest_iso$iso2==isos[j]),],
            aes(y =pct, x = decade, group = forest, fill = forest)) +
             geom_area(alpha=0.6 , size=1, colour="black")+
             ggtitle(cc)
    ggsave(pftplot, file=paste("pft/forest/forest_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
}


########### Per PIXEL


glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,15] - decadal[lon,lat,pfti,1]
        }
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti <- brick(delta_pfti)
delta_pfti <- flip(t(delta_pfti),'y')
names(delta_pfti) <-  c("NET Temperate", "NET Boreal", "NDT Boreal", "BET Tropical", "BET Temperate",
    "BDT Tropical", "BDT Temperate", "BDT Boreal", "BES Temperate", "BDS Temperate", "BDS Boreal", "c3 arctic gras", "c3 grass", "c4 grass","crop")
plot(delta_pfti)    





glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        losses <- which(forest_loss<0)
        forest_loss <-  forest_loss[losses] 
        delta_forest_abs[lon,lat] <- 100* sum(abs(forest_loss)) / sum(abs(decadal[lon,lat,losses,1]))
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_forest)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "PFT percent change") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of Forest Loss (2015-2100)")
#####



### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- sum( delta_forest[lon,lat,1:8])
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of pixel)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent",limits=c(-80,80)) +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of pixel)")
#####

### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of initial coverage)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of initial coverage)")
#####

### RMSE
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:15){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        rmse<- (sum((delta_forest[lon,lat,1:8])^2))/15
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        #change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- rmse
    }
    print(paste("lon",lon,"/288"))
}
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("RMSE of change in PFTs coverage per gridcell (2020 - 2100)")
#####

#### Changes forest per pixel
glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
delta_pfti_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1]
        }
        delta_pfti_abs[lon,lat] <- sum(abs(delta_pfti[lon,lat,1:dim(decadal)[3]]))/2
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti_abs <- raster(delta_pfti_abs)
delta_pfti_abs <- flip(t(delta_pfti_abs),'y')
plot(delta_pfti_abs, main = "PFT percent change") 
gplot(delta_pfti_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = cbp1, values = c(1.0, 0.05,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of disruption (2015-2100)")

#######

#### Read Land Unit

aggregate(delta_pfti,)

  cbp1 <- c( "#0072B2","#009E73","#E69F00")
gplot(delta_pfti) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = c('green','red'), values = c(1.0, 0.51, 0.49, 0),na.value="transparent") +
  coord_equal() + theme_bw()

ggplot() +
  geom_raster(data = delta_pfti , aes(x = lon, y = lat, fill = BES.Temperate)) + 
  coord_quickmap()

ggplot(data = delta_pfti) +
    geom_raster() +
    geom_sf(data = dam_world, aes(fill = sqthreedegdam)) +
    scale_fill_gradient2(low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill")
 ggplot(data = delta_pfti[which(delta_pfti$iso2==isos[j]),],
            aes(y =pct, x = decade, group = forest, fill = forest)) +
             geom_area(alpha=0.6 , size=1, colour="black")+
             ggtitle(cc)

###END
















            #geom_line()
            geom_line(aes(color=value),position = position_stack(reverse = TRUE))+theme_bw()
            
            (aes(fill = pft), width = 1/12)

ggplot(var_iso_decade[which(var_iso_decade$iso2==isos[j]),],
            aes(y = value, axis1 = pft, axis2 = decade)) +
            geom_alluvium(aes(fill = pft), width = 1/12) +
            geom_stratum(width = 1/12, fill = "black", color = "grey") +
            geom_label(stat = "stratum", infer.label = TRUE) +
            scale_x_discrete(limits = c("Category","General"), expand = c(.05, .05)) +
            #scale_fill_brewer(type = "qual", palette = "Set2") +
            ggtitle("Data acquisition")
            #ggsave("Data_acquisition_flow2.png", path="C:/Users/bastien/Documents/Meta analysis Ecosystem Services/figures", dpi=600)



# Preindustrial levels (start)
    #clm50_cesm201R_1deg_GSWP3V1_phsoff_hist.clm2.h0.RAIN.185001-201412
    
    simname1 <- "clm50_cesm201R_1deg_GSWP3V1_phsoff_hist.clm2.h0."
    simname2 <- ".185001-201412.nc"
    vars <- c("NPP","TSA","RAIN")
    var_iso_preind <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    decade=integer())
    for (i in 1:3){
        dname <- vars[i]
        ncname <- paste(dir1,simname1,dname,simname2,sep="")
        ncin <- nc_open(ncname)

        #get units
        lon <- ncvar_get(ncin,"lon")
        lat <- ncvar_get(ncin,"lat")
        time <- ncvar_get(ncin,"time")
        tunits <- ncatt_get(ncin,"time","units")

        #get variable
        var_array <- ncvar_get(ncin,dname)
        dlname <- ncatt_get(ncin,dname,"long_name")
        dunits <- ncatt_get(ncin,dname,"units")
        fillvalue <- ncatt_get(ncin,dname,"_FillValue")
        nc_close(ncin) 
        var_array[var_array == fillvalue$value] <- NA

        #get array of 1850-1860
        
            decade_array <- var_array[,,1:(10*12)] #1850 to 1860
            decadal_mean <- rowMeans(decade_array, dims = 2, na.rm = TRUE)
            decadal <- decadal_mean
        
        decadal_brick <- raster(decadal, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
        #decadal_brick <- brick(decadal, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
        #decadal_brick <- flip(t((subset(decadal_brick,1))),direction='y')


        #Extract country-level data
            data(world)
            isos <- levels(factor(world$iso_a2))
        
                    r <- t(decadal_brick)
                    r <- flip(r,'y')
                    r <-rotate(r)

                    #plot(r)
                for (j in 1:length(isos)){
                    geom_iso <- world$geom[world$iso_a2==isos[j]]
                    geom_iso <- st_cast(geom_iso, "POLYGON")
                    geom_iso <-as_Spatial(geom_iso)
                    
                    #plot(geom_iso, add = T)               
                    
                    mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                    if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                    if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                    if(is.null(mean_var_iso)){mean_var_iso <- NA}
                    
                    #if(vars[i]=="NPP"){var_iso_preind <- rbind(var_iso_preind,c(isos[j],"C13_NPP",mean_var_iso,1850))}else{
                        var_iso_preind <- rbind(var_iso_preind,c(isos[j],vars[i],mean_var_iso,1850))
                    #}
                }
                

            
    }

# Preindustrial levels (end)
glimpse(var_iso_preind)
names(var_iso_preind) <- c("iso2","var","value","decade")


#get first decade ref value

var_iso_preind$isovar <- paste(var_iso_preind$iso2,var_iso_preind$var,sep="")
glimpse(var_iso_preind)
var_iso_preind <- var_iso_preind[,which(names(var_iso_preind)%in%c("value","isovar"))]
names(var_iso_preind) <- c("refvalue","isovar")

#merge with dataset
var_iso_decade$isovar <- paste(var_iso_decade$iso2,var_iso_decade$var,sep="")
glimpse(var_iso_decade)
dam_df <- merge(var_iso_decade,var_iso_preind, by="isovar")
glimpse(dam_df)
dam_df$refvalue <- as.double(dam_df$refvalue)
dam_df$value_change <- dam_df$value - dam_df$refvalue
dam_df$value_change_pct <- log(dam_df$value / dam_df$refvalue)*100


ggplot(data = dam_df[which(dam_df$var=="NPP"),], aes(x=value,y=value_change_pct,color=iso2))+
geom_line()+
theme_bw()

dam_df$isodecade <- paste(dam_df$iso2,dam_df$decade,sep="")
npp_df <- dam_df[which(dam_df$var=="NPP"),]
names(npp_df)[4] <- "npp"
names(npp_df)[7] <- "npp_change"
names(npp_df)[8] <- "npp_change_pct"
glimpse(npp_df)
tsa_df <- dam_df[which(dam_df$var=="TSA"),]
names(tsa_df)[4] <- "tsa"
names(tsa_df)[7] <- "tsa_change"
names(tsa_df)[8] <- "tsa_change_pct"
rain_df <- dam_df[which(dam_df$var=="RAIN"),]
names(rain_df)[4] <- "rain"
names(rain_df)[7] <- "rain_change"
names(rain_df)[8] <- "rain_change_pct"
dam <- merge(npp_df,tsa_df,by="isodecade")
dam <- merge(dam,rain_df,by="isodecade")
glimpse(dam)


ggplot(data=dam, aes(x=tsa_change,y=npp_change_pct,colour=iso2))+geom_line()

mod_all <- felm(npp_change_pct~tsa_change_pct+I(tsa_change_pct^2)+rain_change|iso2+decade|0|0,data=dam[which(is.finite(dam$npp_change_pct)),])
summary(mod_all)
save(dam,file='npp_clim_iso.Rdata')
dam_coef <- data.frame(iso2=factor(),
                sqtemp_coef = double(),
                 sqtemp_sd=double(),
                sqtemp2_coef = double(),
                 sqtemp2_sd=double(),
                temp_coef = double(),
                 temp_sd=double(),
                 iso2=character())
for(i in 1:length(isos)){
    d <- dam[which(dam$iso2==isos[i]),]
    if(sum(is.na(d$npp_change_pct))==length(d$npp_change_pct)){
        print(isos[i])
        print(i)
        next}
    d <- d[which(is.finite(d$npp_change_pct)),]

    mod1 <- lm(npp_change_pct ~ 0 + tsa_change + I(tsa_change^2) + rain_change + I(rain_change^2) + decade, data=d )
    sqtemp_coef <- summary(mod1)$coefficients[1,1]
    sqtemp_sd <- summary(mod1)$coefficients[1,2]
    sqtemp2_coef <- summary(mod1)$coefficients[2,1]
    sqtemp2_sd <- summary(mod1)$coefficients[2,2]
    
    mod1 <- lm(npp_change_pct ~ tsa + rain_change + decade, data=d )
    temp_coef <- summary(mod1)$coefficients[1,1]
    temp_sd <- summary(mod1)$coefficients[1,2]    

    dam_coef <- rbind(dam_coef,c(sqtemp_coef,sqtemp_sd,sqtemp2_coef,sqtemp2_sd,temp_coef,temp_sd,isos[i]))

}
names(dam_coef) <- c("sqtemp_coef","sqtemp_sd","sqtemp2_coef","sqtemp2_sd","temp_coef","temp_sd","iso_a2")
save(dam_coef,file='dam_coefs.rdata')
glimpse(dam_coef)


glimpse(world)
dam_world <- merge(world,dam_coef,by="iso_a2")
dam_world$temp_coef <- as.double(dam_world$temp_coef)
glimpse(dam_world)
dam_world$sqthreedegdam <- as.double(dam_world$sqtemp_coef)*3 + as.double(dam_world$sqtemp2_coef)*9

dam_world$sqthreedegdam[which(dam_world$sqthreedegdam==max(dam_world$sqthreedegdam, na.rm=TRUE))] <- NA

ggplot(data = dam_world) +
    geom_sf() +
    geom_sf(data = dam_world, aes(fill = sqthreedegdam)) +
    scale_fill_gradient2(low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill")

names(dam)[which(names(dam)=="iso2.x")] <- "iso_a2"
dam_world <- merge(dam_world,dam, by = "iso_a2")

glimpse(dam_world)
ggplot(data = dam_world[which(dam_world$decade==2025),]) +
    geom_sf() +
    geom_sf(data = dam_world, aes(fill = tsa_change)) +
    scale_fill_gradient2(low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill")

ggplot(data=dam_world, aes(y=tsa_change,x=decade, color=iso_a2))+geom_line()
ggplot(data=dam_world, aes(y=npp_change_pct,x=decade, color=iso_a2))+geom_line()
#remove outliers
#dam_world$temp_coef[which(dam_world$temp_coef==min(dam_world$temp_coef, na.rm=TRUE))] <- NA
hist(dam_world$temp_coef)

glimpse(dam_world)
library(rworldmap)
library(ggplot2)
map.world <- map_data(map="world")
glimpse(map.world)

gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=dam_world, map=dam_world, aes(map_id=region, x=long, y=lat, fill=temp_coef))

gg <- gg + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
gg <- gg + coord_equal()
gg

plot(dam_world[15])

geom_iso <- world



ggplot(data = dam, aes(x=tsa_change,y=npp_change_pct,color=iso2))+
geom_line()



glimpse(a)
glimpse(npp_df)
#order columns by country, then by decade
var_iso_decade <- var_iso_decade[order(var_iso_decade[,1], var_iso_decade[,4]),]
dam_df <- var_iso_decade[,]
for (k in (1:length(isos))){
    dam_df <- var_iso_decade[which(var_iso_decade$iso2==isos[k]),]
    dam_df 
}
#How to take the difference with respect to baseline?
#What baseline? I can use the first decade for now



class(r)
glimpse(r)
plot(world$geom[world$iso_a2==isos[i]])
class(r)
glimpse(r)
glimpse(geom_iso[[1]][[1]])


#plot(decadal_brick)


class(var_array)
#npp.slice <- npp_array[, , 1] 
#dim(npp.slice)
rr <- brick(npp_array)
r <- raster(t(npp.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)


#Now temperature
ncname <- paste(dir1,"clm50_r270_1deg_GSWP3V1_iso_SSP3-7_Smooth.clm2.h0.TSA.201501-228212.nc",sep="")
ncin <- nc_open(ncname)
glimpse(ncin)
#get units
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
tunits
#get variable
dname <- "TSA"
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(npp_array)
nc_close(ncin) 
tmp_array[tmp_array == fillvalue$value] <- NA
tmp_array <- tmp_array - 273.15
tmp_slice <- tmp_array[, , 1] 
dim(tmp_slice)
r <- raster(t(tmp_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)

years <- rep(2015:2282,each = 12)
months <- rep(1:12,268)
damage_coef <- tmp_array[, , 1] 
damage_pval <- tmp_array[, , 1] 

dim(damage_coef)
le <- length(npp_cell)
for (j in 1:288){
    for (k in 1:192){
        tmp_cell <- tmp_array[j,k , ] 
        npp_cell <- npp_array[j,k , ] 
        if(all(is.na(tmp_cell))){next}
        #range(tmp_cell)
        
        npp_df <- data.frame(npp_cell,years)
        npp_yearly <- aggregate(.~years, data=npp_df, mean, na.rm=TRUE)
        
        tmp_df <- data.frame(tmp_cell,years)
        tmp_yearly <- aggregate(.~years, data=tmp_df, mean, na.rm=TRUE)
        npptmp <- data.frame(npp_yearly, tmp_yearly)

        npptmp_g <- npptmp[1:(dim(npptmp)[1]-1),]
        npptmp_g$npp_g <- npptmp$npp_cell[2:length(npptmp)] - npptmp$npp_cell[1:(length(npptmp)-1)]
        npptmp_g$tmp_g <- npptmp$tmp_cell[2:length(npptmp)] - npptmp$tmp_cell[1:(length(npptmp)-1)]
        
        damage <- lm(npp_g ~ tmp_cell + years, data = npptmp_g)
        damage <- lm(npp_g ~ tmp_cell + I(tmp_cell^2)+ years, data = npptmp_g)
        #summary(damage)

        damage_coef[j,k] <- damage$coefficients[2]
}}
save(damage_coef, file="damage_matrix.Rdata")
rtemp <- raster(t(damage_coef), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rtemp <- flip(rtemp, direction='y')
plot(rtemp)

npp_2015 <- apply(npp_array[,,1020:1032], c(1,2), mean)
rnpp <- raster(t(npp_2015), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rnpp <- flip(rnpp, direction='y')
nppdf <- as.data.frame(rnpp, xy = TRUE)
names(nppdf) <- c("lon", "lat", "gC13/m^2/s")
nppdf <- nppdf[which(!is.na(nppdf[,3])),]
npp_map <- ggplot()+ 
theme_bw()+
geom_raster(data=nppdf, aes(x=lon,y=lat, fill=`gC13/m^2/s`)) +
coord_quickmap()+
scale_fill_viridis_c(option = "D")+ 
ggtitle('Yearly avg. NPP (2100)')
npp_map
glimpse(nppdf)

dim(tmp_array)
tmp_2100 <- apply(tmp_array[,,1020:1032], c(1,2), mean)
rtmp <- raster(t(tmp_2100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rtmp <- flip(rtmp, direction='y')
tmpdf <- as.data.frame(rtmp, xy = TRUE)
names(tmpdf) <- c("lon", "lat", "C")
tmpdf <- tmpdf[which(!is.na(tmpdf[,3])),]
tmp_map <- ggplot()+ 
theme_bw()+
geom_raster(data=tmpdf, aes(x=lon,y=lat, fill=C)) +
coord_quickmap()+
scale_fill_distiller(palette = "Spectral")+
ggtitle('Yearly avg. tmp (2100)')
tmp_map
glimpse(tmpdf)


glimpse(dam)
#gC13/m^2/s
damage_r <- log(rtemp/rnpp) #percent change
damage_r[damage_r==Inf] <- NA 
damage_r[damage_r==-Inf] <- NA 
plot(damage_r)
class(damage_r)
dam <- as.data.frame(damage_r, xy = TRUE)
names(dam) <- c("lon", "lat", "percent change")
dam <- dam[which(!is.na(dam[,3])),]
glimpse(dam)
world <- map_data("world")
npp_change_map <- ggplot()+ 
theme_bw()+
geom_raster(data=dam, aes(x=lon,y=lat, fill=`percent change`)) +
coord_quickmap()+
scale_fill_continuous_diverging(palette = "Blue-Red", trans = "reverse")+
ggtitle('Warming effect on NPP')

npp_change_map

ggarrange(ggarrange(npp_map,tmp_map),npp_change_map,nrow=2)
library('ggpubr')
scale_fill_viridis_c(option="B")
npp_change_map

scale_color_brewer(palette = "Spectral")
scale_fill_continuous(low="red", high="green", 
                       guide="colorbar",na.value="white") + 
ggtitle('Warming effect on NPP')

npp_change_map





scale_fill_viridis() 
scale_fill_viridis(option = "magma", direction = -1)
scale_fill_continuous(low="darkred", high="green", 
                       guide="colorbar",na.value="white")
scale_fill_viridis_c() 

class(npp_array)
mean(npp_array[,,1:12], na.rm=TRUE)
dim(npp_array)
 plot(r)

 plot(rtemp)
 plot(r/rtemp)
 plot(log(rtemp/r))
 
 class(damage_coef)
 class(tmp_array)
#damage_pval[j,k] <- damage$coefficients[1]

glimpse(damage)
glimpse(veg1[[2]][[1]][[1]])




#################### EXTRACT WORLD
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  data(world)
  # or
  world <- st_read(system.file("shapes/world.gpkg", package="spData"))

  plot(world)
                        }

isos <- levels(factor(world$iso_a2))
i <- 1

geom_iso <- world$geom[world$iso_a2==isos[i]]
a <- st_cast(geom_iso, "POLYGON")
a <-as_Spatial(a)
raster::extract(r,a,metdod='simple')
class(r)
glimpse(r)
plot(world$geom[world$iso_a2==isos[i]])
class(r)
glimpse(r)
glimpse(geom_iso[[1]][[1]])
