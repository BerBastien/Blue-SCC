x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales')
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")
datadir <- "G:\\My Drive\\Fishmip_BOATS\\Profits\\"

ssp <- "ssp126"
nc_data <- nc_open(paste0(datadir,ssp,"\\Fishmip_Futur_trev.nc"))
print(nc_data)



    fp <- 'Data\\eez_v11.gpkg'
    eez_gpkg <- st_read(fp)
    eez.spatial <- as(eez_gpkg, 'Spatial')
    eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
    class(eez_gpkg)
    glimpse(eez_gpkg)




        clim_scen <- c("ssp126","ssp585","picontrol")
        period <- c("Futur","Hist")
        vars <- c("tc","tcos","trev")
        clim_scen_i <- 1
        periodi <- 1
        vari <- 3

        for (clim_scen_i in length(clim_scen)){
            for(vari in 1:length(vars)){
                for(periodi in 1:length(period)){

                    ncname <- paste(datadir,
                        clim_scen[clim_scen_i],"\\Fishmip_",period[periodi],"_",
                        vars[vari],".nc",sep="")
                    ncin <- nc_open(ncname)
                    lon <- ncvar_get(ncin,"lon")
                    lat <- ncvar_get(ncin,"lat")
                    time<- ncvar_get(ncin,"time")
                    tunits <- ncatt_get(ncin,"time","units")
                    dname <- vars[vari]
                    
                    var_array <- ncvar_get(ncin,dname, start = c(1,1,1), count=c(-1,-1,-1)) #start, number in dimension to start. count: hopw many in that dimension
        
                    time_y <- 2014 + seq(1: ( dim(var_array)[3])) 
                    numperiods <- 8
                    periods <- array(numeric(),c(360,180,numperiods)) 
                    dif_periods <- array(numeric(),c(360,180,numperiods-1)) 
                    periods_minyear <- c(1,17,27,37,47,57,67,77) #years 2015,2031,2041,2051,2061,2071,2081,2091
                    periods_maxyear <- c(11,26,36,46,56,66,76,86) #years 2025,2040,2060,2099
                    for (d in (1:numperiods)){ 
                        period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
                        period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
                        #period_mean[period_mean>1000] <- NaN 
                        periods[,,d] <- as.numeric(period_mean)
                        if (d>1){
                            dif_periods[,,d-1] <- 100*((periods[,,d] - periods[,,1])/periods[,,1])
                        }
                    }
                    periods_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
                    dif_periods_brick <- brick(dif_periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
                    #plot( (subset(dif_periods_brick,7)), main =paste("Percent Change in", vars[vari],"\n under ",clim_scen[clim_scen_i]," (2090's-2010's)"))
                    
                    df <- raster::as.data.frame(t(subset(periods_brick,8)), xy = TRUE)
                    ggplot(data = df, aes(x = x, y = y, fill = layer)) +
                        geom_tile() +
                        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
                        coord_fixed() +
                        labs(fill = "Value", x = "Longitude", y = "Latitude") +
                        theme_minimal() +
                        labs(title=paste0("Variable ",vars[vari],"\n under ",clim_scen[clim_scen_i]," (2090 decade)"))
                    
                    df <- raster::as.data.frame(t(subset(dif_periods_brick,7)), xy = TRUE)
                    glimpse(df)
                    ggplot(data = df, aes(x = x, y = y, fill = layer)) +
                        geom_tile() +
                        scale_fill_gradient2(low = "indianred", mid = "white", high = "darkcyan", midpoint = 0) +
                        coord_fixed() +
                        labs(fill = "Value", x = "Longitude", y = "Latitude") +
                        theme_minimal() +
                        labs(title=paste0("Percent Change in ", vars[vari],"\n under ",clim_scen[clim_scen_i]," (2090's-2010's)"))
                    
                    
                    glimpse(dif_periods_brick)
                    save(periods_brick,file=paste("Data\\intermediate_output\\BOATS\\decade",vars[vari],clim_scen[clim_scen_i],period[periodi],".Rdata",sep="_"))
                    save(dif_periods_brick,file=paste("Data\\intermediate_output\\BOATS\\decade",vars[vari],clim_scen[clim_scen_i],period[periodi],".Rdata",sep=""))
                    
                }
            }
        }