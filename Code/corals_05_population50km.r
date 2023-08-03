#Calculate the population within 50km of each coral patch
#setup#

    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales')
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
    dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\'
    
#setup#

# Load key data
    #   load shapefile   
    coral_areas <- st_read("Data/corals/coral_area_with_mean_t2coeff_filtered.shp")
    #   load temperature projections
    gdp_raster <- raster("C:/Users/basti/Box/Data/GDP/GDP2005_1km.tif")
    
# Load key data

# glimpse

    class(coral_areas)
    glimpse(coral_areas)
    
    class(gdp_raster)
    glimpse(gdp_raster)    

    plot(gdp_raster)
    plot(coral_areas[c(1:2),])

    crs(gdp_raster)

    gdp_coral <- raster::extract(gdp_raster, coral_areas, buffer = 50000, fun = sum) #double check GDP units to know whether  should get mean or sum
    hist(log(gdp_coral[which(gdp_coral>10)]))
    coral_areas$gdp <- gdp_coral
    st_write(coral_areas,"Data/corals/coral_area_with_mean_t2coeff_filtered.shp")
    
    # 