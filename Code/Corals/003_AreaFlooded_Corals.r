#setup
library(rgdal)
library(sf)
library(dplyr)

setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

filename <- "C:\\Users\\basti\\Box\\Data\\Oceans\\coral_protection_benefits\\Annual_Expected_Benefit_05.31.18\\AEB_Coral.gdb"
aeb <- st_read(dsn = filename, layer = "AEB_Coral")
glimpse(aeb)


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


    coral_areas_wgs84_2_joined <- coral_areas_wgs84 %>%
        st_join(aeb) %>%
        group_by(id) %>%
        summarise(surveys = ifelse(!is.na(A_km2),n(),0),
                    mean_A_km2 = mean(A_km2, na.rm = TRUE),
                    mean_P = mean(P, na.rm = TRUE),
                    mean_BC_USD11 = mean(BC_USD11, na.rm = TRUE),
                    mean_BCD50_USD1 = mean(BCD50_USD1, na.rm = TRUE),
                    geometry = geometry)
            
    
    coral_areas_wgs84_2_joined <- coral_areas_wgs84_2_joined[!duplicated(coral_areas_wgs84_2_joined$id), ]
    glimpse(coral_areas_wgs84_2_joined)

    no_surveys_polygons <- coral_areas_wgs84_2_joined %>%
        filter(surveys == 0)

        # For polygons with no surveys, find the closest three points
        # and calculate the mean values
        glimpse(aeb)
        closest_mean_coef <- no_surveys_polygons %>%
        st_distance(aeb) %>%
        apply(1, function(x) {
            idx <- order(x)[1:3]
            mean_A_km2 <- mean(aeb$A_km2[idx])
            mean_P <- mean(aeb$P[idx])
            mean_BC_USD11 <- mean(aeb$BC_USD11[idx])
            mean_BCD50_USD1 = mean(aeb$BCD50_USD1[idx])
            return(c(mean_A_km2, mean_P,mean_BC_USD11,mean_BCD50_USD1))
        })  %>%
        t() %>%
        as.data.frame() %>%
        setNames(c("mean_A_km2", "mean_P","mean_BC_USD11","mean_BCD50_USD1"))

    glimpse(closest_mean_coef)
     coral_areas_wgs84_2_joined[which(coral_areas_wgs84_2_joined$surveys == 0), c(3,4,5,6)] <- closest_mean_coef
    glimpse(coral_areas_wgs84_2_joined)
    save(coral_areas_wgs84_2_joined,file="Data/modules/corals/flooded_corals.Rds")

        
        

    



# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))

fc_list <- ogrListLayers(filename)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=filename,layer="AEB_Coral")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)
