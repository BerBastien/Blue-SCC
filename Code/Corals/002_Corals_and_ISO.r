
coral_country_file <- 'Data\\output_modules_input_rice50x\\output_modules\\corals\\coral_country.Rds'

# Check if the valid geometry file already exists
if (file.exists(coral_country_file)) {
            
    load(coral_country_file)}else{

    

        #setup 

            dir1 <- 'Data\\corals\\'
            
        #setup 

        #Join with countries 

        dir_wcmc <- paste0(dir_box,"Data\\Oceans\\coral_extent\\14_001_WCMC008_CoralReefs2021_v4_1\\01_Data")
        v4_coral_py <- sf::st_read(dsn = file.path(dir_wcmc), layer = "WCMC008_CoralReef2021_Py_v4_1")
        v4_coral_py <- st_make_valid(v4_coral_py)
        v4_coral_py$id <- seq(1:dim(v4_coral_py)[1])
        glimpse(v4_coral_py)
        #coral_countries <- v4_coral_py %>% st_drop_geometry()
        #glimpse(coral_countries)

            fp <- 'Data/other/eez_v11.gpkg'
            valid_fp <- 'Data/other/eez_v11_valid.gpkg'

            # Check if the valid geometry file already exists
            if (file.exists(valid_fp)) {
            # Load the valid geometry
            eez_gpkg <- st_read(valid_fp)
            } else {
            # Read the original file
            eez_gpkg <- st_read(fp)
            # Make the geometry valid
            eez_gpkg <- st_make_valid(eez_gpkg)
            # Save the valid geometry to a new file
            st_write(eez_gpkg, valid_fp)
            }

            # Convert to Spatial object and get the levels
            eez.spatial <- as(eez_gpkg, 'Spatial')
            eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
        
        coral_country <- v4_coral_py %>%
            st_join(eez_gpkg) %>%
            mutate(countrycode = ISO_SOV1)
            
        glimpse(coral_country)
        save(coral_country,file="Data\\output_modules_input_rice50x\\output_modules\\corals\\coral_country.Rds")
    }