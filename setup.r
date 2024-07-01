## Set-up File

## Libraries (start)
    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',"reshape",
        'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
        'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes',
        "exactextractr","WDI","ggrepel","viridis","scico","scales","stringr","patchwork",
        "countrycode","purrr","tidyr","rlang","rnaturalearthdata","ggalluvial","svglite")
    
    lapply(x, require, character.only = TRUE)



## Libraries (end)

## Dir (start)
    script_path <- here::here()
    setwd(script_path)

    #external directories for datasets
    dir_wcmc <- "C:\\Users\\basti\\Box\\Data\\Oceans\\coral_extent\\14_001_WCMC008_CoralReefs2021_v4_1\\01_Data"
    dir_ssps <- "C:\\Users\\basti\\Box\\Data\\SSPs\\Gridded\\Pop\\"
    dir_box <- "C:\\Users\\basti\\Box\\Data"
    

    # Corals  
    corals_folder <- here::here("Code", "Corals")
    r_files_corals <- list.files(path = corals_folder, pattern = "\\.[rR]$", full.names = TRUE)
    r_files_corals_figures <- list.files(path = corals_folder, pattern = "999", full.names = TRUE)
    source(r_files_corals_figures[1])
    graphics.off()

    # Ports
    ports_folder <- here::here("Code", "Ports")
    r_files_ports <- list.files(path = ports_folder, pattern = "\\.[rR]$", full.names = TRUE)
    r_files_ports_figures <- list.files(path = ports_folder, pattern = "999", full.names = TRUE)
    source(r_files_ports_figures[1])
    graphics.off()

    # Fisheries
    fisheries_folder <- here::here("Code", "Fisheries")
    r_files_fisheries <- list.files(path = fisheries_folder, pattern = "\\.[rR]$", full.names = TRUE)
    r_files_fisheries_figures <- list.files(path = fisheries_folder, pattern = "999", full.names = TRUE)
    source(r_files_fisheries_figures[1])
    graphics.off()

## Dir (end)

## Cross-cutting dataframes


    regions  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\r5regions.csv')
    glimpse(regions)
    names(regions) <- c("R5","countrycode")
    regions$R5 <- as.character(gsub("R5", "", regions$R5))
    library(stringr)
    ssp_gdp <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv')
    ssp_pop <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_pop.csv')
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP245_magicc_202303021423.csv")
    glimpse(ssp_temp)
    ssp_temp %>% dplyr::select(scenario)
    ssp_gdp$countrycode <- ssp_gdp$ISO3
    ssp_pop$countrycode <- ssp_pop$ISO3
    ssp_temp_long <- ssp_temp %>%
        tidyr::pivot_longer(
            cols = starts_with("X"),
            names_to = "year",
            values_to = "value"
        ) %>%
        mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")



    ssp_temp_long$temp2020 <- ssp_temp_long %>% filter(year==2020) %>% dplyr::select(value) %>% unlist()
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2020




    #deflator
    deflator_data <- WDI(country = "USA", indicator = c("NY.GDP.DEFL.ZS"), start = 2000, end = 2021)
    def_mult <- deflator_data %>% 
        summarize(def_2005_to_2020 =NY.GDP.DEFL.ZS[year==2020]/NY.GDP.DEFL.ZS[year==2005] )

##

## Colors

    # Value Types 
    color_ValueTypes <- c("#FFD700","#87CEEB","#9370DB")
    color_capitals <- c(corals="#FF6F61",fisheries="#1E90FF",mangroves="#228B22",ports="#000080")
    ## RCPs
        hex_rcp26 <- "#ADD8E6"
        hex_rcp45 <- "#0000CD"
        hex_rcp60 <- "#FFA500"
        hex_rcp85 <- "#FF0000"
        hex_rcps <- c(RCP26 = hex_rcp26, RCP45 = hex_rcp45, RCP60 = hex_rcp60, RCP85 = hex_rcp85)


# Damage
hex_smalldam <- "#FFFFE0"
hex_meddam <- "#FFDAB9"
hex_highdam <- "#FFA07A"
hex_maxdam <- "#8B0000"


#R5
hex_OECD <-"#4682B4"
hex_ASIA <- "#DC143C"
hex_LAM <-"#FFA500"
hex_MAF <-"#556B2F"
hex_REF <- "#2F4F4F"
hex_R5 <- c(OECD = hex_OECD, ASIA = hex_ASIA, LAM = hex_LAM, REF = hex_REF, MAF = hex_MAF)

hex_pos_1 <- "#90EE90"
hex_pos_2 <- "#3CB371"
hex_pos_3 <- "#228B22"
hex_neg_1 <- "#F4A3A3"
hex_neg_2 <- "#FA8072"
hex_neg_3 <- "#B22222"


hex_ssp1 <- "#006400"
hex_ssp2 <- "#008080"
hex_ssp3 <- "#800080"
hex_ssp4 <- "#A52A2A"
hex_ssp5 <-'#FF8C00'

hex_ssps <- c(hex_ssp1,hex_ssp2,hex_ssp3,hex_ssp4,hex_ssp5)
custom_colors <- c(hex_maxdam, hex_highdam, hex_meddam, hex_smalldam)
