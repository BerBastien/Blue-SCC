## Set-up File

####--- Libraries ---####
    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',"reshape",
        'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
        'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes',
        "exactextractr","WDI","ggrepel","viridis","scico","scales","stringr","patchwork", "readxl",
        "countrycode","purrr","tidyr","rlang","rnaturalearthdata","ggalluvial","svglite","ggplot2", "dplyr","WDI","ggpubr","scico","rnaturalearth","scales","readxlsx","tidyquant","dplyr")
    
    lapply(x, require, character.only = TRUE)




####--- Directories and Misc ---####
    script_path <- here::here()
    setwd(script_path)

    #external directories for datasets
    dir_box <- "G:\\My Drive\\Data\\"
    dir_wcmc <- paste0(dir_box,"\\Oceans\\coral_extent\\14_001_WCMC008_CoralReefs2021_v4_1\\01_Data")
    dir_ssps <- paste0(dir_box,"\\SSPs\\Gridded\\Pop\\")


    # Miscelaneous 
    misc_folder <- here::here("Code", "Misc")
    r_files_misc <- list.files(path = misc_folder, pattern = "\\.[rR]$", full.names = TRUE)
    lapply(r_files_misc, source)
    graphics.off()
    
####--- Blue Capital Modules ---####
    # Corals  
    corals_folder <- here::here("Code", "Corals")
    r_files_corals <- list.files(path = corals_folder, pattern = "\\.[rR]$", full.names = TRUE)
    #lapply(r_files_corals, source) Only run once
    r_files_corals_figures <- list.files(path = corals_folder, pattern = "999", full.names = TRUE)
    source(r_files_corals_figures[1])
    graphics.off()

    # Ports
    ports_folder <- here::here("Code", "Ports")
    r_files_ports <- list.files(path = ports_folder, pattern = "\\.[rR]$", full.names = TRUE)
    #lapply(r_files_ports, source) Only run once
    r_files_ports_figures <- list.files(path = ports_folder, pattern = "999", full.names = TRUE)
    source(r_files_ports_figures[1])
    graphics.off()

    # Fisheries and Mariculture
    fisheries_folder <- here::here("Code", "Fisheries_and_Mariculture")
    r_files_fisheries <- list.files(path = fisheries_folder, pattern = "\\.[rR]$", full.names = TRUE)
    #lapply(r_files_fisheries, source) Only run once
    r_files_fisheries_figures <- list.files(path = fisheries_folder, pattern = "999", full.names = TRUE)
    source(r_files_fisheries_figures[1])
    graphics.off()

    # Mangroves
    mangroves_folder <- here::here("Code", "Mangroves")
    r_files_mangroves <- list.files(path = mangroves_folder, pattern = "\\.[rR]$", full.names = TRUE)
    #lapply(r_files_mangroves, source) Only run once
    r_files_mangroves_figures <- list.files(path = mangroves_folder, pattern = "999", full.names = TRUE)
    source(r_files_mangroves_figures[1])
    graphics.off()


