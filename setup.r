## Set-up File

####--- Libraries ---####
    # Install and load pacman if not already installed
    if (!require("pacman", quietly = TRUE)) install.packages("pacman")

    # Load packages using pacman
    pacman::p_load('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',"reshape",
        'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
        'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes',
        "exactextractr","WDI","ggrepel","viridis","scico","scales","stringr","patchwork", "readxl",
        "countrycode","purrr","tidyr","rlang","rnaturalearthdata","ggalluvial","svglite","ggplot2",
        "dplyr","WDI","ggpubr","scico","rnaturalearth","scales","readxlsx","tidyquant","dplyr", "here")




####--- Directories and Misc ---####
    script_path <- here::here()
    setwd(script_path)

    # Local data directories (using External_Data folder in repository)
    # Original data sources:
    #   - Coral extent: https://drive.google.com/drive/folders/10poBMwF3QM2SenNFkT8lntIX9ZSSDe6n
    #   - SSP gridded population: https://drive.google.com/drive/folders/1_XBbnVRYC-bN0MxDgfA6_d0f_dSS7kZ1
    dir_external <- here::here("External_Data")
    dir_wcmc <- file.path(dir_external, "01_Data")

    # Gridded population data (TIF files) - now at root level of External_Data
    dir_ssps_gridded <- dir_external  # SPP1, SPP2, SPP3, SPP4, SPP5 folders

    # CSV/tabular SSP data directory
    dir_ssps <- file.path(dir_external, "SSPs")

    # Create SSPs directory if it doesn't exist (for CSV files)
    if (!dir.exists(dir_ssps)) {
        dir.create(dir_ssps, recursive = TRUE)
        dir.create(file.path(dir_ssps, "CO2Pulse"), recursive = TRUE)
        warning("\n========================================")
        warning("External_Data/SSPs/ directory created.")
        warning("REQUIRED CSV FILES (not yet present):")
        warning("  - External_Data/SSPs/SspDb_country_data_2013-06-12.csv")
        warning("  - External_Data/SSPs/ssp_gdp.csv")
        warning("  - External_Data/SSPs/ssp_pop.csv")
        warning("  - External_Data/SSPs/CO2Pulse/*.csv (SSP scenario temperature files)")
        warning("========================================\n")
    }


    # Miscellaneous
    misc_folder <- here::here("Code", "Misc")
    r_files_misc <- list.files(path = misc_folder, pattern = "\\.[rR]$", full.names = TRUE)

    # Source misc files with error handling (some may require data files)
    cat("\nLoading miscellaneous utility files...\n")

    # Files to skip (require external data not in repository)
    skip_files <- c("crosscutting_data.r", "estimating_eaths.r")

    for (misc_file in r_files_misc) {
        file_name <- basename(misc_file)

        # Skip files that require external data
        if (file_name %in% skip_files) {
            cat("  Skipping", file_name, "(requires external data files)\n")
            next
        }

        tryCatch({
            source(misc_file)
            cat("  Loaded:", file_name, "\n")
        }, error = function(e) {
            warning(paste("Could not load", file_name, ":", conditionMessage(e)))
        })
    }
    graphics.off()

####--- Blue Capital Modules ---####
    # Note: Module scripts are NOT run automatically during setup.
    # They require data files and should be run via main.R or manually.
    #
    # To run modules manually:
    #   source("Code/Corals/001_TempCoefficients.r")
    #   source("Code/Mangroves/001_Read_Data.r")
    # etc.
    #
    # To run complete workflow:
    #   source("main.R")

    cat("\n")
    cat("========================================\n")
    cat("SETUP COMPLETE\n")
    cat("========================================\n")
    cat("\nEnvironment ready!\n")
    cat("\nNext steps:\n")
    cat("  1. Run complete workflow: source('main.R')\n")
    cat("  2. Or run individual modules in Code/\n")
    cat("\nFor help, see README.md\n\n")


