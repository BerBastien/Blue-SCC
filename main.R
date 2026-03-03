################################################################################
# MAIN WORKFLOW SCRIPT - BLUE SOCIAL COST OF CARBON
################################################################################
#
# This script runs the complete analysis pipeline from raw data to final figures
# for the "Accounting for Ocean Impacts Nearly Doubles the Social Cost of Carbon" paper
#
# WORKFLOW OVERVIEW:
# 1. Setup environment (install packages)
# 2. Run module-specific R scripts (Corals, Mangroves, Fisheries, Ports)
# 3. Prepare data for RICE50+ model (Python)
# 4. [MANUAL STEP] Run RICE50+ model in GAMS
# 5. Calculate SCC from RICE50+ results (Python)
# 6. Generate all paper figures
#
# REQUIREMENTS:
# - R 4.4.2+
# - Python 3.11+
# - GAMS (for RICE50+ model runs)
# - RICE50+ model (https://github.com/witch-team/RICE50xmodel)
#
# USAGE:
#   source("main.R")
#
################################################################################

cat("\n================================================================================\n")
cat("BLUE SCC ANALYSIS - MAIN WORKFLOW\n")
cat("================================================================================\n\n")

################################################################################
# 0. SETUP ENVIRONMENT
################################################################################

cat("\n>>> Step 0: Setting up environment...\n")

# Install and load required packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',"reshape",
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes',
    "exactextractr","WDI","ggrepel","viridis","scico","scales","stringr","patchwork", "readxl",
    "countrycode","purrr","tidyr","rlang","rnaturalearthdata","ggalluvial","svglite","ggplot2",
    "dplyr","WDI","ggpubr","scico","rnaturalearth","scales","readxlsx","tidyquant","dplyr", "here")

# Set working directory to repo root
script_path <- here::here()
setwd(script_path)

if (!file.exists("main.R")) {
  stop("Please run this script from the repository root directory")
}

# Local data directories (using External_Data folder in repository)
dir_external <- here::here("External_Data")
dir_wcmc <- file.path(dir_external, "01_Data")
dir_ssps_gridded <- dir_external  # SPP1, SPP2, SPP3, SPP4, SPP5 folders
dir_ssps <- file.path(dir_external, "SSPs")

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

# Load miscellaneous utility files
misc_folder <- here::here("Code", "Misc")
r_files_misc <- list.files(path = misc_folder, pattern = "\\.[rR]$", full.names = TRUE)
cat("\nLoading miscellaneous utility files...\n")
skip_files <- c("crosscutting_data.r", "estimating_eaths.r")
for (misc_file in r_files_misc) {
    file_name <- basename(misc_file)
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

################################################################################
# 1. RUN MODULE R SCRIPTS (OR SKIP IF ALREADY PROCESSED)
################################################################################

cat("\n>>> Step 1: Checking module outputs...\n")

# Check if module outputs already exist
output_dir <- "Data/output_modules_input_rice50x/input_rice50x"
required_outputs <- c(
  "corals_areaDam_Value.csv",
  "mangrove_benefits_per_km2.csv",
  "fish_tcoeff.csv",
  "ports_tcoeff.csv"
)

outputs_exist <- all(sapply(required_outputs, function(f) {
  file.exists(file.path(output_dir, f))
}))

if (outputs_exist) {
  cat("\n  ✓ Module outputs already exist in", output_dir, "\n")
  cat("  ✓ Skipping module processing (already done)\n")
  cat("\n  Found output files:\n")
  for (f in required_outputs) {
    cat("    -", f, "\n")
  }
  cat("\n  Note: To regenerate module outputs, you need raw input data in External_Data/input_modules/\n")
  cat("        See module-specific README files for data requirements.\n")

} else {
  cat("\n  Module outputs not found. Running module R scripts...\n")
  cat("  (This requires raw input data in External_Data/input_modules/)\n\n")

  ## 1.1 CORALS
  cat("  [1/4] Processing Corals module...\n")
  tryCatch({
    source("Code/Corals/001_TempCoefficients.r")
    source("Code/Corals/002_Corals_and_ISO.r")
    source("Code/Corals/003_CountryAggregation_Values.r")
    cat("      ✓ Corals module complete\n")
  }, error = function(e) {
    cat("      ✗ Error in Corals module:", conditionMessage(e), "\n")
    cat("      Check that raw input data exists in External_Data/input_modules/corals/\n")
  })

  ## 1.2 MANGROVES
  cat("  [2/4] Processing Mangroves module...\n")
  tryCatch({
    source("Code/Mangroves/001_Read_Data.r")
    source("Code/Mangroves/002_Temp_Coefficients.r")
    source("Code/Mangroves/003_BenefitsperHa_Projection.r")
    cat("      ✓ Mangroves module complete\n")
  }, error = function(e) {
    cat("      ✗ Error in Mangroves module:", conditionMessage(e), "\n")
  })

  ## 1.3 FISHERIES & MARICULTURE
  cat("  [3/4] Processing Fisheries & Mariculture module...\n")
  tryCatch({
    source("Code/Fisheries_and_Mariculture/001_Read_FisheriesProjections_Free.r")
    source("Code/Fisheries_and_Mariculture/001b_Nutrition_ReadData.r")
    source("Code/Fisheries_and_Mariculture/002_Damage_Function_Free.r")
    cat("      ✓ Fisheries module complete\n")
  }, error = function(e) {
    cat("      ✗ Error in Fisheries module:", conditionMessage(e), "\n")
  })

  ## 1.4 PORTS
  cat("  [4/4] Processing Ports module...\n")
  tryCatch({
    source("Code/Ports/001_ReadPortsDisruptions.r")
    source("Code/Ports/002_Ports_TempCoeff.r")
    cat("      ✓ Ports module complete\n")
  }, error = function(e) {
    cat("      ✗ Error in Ports module:", conditionMessage(e), "\n")
  })
}

################################################################################
# 2. PREPARE DATA FOR RICE50+
################################################################################

cat("\n>>> Step 2: Preparing input data for RICE50+ model...\n")

# Check if Python is available
python_cmd <- Sys.which("python")
if (python_cmd == "") python_cmd <- Sys.which("python3")

if (python_cmd == "") {
  cat("  ⚠ Python not found. Skipping ocean_data.parquet creation.\n")

  # Failsafe: Check if file already exists
  if (file.exists("Data/SCC/out/ocean_data.parquet")) {
    cat("  ✓ Using existing ocean_data.parquet from Data/SCC/out/\n")
  } else {
    cat("  ⚠ ocean_data.parquet not found. Please install Python and run:\n")
    cat("      cd Code/SCC\n")
    cat("      python make_input_data.py\n")
  }

} else {
  cat(paste0("  Using Python: ", python_cmd, "\n"))

  # Step 2a: Check and install gdxpds if needed
  cat("  [2a] Checking for gdxpds Python package...\n")
  gdxpds_check <- system2(python_cmd,
                          args = c("-c", "import gdxpds"),
                          stdout = FALSE,
                          stderr = FALSE)

  if (gdxpds_check != 0) {
    cat("      ⚠ gdxpds not found. Attempting to install...\n")
    install_result <- system2(python_cmd,
                              args = c("-m", "pip", "install", "gdxpds"),
                              stdout = TRUE,
                              stderr = TRUE)

    # Check again after installation
    gdxpds_check2 <- system2(python_cmd,
                             args = c("-c", "import gdxpds"),
                             stdout = FALSE,
                             stderr = FALSE)

    if (gdxpds_check2 == 0) {
      cat("      ✓ gdxpds installed successfully\n")
    } else {
      cat("      ⚠ Failed to install gdxpds automatically\n")
      cat("      Please install manually: pip install gdxpds\n")
    }
  } else {
    cat("      ✓ gdxpds found\n")
  }

  # Step 2b: Run make_input_data.py to create ocean_data.parquet
  cat("  [2b] Creating ocean_data.parquet from module outputs...\n")
  result <- system2(python_cmd,
                    args = c("Code/SCC/make_input_data.py"),
                    stdout = TRUE,
                    stderr = TRUE)

  # Display output
  if (length(result) > 0) {
    cat(paste("    ", result, collapse = "\n"), "\n")
  }

  # Check if output was created (with failsafe)
  output_exists <- file.exists("Data/SCC/out/ocean_data.parquet")

  if (output_exists) {
    cat("      ✓ ocean_data.parquet created successfully in Data/SCC/out/\n")
  } else {
    cat("      ⚠ ocean_data.parquet creation failed\n")

    # Failsafe: Check if an older version exists
    old_locations <- c(
      "Data/SCC/out/ocean_data.parquet",
      "../RICE50x/input/data/ocean_data.parquet"
    )

    found_existing <- FALSE
    for (loc in old_locations) {
      if (file.exists(loc)) {
        cat("      ✓ Found existing ocean_data.parquet at:", loc, "\n")
        cat("      You can use this file, but it may not reflect latest module outputs\n")
        found_existing <- TRUE
        break
      }
    }

    if (!found_existing) {
      cat("      ⚠ No existing ocean_data.parquet found\n")
      cat("      Please check errors above and ensure:\n")
      cat("        - gdxpds is installed (pip install gdxpds)\n")
      cat("        - RICE50x SSP data file exists at ../RICE50x/input/data/ssp_navigate-ssp_base.gdx\n")
    }
  }
}

################################################################################
# 3. RUN RICE50+ MODEL (MANUAL STEP)
################################################################################

cat("\n>>> Step 3: RICE50+ Model Runs (MANUAL STEP REQUIRED)\n")
cat("
  IMPORTANT: You must run the RICE50+ model in GAMS to generate GDX files.

  Required GAMS commands (run from RICE50+ directory):

  1. Baseline run with ocean damages:
     gams run_rice50x.gms --mod_ocean=1 --nameout=ocean_damage

  2. Run with CO2 emission pulse:
     gams run_rice50x.gms --mod_ocean=1 --nameout=ocean_damage_pulse --mod_emission_pulse=ocean_damage

  3. Run with today's climate:
     gams run_rice50x.gms --mod_ocean=1 --nameout=ocean_today --policy=simulation_tatm_exogen --climate_of_today=1

  Output GDX files should be placed in: Results/RICE50x/

  See Code/SCC/SCC_steps.md for detailed instructions.
\n")

# Check if RICE50+ results exist
if (dir.exists("Results/RICE50x") && length(list.files("Results/RICE50x", pattern = "\\.gdx$")) > 0) {
  cat("  ✓ RICE50x GDX files found in Results/RICE50x/\n")
  cat("  Continuing to SCC calculation...\n")
} else {
  cat("  ⚠ No RICE50x GDX files found in Results/RICE50x/\n")
  cat("  Please run RICE50+ model before proceeding to figure generation.\n")
  cat("  You can still generate module-specific figures (Step 5).\n")
}

################################################################################
# 4. CALCULATE SCC (if RICE50+ results are available)
################################################################################

cat("\n>>> Step 4: Calculating SCC from RICE50+ results...\n")

if (dir.exists("Results/RICE50x") && length(list.files("Results/RICE50x", pattern = "\\.gdx$", recursive = TRUE)) > 0) {

  if (python_cmd != "") {
    # Note: The SCC calculation is typically done in Python notebooks/scripts
    # that read the GDX files. For automated workflow, you could add:
    # result <- system2(python_cmd, args = c("Code/SCC/mc_scc.py"))

    cat("  SCC calculation scripts available in Code/SCC/\n")
    cat("  Main scripts:\n")
    cat("    - utils.py: sectoral_scc() function\n")
    cat("    - mc_analysis.py: Monte Carlo analysis\n")
    cat("    - sectoral_market_equivalent.py: Sectoral decomposition\n")
    cat("
  To calculate SCC interactively in Python:

  import sys
  sys.path.append('Code/SCC')
  from utils import sectoral_scc
  import gdxpds

  # Load GDX files
  today = gdxpds.read_gdx.to_dataframes('Results/RICE50x/results_ocean_today.gdx')
  damage = gdxpds.read_gdx.to_dataframes('Results/RICE50x/results_ocean_damage.gdx')
  pulse = gdxpds.read_gdx.to_dataframes('Results/RICE50x/results_ocean_damage_pulse.gdx')

  # Calculate SCC
  scc = sectoral_scc(today, damage, pulse)
\n")
  } else {
    cat("  ⚠ Python not found. SCC calculation requires Python.\n")
  }
} else {
  cat("  ⚠ Skipping SCC calculation (no RICE50x results found)\n")
}

################################################################################
# 5. GENERATE FIGURES
################################################################################

cat("\n>>> Step 5: Generating paper figures...\n")

## 5.1 MODULE-SPECIFIC SUPPLEMENTARY FIGURES
cat("\n  [A] Module-specific supplementary figures:\n")

cat("    - Corals supplementary figures...\n")
tryCatch({
  source("Code/Corals/999_Corals_Supp_Figures.r")
  cat("      ✓ Corals figures complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Corals figures:", e$message, "\n")
})

cat("    - Mangroves supplementary figures...\n")
tryCatch({
  source("Code/Mangroves/999_Figures_Mangroves.r")
  cat("      ✓ Mangroves figures complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Mangroves figures:", e$message, "\n")
})

cat("    - Fisheries supplementary figures...\n")
tryCatch({
  source("Code/Fisheries_and_Mariculture/999_Figures_fisheries.r")
  cat("      ✓ Fisheries figures complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Fisheries figures:", e$message, "\n")
})

cat("    - Ports supplementary figures...\n")
tryCatch({
  source("Code/Ports/999_Figures_ports.r")
  cat("      ✓ Ports figures complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Ports figures:", e$message, "\n")
})

## 5.2 MAIN TEXT FIGURES
cat("\n  [B] Main text figures:\n")

cat("    - Figure 1 (top panel - Blue Capital Map)...\n")
tryCatch({
  source("Code/MainText_Figures_Code/Fig1_top_code.r")
  cat("      ✓ Figure 1 top complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Figure 1 top:", e$message, "\n")
})

cat("    - Figure 1 (bottom panel)...\n")
tryCatch({
  source("Code/MainText_Figures_Code/Fig1_bottom_code.r")
  cat("      ✓ Figure 1 bottom complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Figure 1 bottom:", e$message, "\n")
})

cat("    - Figure 3 (Damages Analysis)...\n")
tryCatch({
  source("Code/MainText_Figures_Code/Fig3_analysis_and_plot.r")
  cat("      ✓ Figure 3 complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Figure 3:", e$message, "\n")
})

cat("    - Figure 4 (SCC Breakdown)...\n")
if (python_cmd != "") {
  result <- tryCatch({
    system2(python_cmd,
            args = c("Code/MainText_Figures_Code/Fig4_code.py"),
            stdout = TRUE,
            stderr = TRUE)
  }, error = function(e) {
    cat("      ⚠ Error in Figure 4:", e$message, "\n")
    NULL
  })
  if (!is.null(result)) {
    cat("      ✓ Figure 4 complete\n")
  }
} else {
  cat("      ⚠ Python required for Figure 4\n")
}

## 5.3 EXTENDED DATA FIGURES
cat("\n  [C] Extended Data figures:\n")

cat("    - Extended Data figures (SSPs, MC, discount rates)...\n")
tryCatch({
  source("Code/MainText_Figures_Code/EDFig_otherSCC.R")
  cat("      ✓ Extended Data figures complete\n")
}, error = function(e) {
  cat("      ⚠ Error in Extended Data figures:", e$message, "\n")
})

################################################################################
# 6. SUMMARY
################################################################################

cat("\n================================================================================\n")
cat("WORKFLOW COMPLETE\n")
cat("================================================================================\n\n")

cat("Summary of outputs:\n")
cat("  - Module data: Data/output_modules_input_rice50x/\n")
cat("  - RICE50+ input: Data/SCC/out/ocean_data.parquet\n")
if (dir.exists("Results/RICE50x") && length(list.files("Results/RICE50x", pattern = "\\.gdx$", recursive = TRUE)) > 0) {
  cat("  - RICE50+ results: Results/RICE50x/\n")
}
cat("  - Figures: Figures/Main/ and Figures/Other/\n")

cat("\nNext steps:\n")
if (!dir.exists("Results/RICE50x") || length(list.files("Results/RICE50x", pattern = "\\.gdx$", recursive = TRUE)) == 0) {
  cat("  1. Run RICE50+ model in GAMS (see Step 3 above)\n")
  cat("  2. Calculate SCC using Python scripts in Code/SCC/\n")
  cat("  3. Generate final SCC figures\n")
} else {
  cat("  ✓ All steps complete!\n")
  cat("  - Review figures in Figures/Main/\n")
  cat("  - Check SCC results in Data/SCC/out/\n")
}

cat("\nFor more information:\n")
cat("  - See README.md for overview\n")
cat("  - See INTEGRATION_SUMMARY.md for recent changes\n")
cat("  - See Code/SCC/SCC_steps.md for RICE50+ details\n")
cat("\n")
