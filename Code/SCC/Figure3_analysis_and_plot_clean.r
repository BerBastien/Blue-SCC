# ==============================================================================
# FIGURE 3: OCEAN-BASED DAMAGES ANALYSIS AND VISUALIZATION
# ==============================================================================
# This script creates Figure 3 showing ocean-based damages across three panels:
# Panel A: Ocean-based damages under SSP2 (time series by GDP per capita)
# Panel B: Market equivalent losses from substitutability analysis 
# Panel C: Substitutability-adjusted damages in 2050 (spatial maps)
#
# Final output: Combined plot using ggarrange with specified layout
# ==============================================================================

# LIBRARIES AND DEPENDENCIES -----------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(readr)
  library(tidyr)
  library(sf)
  library(rnaturalearth)
  library(ggpubr)
  library(ggrepel)
  library(scales)
  library(forcats)
  library(purrr)
  library(WDI)  # For deflator data
})

# CONFIGURATION AND CONSTANTS ----------------------------------------------
# File paths
DATA_PATH <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/"
ANALYSIS_PATH <- paste0(DATA_PATH, "analysis_output/")
GDX_CONVERT_COMMANDS <- c(
  paste0("gdx2xls ", DATA_PATH, "results_ocean_today.gdx"),
  paste0("gdx2xls ", DATA_PATH, "results_ocean_damage_pulse.gdx"),
  paste0("gdx2xls ", DATA_PATH, "results_ocean_damage.gdx")
)

# Color palettes
COLOR_CAPITALS <- c(
  "Corals" = "#ff00c5",
  "Fisheries & Mariculture" = "#0a3e7b", 
  "Mangroves" = "#1ad29b",
  "Ports" = "#ffbf00"
)

# Analysis parameters
REFERENCE_YEAR <- 2025
END_YEAR <- 2100
OCEAN_HEALTH_ETA <- 0.05
UTILITY_ETA <- 0.5
DAMAGE_THRESHOLD <- 1  # 1% of GDP threshold

# HELPER FUNCTIONS ---------------------------------------------------------

#' Process variable data table from Excel files
#' @param var_table Raw data from Excel
#' @param exp_name Experiment name
#' @param var_name Variable name
process_var_table <- function(var_table, exp_name, var_name) {
  var_table <- var_table[3:nrow(var_table), ]
  var_table <- as.data.frame(var_table)
  
  if (ncol(var_table) < 4) {
    if (var_name %in% c("scc", "pop")) {
      names(var_table) <- c("year", "country", var_name)
      var_table$country <- as.factor(var_table$country)
      var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
      var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
      var_table$exp <- exp_name
    } else {
      # Standard processing for other variables
      names(var_table) <- c("year", "country", "capital", var_name)
      var_table$capital <- as.factor(var_table$capital)
      var_table$country <- as.factor(var_table$country)
      var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
      var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
      var_table$exp <- exp_name
    }
  } else {
    # Multi-column processing
    var_table <- var_table[, 1:4]
    names(var_table) <- c("year", "country", "capital", var_name)
    var_table$capital <- as.factor(var_table$capital)
    var_table$country <- as.factor(var_table$country)
    var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
    var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
    var_table$exp <- exp_name
  }
  
  var_table <- var_table[!is.na(var_table$year), ]
  return(var_table)
}

#' Process multiple variables from RICE50x results
#' @param exp_names Experiment names
#' @param var_names Variable names
#' @param input_path Base path for input files
process_data <- function(exp_names, var_names, input_path = paste0(DATA_PATH, 'results_ocean_')) {
  for (i in seq_along(exp_names)) {
    for (j in seq_along(var_names)) {
      var_table <- read_excel(paste0(input_path, exp_names[i], '.xlsx'), sheet = var_names[j])
      var_table <- process_var_table(var_table, exp_names[i], var_names[j])
      
      if (j == 1) {
        assign(paste0("exp_data_", exp_names[i]), var_table, envir = .GlobalEnv)
      } else {
        existing_data <- get(paste0("exp_data_", exp_names[i]))
        common_cols <- intersect(names(existing_data), names(var_table))
        
        if (length(common_cols) == 0) {
          assign(paste0("exp_data_", exp_names[i]), rbind(existing_data, var_table), envir = .GlobalEnv)
        } else {
          assign(paste0("exp_data_", exp_names[i]), merge(existing_data, var_table, by = common_cols, all = TRUE), envir = .GlobalEnv)
        }
      }
    }
  }
}

#' Find year when cumulative damages surpass threshold
#' @param df Data frame with damages
#' @param country_col Country column name
#' @param year_col Year column name  
#' @param damage_col Damage column name
#' @param threshold Damage threshold
find_surpass_year <- function(df, country_col, year_col, damage_col, threshold = 1) {
  df %>%
    arrange(!!sym(country_col), !!sym(year_col)) %>%
    group_by(!!sym(country_col)) %>%
    mutate(
      is_opposite_sign_in_2100 = ifelse(
        any(!!sym(year_col) == 2100 & sign(!!sym(damage_col)) != sign(threshold)),
        TRUE, FALSE
      )
    ) %>%
    summarize(
      year_surpass = if_else(
        is_opposite_sign_in_2100, 
        99999,  # Benefits case
        approx(
          x = !!sym(damage_col), 
          y = !!sym(year_col), 
          xout = threshold, 
          rule = 2
        )$y[1]
      ),
      .groups = 'drop'
    )
}

#' Calculate utility for substitutability analysis
#' @param theta Substitutability parameter
#' @param market Market component value
#' @param nonmarket Non-market component value
calculate_utility <- function(theta, market, nonmarket) {
  (1 / (1 - UTILITY_ETA)) * (market^theta + nonmarket^theta)^((1 - UTILITY_ETA) / theta)
}

# DATA LOADING AND PROCESSING ----------------------------------------------

# Convert GDX files to Excel (if needed)
# Uncomment the following lines if GDX conversion is required:
# for(cmd in GDX_CONVERT_COMMANDS) {
#   system(cmd)
# }

# Load deflator data (needed for currency conversions)
deflator_data <- WDI(country = "USA", indicator = c("NY.GDP.DEFL.ZS"), start = 2000, end = 2021)
def_mult <- deflator_data %>% 
  summarize(def_2005_to_2020 = NY.GDP.DEFL.ZS[year==2020]/NY.GDP.DEFL.ZS[year==2005]) %>%
  pull(def_2005_to_2020)

# Load key variables from RICE50x results
key_variables <- c(
  "C", "ocean_consump_damage_coef", "ocean_consump_damage_coef_sq", "YNET",
  "OCEAN_USENM_VALUE_PERKM2", "VSL", "CPC", "OCEAN_NONUSE_VALUE_PERKM2",
  "OCEAN_AREA", "ocean_area_start", "ocean_value_intercept_unm", "ocean_value_exp_unm",
  "ocean_value_intercept_nu", "ocean_value_exp_nu", "pop", 
  "ocean_health_beta", "ocean_health_mu", "ocean_health_tame"
)
experiment_names <- "damage"

# Process main dataset
process_data(experiment_names, key_variables)

# Load temperature data
temperature_data <- read_excel(paste0(DATA_PATH, 'results_ocean_damage.xlsx'), sheet = "TATM") %>%
  select(year = 1, tatm = 3) %>%
  mutate(
    year = 1980 + (as.integer(year) - 1) * 5,
    tatm = as.double(tatm)
  ) %>%
  filter(!is.na(year))

# Calculate temperature differences from reference year
reference_temp <- temperature_data %>%
  filter(year == REFERENCE_YEAR) %>%
  pull(tatm)

temperature_data <- temperature_data %>%
  mutate(tatm_diff_2025 = tatm - reference_temp)

# Merge temperature with main dataset
exp_data_damage <- exp_data_damage %>%
  left_join(temperature_data, by = "year")

# DAMAGE CALCULATIONS BY CAPITAL TYPE -------------------------------------

#' Calculate port damages
calculate_port_damages <- function(data) {
  data %>% 
    filter(capital == "ports", year > 2024) %>% 
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      # Market damages (YNET in trillions, convert to millions)
      YNET_dam_mkt_dif = YNET * tatm_diff_2025 * ocean_consump_damage_coef * 1e6,
      YNET_dam_mkt_dif_perc = 100 * tatm_diff_2025 * ocean_consump_damage_coef,
      
      # Reference GDP for percentage calculations
      net_GDP_2025 = first(YNET[year == REFERENCE_YEAR]),
      YNET_dam_mkt_dif_perc2025 = (YNET_dam_mkt_dif / net_GDP_2025) * 100,
      
      # Add empty columns for consistency with other damage types
      usenm_dif = 0,
      nonuse_dif = 0,
      
      # Cumulative damages
      cumulative_YNET_dam_mkt_dif = cumsum(-YNET_dam_mkt_dif),
      cumulative_YNET_dam_mkt_dif_perc = cumsum(-YNET_dam_mkt_dif_perc),
      cumulative_dam_perc2025 = cumsum(YNET_dam_mkt_dif_perc2025)
    ) %>%
    ungroup() %>%
    filter(!all(cumulative_YNET_dam_mkt_dif == 0)) %>%
    mutate(capital = "Ports")
}

#' Calculate coral damages  
calculate_coral_damages <- function(data) {
  data %>% 
    filter(capital == "coral", year > 2024) %>% 
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      # Non-market values (no damage vs. with damage)
      usenm_nodam = OCEAN_USENM_VALUE_PERKM2 * OCEAN_AREA[year == REFERENCE_YEAR],
      usenm_dam = OCEAN_USENM_VALUE_PERKM2 * OCEAN_AREA,
      nonuse_nodam = OCEAN_NONUSE_VALUE_PERKM2 * OCEAN_AREA[year == REFERENCE_YEAR],
      nonuse_dam = OCEAN_NONUSE_VALUE_PERKM2 * OCEAN_AREA,
      
      # Damage differences
      nonuse_dif = nonuse_dam - nonuse_nodam,
      usenm_dif = usenm_dam - usenm_nodam,
      
      # Market damages
      YNET_dam_mkt_dif = YNET * tatm_diff_2025 * ocean_consump_damage_coef * 1e6,
      
      # Reference GDP
      net_GDP_2025 = first(YNET[year == REFERENCE_YEAR]),
      
      # Total damage as percentage of 2025 GDP
      dam_dif_perc2025 = (-(YNET_dam_mkt_dif + nonuse_dif + usenm_dif) / net_GDP_2025) * 100,
      
      # Cumulative damages
      cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)
    ) %>%
    ungroup() %>%
    filter(!all(cumulative_dam_perc2025 == 0), !is.na(cumulative_dam_perc2025)) %>%
    mutate(capital = "Corals")
}

#' Calculate mangrove damages
calculate_mangrove_damages <- function(data) {
  data %>% 
    filter(capital == "mangrove", year > 2024) %>% 
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      # Use and non-use values (no damage scenario)
      usenm_nodam = (exp(ocean_value_intercept_unm) * (YNET / pop * 1e6)^ocean_value_exp_unm) * 
                    OCEAN_AREA[year == REFERENCE_YEAR],
      nu_nodam = (exp(ocean_value_intercept_nu) * (YNET / pop * 1e6)^ocean_value_exp_nu) * 
                 OCEAN_AREA[year == REFERENCE_YEAR],
      
      # Use and non-use values (with damage)
      usenm_dam = (exp(ocean_value_intercept_unm) * (YNET / pop * 1e6)^ocean_value_exp_unm) * OCEAN_AREA,
      nu_dam = (exp(ocean_value_intercept_nu) * (YNET / pop * 1e6)^ocean_value_exp_nu) * OCEAN_AREA,
      
      # Damage differences (convert from trillions to millions and deflate from 2020 to 2005 USD)
      nonuse_dif = (nu_dam - nu_nodam) * 1e6 / 1.298763,
      usenm_dif = (usenm_dam - usenm_nodam) * 1e6 / 1.298763,
      
      # Market damages (quadratic damage function)
      YNET_dam_mkt_dif = YNET * (tatm_diff_2025 * ocean_consump_damage_coef + 
                                ocean_consump_damage_coef_sq * tatm_diff_2025^2) * 1e6,
      
      # Reference GDP
      net_GDP_2025 = first(YNET[year == REFERENCE_YEAR]),
      
      # Total damages
      all_dam = -(YNET_dam_mkt_dif + nonuse_dif + usenm_dif),
      
      # Post-2025 damages
      all_dam_post2025 = all_dam - first(all_dam[year == REFERENCE_YEAR]),
      dam_dif_perc2025 = (all_dam_post2025 / net_GDP_2025) * 100,
      
      # Cumulative damages
      cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)
    ) %>%
    ungroup() %>%
    filter(!all(cumulative_dam_perc2025 == 0), !is.na(cumulative_dam_perc2025)) %>%
    mutate(capital = "Mangroves")
}

#' Calculate fisheries damages
calculate_fisheries_damages <- function(data) {
  data %>% 
    filter(capital == "fisheries", year > 2024) %>% 
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      # Health-related non-market damages
      usenm_dif = (ocean_health_beta * tatm_diff_2025) * ocean_health_tame * 
                  pop * 1e6 * ocean_health_mu * OCEAN_HEALTH_ETA * VSL,
      
      # Add empty column for consistency
      nonuse_dif = 0,
      
      # Market damages
      YNET_dam_mkt_dif = YNET * (tatm_diff_2025 * ocean_consump_damage_coef) * 1e6,
      
      # Reference GDP
      net_GDP_2025 = first(YNET[year == REFERENCE_YEAR]),
      
      # Total damages
      all_dam = -(YNET_dam_mkt_dif + usenm_dif),
      
      # Post-2025 damages
      all_dam_post2025 = all_dam - first(all_dam[year == REFERENCE_YEAR]),
      dam_dif_perc2025 = (all_dam_post2025 / net_GDP_2025) * 100,
      
      # Cumulative damages
      cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)
    ) %>%
    ungroup() %>%
    filter(!all(cumulative_dam_perc2025 == 0), !is.na(cumulative_dam_perc2025)) %>%
    mutate(capital = "Fisheries & Mariculture")
}

# Calculate damages for each capital type
port_damages <- calculate_port_damages(exp_data_damage)
coral_damages <- calculate_coral_damages(exp_data_damage)
mangrove_damages <- calculate_mangrove_damages(exp_data_damage)
fisheries_damages <- calculate_fisheries_damages(exp_data_damage)

# PANEL A: TIME SERIES DAMAGE PLOT ----------------------------------------

# Combine all damage data
combined_damages <- bind_rows(
  fisheries_damages,
  mangrove_damages, 
  coral_damages,
  port_damages
)

# Prepare data for plotting
plot_data <- combined_damages %>%
  select(year, country, YNET, pop, capital, usenm_dif, nonuse_dif, YNET_dam_mkt_dif, CPC) %>%
  pivot_longer(
    cols = c(usenm_dif, nonuse_dif, YNET_dam_mkt_dif), 
    names_to = "variable", 
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(variable,
                     usenm_dif = "Non-market Use",
                     nonuse_dif = "Non-use", 
                     YNET_dam_mkt_dif = "Market"),
    variable = factor(variable, levels = c("Market", "Non-market Use", "Non-use"))
  ) %>%
  filter(country != "row") # Exclude rest-of-world

# Define shapes for damage types
damage_shapes <- c("Market" = 15, "Non-market Use" = 16, "Non-use" = 17)

# Create Panel A: Time damages plot
time_damages_plot <- ggplot(
  plot_data %>% filter(value < 0, year < END_YEAR + 1),
  aes(x = CPC / 1000, y = -value, color = capital, 
      group = interaction(country, capital, variable))
) +
  geom_line(alpha = 0.5) +
  geom_point(
    data = plot_data %>% filter(year == END_YEAR),
    aes(shape = variable), size = 2, alpha = 0.4
  ) +
  geom_text_repel(
    data = plot_data %>% filter(year == END_YEAR),
    aes(label = country), size = 2
  ) +
  scale_shape_manual(values = damage_shapes) +
  scale_color_manual(values = COLOR_CAPITALS) +
  scale_y_log10(labels = dollar_format(suffix = "M", accuracy = 0.01)) +
  facet_wrap(~variable) +
  labs(
    title = "A. Ocean-based damages under SSP2",
    x = "GDP per capita (Thousand USD)",
    y = "Damages (Million USD)",
    color = "Blue Capital",
    shape = "Value Category"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# PANEL B: MARKET EQUIVALENT ANALYSIS ------------------------------------

# Get reference values from coral damages (Australia, 2025)
aus_coral_2025 <- coral_damages %>% 
  filter(year == REFERENCE_YEAR, country == "aus")

if(nrow(aus_coral_2025) > 0) {
  market_value <- as.numeric(aus_coral_2025$YNET) * def_mult * 1e6  # Convert to millions
  nonmarket_value <- as.numeric(aus_coral_2025$usenm_nodam) * def_mult  # Already in millions  
} else {
  # Fallback values if Australia data not available
  market_value <- 10
  nonmarket_value <- 5
}

# Simulation parameters
dam <- 1  # $1 million damage
eta <- 0.5  # Utility parameter (same as UTILITY_ETA)
theta_values <- seq(-0.01, 1, by = 0.01)
theta_values <- theta_values[theta_values != 0]  # Exclude theta = 0

# Calculate market equivalent losses for different theta values
results <- data.frame(theta = theta_values, market_dollar_equivalent_loss = NA)

for (i in seq_along(theta_values)) {
  theta <- theta_values[i]
  
  # Calculate base and damaged utility
  utility_base <- calculate_utility(theta, market_value, nonmarket_value)
  utility_dam <- calculate_utility(theta, market_value, nonmarket_value - dam)
  
  # Delta utility
  delta_utility <- utility_dam - utility_base
  
  # Partial derivative of utility with respect to market component
  partial_deriv_market <- (market_value^theta + (nonmarket_value - dam/2)^theta)^(((1 - eta)/theta) - 1) * 
                         market_value^(theta - 1)
  
  # Market dollar equivalent loss
  market_dollar_equivalent <- abs(delta_utility / partial_deriv_market)
  results$market_dollar_equivalent_loss[i] <- as.numeric(market_dollar_equivalent)
}

# Create Panel B: Market equivalent plot
market_eq_plot <- ggplot(results, aes(x = theta, y = market_dollar_equivalent_loss)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 0, y = 15, label = "Cobb-Douglas", vjust = -0.5, angle = 90, size = 2.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  annotate("text", x = 1, y = 19, label = "Perfect substitutes", color = "steelblue", 
           angle = 90, vjust = -0.5, size = 2.8) +
  annotate("text", x = 1.08, y = 19, label = "Panel A", color = "steelblue", 
           angle = 90, vjust = -0.5, size = 2.5, fontface = 'italic') +
  geom_vline(xintercept = 0.21, linetype = "dashed", color = "indianred") +
  annotate("text", x = 0.21, y = 19, label = "Imperfect Substitutes", color = "indianred", 
           angle = 90, vjust = -0.5, size = 2.8) +
  annotate("text", x = 0.28, y = 19, label = "Panel C", color = "indianred", 
           angle = 90, vjust = -0.5, size = 2.5, fontface = 'italic') +
  xlim(c(-0.1, 1.12)) +
  labs(
    title = "B. $1 Non-market Loss",
    x = expression("Substitutability Parameter" ~ theta),
    y = "Equivalent Loss in \nMarket Consumption ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(50, 10, 50, 10)
  )

# PANEL C: SECTORAL DAMAGES MAP ------------------------------------------

# Load market equivalent values data
market_equivalent_file <- paste0(ANALYSIS_PATH, "MarketEquivalentValues.csv")
market_equiv_data <- read_csv(market_equivalent_file) %>%
  mutate(
    year = t * 5 + 2010,
    iso_a3 = toupper(n),
    oc_capital = recode(oc_capital,
                       coral = "Corals",
                       fisheries = "Fisheries & Mariculture", 
                       mangrove = "Mangroves",
                       ports = "Ports"),
    total_value = -delta_UTARG_in_consumption
  )

# Filter for 2050 and aggregate by capital and country
data_2050 <- market_equiv_data %>%
  filter(year == 2050) %>%
  mutate(iso_a3 = toupper(n))

aggregated_data <- data_2050 %>%
  group_by(iso_a3, oc_capital, valuation) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(iso_a3, oc_capital) %>%
  summarize(total_value = -sum(delta_UTARG_in_consumption, na.rm = TRUE), .groups = 'drop')

# Load GDP and population data for 2050
gdp_file <- paste0(DATA_PATH, "results_ocean_today.xlsx")
gdp_data <- read_excel(gdp_file, sheet = "YGROSS", skip = 3, col_names = FALSE) %>%
  set_names(c("t", "country", "info", "gdp_value", "upperbound", "marginal")) %>%
  mutate(
    iso_a3 = toupper(country),
    year = as.integer(t) * 5 + 2010,
    gdp_2020_trillion = gdp_value * (113.647 / 87.504)  # Convert to 2020 trillion USD
  ) %>%
  filter(year == 2050)

pop_data <- read_excel(gdp_file, sheet = "pop", skip = 3, col_names = FALSE) %>%
  set_names(c("t", "country", "pop")) %>%
  mutate(
    iso_a3 = toupper(country),
    year = as.integer(t) * 5 + 2010
  ) %>%
  filter(year == 2050)

# Get world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Prepare mapping data
map_data <- world_map %>%
  left_join(aggregated_data, by = "iso_a3") %>%
  left_join(gdp_data, by = "iso_a3") %>%
  left_join(pop_data, by = "iso_a3") %>%
  mutate(
    percentage_of_YGROSS = (total_value * pop * 1e6 / (gdp_2020_trillion * 1e12)) * 100,
    percentage_of_YGROSS = ifelse(percentage_of_YGROSS == 0, NA, percentage_of_YGROSS)
  )

# Handle missing capital data by expanding for all capital types
oc_capital_levels <- unique(aggregated_data$oc_capital)
na_rows_expanded <- map_data %>%
  filter(is.na(oc_capital)) %>%
  select(-oc_capital) %>%
  crossing(oc_capital = oc_capital_levels)

map_data <- map_data %>%
  filter(!is.na(oc_capital)) %>%
  bind_rows(na_rows_expanded)

# Add ID for tracking and create quantile groups
map_data <- map_data %>%
  mutate(id = row_number())

# Create quantile groups for visualization  
map_data <- map_data %>%
  mutate(
    quantile_group = case_when(
      percentage_of_YGROSS < -0.001 ~ "Benefits",
      TRUE ~ NA_character_
    )
  )

# Calculate quantiles for positive values
positive_values <- map_data %>% filter(percentage_of_YGROSS >= 0)
if(nrow(positive_values) > 0) {
  quantile_breaks <- quantile(positive_values$percentage_of_YGROSS, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
  
  quantile_labels <- c(
    paste0("Q1 (", round(quantile_breaks[1], 2), " - ", round(quantile_breaks[2], 2), ")"),
    paste0("Q2 (", round(quantile_breaks[2], 2), " - ", round(quantile_breaks[3], 2), ")"),
    paste0("Q3 (", round(quantile_breaks[3], 2), " - ", round(quantile_breaks[4], 2), ")"),
    paste0("Q4 (", round(quantile_breaks[4], 2), " - ", round(quantile_breaks[5], 2), ")")
  )
  
  positive_quantiles <- positive_values %>%
    mutate(
      quantile_group = cut(
        percentage_of_YGROSS,
        breaks = quantile_breaks,
        include.lowest = TRUE,
        labels = quantile_labels
      )
    ) %>%
    select(id, quantile_group)
  
  # Join quantiles back to map data
  map_data <- map_data %>%
    left_join(st_drop_geometry(positive_quantiles), by = "id") %>%
    mutate(quantile_group = coalesce(quantile_group.x, quantile_group.y)) %>%
    select(-quantile_group.x, -quantile_group.y, -id)
} else {
  # Fallback if no positive values
  quantile_labels <- c("Q1 (0 - 0.08)", "Q2 (0.08 - 0.35)", "Q3 (0.35 - 1.9)", "Q4 (1.9 - 20.5)")
  map_data <- map_data %>% select(-id)
}

# Relevel quantile groups
map_data <- map_data %>%
  mutate(quantile_group = fct_relevel(quantile_group, "Benefits", after = Inf))

# Define custom colors for map
map_colors <- c(
  "Benefits" = "darkgreen",
  "Q1 (0 - 0.08)" = "#FADBD8",
  "Q2 (0.08 - 0.35)" = "#F1948A", 
  "Q3 (0.35 - 1.9)" = "#CD6155",
  "Q4 (1.9 - 20.5)" = "#A93226"
)

# Create Panel C: Sectoral damages map
sectoral_damages_plot <- ggplot(map_data %>% filter(continent != "Antarctica")) +
  geom_sf(aes(fill = quantile_group), color = "grey", linewidth = 0.1) +
  facet_wrap(~oc_capital) +
  scale_fill_manual(values = map_colors, na.value = "transparent", name = "% of GDP") +
  coord_sf(crs = "+proj=robin") +
  theme_void() +
  labs(
    title = "C. Substitutability-adjusted Damages in 2050",
    fill = "% of GDP"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0)
  )

# FINAL COMBINED PLOT -----------------------------------------------------

# Adjust plot margins for better alignment
market_eq_gg2 <- market_eq_plot + theme(plot.margin = margin(50, 10, 50, 10))
plot_sectoral_damages_legbottom <- sectoral_damages_plot

# Create the final combined plot as specified in the original code
final_plot <- ggarrange(
  time_damages_plot, 
  ggarrange(
    market_eq_gg2, 
    plot_sectoral_damages_legbottom + 
      labs(title = "C. Substitutability-adjusted Damages in 2050"),
    align = "h",
    ncol = 2,
    widths = c(2, 3)
  ),
  ncol = 1,
  heights = c(9, 10)
)

# Display the final plot
print(final_plot)

# Optional: Save the plot
# ggsave("Figure3_Ocean_Damages_Analysis.png", final_plot, 
#        width = 16, height = 12, dpi = 300)
