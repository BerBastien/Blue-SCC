# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(sf)  # For spatial data handling
library(rnaturalearth)  # For country boundaries
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)

# Step 1: Read the CSV file
file_path <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/analysis_output/MarketEquivalentValues.csv"
data <- read_csv(file_path)

# Step 2: Adjust 't' to get the actual year
data <- data %>%
  mutate(year = t * 5 + 2010)

# Step 3: Filter for the year 2050
data_2050 <- data %>%
  filter(year == 2050) %>% 
  mutate(iso_a3=toupper(n))
glimpse(data_2050)
# Step 4: Aggregate by 'oc_capital' and 'country'
# Assuming 'country' is a column in your data. If not, adjust accordingly.
aggregated_data <- data_2050 %>%
  group_by(iso_a3, oc_capital) %>%
  summarize(total_value = sum(delta_UTARG_in_consumption, na.rm = TRUE))
# Step 4: Read the Excel file's 'YGROSS' sheet
file_path_excel <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today.xlsx"
ygross_data <- read_excel(file_path_excel, sheet = "YGROSS", skip = 3, col_names = FALSE)

# Step 2: Rename columns for easier access
# Assuming the data starts from the second row and has the following structure
names(ygross_data) <- c("t", "country", "info", "gdp_value", "upperbound", "marginal")

glimpse(ygross_data)
# Step 5: Ensure YGROSS data is in the correct year and in 2020 trillion USD
# Assuming `YGROSS` has columns for year and country with GDP values in trillion USD
ygross_data <- ygross_data %>%
    mutate(iso_a3=toupper(country), year = as.integer(t)*5+2010) %>%
  filter(year == 2050) %>%  # Filter for 2050
  mutate(YGROSS_2020 = gdp_value * (113.647 / 87.504))  # Convert to 2020 trillion USD

# Step 5: Prepare data for mapping
# Get world map data for country boundaries

map_data <- world %>%
  left_join(aggregated_data, by = "iso_a3") %>%
  left_join(ygross_data, by = "iso_a3") %>%
  mutate(percentage_of_YGROSS = (total_value / YGROSS_2020) * 100)

# Step 2: Separate rows with NA in `oc_capital`, duplicate for each unique `oc_capital`, and recombine
oc_capital_levels <- unique(aggregated_data$oc_capital)

# Separate rows with NA in `oc_capital` and expand for each `oc_capital`
na_rows_expanded <- map_data %>%
  filter(is.na(oc_capital)) %>%
  select(-oc_capital) %>%
  crossing(oc_capital = oc_capital_levels)

# Combine expanded NA rows with the original data (excluding the original NA rows)
map_data <- map_data %>%
  filter(!is.na(oc_capital)) %>%
  bind_rows(na_rows_expanded)

# Step 3: Create quantile categories for `percentage_of_YGROSS`
map_data <- map_data %>%
  mutate(
    percentage_quantile = cut(
      percentage_of_YGROSS,
      breaks = quantile(percentage_of_YGROSS, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)")
    )
  )

# Step 4: Plot the map with color-coded quantiles in facets, including all oc_capital values
ggplot(map_data %>%filter(continent != "Antarctica")) +
  geom_sf(aes(fill = percentage_quantile), color = "black") +
  facet_wrap(~oc_capital) +
  scale_fill_brewer(palette = "YlGnBu", na.value = "grey90", name = "Percentage of GDP") +
  theme_minimal() +
  labs(
    title = "Country-Level Market-Equivalent Losses in 2050",
    fill = "% of GDP (Quantiles)"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
  ggsave("mkt_equivalent_damages_aspercentageofGDP.png")
# Calculate quantile breaks for 'percentage_of_YGROSS' in four quantiles
quantile_breaks <- quantile(map_data$percentage_of_YGROSS, probs = seq(0, 1, by = 0.25), na.rm = TRUE)

# Display the quantile breaks
print(quantile_breaks/100)

map_data <- map_data %>%
  mutate(
    quantile_category = cut(
      percentage_of_YGROSS,
      breaks = quantile_breaks,
      include.lowest = TRUE,
      labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)")
    )
  )
ggplot(map_data, aes(x = log(percentage_of_YGROSS), fill = quantile_category)) +
  geom_histogram(color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "YlGnBu", name = "Quantiles") +
  labs(
    title = "Histogram of Percentage of YGROSS",
    x = "Percentage of YGROSS",
    y = "Count"
  ) +
  theme_minimal()
