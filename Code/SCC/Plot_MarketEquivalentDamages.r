
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
  group_by(iso_a3, oc_capital, valuation) %>%
  slice(1) %>%
  ungroup() %>% 
  group_by(iso_a3, oc_capital) %>% 
  summarize(total_value = -sum(delta_UTARG_in_consumption, na.rm=TRUE))%>% 
  ungroup() %>% 
  mutate( oc_capital = recode(oc_capital,coral="Corals",fisheries="Fisheries & Mariculture",mangrove = "Mangroves", ports="Ports"))

glimpse(aggregated_data)
# Step 4: Read the Excel file's 'YGROSS' sheet
file_path_excel <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today_BASELINE.xlsx"
ygross_data <- read_excel(file_path_excel, sheet = "YGROSS", skip = 3, col_names = FALSE)
pop_data <- read_excel(file_path_excel, sheet = "pop", skip = 3, col_names = FALSE)
glimpse(pop_data)
# Step 2: Rename columns for easier access
# Assuming the data starts from the second row and has the following structure
names(ygross_data) <- c("t", "country", "info", "gdp_value", "upperbound", "marginal")
names(pop_data) <- c("t", "country", "pop")

glimpse(ygross_data)
# Step 5: Ensure YGROSS data is in the correct year and in 2020 trillion USD
# Assuming `YGROSS` has columns for year and country with GDP values in trillion USD
ygross_data <- ygross_data %>%
    mutate(iso_a3=toupper(country), year = as.integer(t)*5+2010) %>%
  filter(year == 2050) %>%  # Filter for 2050
  mutate(YGROSS_2020 = gdp_value * (113.647 / 87.504))  # Convert to 2020 trillion USD

pop_data <- pop_data %>%
    mutate(iso_a3=toupper(country), year = as.integer(t)*5+2010) %>%
  filter(year == 2050)

# Step 5: Prepare data for mapping
# Get world map data for country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- world %>%
  left_join(aggregated_data, by = "iso_a3") %>%
  left_join(ygross_data, by = "iso_a3") %>%
  left_join(pop_data, by="iso_a3") %>%
  mutate(percentage_of_YGROSS = (total_value * pop * 1e6/ (YGROSS_2020*1e12)) * 100) %>% 
  mutate(percentage_of_YGROSS = ifelse(percentage_of_YGROSS==0,NA,percentage_of_YGROSS))

# Step 2: Separate rows with NA in `oc_capital`, duplicate for each unique `oc_capital`, and recombine
oc_capital_levels <- unique(aggregated_data$oc_capital)

# Separate rows with NA in `oc_capital` and expand for each `oc_capital`
na_rows_expanded <- map_data %>%
  filter(is.na(oc_capital)) %>%
  dplyr::select(-oc_capital) %>%
  crossing(oc_capital = oc_capital_levels)

# Combine expanded NA rows with the original data (excluding the original NA rows)
map_data <- map_data %>%
  filter(!is.na(oc_capital)) %>%
  bind_rows(na_rows_expanded)

library(dplyr)
library(sf)

library(dplyr)
library(sf)

# Step 1: Add an ID column for easy tracking
map_data <- map_data %>%
  mutate(id = row_number())

# Step 2: Tag negative values and assign them "Negative" in quantile_group
map_data <- map_data %>%
  mutate(
    quantile_group = case_when(
      percentage_of_YGROSS < 0 ~ "Benefits",  # Tag all negative values
      TRUE ~ NA_character_  # Placeholder for positive values
    )
  )

# Step 3: Calculate quantiles for positive values only
positive_values <- map_data %>% filter(percentage_of_YGROSS >= 0)
quantile_breaks <- quantile(positive_values$percentage_of_YGROSS, probs = seq(0, 1, by = 0.25), na.rm = TRUE)

# Step 4: Create labels with specific break values
quantile_labels <- c(
  paste0("Q1 (", round(quantile_breaks[1], 2), " - ", round(quantile_breaks[2], 2), ")"),
  paste0("Q2 (", round(quantile_breaks[2], 2), " - ", round(quantile_breaks[3], 2), ")"),
  paste0("Q3 (", round(quantile_breaks[3], 2), " - ", round(quantile_breaks[4], 2), ")"),
  paste0("Q4 (", round(quantile_breaks[4], 2), " - ", round(quantile_breaks[5], 2), ")")
)

# Step 5: Assign quantile categories to positive values
positive_quantiles <- positive_values %>%
  mutate(
    quantile_group = cut(
      percentage_of_YGROSS,
      breaks = quantile_breaks,
      include.lowest = TRUE,
      labels = quantile_labels
    )
  ) %>%
  dplyr::select(id, quantile_group)

# Step 6: Join back using `st_join`, then remove unnecessary columns
map_data <- map_data %>%
  left_join(st_drop_geometry(positive_quantiles), by = "id") %>%
  mutate(quantile_group = coalesce(quantile_group.x, quantile_group.y)) %>%
  dplyr::select(-quantile_group.x, -quantile_group.y, -id) 


glimpse(map_data)

library(ggplot2)
library(dplyr)
library(forcats)

# Relevel quantile_group so that "Benefits" is at the end
map_data <- map_data %>%
  mutate(
    quantile_group = fct_relevel(quantile_group, "Benefits", after = Inf)
  )
levels(factor(map_data$quantile_group))
# Define custom color palette
custom_colors <- c(
  "Benefits" = "darkgreen",
  "Q1 (0 - 0.05)" = "#FADBD8",   # Lightest shade of red for Q1
  "Q2 (0.05 - 0.21)" = "#F1948A",        # Light red for Q2
  "Q3 (0.21 - 0.83)" = "#CD6155",        # Medium red for Q3
  "Q4 (0.83 - 9.2)" = "#A93226"   # Darkest red for Q4
)

# Create the plot
plot_sectoral_damages <- ggplot(map_data %>% filter(continent != "Antarctica")) +
  geom_sf(aes(fill = quantile_group),color="grey",linewidth=0.1) +
  facet_wrap(~oc_capital) +
  scale_fill_manual(values = custom_colors, na.value = "transparent", name = "% of GDP") +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme_void() +
  labs(
    title = "B. Damages in 2050",
    fill = "% of GDP (Quantiles)"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )+
            theme(
                plot.title = element_text(hjust = 0.5)
            )
plot_sectoral_damages 


plot_sectoral_damages_legbottom <- ggplot(map_data %>% filter(continent != "Antarctica")) +
  geom_sf(aes(fill = quantile_group), color = "grey", linewidth = 0.1) +
  facet_wrap(~oc_capital) +
  scale_fill_manual(values = custom_colors, na.value = "transparent", name = "% of GDP") +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme_void() +
  labs(
    title = "B. Damages in 2050",
    fill = "% of GDP (Quantiles)"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
  
plot_sectoral_damages_legbottom

  #ggsave("Figures/mkt_equivalent_damages_aspercentageofGDP.png")

  map_data %>% group_by(oc_capital) %>% 
    arrange(percentage_of_YGROSS) %>% 
    slice_max(percentage_of_YGROSS, n = 5, with_ties = FALSE) %>% 
    dplyr::select(country.x,percentage_of_YGROSS,oc_capital,sovereignt) %>% as.data.frame()
    glimpse(map_data)
    glimpse(plot_data)
map_data_gdp <- map_data %>% 
  mutate(country=as.character(country.x)) %>% 
  left_join(plot_data %>% filter(year==2025,capital=="Corals"),by="country")

ggplot(map_data_gdp, aes(y = percentage_of_YGROSS, x = (YNET/pop.x), color = oc_capital)) + 
  geom_point() +
  #geom_smooth(formula = y ~ x, method = "lm", aes(fill = oc_capital)) + 
  geom_smooth(formula = y ~ x, aes(fill = oc_capital)) + 
  theme_minimal() +
  scale_color_manual(values = Color_capitals_black) +
  scale_fill_manual(values = Color_capitals_black) +
  labs(x = "Log GDP in 2030", y = "Damages in 2050") 
  #+
  #coord_cartesian(ylim = c(0, 4))

