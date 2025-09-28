library(tidyverse)

# Load deaths_by_country_fisheries
deaths_by_country_fisheries <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/deaths_by_country_Globalvsl.csv")
deaths_by_country_fisheries <- deaths_by_country_vsl
glimpse(deaths_by_country_fisheries)


fisheries_1C <- deaths_by_country_fisheries %>%
    filter(scenario=="SSP245",year==2065)



glimpse(fisheries_1C)
# Sum deaths_future and calculate standard error for the sum at 1C
mean_fisheries <- sum(fisheries_1C$deaths_future, na.rm = TRUE)
se_fisheries <- sqrt(sum((fisheries_1C$deaths_future_se)^2, na.rm = TRUE))

deaths_percapita_future <- mean(fisheries_1C$deaths_percapita_future, na.rm = TRUE)
#[1] 1.788938e-05 adjusted by nutritional dependcency
#[1] 1.507979e-05 adjusted by Nutritional dependcency and undernourishment
1.507979e-05/1.788938e-05
1.788938e-05/1.507979e-05
deaths_percapita_future_se <- sqrt(mean((fisheries_1C$deaths_percapita_future_se)^2, na.rm = TRUE))

# Load temperature and excess deaths data
temp_df <- read.csv("Data/other/excess_deaths/give-mortality-ph/output_vars/results/CromarMortality_temperature.csv")
excess_df <- read.csv("Data/other/excess_deaths/give-mortality-ph/output_vars/results/CromarMortality_excess_deaths_global.csv")
mortality_change_df <- read.csv("Data/other/excess_deaths/give-mortality-ph/output_vars/results/CromarMortality_mortality_change_global.csv")
# Merge by trialnum
merged_df <- inner_join(temp_df, excess_df, by = c("trialnum", "time"))
merged_df <- inner_join(merged_df, mortality_change_df, by = c("trialnum", "time"))
# Filter for temperature ~ 1C (allowing a small tolerance)
excess_1C <- merged_df %>%
    filter(abs(temperature - 1) < 0.01)

# Fit normal distribution to excess_deaths_global at 1C
mean_excess <- mean(excess_1C$excess_deaths_global, na.rm = TRUE)
se_excess <- sd(excess_1C$excess_deaths_global, na.rm = TRUE)
mean_mortality_change <- mean(excess_1C$mortality_change_global, na.rm = TRUE)
se_mortality_change <- sd(excess_1C$mortality_change_global, na.rm = TRUE)

# Read the new CSV file
edfcsv <- read.csv("C:/Users/basti/Downloads/data-001/data/2_projection/3_impacts/main_specification/extracted/montecarlo/rcp85/low/SSP3/rcp85-SSP3-combined-noadapt-low-aggregated-rates-edfcsv.csv")
# Filter for region == "" and year == 2048
edfcsv_filtered <- edfcsv %>% filter(region == "", year == 2048)

# Get mean value (assuming column is named 'mean')
mean_edfcsv <- edfcsv_filtered$mean


cat(sprintf("Mean value at region=='' and year==2048: %.6f\n", mean_edfcsv))

cat(sprintf("Mean mortality change at 1C (fisheries model): %.6f (SE = %.6f)\n", deaths_percapita_future, deaths_percapita_future_se))
cat(sprintf("Mean mortality change at 1C (GIVE model): %.6f (SE = %.6f)\n", mean_mortality_change, se_mortality_change))
cat(sprintf("Mean excess deaths at 1C (GIVE model): %.6f (SE = %.6f)\n", mean_excess, se_excess))
cat(sprintf("Mean deaths at 1C (fisheries model): %.0f (SE = %.0f)\n", mean_fisheries, se_fisheries))

cat("The ratio of deaths in our model versus the GIVE model mean at 1C is:", mean_fisheries/mean_excess, "\n")
cat("The ratio of mortality rate in our model versus the GIVE model mean at 1C is:", deaths_percapita_future/mean_mortality_change, "\n")
cat("The ratio of excess deaths in our model versus the GIVE model mean at 1C is:", deaths_percapita_future/(mean_edfcsv), "\n")




## Table A9.6: Climate-related deaths per capita (in deaths per million) by region and risk factor
## (FVC: fruit and vegetable consumption; MTC: red-meat consumption; UND: underweight; OVW: overweight; OBS: obesity).  
    deaths_nut_springmann_risk <- read_excel("Data/other/excess_deaths/Springmann_nutrition/NutritionDeaths_Springmann2016.xlsx", sheet = "Table A9.6",skip=2)
    names(deaths_nut_springmann_risk)<- c("iso_a3","total","fvc","mtc","und","ovw","obs")
    deaths_nut_springmann_risk <- deaths_nut_springmann_risk %>% mutate(total_adj = fvc + mtc + und)
    deaths_nut_springmann_risk <- gather(deaths_nut_springmann_risk, key = "risk_factor", value = "value", total:total_adj) %>%
    #deaths_nut_springmann_risk_map <- left_join(world %>% dplyr::select(iso_a3), deaths_nut_springmann_risk, by = "iso_a3") %>% 
                        filter(risk_factor=="total_adj")
                        
    ssp_pop <- read.csv(file="Data/other/excess_deaths/Springmann_nutrition/ssp_pop.csv")
    
    pop_2050 <- ssp_pop %>%
        filter(year == 2050) %>%
        dplyr::select(ISO3, scenario, Pop.million)

        # Merge deaths_nut with population
        merged_data <- deaths_nut_springmann_risk %>%
        inner_join(pop_2050, by = c("iso_a3" = "ISO3"))

        # Calculate weighted average of 'value' by SSP scenario
        weighted_avg <- merged_data %>%
        group_by(scenario) %>%
        summarise(weighted_value = sum(value * Pop.million) / sum(Pop.million))

        mean_weighted_avg <- mean(weighted_avg$weighted_value, na.rm = TRUE)
cat("In 2050, Springmann et al estimate", mean_weighted_avg, 
    "deaths per million due to nutrition-related risk factors. This is an average of all RCP scenarios.\n")
