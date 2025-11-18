# Convert GDX Files to XLSX ----
    system("gdx2xls C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today.gdx")
    system("gdx2xls C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_damage_pulse.gdx")
    system("gdx2xls C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_damage.gdx")


## Read BLUERICE50x Results
    # Key variables
    var_names <- c(
        "C", "ocean_consump_damage_coef", "ocean_consump_damage_coef_sq", "YNET",
        "OCEAN_USENM_VALUE_PERKM2", "VSL", "CPC", "OCEAN_NONUSE_VALUE_PERKM2",
        "OCEAN_AREA", "ocean_area_start", "ocean_value_intercept_unm", "ocean_value_exp_unm",
        "ocean_value_intercept_nu", "ocean_value_exp_nu", "pop", 
        "ocean_health_beta", "ocean_health_mu", "ocean_health_tame"
    )
    exp_names <- "damage"

    process_data(exp_names, var_names)

    ## Temperature
    tatm <- read_excel('Data/output_rice50x/results_ocean_damage.xlsx', sheet = "TATM") %>%
        dplyr::select(year = 1, tatm = 3) %>%
        mutate(
            year = 1980 + (as.integer(year) - 1) * 5,
            tatm = as.double(tatm)
        ) %>%
        filter(!is.na(year)) %>%
        as.data.frame()

    tatm_2025 <- tatm %>%
        filter(year == 2025) %>%
        pull(tatm)

    tatm <- tatm %>%
        mutate(tatm_diff_2025 = tatm - tatm_2025)

    ## Merge Temp with Key variables
    exp_data_damage <- exp_data_damage %>%
        left_join(tatm, by = "year")
## Read BLUERICE50x Results


## Ports  (start)
    ports_dam <- exp_data_damage %>% 
        filter(capital =="ports",year>2024) %>% 
        arrange(country, year) %>%  # Ensure the data is sorted by country and year
        group_by(country) %>%  # Group by country
        mutate(YNET_dam_mkt_dif = YNET*tatm_diff_2025*ocean_consump_damage_coef*10^6, #YNET is in Trillions, so this value is in Millions
        YNET_dam_mkt_dif_perc = 100*tatm_diff_2025*ocean_consump_damage_coef, 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        YNET_dam_mkt_dif_perc2025 = (YNET_dam_mkt_dif / net_GDP_2025) * 100) %>% 
        mutate(cumulative_YNET_dam_mkt_dif = cumsum(-YNET_dam_mkt_dif), 
        cumulative_YNET_dam_mkt_dif_perc = cumsum(-YNET_dam_mkt_dif_perc), 
        cumulative_YNET_dam_mkt_dif_perc2025 = cumsum(YNET_dam_mkt_dif_perc2025)) %>%  # Compute the cumulative sum
        ungroup() %>% as.data.frame()  

## Ports 

## Corals 
   
    cor_dam <- exp_data_damage %>% 
        filter(capital =="coral",year>2024) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(usenm_nodam = OCEAN_USENM_VALUE_PERKM2*OCEAN_AREA[year==2025],
        usenm_dam = OCEAN_USENM_VALUE_PERKM2*OCEAN_AREA,
        nonuse_nodam = OCEAN_NONUSE_VALUE_PERKM2*OCEAN_AREA[year==2025],
        nonuse_dam = OCEAN_NONUSE_VALUE_PERKM2*OCEAN_AREA,
        nonuse_dif = nonuse_dam - nonuse_nodam,
        usenm_dif = usenm_dam - usenm_nodam,
        YNET_dam_mkt_dif = YNET*tatm_diff_2025*ocean_consump_damage_coef*10^6, #YNET is in Trillions, so this value is in Millions
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        dam_dif_perc2025 = (-(YNET_dam_mkt_dif+nonuse_dif+usenm_dif) / net_GDP_2025) * 100) %>%

 
        mutate(cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)) %>%
        ungroup() %>% as.data.frame()  


## Corals 

## Mangroves 

    man_dam <- exp_data_damage %>% 
        filter(capital =="mangrove",year>2024) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(

        usenm_nodam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*OCEAN_AREA[year==2025],
        nu_nodam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*OCEAN_AREA[year==2025],

        usenm_dam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*OCEAN_AREA,
        nu_dam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*OCEAN_AREA,


        nonuse_dif =(nu_dam - nu_nodam)*10^6/1.298763, #It was in trillions, Convert to Millions and deflate it from 2020 to 2005 usd
        usenm_dif = (usenm_dam - usenm_nodam)*10^6/1.298763, #It was in trillions, Convert to Millions and deflate it from 2020 to 2005 usd

        YNET_dam_mkt_dif = YNET*(tatm_diff_2025*ocean_consump_damage_coef + ocean_consump_damage_coef_sq*tatm_diff_2025^2)*10^6, #YNET is in Trillions, so this value is in Millions
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        all_dam =-(YNET_dam_mkt_dif+nonuse_dif+usenm_dif)) %>%

        mutate(all_dam_post2025 = all_dam - first(all_dam[year==2025]),
        dam_dif_perc2025 = (all_dam_post2025 / net_GDP_2025) * 100, 
        dam_dif_perc2025_all = (all_dam / net_GDP_2025) * 100) %>% as.data.frame()  
        man_dam %>% filter(country %in% c("ind","idn")) %>% select(OCEAN_AREA,YNET,year,ocean_value_intercept_nu,ocean_value_exp_nu,pop, nu_dam,country)

        ggplot(man_dam %>% filter(country %in% c("ind","bra","idn")), 
        aes(x = year, y = OCEAN_AREA,color=country)) +
            geom_line() +
            geom_point() +
            labs(title = "Ocean Area Over Time (India)",
                     x = "Year",
                     y = "Ocean Area") +
            theme_minimal()


## Mangroves 


## Fish 
    exp_data_damage %>% 
        filter(year %in% c(2020,2025,2030),country=="brb", capital=="fisheries") %>% 
        dplyr::select(ocean_health_beta,ocean_health_tame,pop,year,ocean_health_mu)
    
    ocean_health_eta = 0.05
    fish_dam <- exp_data_damage %>% 
        filter(capital =="fisheries",year>2024) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(
                usenm_dif = (ocean_health_beta * (tatm_diff_2025)) * ocean_health_tame * pop*1e6 * ocean_health_mu* ocean_health_eta * VSL,

        # usenm_nodam = ocean_health_tame* pop*1e6 * ocean_health_mu * health_eta * VSL,
        # usenm_dam = ocean_health_beta*tatm*ocean_health_tame* pop*1e6 * ocean_health_mu * health_eta * VSL,

        # usenm_dif = usenm_dam - usenm_nodam,

        YNET_dam_mkt_dif = YNET*(tatm_diff_2025*ocean_consump_damage_coef)*10^6, #YNET is in Trillions, so this value is in Millions
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        all_dam =-(YNET_dam_mkt_dif+usenm_dif)) %>%
        ungroup() %>% as.data.frame()

## Fish 
       
####
## Across Time

fish_dam <- fish_dam %>% mutate(capital = "Fisheries & Mariculture")# %>% filter(country!="ken")
man_dam <- man_dam %>% mutate(capital = "Mangroves")
cor_dam <- cor_dam %>% mutate(capital = "Corals")
ports_dam <- ports_dam %>% mutate(capital = "Ports")

merged_df <- bind_rows(fish_dam, man_dam, cor_dam, ports_dam)
# Filter data for the year 2100
end_year <- 2100

# Reshape data to long format
plot_data <- merged_df %>% 
  dplyr::select(year, country,YNET,pop, capital, usenm_dif, nonuse_dif, YNET_dam_mkt_dif,CPC) %>%
  pivot_longer(cols = c(usenm_dif, nonuse_dif, YNET_dam_mkt_dif), 
               names_to = "variable", 
               values_to = "value")%>%
  mutate(variable = recode(variable,
                           usenm_dif = "Non-market Use",
                           nonuse_dif = "Non-use",
                           YNET_dam_mkt_dif = "Market"),
         variable = factor(variable, levels = c("Market", "Non-market Use", "Non-use"))) %>% filter(country!="row") #Escluding rest of the world category

# Define shapes for each variable
shapes <- c("Market" = 15, "Non-market Use" = 16, "Non-use" = 17)



    ##Overall, market use damages are the largest in absolute terms, totaling global annual losses of US$1.66 trillion in 2100, followed by damages in non-market use values amounting to US$182 billion in 2100, and non-use values adding up to US$13 billion in annual losses, all results are expressed in 2020 USD equivalent
    plot_data %>% filter(year == end_year) %>% 
        group_by(variable) %>% 
        summarize(median_damage = median(-value, na.rm = TRUE),
                  mean_damage = mean(-value, na.rm = TRUE),
                  total_damage = sum(-value, na.rm = TRUE)*1.298763) %>%  #Convert to 2020 USD
        arrange(desc(median_damage))

    

    # Market damages are dominated by losses in seaport activities (93.6%), followed by corals (3.9%), fisheries and mariculture (2.4%), and then mangroves (0.1%). 
    market_summary <- plot_data %>% 
            filter(year == end_year, variable == "Market") %>%
            group_by(capital) %>%
            summarize(
                median_damage = median(-value, na.rm = TRUE),
                mean_damage = mean(-value, na.rm = TRUE),
                total_damage = sum(-value, na.rm = TRUE) 
            ) %>%
            ungroup()

        market_total <- sum(market_summary$total_damage, na.rm = TRUE)

        market_summary <- market_summary %>%
            mutate(percentage_of_total = 100 * total_damage / market_total) %>%
            arrange(desc(median_damage))

        market_summary

        # Merge plot_data with regions to add region info
        plot_data_region <- plot_data %>%
            mutate(country_upper = toupper(as.character(country))) %>%
            left_join(regions, by = c("country_upper" = "countrycode"))

        # Calculate total 2100 market damages by region
        market_damages_2100 <- plot_data_region %>%
            filter(year == 2100, variable == "Market", !is.na(R5)) %>%
            group_by(R5) %>%
            summarize(total_market_damage = sum(-value, na.rm = TRUE)) %>%
            ungroup()

        # Calculate percentage by region
        market_damages_2100 <- market_damages_2100 %>%
            mutate(percentage = 100 * total_market_damage / sum(total_market_damage, na.rm = TRUE))

        #The regions bearing the highest losses are ASIA (37%) and MAF (34%), followed by OECD (16%), LAM (12%), and REF (1%).
        market_damages_2100



        # Calculate total 2100 market damages by region
        nonmarket_damages_2100 <- plot_data_region %>%
            filter(year == 2100, variable == "Non-market Use", !is.na(R5)) %>%
            group_by(R5) %>%
            summarize(total_nonmarket_damage = sum(-value, na.rm = TRUE)) %>%
            ungroup()

        # Calculate percentage by region
        nonmarket_damages_2100 <- nonmarket_damages_2100 %>%
            mutate(percentage = 100 * total_nonmarket_damage / sum(total_nonmarket_damage, na.rm = TRUE))

        #Non-market use value losses from corals and mangroves, which provide important coastal protection services, are also large. Regional losses are highest in MAF (44%) and ASIA (40%), followed by OECD (12%), LAM (2%), and REF (2%).
        nonmarket_damages_2100


        # Calculate total 2100 market damages by region
        nonuse_damages_2100 <- plot_data_region %>%
            filter(year == 2100, variable == "Non-use", !is.na(R5)) %>%
            group_by(R5) %>%
            summarize(total_nonuse_damage = sum(-value, na.rm = TRUE)) %>%
            ungroup()

        # Calculate percentage by region
        nonuse_damages_2100 <- nonuse_damages_2100 %>%
            mutate(percentage = 100 * total_nonuse_damage / sum(total_nonuse_damage, na.rm = TRUE))

        #Consequently, ASIA bears 94% of these damages, followed by OECD (5%), LAM, and MAF. 
        nonuse_damages_2100

  time_damages_plot <- ggplot(plot_data %>% filter(value<0,year < end_year+1,country!="ken" ), 
            aes(x = CPC/1000, y = -value, color = capital, group = interaction(country,capital, variable))) +
        geom_line(alpha=0.5) +
        geom_point(data = plot_data %>% filter(year == end_year,country!="ken" ), 
                    aes(shape = variable), size = 2,alpha=0.4) +
        geom_text_repel(data = plot_data %>% filter(year == end_year,country!="ken" ), 
                    aes(label=country), size = 2) +
        scale_shape_manual(values = shapes) +
        facet_wrap(~variable)+
        #facet_wrap(~variable,scales="free")+ 
        scale_color_manual(values=Color_capitals_black)+
  scale_y_log10(labels = scales::dollar_format(suffix = "M", accuracy = 0.0001)) +
        #scale_x_log10() +
        labs(title = "A. Ocean-based damages under SSP2",
            x = "GDP per capita (Thousand USD)",
            y = "Damages (Million USD)",
            color = "Blue Capital",
            shape = "Value Category") +
        theme_minimal()+ 
                    theme(
                        plot.title = element_text(hjust = 0.5)
                    )

                    time_damages_plot

  time_damages_plot_onlyfisheries <- ggplot(plot_data %>% filter(year < end_year+1,capital=="Fisheries & Mariculture",variable=="Market"), 
            aes(x = CPC/1000, y = -value, color = capital, group = interaction(country,capital, variable))) +
        geom_line(alpha=0.5) +
        geom_point(data = plot_data %>% filter(year == end_year,capital=="Fisheries & Mariculture",variable=="Market"), 
                    aes(shape = variable), size = 2,alpha=0.4) +
        geom_text_repel(data = plot_data %>% filter(year == end_year,capital=="Fisheries & Mariculture",variable=="Market"), 
                    aes(label=country), size = 3) +
        scale_shape_manual(values = shapes) +
        facet_wrap(~variable)+
        #facet_wrap(~variable,scales="free")+ 
        scale_color_manual(values=Color_capitals_black)+
        #scale_x_log10() +
        labs(title = "A. Fisheries damages under SSP2",
            x = "GDP per capita (Thousand USD)",
            y = "Damages (Million USD)",
            color = "Blue Capital",
            shape = "Value Category") +
        theme_minimal()+ 
                    theme(
                        plot.title = element_text(hjust = 0.5)
                    )
                    
  time_damages_plot_onlyfisheries 
  plot_data %>% filter(year < end_year+1,capital=="Fisheries & Mariculture",variable=="Market",country=="rus") 
        #ggsave("C:/Users/basti/Documents/GitHub/BlueDICE/Figures/SM/fisheries/fisheries_losses.jpg")





###### Figure 3C
# Step 1: Read the CSV file
file_path <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/analysis_output/MarketEquivalentValues.csv"
data <- read_csv(file_path)


data_adjusted <- data %>%
  mutate(year = t * 5 + 2010)%>% 
  mutate(iso_a3=toupper(n))%>% 
  mutate( oc_capital = recode(oc_capital,coral="Corals",fisheries="Fisheries & Mariculture",mangrove = "Mangroves", ports="Ports")) %>%
  mutate(total_value = -delta_UTARG_in_consumption)




# Step 2: Adjust 't' to get the actual year
data <- data %>%
  mutate(year = t * 5 + 2010)

# Step 3: Filter for the year 2050
data_2050 <- data %>%
  filter(year == 2050) %>% 
  mutate(iso_a3=toupper(n))
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
file_path_excel <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today.xlsx"
ygross_data <- read_excel(file_path_excel, sheet = "YGROSS", skip = 3, col_names = FALSE)
pop_data <- read_excel(file_path_excel, sheet = "pop", skip = 3, col_names = FALSE)
# Step 2: Rename columns for easier access
# Assuming the data starts from the second row and has the following structure
names(ygross_data) <- c("t", "country", "info", "gdp_value", "upperbound", "marginal")
names(pop_data) <- c("t", "country", "pop")
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



file_path_excel <- "C:/Users/basti/Documents/GitHub/BlueDICE/Data/output_rice50x/results_ocean_today.xlsx"
ygross_data <- read_excel(file_path_excel, sheet = "YGROSS", skip = 3, col_names = FALSE)
pop_data <- read_excel(file_path_excel, sheet = "pop", skip = 3, col_names = FALSE)
names(ygross_data) <- c("t", "country", "info", "gdp_value", "upperbound", "marginal")
names(pop_data) <- c("t", "country", "pop")
ygross_data <- ygross_data %>%
    mutate(iso_a3=toupper(country), year = as.integer(t)*5+2010) %>%
  mutate(YGROSS_2020 = gdp_value * (113.647 / 87.504))  # Convert to 2020 trillion USD

pop_data <- pop_data %>%
    mutate(iso_a3=toupper(country), year = as.integer(t)*5+2010) 
data_adjusted <- data_adjusted %>%
  #left_join(aggregated_data, by = "iso_a3") %>%
  left_join(ygross_data, by = c("iso_a3","year")) %>%
  left_join(pop_data, by= c("iso_a3","year")) %>%
  mutate(percentage_of_YGROSS = (total_value * pop * 1e6/ (YGROSS_2020*1e12)) * 100) %>% 
  mutate(percentage_of_YGROSS = ifelse(percentage_of_YGROSS==0,NA,percentage_of_YGROSS))%>% 
  filter(year %% 5 == 0, year >= 2025, year <= 2100)



data_adjusted <- data_adjusted %>% mutate(valuation= recode(valuation,
                         usenm = "Non-market Use",
                         nonuse = "Non-use",
                         consumption = "Market"),
       valuation = factor(valuation, levels = c("Market", "Non-market Use", "Non-use")),
       n = factor(n)) %>%
       mutate( adjusted_value = delta_UTARG_in_consumption * pop * 1e6) %>%
       mutate( adjusted_value_trillions = adjusted_value / 1e12) %>%
left_join(plot_data %>% 
  filter(year %% 5 == 0, year >= 2025, year <= 2100), by = c("n"="country","year","oc_capital"="capital","valuation"="variable"))


                    time_damages_plot_adjusted <- ggplot(data_adjusted %>% filter(year > 2025, year < 2101, n !="ken" ), 
                      aes(x = -value, y = -adjusted_value_trillions*10^6, color = oc_capital, group = interaction(n,oc_capital))) +
                      geom_line(alpha=0.5) +
                      geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1) + # identity line
                      # geom_point(data = plot_data %>% filter(year == end_year), 
                      #             aes(shape = variable), size = 2,alpha=0.4) +
                      # geom_text_repel(data = plot_data %>% filter(year == end_year), 
                      #             aes(label=country), size = 2) +
                      #scale_shape_manual(values = shapes) +
                      facet_wrap(~valuation) +
                      #facet_wrap(~variable,scales="free")+ 
                      scale_color_manual(values = Color_capitals_black) +
                      scale_y_log10(labels = scales::dollar_format(accuracy = 0.001)) +
                      scale_x_log10(labels = scales::dollar_format(accuracy = 0.001)) +
                      #scale_y_log10(labels = scales::dollar_format(suffix = "B", accuracy = 0.001)) +
                      #scale_x_log10(labels = scales::dollar_format(suffix = "B", accuracy = 0.001)) +
                      labs(title = "Welfare adjustment effect",
                      x = "Unadjusted Damages (Million USD)",
                      y = "Welfare-adjusted Damages\n (Million USD)",
                      color = "Blue Capital",
                      shape = "Value Category") +
                      theme_minimal()+ 
                      theme(
                      plot.title = element_text(hjust = 0.5)
                      )

                    time_damages_plot_adjusted
                    #ggsave("Figures/FigS26_welfare-adjustment-effect-sectoral_damages.jpg",  dpi = 300)

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

# Step 1: Add an ID column for easy tracking
map_data <- map_data %>%
  mutate(id = row_number())

# Step 2: Tag negative values and assign them "Negative" in quantile_group
map_data %>% filter(percentage_of_YGROSS < 0, oc_capital=="Mangroves") %>% as.data.frame()

map_data <- map_data %>%
  mutate(
    quantile_group = case_when(
      percentage_of_YGROSS < -0.001 ~ "Benefits",  # Tag all negative values
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



# Relevel quantile_group so that "Benefits" is at the end
map_data <- map_data %>%
  mutate(
    quantile_group = fct_relevel(quantile_group, "Benefits", after = Inf)
  )
levels(factor(map_data$quantile_group))
# Define custom color palette
custom_colors <- c(
  "Benefits" = "darkgreen",
  "Q1 (0 - 0.08)" = "#FADBD8",   # Lightest shade of red for Q1
  "Q2 (0.08 - 0.35)" = "#F1948A",        # Light red for Q2
  "Q3 (0.35 - 1.9)" = "#CD6155",        # Medium red for Q3
  "Q4 (1.9 - 20.5)" = "#A93226"   # Darkest red for Q4
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





#### Figure 3 Panel B



#Run Damages.R nd get cor_dam

# Define parameters
#market_value <- 10  # Initial value of the market component
market_value <- cor_dam %>% filter(year==2025,country=="aus") %>% select(YNET)*def_mult[[1]]*10^6 #YNET is in Trillion, here we convert to millions
nonmarket_value <- cor_dam %>% filter(year==2025,country=="aus") %>% select(usenm_nodam)*def_mult[[1]] #already in millions
market_value/nonmarket_value
dam <- 1 #damage is in million
eta <- 0.5  # Set a value for eta
#eta <- 1.45  # Set a value for eta


# Define theta values to evaluate (excluding theta = 0)
theta_values <- seq(-0.01, 1, by = 0.01)
theta_values <- theta_values[theta_values != 0]

# Function to calculate utility based on theta, market, and nonmarket values
calculate_utility <- function(theta, market, nonmarket) {
  #(1 / (1 - eta)) *(alpha*market^theta + (1-alpha)*nonmarket^theta)^((1-eta)/theta)
  (1 / (1 - eta)) *(market^theta + nonmarket^theta)^((1-eta)/theta)
}

# Calculate DeltaUtility for each theta
results <- data.frame(theta = theta_values, market_dollar_equivalent_loss = NA)

for (i in seq_along(theta_values)) {
  theta <- theta_values[i]
  
  # Calculate base and damaged utility
  utility_base <- calculate_utility(theta, market_value, nonmarket_value)
  utility_dam <- calculate_utility(theta, market_value, nonmarket_value - dam)
  #utility_base <- (market_value^theta + nonmarket_value^theta)^((1-eta)/theta)
  #utility_dam <- (market_value^theta + (nonmarket_value-dam)^theta)^((1-eta)/theta)
  
  # Delta Utility
  delta_utility <- utility_dam - utility_base
  
  # partial derivative of utility with respect to market
  partial_deriv_market <- (market_value^theta + (nonmarket_value-dam/2)^theta)^(((1-eta)/theta) - 1) * market_value^(theta - 1)
  
  # Calculate market dollar equivalent loss
  market_dollar_equivalent <- abs(delta_utility / partial_deriv_market)
  results$market_dollar_equivalent_loss[i] <- unlist(market_dollar_equivalent)
}

glimpse(results)
results
# Plot the results
market_eq <- ggplot(results, aes(x = theta, y = market_dollar_equivalent_loss)) +
  geom_line() +
  labs(
    title = "B. $1 Non-market Loss",
    x = expression("Substitutability Parameter" ~ theta),
    y = "Equivalent Loss in \nMarket Consumption ($)"
  ) +
  # Vertical dashed line at theta = 0 with label "Cobb-Douglas specification"
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 0., y = 15, 
           label = "Cobb-Douglas", vjust = -0.5, angle = 90,size=2.7) +
  # Blue line at (1, 1) with label "Perfect substitutes"
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  annotate("text", x = 1, y = 19, label = "Perfect substitutes", color = "steelblue", angle = 90,
           vjust = -0.5, size=2.8) +
  annotate("text", x = 1.08, y = 19, label = "Panel A", color = "steelblue", angle = 90,
           vjust = -0.5,size=2.5, 
         fontface = 'italic') +
  # Line at y = 0.21 with label "Drupp et al"
  geom_vline(xintercept = 0.21, linetype = "dashed", color = "indianred") +
  # annotate("text", x = 0.21, y = 3.5, label = "Drupp et al (chosen value)", angle = 90, 
  #          color = "red", vjust = -0.5,size=3.5) +
    annotate("text", x = 0.21, y = 19, label = "Imperfect Substitutes", angle = 90, 
           color = "indianred", vjust = -0.5,size=2.8) +
  annotate("text", x = 0.28, y = 19, label = "Panel C", angle = 90, 
           color = "indianred", vjust = -0.5,size=2.5, 
         fontface = 'italic') +
  # Bottom-left text with mathematical expression
  #annotate("text", x = min(results$theta), y = min(results$market_dollar_equivalent_loss) + 0.32, 
  #         label = "Perfect complements", hjust = 0, vjust = 1, size = 2.5) +
  #annotate("text", x = min(results$theta), y = min(results$market_dollar_equivalent_loss)+0.1, 
  #         label = expression(theta %->% -infinity), hjust = 0, vjust = 1, size = 2.5) + 
  #scale_y_continuous(trans="log10")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Left-align the title
  )+ xlim(c(-0.1,1.12))
market_eq
#ggsave("Figures/Utility/Simulation_Utility_MarketLoss.png")


market_eq_gg <- ggarrange(ggplot() + theme_void() ,market_eq,ggplot()+ theme_void(), ncol=3,widths=c(1,2,1))
#ggarrange(time_damages_plot, market_eq_gg, plot_sectoral_damages + labs(title="C. Substitutability-adjusted Damages in 2050"),ncol=1, heights=c(3,2,3))

#ggsave("Figures/Main/Fig2_Damages2050_v4.jpg",dpi=300)
market_eq_gg2 <- market_eq +
  theme(
    plot.margin = margin(t = 50, r = 10, b = 50, l = 10)  # Top, Right, Bottom, Left in 'pt'
  )


  market_eq_gg2 <- market_eq_gg2 + theme(plot.margin = margin(50, 10, 50, 10))
plot_sectoral_damages <- plot_sectoral_damages + theme(plot.margin = margin(0, 0, 0, 0))


# Save as PDF with default dimensions
ggsave("Figures/Fig3_Damages.pdf", 
  plot = ggarrange(time_damages_plot, ggarrange(market_eq_gg2, plot_sectoral_damages_legbottom + 
  labs(title="C. Substitutability-adjusted Damages in 2050"),align="h",
  ncol=2,widths=c(2,3)),ncol=1,heights=c(9,10)),
  dpi=600)

# Save as SVG with default dimensions
ggsave("Figures/Fig3_Damages.svg", 
  plot = ggarrange(time_damages_plot, ggarrange(market_eq_gg2, plot_sectoral_damages_legbottom + 
  labs(title="C. Substitutability-adjusted Damages in 2050"),align="h",
  ncol=2,widths=c(2,3)),ncol=1,heights=c(9,10)),
  dpi=600)
#ggsave("Figures/Fig3_Damages.jpg",dpi=600)









