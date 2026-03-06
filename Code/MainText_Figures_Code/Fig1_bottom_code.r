## Figure 1 Bottom — Blue Capital Scatter + Sankey

## Socioeconomics
    # Cached WDI data (to regenerate: see commented lines below)
    # gdp_data <- WDI(country="all", indicator="NY.GDP.MKTP.PP.KD", start=2020, end=2020)
    # population_data <- WDI(country="all", indicator="SP.POP.TOTL", start=2020, end=2020)
    # gdp_data <- gdp_data %>% dplyr::rename(countrycode=iso3c, GDP_2020usd=NY.GDP.MKTP.PP.KD)
    # saveRDS(gdp_data, "External_Data/other/gdp_data_2020.rds")
    # population_data <- population_data %>% dplyr::rename(countrycode=iso3c, Pop2020=SP.POP.TOTL)
    # saveRDS(population_data, "External_Data/other/population_data_2020.rds")
    population_data <- readRDS("External_Data/other/population_data_2020.rds")
    gdp_data        <- readRDS("External_Data/other/gdp_data_2020.rds")

## Regions (used for R5 grouping)
    if (!exists("regions")) {
      regions <- read.csv("External_Data/other/r5regions.csv")
      names(regions) <- c("R5", "countrycode")
      regions$R5 <- as.character(gsub("R5", "", regions$R5))
    }

## Temperature from RICE50x
    tatm <- read_excel("Results/RICE50x/results_ocean_damage.xlsx", sheet = "TATM") %>%
      dplyr::select(year = 1, tatm = 3) %>%
      mutate(year = 1980 + (as.integer(year) - 1) * 5, tatm = as.double(tatm)) %>%
      filter(!is.na(year)) %>%
      as.data.frame()
    tatm_2025 <- tatm %>% filter(year == 2025) %>% pull(tatm)
    tatm <- tatm %>% mutate(tatm_diff_2025 = tatm - tatm_2025)

## Corals
    ssp_corals_growth <- read.csv("External_Data/output_modules/corals/ssp_corals_growth.csv")

    coral_values <- ssp_corals_growth %>%
      filter(year == 2020, scenario == "SSP2") %>%
      dplyr::select(countrycode, Market_Use_Values_Undamaged_percGDP,
                    nuV_Undamaged_percGDP, nV_Undamaged_percGDP) %>%
      dplyr::rename(market_percGDP       = Market_Use_Values_Undamaged_percGDP,
                    nonmarketuse_percGDP  = nuV_Undamaged_percGDP,
                    nonuse_percGDP        = nV_Undamaged_percGDP)

    coral_values_long <- coral_values %>%
      pivot_longer(cols = starts_with("market_percGDP") |
                          starts_with("nonmarketuse_percGDP") |
                          starts_with("nonuse_percGDP"),
                   names_to = "category", values_to = "value") %>%
      mutate(category = sub("_percGDP$", "", category), capital = "Corals") %>%
      mutate(category = case_when(
        category == "nonuse"       ~ "Non-use",
        category == "market"       ~ "Market",
        category == "nonmarketuse" ~ "Non-market Use"
      ))

## Ports
    port_ssp <- read.csv("External_Data/output_modules/ports/ports_ssps_rcps.csv")

    port_values <- port_ssp %>%
      filter(year == 2022, SSP == "SSP2", RCP == "RCP26") %>%
      group_by(iso3) %>%
      summarize(risk_base_perc = sum(risk_base_perc), iso3 = iso3) %>%
      dplyr::select(iso3, risk_base_perc) %>%
      slice(1) %>%
      mutate(category = "Market", capital = "Ports") %>%
      dplyr::rename(value = risk_base_perc, countrycode = iso3) %>%
      ungroup()

## Fisheries
    fisheries_df_temp_gdp <- read.csv("External_Data/output_modules/fish/fisheries_Free_EtAl.csv")

    fish_values <- fisheries_df_temp_gdp %>%
      filter(year == 2020, rcp == "RCP26", scenario == "Full Adaptation") %>%
      dplyr::rename(value = profits_usd_percGDP_baseline, countrycode = country_iso3) %>%
      dplyr::select(value, countrycode) %>%
      mutate(category = "Market", capital = "Fisheries & Mariculture")

    VSL <- 10.05 * 10^6
    nutrition_dep <- read.csv("Data/output_modules_input_rice50x/input_rice50x/seafood_dependence.csv") %>%
      mutate(countrycode = countrycode(World_Country, origin = "country.name", destination = "iso3c"))
    nutrition_health <- read.csv("Data/output_modules_input_rice50x/input_rice50x/mortality_seafood_nutrition.csv") %>%
      left_join(gdp_data %>% dplyr::select(GDP_2020usd, countrycode), by = "countrycode") %>%
      left_join(population_data %>% dplyr::select(Pop2020, countrycode), by = "countrycode") %>%
      mutate(value = 100 * TAME_nutrients_MortalityEffect * Pop2020 * Nutritional_D * 0.05 * VSL / GDP_2020usd) %>%
      dplyr::select(value, countrycode) %>%
      mutate(category = "Non-market Use", capital = "Fisheries & Mariculture")

## Mangroves
    man_ben_perkm2 <- read.csv("External_Data/output_modules/weighted_avg_benefits.csv")
    area_man       <- read.csv("Data/output_modules_input_rice50x/input_rice50x/mangrove_area_coefficients_sq.csv")

    man_values0 <- man_ben_perkm2 %>%
      dplyr::filter(year == 2020, forcing == "onlyCC") %>%
      dplyr::select(-X, -year, -forcing, -gdppc, -GDP_SSP2, -weighted_avg_benefit_perha) %>%
      left_join(area_man %>% dplyr::select(countrycode, MangroveArea_2020_km2), by = "countrycode") %>%
      mutate(
        value    = weighted_avg_benefit_perha_percGDP * MangroveArea_2020_km2 * 100,
        category = case_when(
          type == "cultural"   ~ "Non-use",
          type == "provision"  ~ "Market",
          type == "regulation" ~ "Non-market Use"
        ),
        capital = "Mangroves"
      ) %>%
      dplyr::select(-MangroveArea_2020_km2, -weighted_avg_benefit_perha_percGDP, -type)

## Merge all capitals
    blue_cap0 <- as.data.frame(rbind(coral_values_long, port_values, fish_values, nutrition_health, man_values0))

    blue_cap <- blue_cap0 %>%
      left_join(regions, by = "countrycode") %>%
      left_join(gdp_data %>% dplyr::select(GDP_2020usd, countrycode), by = "countrycode") %>%
      left_join(population_data %>% dplyr::select(Pop2020, countrycode), by = "countrycode") %>%
      mutate(R5 = factor(R5, levels = rev(sort(unique(R5)))))

## Figure 1B — Scatter plot (value vs GDP per capita by region)
    blue_cap <- blue_cap %>%
      mutate(value_capped = ifelse(value > 100, 100, ifelse(value < 1, 1, value)))

    # Save source data
    figure_data <- blue_cap %>%
      filter(value_capped > 0) %>%
      dplyr::select(countrycode, R5, capital, category, value_capped, GDP_2020usd, Pop2020) %>%
      mutate(GDP_per_capita_thousand_2020USD = (GDP_2020usd / Pop2020) / 1000)
    write.csv(figure_data,
              "Code/MainText_Figures_Code/figure_source_data/Fig1_bottom_scatter_data.csv",
              row.names = FALSE)

    capital_plot <- ggplot(blue_cap %>% filter(value_capped > 0)) +
      geom_point(aes(x = (GDP_2020usd / Pop2020) / 1000, y = R5,
                     shape = category, size = value_capped, color = capital),
                 position = position_jitter(width = 0, height = 0.3), alpha = 0.5) +
      scale_color_manual(values = Color_capitals_black) +
      scale_size_continuous(range = c(1, 8)) +
      scale_x_continuous(trans = "log10") +
      ylab("") + xlab("") +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      labs(size  = "Value of Benefit\n(shown as %GDP)",
           shape = "Value Category",
           color = "Blue Capital",
           x     = "GDP per capita (Thousand 2020 USD)")

## Figure 1C — Sankey diagram
    blue_cap_summary <- blue_cap %>%
      filter(!is.na(R5) & !is.na(capital) & !is.na(value)) %>%
      group_by(R5, capital) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      mutate(R5 = factor(R5, levels = rev(sort(unique(R5)))))

    # Calculate spacing
    total_counts <- blue_cap_summary %>%
      group_by(R5) %>%
      summarise(total_count = sum(count)) %>%
      arrange(total_count) %>%
      mutate(region_spacing = 500 - (cumsum(total_count) / sum(total_count) * 100))

    blue_cap_summary2 <- blue_cap_summary %>%
      left_join(total_counts, by = "R5") %>%
      mutate(R5 = factor(R5, levels = total_counts$R5))

    spacing_new <- blue_cap_summary2 %>%
      group_by(R5) %>% slice(1) %>%
      mutate(capital = "transparent",
             count   = 252 * (region_spacing / 100),
             color   = capital,
             r5      = as.character(R5))
    spacing_new$capital <- c("t5", "t2", "t3", "t4", "t1")
    spacing_new$r5[spacing_new$R5 == "ASIA"] <- "ra"
    spacing_new$r5[spacing_new$R5 == "MAF"]  <- "rm"
    spacing_new$r5[spacing_new$R5 == "LAM"]  <- "rl"
    spacing_new$r5[spacing_new$R5 == "OECD"] <- "ro"
    spacing_new$r5[spacing_new$R5 == "REF"]  <- "rr"

    blue_cap_summary2$color <- blue_cap_summary2$capital
    blue_cap_summary2$r5    <- as.character(blue_cap_summary2$R5)

    blue_cap_summary3 <- rbind(blue_cap_summary2, spacing_new) %>%
      mutate(
        capital = factor(capital, levels = c("t1","Corals","t2","Fisheries & Mariculture",
                                             "t3","Mangroves","t4","Ports","t5")),
        r5 = factor(r5, levels = c("ASIA","ra","LAM","rl","MAF","rm","OECD","ro","REF","rr"))
      )

    # Save source data
    sankey_data <- blue_cap_summary3 %>%
      dplyr::select(capital, r5, count, R5, total_count, region_spacing, color)
    write.csv(sankey_data,
              "Code/MainText_Figures_Code/figure_source_data/Fig1_bottom_sankey_data.csv",
              row.names = FALSE)

    sankey <- ggplot(data = blue_cap_summary3,
                     aes(axis1 = capital, axis2 = r5, y = count)) +
      geom_alluvium(aes(fill = capital), width = 0.1, knot.pos = 0.4, alpha = 1) +
      geom_stratum(width = 0.1, fill = "transparent", color = "transparent") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      theme_void() +
      scale_fill_manual(values = c(Color_capitals_black,
                                   t1 = "transparent", t2 = "transparent",
                                   t3 = "transparent", t4 = "transparent",
                                   t5 = "transparent")) +
      theme(legend.position = "none")
