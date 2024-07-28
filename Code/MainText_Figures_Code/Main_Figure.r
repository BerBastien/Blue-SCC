#Data Arrange (start)

    ## Socioeconomics (start)
        # gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 2020, end = 2020) 
        # population_data <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2020, end = 2020)
        # gdp_data <- gdp_data %>% dplyr::rename(countrycode=iso3c,GDP_2020usd=NY.GDP.MKTP.PP.KD)
        # saveRDS(gdp_data, "Data/other/gdp_data_2020.rds")
        # population_data  <- population_data  %>% dplyr::rename(countrycode=iso3c,Pop2020=SP.POP.TOTL)
        # saveRDS(population_data, "Data/other/population_data_2020.rds")

        population_data <- readRDS("Data/other/population_data_2020.rds")
        gdp_data_data <- readRDS("Data/other/gdp_data_2020.rds")

        
    ## Socioeconomics (end)

    ## Temperature from RICE50x
        # Load and preprocess data
        tatm <- read_excel(paste0('Data/output_rice50x/results_ocean_', exp_names[i], '.xlsx'), sheet = "TATM") %>%
        select(year = 1, tatm = 3) %>%
        mutate(year = 1980 + (as.integer(year) - 1) * 5, tatm = as.double(tatm)) %>%
        filter(!is.na(year)) %>%
        as.data.frame()

        # Get tatm value for the year 2025
        tatm_2025 <- tatm %>% filter(year == 2025) %>% pull(tatm)

        # Calculate difference from tatm value in 2025
        tatm <- tatm %>% mutate(tatm_diff_2025 = tatm - tatm_2025)
    ## Temperature from RICE50x


    ## Corals (start)
        ssp_corals_growth <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\corals\\ssp_corals_growth.csv")
        glimpse(ssp_corals_growth)

        ## Present Value as Percent
            coral_values <- ssp_corals_growth %>% filter(year==2020,scenario=="SSP2") %>% 
                select(countrycode,Market_Use_Values_Undamaged_percGDP,nuV_Undamaged_percGDP,nV_Undamaged_percGDP) %>% 
                dplyr::rename( market_percGDP = Market_Use_Values_Undamaged_percGDP, nonmarketuse_percGDP =nuV_Undamaged_percGDP, nonuse_percGDP=nV_Undamaged_percGDP)
            glimpse(coral_values)

            coral_values_long <- coral_values %>%
                pivot_longer(cols = starts_with("market_percGDP") | starts_with("nonmarketuse_percGDP") | starts_with("nonuse_percGDP"), 
                            names_to = "category", 
                            values_to = "value") %>%
                mutate(category = sub("_percGDP$", "", category),capital="Corals") %>% 
                mutate(category = case_when(category=="nonuse"~"Non-use", category=="market"~"Market",category=="nonmarketuse"~"Non-market Use"))

            glimpse(coral_values_long )

        ## Year when future loss reaches 1 Trillion
            
            ssp_corals_growth_ssp2 <- ssp_corals_growth %>% filter(scenario=="SSP2") %>% 
                
                mutate(Market_Use_Values_Undamaged = muV_value_perkm2year_adjusted * CoralArea_2020_km2,
                nonMarket_Use_Values_Undamaged = nuV_value_perkm2year_adjusted * CoralArea_2020_km2,
                nonUse_Values_Undamaged = nV_value_perkm2year_adjusted * CoralArea_2020_km2, 
                Market_Use_Values_damaged = muV_value_perkm2year_adjusted * area,
                nonMarket_Use_Values_damaged = nuV_value_perkm2year_adjusted *  area,
                nonUse_Values_damaged = nV_value_perkm2year_adjusted * area) %>% 
                
                mutate(Losses_MarketUse = Market_Use_Values_Undamaged - Market_Use_Values_damaged, 
                Losses_nonMarketUse = nonMarket_Use_Values_Undamaged - nonMarket_Use_Values_damaged, 
                Losses_nonUse = nonUse_Values_Undamaged - nonUse_Values_damaged) %>% 
                
                mutate(Total_losses = Losses_MarketUse + Losses_nonMarketUse + Losses_nonUse)%>% 
                group_by(countrycode) %>% 
                mutate(Cum_Total_losses = cumsum(Total_losses)) %>% ungroup()

            glimpse(ssp_corals_growth_ssp2)
            ggplot(ssp_corals_growth_ssp2,aes(x=year,y=Cum_Total_losses,color=countrycode))+
            geom_point()
                    
            
            surpass_year_data <- find_surpass_year(ssp_corals_growth_ssp2 %>% filter(!is.na(Cum_Total_losses)), "countrycode", "year", "Cum_Total_losses", threshold = 1e9) #%#>% as.data.frame() %>% rename(ed57=country)


            world <- ne_countries(scale = "medium", returnclass = "sf")

            # Merge your data with the world map data
            surpass_corals <- left_join(world, surpass_year_data, by = c("iso_a3" = "countrycode"))

            # # Plot
            # dam <- ggplot(data = surpass_corals  %>% filter(continent != "Antarctica")) +
            # geom_sf(aes(fill = year_surpass )) +
            # scale_fill_scico(palette = "lajolla", oob=squish, na.value=NA,direction=1) + # Use the desired scico palette
            # coord_sf(crs = "+proj=robin") + # Robinson projection
            # theme_minimal() +
            # labs(fill = "Year surpassing \n$1 Trillion in Losses")
            # dam
            # #ggsave("Figures/SM/corals/coral_countrymap_damage.png",dpi=600) 

            # quantile_breaks <- quantile(surpass_corals$year_surpass, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

            # labels2 <- c(
            #     paste0("<", round(quantile_breaks[2], 2)),
            #     paste0("(", round(quantile_breaks[2], 2), ",", round(quantile_breaks[3], 2), "]"),
            #     paste0("(", round(quantile_breaks[3], 2), ",", round(quantile_breaks[4], 2), "]"),
            #     paste0(">", round(quantile_breaks[4], 2))
            # )

            surpass_corals$damage_category <- cut(
                surpass_corals$year_surpass,
                breaks = c(2020,2030,2040,2050,2060,2070,2080,2090,2100),
                labels = c(2020,2030,2040,2050,2060,2070,2080,2090),
                include.lowest = TRUE
            )

            # fig_c5 <- ggplot(data = surpass_corals %>% filter(continent != "Antarctica")) +
            # geom_sf(aes(fill = damage_category )) +
            # #scale_fill_manual(values = custom_colors, name = "Year surpassing \n$1 Trillion in Losses",na.value="transparent") +
            # coord_sf(crs = "+proj=robin") + # Robinson projection
            # theme_minimal() +
            # labs(fill = "Year surpassing \n$1 Trillion in Losses")
            # windows()
            
            # print(fig_c5)





    ## Corals (end)
        

    
    ## Ports (start)

        port_ssp <- read.csv("Data/output_modules_input_rice50x\\output_modules/ports/ports_ssps_rcps.csv")
        glimpse(port_ssp)
        glimpse(ssp_corals_growth_ssp2)
        levels(factor(port_ssp$type))

        port_values <- port_ssp %>% filter(year==2022,SSP=="SSP2",RCP=="RCP26") %>% 
            group_by(iso3) %>% 
            summarize( risk_base_perc = sum(risk_base_perc), iso3=iso3) %>% 
            select(iso3,risk_base_perc) %>% slice(1) %>% 
            mutate(category="Market",capital="Ports")%>% 
            dplyr::rename( value = risk_base_perc, countrycode=iso3)  %>% ungroup()
        glimpse(port_values)
        ## Surpass years
            
            ports_coef <- read.csv("Data/output_modules_input_rice50x/input_rice50x/ports_tcoeff.csv")
            glimpse(ports_coef)

            def_mult <- deflator_data %>% 
                summarize(def_2005_to_2020 =NY.GDP.DEFL.ZS[year==2020]/NY.GDP.DEFL.ZS[year==2005] )
            ports_coef <- ports_coef %>% left_join(ssp_corals_growth_ssp2, by = c("iso3" = "countrycode"))
            ports_coef <- ports_coef %>% 
                mutate(Damage_ports = GDP_FractionChange_perC * temp * GDP.billion2005USDperYear*def_mult[[1]]*1e9) %>% 
            mutate(countrycode = iso3) %>% 
            group_by(countrycode) %>%      
                    mutate(Cum_Total_losses = cumsum(-Damage_ports)) %>% 
            filter(!is.na(Cum_Total_losses)) %>% 
            filter(!all(Cum_Total_losses == 0)) %>% 
            ungroup()
            
            
            ggplot(ports_coef)+geom_point(aes(x=year,y=Cum_Total_losses))

            surpass_year_ports <- find_surpass_year(ports_coef %>% filter(!is.na(Cum_Total_losses)), "countrycode", "year", "Cum_Total_losses", threshold = 1e9) %>% as.data.frame() #%>% rename(ed57=country)
            
            
            world <- ne_countries(scale = "medium", returnclass = "sf")

                # Merge your data with the world map data
                surpass_ports <- left_join(world, surpass_year_ports, by = c("iso_a3" = "countrycode"))

                surpass_ports$damage_category <- cut(
                    surpass_ports$year_surpass,
                    breaks = c(2020,2030,2040,2050,2060,2070,2080,2090,2100),
                    labels = c(2020,2030,2040,2050,2060,2070,2080,2090),
                    include.lowest = TRUE
                )
    ## Ports (end)

    ## Fisheries (start)
        
        fisheries_df_temp_gdp <- read.csv("Data/output_modules_input_rice50x\\output_modules/fish/fisheries_Free_EtAl.csv")
        glimpse(fisheries_df_temp_gdp)

        fish_values <- fisheries_df_temp_gdp %>% filter(year==2020,rcp=="RCP26",scenario=="Full Adaptation") %>% 
            dplyr::rename(value=profits_usd_percGDP_baseline,countrycode=country_iso3) %>% 
            dplyr::select(value,countrycode)%>% 
            mutate(category="Market",capital="Fisheries & Mariculture")
        glimpse(fish_values)    

        
        VSL = 10.05 * 10^6
        nutrition_dep <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\seafood_dependence.csv") %>% 
            mutate(countrycode = countrycode(World_Country,origin="country.name",destination="iso3c"))
        nutrition_health <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\mortality_seafood_nutrition.csv") %>% 
            left_join(nutrition_dep,by="countrycode") %>% 
                    left_join(gdp_data %>% dplyr::select(GDP_2020usd,countrycode),by="countrycode")%>% 
                    left_join(population_data %>% dplyr::select(Pop2020,countrycode),by="countrycode") %>% 
                    mutate(value = 100*TAME_nutrients_MortalityEffect*Pop2020*Nutritional_D*0.1*VSL/GDP_2020usd) %>% 
                    select(value,countrycode) %>% 
                    mutate(category="Non-market Use",capital="Fisheries & Mariculture")
        glimpse(nutrition_health)

        ## Surpass years (start)
            fish_coef <- read.csv("Data/output_modules_input_rice50x/input_rice50x/fish_tcoeff.csv")
            glimpse(fish_coef)
            fish_coef %>% filter(country_iso3=="CAN")
            
            fish_coef <- fish_coef %>% left_join(ssp_corals_growth_ssp2, by = c("country_iso3" = "countrycode")) %>% 
                mutate(Damage_fish = GDP_FractionChange_perC * temp * GDP.billion2005USDperYear*def_mult[[1]]*1e9) %>% 
            mutate(countrycode = country_iso3) %>% 
            group_by(countrycode) %>%      
                    mutate(Cum_Total_losses_fishmarket = cumsum(-Damage_fish), 
                    Cum_Total_losses_fishmarket_percGDP = cumsum(-100*Damage_fish/(GDP.billion2005USDperYear*def_mult[[1]]*10^9))) %>% 
            ungroup()
            
            
            nutrition_health2 <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\mortality_seafood_nutrition.csv") %>% 
                left_join(nutrition_dep,by="countrycode")#         
            nutrition_health2 <- nutrition_health2 %>% left_join(ssp_corals_growth_ssp2, by = "countrycode") %>% 
                mutate(damages = - temp * (beta_nutrient_percChange_perDegreeC/100)* TAME_nutrients_MortalityEffect*Pop.million*10^6*Nutritional_D*0.1*VSL) %>% 
                group_by(countrycode) %>%      
                    mutate(Cum_Total_losses_fishhealth = cumsum(damages), 
                    Cum_Total_losses_fishhealth_percGDP = cumsum(100*damages/(GDP.billion2005USDperYear*def_mult[[1]]*10^9))) %>% 
            ungroup() %>% 
            left_join(fish_coef,by=c("year","countrycode")) %>% 
            mutate(Cum_Total_losses = Cum_Total_losses_fishhealth + Cum_Total_losses_fishmarket,
            Cum_Total_losses_percGDP = Cum_Total_losses_fishhealth_percGDP + Cum_Total_losses_fishmarket_percGDP)

            
            nutrition_health2 %>% filter(countrycode =="CAN")   %>% filter(!is.na(Cum_Total_losses)) %>% select(Cum_Total_losses) %>% as.data.frame()
            

            surpass_year_fish <- find_surpass_year(nutrition_health2 %>% filter(!is.na(Cum_Total_losses)), "countrycode", "year", "Cum_Total_losses", threshold = 1e9) %>% as.data.frame() #%>% rename(ed57=country)
            surpass_year_fish %>% filter(countrycode=="CAN")
            surpass_year_fish_percGDP <- find_surpass_year(nutrition_health2 %>% filter(!is.na(Cum_Total_losses_percGDP)), "countrycode", "year", "Cum_Total_losses_percGDP", threshold = 1) %>% as.data.frame() #%>% rename(ed57=country)
            surpass_year_fish_percGDP            
            
            world <- ne_countries(scale = "medium", returnclass = "sf")

                # Merge your data with the world map data
                surpass_fish <- left_join(world, surpass_year_fish, by = c("iso_a3" = "countrycode"))

                surpass_fish$damage_category <- cut(
                    surpass_fish$year_surpass,
                    breaks = c(2020,2030,2040,2050,2060,2070,2080,2090,2100),
                    labels = c(2020,2030,2040,2050,2060,2070,2080,2090),
                    include.lowest = TRUE
                )


        ## Surpass years (end)
    ## Fisheries (end)

    ## Mangroves (start)
        man_ben_perkm2 <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\weighted_avg_benefits.csv") #read.csv("Data/intermediate_output/mangrove_benefits_per_km2.csv") 
        area_man <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\mangrove_area_coefficients_sq.csv")
        glimpse(area_man)
        
        levels(factor(man_ben_perkm2$type))

        man_values0 <- man_ben_perkm2 %>% dplyr::filter(year==2020,forcing=="onlyCC") %>% dplyr::select(-X,-year,-forcing,-gdppc,-GDP_SSP2,-weighted_avg_benefit_perha) %>% 
                        left_join(area_man %>% dplyr::select(countrycode,MangroveArea_2020_km2),by="countrycode") %>% 
                        #dplyr::rename(category=type) %>% 
                        mutate(value =weighted_avg_benefit_perha_percGDP * MangroveArea_2020_km2 * 100, 
                        category = case_when(type=="cultural"~"Non-use", type=="provision"~"Market",type=="regulation"~"Non-market Use"),
                        capital = "Mangroves") %>% dplyr::select(-MangroveArea_2020_km2,-weighted_avg_benefit_perha_percGDP,-type)
        glimpse(man_values0)

        

        #Surpass year
            glimpse(man_ben_perkm2)

            man_ben_all <- man_ben_perkm2 %>% dplyr::filter(forcing=="onlyCC") %>% 
                        group_by(countrycode,year) %>% 
                        summarise(benefit_perha_percGDP = sum(weighted_avg_benefit_perha_percGDP,na.rm=TRUE))
            glimpse(man_ben_all)
            
            interpolated_man_values <- man_ben_all %>%
                group_by(countrycode) %>%
                do({
                    data <- .
                    years <- seq(min(data$year), max(data$year))
                    interpolated_benefits <- approx(data$year, data$benefit_perha_percGDP, xout = years)$y
                    data.frame(year = years, benefit_perha_percGDP = interpolated_benefits)
                }) %>%
                ungroup()

            glimpse(interpolated_man_values)

            man_values <- interpolated_man_values %>% 
                        left_join(area_man %>% dplyr::select(countrycode,MangroveArea_2020_km2,FractionChange_perC),by="countrycode") %>% 
                        left_join(ssp_corals_growth_ssp2, by = c("countrycode","year")) %>% 
                        mutate(Damage_man = -MangroveArea_2020_km2*FractionChange_perC*temp*benefit_perha_percGDP*GDP.billion2005USDperYear*def_mult[[1]]*10^9) %>% 
                group_by(countrycode) %>%      
                    mutate(Cum_Total_losses_man = cumsum(Damage_man), 
                    Cum_Total_losses_man_percGDP = cumsum(Damage_man/(GDP.billion2005USDperYear*def_mult[[1]]*10^9))) %>% 
            filter(!is.na(Cum_Total_losses_man )) %>% 
            filter(!all(Cum_Total_losses_man  == 0)) %>% 
            ungroup()

            glimpse(man_values)


            

            surpass_year_man <- find_surpass_year(man_values %>% filter(!is.na(Cum_Total_losses_man)), "countrycode", "year", "Cum_Total_losses_man", threshold = 1e9) %>% as.data.frame() #%>% rename(ed57=country)
            
            
            world <- ne_countries(scale = "medium", returnclass = "sf")

                # Merge your data with the world map data
                surpass_man <- left_join(world, surpass_year_man, by = c("iso_a3" = "countrycode"))

                surpass_man$damage_category <- cut(
                    surpass_man$year_surpass,
                    breaks = c(2020,2030,2040,2050,2060,2070,2080,2090,2100),
                    labels = c(2020,2030,2040,2050,2060,2070,2080,2090),
                    include.lowest = TRUE
                )  
    ## Mangroves (end)

    ## Merge all surpasses (start)
        surpass_man
        surpass_corals
        surpass_ports
        surpass_fish


        surpass_all <- rbind(surpass_corals %>% mutate(capital = "Corals"), surpass_fish %>% mutate(capital="Fisheries & Mariculture"), 
        surpass_ports %>% mutate(capital = "Ports"), surpass_man %>% mutate(capital="Mangroves"))
        glimpse(surpass_all)    


        surpass_fish$damage_category <- cut(
                            surpass_fish$year_surpass,
                            breaks = c(2020,2030,2040,2050,2060,2070,2080,2090,2100),
                            labels = c(2020,2030,2040,2050,2060,2070,2080,2090),
                            include.lowest = TRUE
                        )

        custom_colors <- c(
        "2020" = "#B2182B",   # Dark Red
        "2030" = "#D6604D",   # Coral
        "2040" = "#F4A582",   # Light Coral
        "2050" = "#FDDBC7",   # Pale Pink
        "2060" = "#D1E5F0",   # Light Sky Blue
        "2070" = "#92C5DE",   # Sky Blue
        "2080" = "#4393C3",   # Steel Blue
        "2090" = "#2166AC"    # Dark Blue
        )

         fig_M2 <- ggplot(data = surpass_all %>% filter(continent != "Antarctica")) +
            geom_sf(aes(fill = damage_category )) +
            facet_wrap(~capital)+
            #scale_fill_manual(values = custom_colors, name = "Year surpassing \n$1 Trillion in Losses",na.value="transparent") +
            scale_fill_manual(values = custom_colors, name = "Year surpassing \n$1 Billion in Losses", na.value = "transparent") +
            coord_sf(crs = "+proj=robin") + # Robinson projection
            theme_void() +
            labs(fill = "Year surpassing \n$1 Billion in Losses")
            #windows()
            
            print(fig_M2)
            ggsave("Figures/Main2.png")

            surpass_all <- surpass_all %>%
         mutate(year_surpass_transformed = log(as.integer(year_surpass) - 2019))

            ggplot(data = surpass_all %>% filter(continent != "Antarctica")) +
            geom_sf(aes(fill = as.integer(year_surpass))) +
            facet_wrap(~capital)+
            scale_fill_scico(palette="batlow",
                direction=-1, end=0.8,
                "Year surpassing \n$1 Billion in Losses",
                na.value="transparent")+
            coord_sf(crs = "+proj=robin") + # Robinson projection
            theme_void() +
            labs(fill = "Year surpassing \n$1 Billion in Losses")+
            guides(fill = guide_colorbar(
                title.position = "top",
                title.hjust = 0.5,
                label.position = "bottom",
                barwidth = 20,
                barheight = 1,direction="horizontal",position="bottom",ticks.colour = "black", frame.colour = "black"
                ))
            #windows()v2

            ggsave("Figures/Main2.png")
            
            print(fig_M2v2)


            ggplot(data = surpass_all %>% filter(continent != "Antarctica")) +
            geom_sf(aes(fill = year_surpass_transformed)) +
            facet_wrap(~capital) +
            scale_fill_scico(
                palette = "batlow",
                direction = -1,
                end = 0.8,
                name = "Year surpassing $1 Billion\n in Cumulative Losses",
                na.value = "transparent",
                breaks = log(c(2020, 2030, 2040, 2050, 2070, 2099) - 2019),
                labels = c(2020,2030,2040, 2050,2070,2100)
            ) +
            coord_sf(crs = "+proj=robin") + # Robinson projection
            theme_void() +
            guides(
                fill = guide_colorbar(
                title.position = "top",
                title.hjust = 0.5,
                label.position = "bottom",
                barwidth = 20,
                barheight = 1,
                direction = "horizontal",
                position = "bottom",
                ticks.colour = "black",
                frame.colour = "black"
                )
            )
            ggsave("Figures/Main2vlog.png")
    ## Merge all surpasses (end)

    ## Merge all capitals (start)
        blue_cap0 <- as.data.frame(rbind(coral_values_long ,port_values,fish_values,nutrition_health, man_values0))    ## Merge all capitals (end)
        # Add GDP and GDP per capita in 2020

    ## Add co-variates to dataframe (start)
        
        
        blue_cap <- blue_cap0 %>% left_join(regions,by="countrycode") %>% 
                    left_join(gdp_data %>% dplyr::select(GDP_2020usd,countrycode),by="countrycode")%>% 
                    left_join(population_data %>% dplyr::select(Pop2020,countrycode),by="countrycode") %>%
                    mutate(R5 = factor(R5, levels = rev(sort(unique(R5)))))

        glimpse(blue_cap)
    ## Add co-variates to dataframe (end)
#Data Arrange (send)

## Figure 1 bottom (start)
    ## Scatter plot ----
        blue_cap <- blue_cap %>%
                    mutate(value_capped = ifelse(value > 100, 100, ifelse(value < 1, 1, value)))
        color_capitals

        capital_plot <- ggplot(blue_cap %>% filter(value_capped >0 )) +
        geom_point(aes(
                x = (GDP_2020usd/Pop2020)/1000, 
                y = R5, 
                shape = category, 
                size = value_capped, 
                color=capital), 
            position = position_jitter(width = 0, height = 0.3),
            alpha=0.5) +
        scale_color_manual(values=Color_capitals)+
        scale_size_continuous(range = c(1, 8)) +
        #scale_color_manual(values=color_ValueTypes)+
        scale_x_continuous(trans="log10")+ ylab("")+xlab("")+
        #theme(legend.position = "none")+
        theme_bw()+
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        ) +
        labs(size="Value of Benefit\n(shown as %GDP)",shape="Value Category",color="Blue Capital")
  
        capital_plot
        #ggsave("Figures\\Main\\Values_Across_Regions.png",dpi=300)

        #svglite::svglite("capital_diagrams.svg", width = 10, height = 7)
        #print(capital_plot)
        #dev.off()
            
            #ggsave("Figures/Main/Fig1_a_corals.png",dpi=600)

    ## Sankey diagram ----
        blue_cap_summary <- blue_cap %>%
            filter(!is.na(R5) & !is.na(capital) & !is.na(value)) %>%
            group_by(R5, capital) %>%
            summarise(count = n()) %>%
            ungroup()%>%
        mutate(R5 = factor(R5, levels = rev(sort(unique(R5)))))

        glimpse(blue_cap_summary)

        blue_cap_summary 
        sankey <- ggplot(data = blue_cap_summary,
        aes(axis1 = capital, axis2 = R5, y = count)) +
            geom_alluvium(aes(fill = capital), width = 0.1, knot.pos = 0.4) +
            geom_stratum(width = 0.1, fill = "transparent", color = "transparent") +
            geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
            #scale_x_discrete(limits = c("R5", "Capital"), expand = c(0.15, 0.05)) +
            theme_void() +
            scale_fill_manual(values=Color_capitals)+
        theme(legend.position = "none")

        sankey

        mixed_plot <- ggarrange(sankey,capital_plot)


        svglite::svglite("combined_diagrams_v3.svg", width = 7.5, height = 4)
        print(mixed_plot )
        dev.off()

        # Calculate total count for each region
        total_counts <- blue_cap_summary %>%
        group_by(R5) %>%
        summarise(total_count = sum(count)) %>%
        arrange(total_count)

        # Determine the spacing based on total counts
        total_counts <- total_counts %>%
        mutate(region_spacing = 500 - (cumsum(total_count) / sum(total_count) * 100))

        # Join the total counts and spacing back to the original data
        blue_cap_summary2 <- blue_cap_summary %>%
        left_join(total_counts, by = "R5") %>%
        mutate(R5 = factor(R5, levels = total_counts$R5)) 

        spacing_new <- blue_cap_summary2 %>% group_by(R5) %>% slice(1)  %>% mutate(capital="transparent") %>% 
        mutate(count=252*(region_spacing/100), color=capital)%>% mutate(r5 = as.character(R5))
        spacing_new$capital <- c("t5","t2","t3","t4","t1") 

        spacing_new$r5[spacing_new$R5=="ASIA"] <- "ra" 
        spacing_new$r5[spacing_new$R5=="MAF"] <- "rm" 
        spacing_new$r5[spacing_new$R5=="LAM"] <- "rl" 
        spacing_new$r5[spacing_new$R5=="OECD"] <- "ro" 
        spacing_new$r5[spacing_new$R5=="REF"] <- "rr" 


        #spacing_new$capital <- c("corals","fisheries","mangroves","ports","ports")
        blue_cap_summary2$color = blue_cap_summary2$capital

        blue_cap_summary2$r5 = as.character(blue_cap_summary2$R5)

        blue_cap_summary3 <- rbind(blue_cap_summary2,spacing_new) %>%
        mutate(capital = factor(capital, levels = c( "t1","Corals", "t2", "Fisheries & Mariculture", "t3", "Mangroves", "t4", "Ports", "t5"))) %>% 
        mutate(r5 = factor(r5, levels = c("ASIA","ra","LAM","rl","MAF","rm","OECD","ro","REF","rr")))


        sankey <- ggplot(data = blue_cap_summary3,
            aes(axis1 = capital, axis2 = r5, y = count)) +
                geom_alluvium(aes(fill = capital), width = 0.1, knot.pos = 0.4, alpha=0.3) +
                geom_stratum(width = 0.1, fill = "transparent", color = "transparent") +
                geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
                #scale_x_discrete(limits = c("R5", "Capital"), expand = c(0.15, 0.05)) +
                theme_void() +
                scale_fill_manual(values=c(Color_capitals,t1="transparent",t2="transparent",t3="transparent",t4="transparent",t5="transparent"))+
                #scale_fill_manual(values=c(color_capitals))+
            theme(legend.position = "none")

            sankey



            mixed_plot <- ggarrange(sankey,capital_plot,legend=FALSE,ncol=2,widths=c(1,2))
            ggarrange(sankey,capital_plot,legend=FALSE,ncol=2,widths=c(1,2))

            #svglite::svglite("combined_diagrams_separatedv4_mastransparente.svg", width = 7.5, height = 6)

            print(mixed_plot )
            #dev.off()


## Figure 1 bottom (end)

# Year of the Trillion Damages ----