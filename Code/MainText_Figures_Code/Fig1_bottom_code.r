#Data Arrange (start)

    ## Socioeconomics (start)
        # gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 2020, end = 2020) 
        # population_data <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2020, end = 2020)
        # gdp_data <- gdp_data %>% dplyr::rename(countrycode=iso3c,GDP_2020usd=NY.GDP.MKTP.PP.KD)
        # saveRDS(gdp_data, "Data/other/gdp_data_2020.rds")
        # population_data  <- population_data  %>% dplyr::rename(countrycode=iso3c,Pop2020=SP.POP.TOTL)
        # saveRDS(population_data, "Data/other/population_data_2020.rds")

        population_data <- readRDS("Data/other/population_data_2020.rds")
        gdp_data <- readRDS("Data/other/gdp_data_2020.rds")
        library(dplyr)

        
    ## Socioeconomics (end)

    ## Temperature from RICE50x
        # Load and preprocess data
        tatm <- read_excel(paste0('Data/output_rice50x/results_ocean_damage.xlsx'), sheet = "TATM") %>%
        dplyr::select(year = 1, tatm = 3) %>%
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
                dplyr::select(countrycode,Market_Use_Values_Undamaged_percGDP,nuV_Undamaged_percGDP,nV_Undamaged_percGDP) %>% 
                dplyr::rename( market_percGDP = Market_Use_Values_Undamaged_percGDP, nonmarketuse_percGDP =nuV_Undamaged_percGDP, nonuse_percGDP=nV_Undamaged_percGDP)
            glimpse(coral_values)

            coral_values_long <- coral_values %>%
                pivot_longer(cols = starts_with("market_percGDP") | starts_with("nonmarketuse_percGDP") | starts_with("nonuse_percGDP"), 
                            names_to = "category", 
                            values_to = "value") %>%
                mutate(category = sub("_percGDP$", "", category),capital="Corals") %>% 
                mutate(category = case_when(category=="nonuse"~"Non-use", category=="market"~"Market",category=="nonmarketuse"~"Non-market Use"))

            glimpse(coral_values_long )

       

    ## Corals (end)
        

    
    ## Ports (start)

        port_ssp <- read.csv("Data/output_modules_input_rice50x\\output_modules/ports/ports_ssps_rcps.csv")
        
        port_values <- port_ssp %>% filter(year==2022,SSP=="SSP2",RCP=="RCP26") %>% 
            group_by(iso3) %>% 
            summarize( risk_base_perc = sum(risk_base_perc), iso3=iso3) %>% 
            dplyr::select(iso3,risk_base_perc) %>% slice(1) %>% 
            mutate(category="Market",capital="Ports")%>% 
            dplyr::rename( value = risk_base_perc, countrycode=iso3)  %>% ungroup()
        
    ## Ports (end)

    ## Fisheries (start)
        
        fisheries_df_temp_gdp <- read.csv("Data/output_modules_input_rice50x\\output_modules/fish/fisheries_Free_EtAl.csv")
        
        fish_values <- fisheries_df_temp_gdp %>% filter(year==2020,rcp=="RCP26",scenario=="Full Adaptation") %>% 
            dplyr::rename(value=profits_usd_percGDP_baseline,countrycode=country_iso3) %>% 
            dplyr::select(value,countrycode)%>% 
            mutate(category="Market",capital="Fisheries & Mariculture")
        glimpse(fish_values)    

        
        VSL = 10.05 * 10^6
        nutrition_dep <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\seafood_dependence.csv") %>% 
            mutate(countrycode = countrycode(World_Country,origin="country.name",destination="iso3c"))
        nutrition_health <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\mortality_seafood_nutrition.csv") %>% 
            #left_join(nutrition_dep,by="countrycode") %>% 
                    left_join(gdp_data %>% dplyr::select(GDP_2020usd,countrycode),by="countrycode")%>% 
                    left_join(population_data %>% dplyr::select(Pop2020,countrycode),by="countrycode") %>% 
                    mutate(value = 100*TAME_nutrients_MortalityEffect*Pop2020*Nutritional_D*0.05*VSL/GDP_2020usd) %>% 
                    dplyr::select(value,countrycode) %>% 
                    mutate(category="Non-market Use",capital="Fisheries & Mariculture")
        
        
    ## Fisheries (end)

    ## Mangroves (start)
        man_ben_perkm2 <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\weighted_avg_benefits.csv") #read.csv("Data/intermediate_output/mangrove_benefits_per_km2.csv") 
        area_man <- read.csv("Data\\output_modules_input_rice50x\\input_rice50x\\mangrove_area_coefficients_sq.csv")
        
        man_values0 <- man_ben_perkm2 %>% dplyr::filter(year==2020,forcing=="onlyCC") %>% dplyr::select(-X,-year,-forcing,-gdppc,-GDP_SSP2,-weighted_avg_benefit_perha) %>% 
                        left_join(area_man %>% dplyr::select(countrycode,MangroveArea_2020_km2),by="countrycode") %>% 
                        #dplyr::rename(category=type) %>% 
                        mutate(value =weighted_avg_benefit_perha_percGDP * MangroveArea_2020_km2 * 100, 
                        category = case_when(type=="cultural"~"Non-use", type=="provision"~"Market",type=="regulation"~"Non-market Use"),
                        capital = "Mangroves") %>% dplyr::select(-MangroveArea_2020_km2,-weighted_avg_benefit_perha_percGDP,-type)
        
             
    ## Mangroves (end)


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
        scale_color_manual(values=Color_capitals_black)+
        scale_size_continuous(range = c(1, 8)) +
        #scale_color_manual(values=color_ValueTypes)+
        scale_x_continuous(trans="log10")+ ylab("")+xlab("")+
        #theme(legend.position = "none")+
        theme_bw()+
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        ) +
        labs(size="Value of Benefit\n(shown as %GDP)",shape="Value Category",color="Blue Capital",x="GDP per capita (Thousand 2020 USD)")
  
        capital_plot
        ggsave("Figures\\Main\\Panels\\Figure1_B.png",dpi=300)

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


        # svglite::svglite("combined_diagrams_v3.svg", width = 7.5, height = 4)
        # print(mixed_plot )
        # dev.off()

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
                geom_alluvium(aes(fill = capital), width = 0.1, knot.pos = 0.4, alpha=01) +
                geom_stratum(width = 0.1, fill = "transparent", color = "transparent") +
                geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
                #scale_x_discrete(limits = c("R5", "Capital"), expand = c(0.15, 0.05)) +
                theme_void() +
                scale_fill_manual(values=c(Color_capitals_black,t1="transparent",t2="transparent",t3="transparent",t4="transparent",t5="transparent"))+
                #scale_fill_manual(values=c(color_capitals))+
            theme(legend.position = "none")

            sankey
        ggsave("Figures\\Main\\Panels\\Figure1_C.png",dpi=300)




            # mixed_plot <- ggarrange(sankey,capital_plot+guides(legend.position="right"),ncol=2,widths=c(1,2))
            # ggarrange(sankey,capital_plot+guides(legend.position="right"),ncol=2,widths=c(1,2))

            #svglite::svglite("Figures/sankey_capital.svg", width = 7.5, height = 6)

            #print(mixed_plot )
            #dev.off()


## Figure 1 bottom (end)
