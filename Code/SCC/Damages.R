# Convert GDX Files to XLSX ----



    #system("gdx2xls Data/output_rice50x/results_ocean_today.gdx")
    #system("gdx2xls Data/output_rice50x/results_ocean_damage_pulse.gdx")
    #system("gdx2xls Data/output_rice50x/results_ocean_damage.gdx")


## Read BLUERICE50x Results
    # Key variables
    var_names <- c("C","ocean_consump_damage_coef","ocean_consump_damage_coef_sq","YNET","OCEAN_USENM_VALUE_PERKM2","VSL",
        "OCEAN_NONUSE_VALUE_PERKM2","OCEAN_AREA","ocean_area_start","ocean_value_intercept_unm","ocean_value_exp_unm","ocean_value_intercept_nu","ocean_value_exp_nu","pop", 
        "ocean_health_beta", "health_mu", "health_eta")
    exp_names <- "damage" # c("today","damage")

    process_data(exp_names, var_names)
    glimpse(exp_data_damage)
    
    ## Temperature
    tatm <- read_excel(paste0('Data/output_rice50x/results_ocean_damage.xlsx'), sheet = "TATM") %>%
    select(year = 1, tatm = 3) %>%  # Select the first and third columns and rename them
    mutate(year = 1980 + (as.integer(year) - 1) * 5, 
    tatm = as.double(tatm))  %>% # Modify the 'year' column
    filter(!is.na(year)) %>% as.data.frame()

    tatm_2025 <- tatm %>%
    filter(year == 2025) %>%
    pull(tatm)

    tatm <- tatm %>%
    mutate(tatm_diff_2025 = tatm - tatm_2025)


    ## Merge Temp with Key variables
    exp_data_damage <- exp_data_damage %>% 
    left_join(tatm,by="year")
## Read BLUERICE50x Results


## Ports year surpass (start)
    ports_dam <- exp_data_damage %>% 
        filter(capital =="ports",year>2024) %>% 
        arrange(country, year) %>%  # Ensure the data is sorted by country and year
        group_by(country) %>%  # Group by country
        mutate(YNET_dam_mkt_dif =- YNET*tatm_diff_2025*ocean_consump_damage_coef,
        YNET_dam_mkt_dif_perc = -100*tatm_diff_2025*ocean_consump_damage_coef, 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        YNET_dam_mkt_dif_perc2025 = (YNET_dam_mkt_dif / net_GDP_2025) * 100) %>% 
        mutate(cumulative_YNET_dam_mkt_dif = cumsum(YNET_dam_mkt_dif), 
        cumulative_YNET_dam_mkt_dif_perc = cumsum(YNET_dam_mkt_dif_perc), 
        cumulative_YNET_dam_mkt_dif_perc2025 = cumsum(YNET_dam_mkt_dif_perc2025)) %>%  # Compute the cumulative sum
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_YNET_dam_mkt_dif == 0))

    glimpse(ports_dam)
    ggplot(ports_dam, aes(x = year, y= YNET_dam_mkt_dif_perc)) + geom_point()
    ggplot(ports_dam, aes(x = year, y= cumulative_YNET_dam_mkt_dif_perc2025)) + geom_point()

    surpass_year_data <- find_surpass_year(ports_dam, "country", "year", "cumulative_YNET_dam_mkt_dif_perc2025", threshold = 1) %>% 
        as.data.frame() %>% 
        rename(ed57=country)

    ports_year_1perc <- ed57 %>% left_join(surpass_year_data)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    ports_year_1perc_map <- left_join(world, ports_year_billion, by = c("iso_a3" = "country"))%>% filter(continent != "Antarctica")

    ggplot(ports_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()
## Ports Year Surpass

## Corals Year Surpass
    glimpse(exp_data_damage)

    
    cor_dam <- exp_data_damage %>% 
        filter(capital =="coral",year>2024) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(usenm_nodam = OCEAN_USENM_VALUE_PERKM2*ocean_area_start,
        usenm_dam = OCEAN_USENM_VALUE_PERKM2*OCEAN_AREA,
        nonuse_nodam = OCEAN_NONUSE_VALUE_PERKM2*ocean_area_start,
        nonuse_dam = OCEAN_NONUSE_VALUE_PERKM2*OCEAN_AREA,
        nonuse_dif = nonuse_dam - nonuse_nodam,
        usenm_dif = usenm_dam - usenm_nodam,
        YNET_dam_mkt_dif = YNET*tatm_diff_2025*ocean_consump_damage_coef, 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        dam_dif_perc2025 = (-(YNET_dam_mkt_dif+nonuse_dif+usenm_dif) / net_GDP_2025) * 100) %>%

 
        mutate(cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)) %>%
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_dam_perc2025  == 0), 
            !is.na(cumulative_dam_perc2025))

    glimpse(cor_dam)

    ggplot(cor_dam %>% filter(year<2025)) + geom_point(aes(x = year, y =cumulative_dam_perc2025 ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =dam_dif_perc2025))
    ggplot(cor_dam) + geom_point(aes(x = year, y =YNET_dam_mkt_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =usenm_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =nonuse_dif ))
    glimpse(exp_data_damage)

    #cor_dam %>% filter(country=="aus")

    surpass_year_data <- find_surpass_year(cor_dam, "country", "year", "cumulative_dam_perc2025", threshold = 1) %>% 
        as.data.frame() %>% 
        rename(ed57=country)
    
    surpass_year_data %>% filter(ed57=="aus")
    
    cor_year_1perc <- ed57 %>% left_join(surpass_year_data)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    cor_year_1perc_map <- left_join(world, cor_year_1perc, by = c("iso_a3" = "country"))%>% filter(continent != "Antarctica")

    ggplot(cor_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()

## Corals Year Surpass

## Mangroves Year Surpass
    glimpse(exp_data_damage)

    exp_data_damage %>% filter(country=="mex") %>% select(YNET,year)
    man_dam <- exp_data_damage %>% 
        filter(capital =="mangrove",year>2024,!is.na(ocean_consump_damage_coef_sq)) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(

        usenm_nodam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*ocean_area_start,
        nu_nodam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*ocean_area_start,
        usenm_dam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*OCEAN_AREA,
        nu_dam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*OCEAN_AREA,


        nonuse_dif =nu_dam - nu_nodam,
        usenm_dif = usenm_dam - usenm_nodam,

        YNET_dam_mkt_dif = YNET*(tatm*ocean_consump_damage_coef + ocean_consump_damage_coef_sq*tatm), 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        all_dam =-(YNET_dam_mkt_dif+nonuse_dif+usenm_dif)) %>%

        mutate(all_dam_post2025 = all_dam - first(all_dam[year==2025]),
        dam_dif_perc2025 = (all_dam_post2025 / net_GDP_2025) * 100, 
        dam_dif_perc2025_all = (all_dam / net_GDP_2025) * 100) %>%

 
        mutate(cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)) %>%
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_dam_perc2025  == 0), 
            !is.na(cumulative_dam_perc2025))

    glimpse(man_dam)

    ggplot(man_dam %>% filter(year<2100)) + geom_point(aes(x = year, y =cumulative_dam_perc2025 ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =dam_dif_perc2025))
    ggplot(cor_dam) + geom_point(aes(x = year, y =YNET_dam_mkt_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =usenm_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =nonuse_dif ))
    glimpse(exp_data_damage)


    surpass_year_data <- find_surpass_year(man_dam, "country", "year", "cumulative_dam_perc2025", threshold = 1) %>% 
        as.data.frame() %>% 
        rename(ed57=country)
    
    man_year_1perc <- ed57 %>% left_join(surpass_year_data)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    man_year_1perc_map <- left_join(world, man_year_1perc, by = c("iso_a3" = "country"))%>% filter(continent != "Antarctica")

    ggplot(man_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()

## Mangroves Year Surpass


## Fish Year Surpass
    glimpse(exp_data_damage)

    fish_dam <- exp_data_damage %>% 
        filter(capital =="fisheries",year>2024) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(

        usenm_nodam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*ocean_area_start,
        nu_nodam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*ocean_area_start,
        usenm_dam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*OCEAN_AREA,
        nu_dam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*OCEAN_AREA,


        nonuse_dif =nu_dam - nu_nodam,
        usenm_dif = usenm_dam - usenm_nodam,

        YNET_dam_mkt_dif = YNET*(tatm*ocean_consump_damage_coef + ocean_consump_damage_coef_sq*tatm), 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        all_dam =-(YNET_dam_mkt_dif+nonuse_dif+usenm_dif)) %>%

        mutate(all_dam_post2025 = all_dam - first(all_dam[year==2025]),
        dam_dif_perc2025 = (all_dam_post2025 / net_GDP_2025) * 100, 
        dam_dif_perc2025_all = (all_dam / net_GDP_2025) * 100) %>%

 
        mutate(cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)) %>%
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_dam_perc2025  == 0), 
            !is.na(cumulative_dam_perc2025))

    glimpse(fish_dam)

    ggplot(man_dam %>% filter(year<2100)) + geom_point(aes(x = year, y =cumulative_dam_perc2025 ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =dam_dif_perc2025))
    ggplot(cor_dam) + geom_point(aes(x = year, y =YNET_dam_mkt_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =usenm_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =nonuse_dif ))
    glimpse(exp_data_damage)


    surpass_year_data <- find_surpass_year(man_dam, "country", "year", "cumulative_dam_perc2025", threshold = 1) %>% 
        as.data.frame() %>% 
        rename(ed57=country)
    
    man_year_1perc <- ed57 %>% left_join(surpass_year_data)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    man_year_1perc_map <- left_join(world, man_year_1perc, by = c("iso_a3" = "country"))%>% filter(continent != "Antarctica")

    ggplot(man_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()

## Mangroves Year Surpass
    



    dam <- ggplot(data = ports_year_1perc_map) +
    geom_sf(aes(fill = year_surpass )) +
    scale_fill_scico(palette = "lajolla", oob=squish, na.value=NA,direction=1) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Year when it surpasses 1Billion USD Loss")
    dam
    #ggsave("Figures/SM/corals/coral_countrymap_damage.png",dpi=600) 

    quantile_breaks <- quantile(merged_data$year_surpass, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

    labels2 <- c(
        paste0("<", round(quantile_breaks[2], 2)),
        paste0("(", round(quantile_breaks[2], 2), ",", round(quantile_breaks[3], 2), "]"),
        paste0("(", round(quantile_breaks[3], 2), ",", round(quantile_breaks[4], 2), "]"),
        paste0(">", round(quantile_breaks[4], 2))
    )

    merged_data$damage_category <- cut(
        merged_data$year_surpass,
        breaks = quantile_breaks,
        labels = labels2,
        include.lowest = TRUE
    )

    fig_c5 <- ggplot(data = merged_data %>% filter(continent != "Antarctica")) +
    geom_sf(aes(fill = damage_category )) +
    scale_fill_manual(values = custom_colors, name = "Year when it surpasses 1Billion USD Loss",na.value="transparent") +
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Year when it surpasses 1Billion USD Loss")
    windows()
    
    print(fig_c5)
    

# View the resulting dataframe
print(surpass_year_data)

as.data.frame(surpass_year_data)


ed57

ggplot(exp_data_damage)+geom_line(aes(x=year,y=C_dif,color=country))








#Get dollars without ports damage (coming from C)
#Get Temp and Get Ports Coef, calculate Damages (C - C*coef/*Temp)


