# Convert GDX Files to XLSX ----



    system("gdx2xls Data/output_rice50x/results_ocean_today.gdx")
    system("gdx2xls Data/output_rice50x/results_ocean_damage_pulse.gdx")
    system("gdx2xls Data/output_rice50x/results_ocean_damage.gdx")


## Read BLUERICE50x Results
    # Key variables
    var_names <- c("C","ocean_consump_damage_coef","ocean_consump_damage_coef_sq","YNET","OCEAN_USENM_VALUE_PERKM2","VSL","CPC",
        "OCEAN_NONUSE_VALUE_PERKM2","OCEAN_AREA","OCEAN_AREA[year==2025]","ocean_value_intercept_unm","ocean_value_exp_unm","ocean_value_intercept_nu","ocean_value_exp_nu","pop", 
        "ocean_health_beta", "ocean_health_mu","ocean_health_tame")
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
        mutate(YNET_dam_mkt_dif = YNET*tatm_diff_2025*ocean_consump_damage_coef,
        YNET_dam_mkt_dif_perc = 100*tatm_diff_2025*ocean_consump_damage_coef, 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        YNET_dam_mkt_dif_perc2025 = (YNET_dam_mkt_dif / net_GDP_2025) * 100) %>% 
        mutate(cumulative_YNET_dam_mkt_dif = cumsum(-YNET_dam_mkt_dif), 
        cumulative_YNET_dam_mkt_dif_perc = cumsum(-YNET_dam_mkt_dif_perc), 
        cumulative_YNET_dam_mkt_dif_perc2025 = cumsum(YNET_dam_mkt_dif_perc2025)) %>%  # Compute the cumulative sum
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_YNET_dam_mkt_dif == 0))

    glimpse(ports_dam)
    ggplot(ports_dam, aes(x = year, y= YNET_dam_mkt_dif_perc)) + geom_point()
    ggplot(ports_dam, aes(x = year, y= cumulative_YNET_dam_mkt_dif_perc2025)) + geom_point()

    surpass_year_data <- find_surpass_year(ports_dam, "country", "year", "cumulative_YNET_dam_mkt_dif_perc2025", threshold = -1) %>% 
        as.data.frame() #%>% 
        #rename(ed57=country)

    #ports_year_1perc <- ed57 %>% left_join(surpass_year_data)
    ports_year_1perc <- surpass_year_data %>% mutate(iso_a3=toupper(country))
    world <- ne_countries(scale = "medium", returnclass = "sf")
    glimpse(ports_year_1perc)
    glimpse(world)

    ports_year_1perc_map <- left_join(world, ports_year_1perc)%>% filter(continent != "Antarctica")

    glimpse(ports_year_1perc_map )
    ggplot(ports_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()
## Ports Year Surpass

## Corals Year Surpass
    glimpse(exp_data_damage)
   
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
        YNET_dam_mkt_dif = YNET*tatm_diff_2025*ocean_consump_damage_coef, 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        dam_dif_perc2025 = (-(YNET_dam_mkt_dif+nonuse_dif+usenm_dif) / net_GDP_2025) * 100) %>%

 
        mutate(cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)) %>%
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_dam_perc2025  == 0), 
            !is.na(cumulative_dam_perc2025))

    glimpse(cor_dam)

    ggplot(cor_dam %>% filter(year<2025)) + geom_point(aes(x = year, y =cumulative_dam_perc2025 ))
    ggplot(cor_dam %>% filter(year>2024)) + geom_point(aes(x = year, y =dam_dif_perc2025))
    ggplot(cor_dam) + geom_point(aes(x = year, y =YNET_dam_mkt_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =usenm_dif ))
    ggplot(cor_dam) + geom_point(aes(x = year, y =nonuse_dif ))
    glimpse(exp_data_damage)

    #cor_dam %>% filter(country=="aus")

    surpass_year_data <- find_surpass_year(cor_dam, "country", "year", "cumulative_dam_perc2025", threshold = 1) %>% 
        as.data.frame() #%>% 
        #rename(ed57=country)
    
    
    #cor_year_1perc <- ed57 %>% left_join(surpass_year_data)
    cor_year_1perc <- surpass_year_data  %>% mutate(iso_a3=toupper(country))
    world <- ne_countries(scale = "medium", returnclass = "sf")
    cor_year_1perc_map <- left_join(world, cor_year_1perc)%>% filter(continent != "Antarctica")

    ggplot(cor_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()

## Corals Year Surpass

## Mangroves Year Surpass
    glimpse(exp_data_damage)

    exp_data_damage %>% filter(country=="ind") %>% select(YNET,year)

    man_dam <- exp_data_damage %>% 
        filter(capital =="mangrove",year>2024,!is.na(ocean_consump_damage_coef_sq)) %>% 
        arrange(country, year) %>%  
        group_by(country) %>% 

        mutate(

        usenm_nodam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*OCEAN_AREA[year==2025],
        nu_nodam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*OCEAN_AREA[year==2025],

        usenm_dam = (exp(ocean_value_intercept_unm)* (YNET/pop*1e6)^ocean_value_exp_unm)*OCEAN_AREA,
        nu_dam = (exp(ocean_value_intercept_nu)* (YNET/pop*1e6)^ocean_value_exp_nu)*OCEAN_AREA,


        nonuse_dif =nu_dam - nu_nodam,
        usenm_dif = usenm_dam - usenm_nodam,

        YNET_dam_mkt_dif = YNET*(tatm_diff_2025*ocean_consump_damage_coef + ocean_consump_damage_coef_sq*tatm_diff_2025^2), 
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
    glimpse(exp_data_damage)


    surpass_year_data <- find_surpass_year(man_dam, "country", "year", "cumulative_dam_perc2025", threshold = 1) %>% 
        as.data.frame() 

    surpass_year_data %>% filter(year_surpass==-9999)       
    man_year_1perc <- surpass_year_data  %>% mutate(iso_a3=toupper(country))
    world <- ne_countries(scale = "medium", returnclass = "sf")
    man_year_1perc_map <- left_join(world, man_year_1perc)%>% filter(continent != "Antarctica")

    ggplot(man_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()

## Mangroves Year Surpass


## Fish Year Surpass
    glimpse(exp_data_damage)
    exp_data_damage %>% 
        filter(year %in% c(2020,2025,2030),country=="brb", capital=="fisheries") %>% 
        select(ocean_health_beta,ocean_health_tame,pop,year,ocean_health_mu)
    
    glimpse(fish_dam)
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

        YNET_dam_mkt_dif = YNET*(tatm_diff_2025*ocean_consump_damage_coef), 
        net_GDP_2025 = first(YNET[year == 2025]),  # Get the net GDP in 2025 for each country
        all_dam =-(YNET_dam_mkt_dif+usenm_dif)) %>%

        mutate(all_dam_post2025 = all_dam - first(all_dam[year==2025]),
        dam_dif_perc2025 = (all_dam_post2025 / net_GDP_2025) * 100, 
        dam_dif_perc2025_all = (all_dam / net_GDP_2025) * 100) %>%

 
        mutate(cumulative_dam_perc2025 = cumsum(dam_dif_perc2025)) %>%
        ungroup() %>% as.data.frame()  %>% 
        filter(!all(cumulative_dam_perc2025  == 0), 
            !is.na(cumulative_dam_perc2025))

    glimpse(fish_dam)

    ggplot(fish_dam %>% filter(year<2100)) + geom_point(aes(x = year, y =cumulative_dam_perc2025 ))

    fish_dam %>% filter(year2100,cumulative_dam_perc2025<0)
    
    ggplot(fish_dam) + geom_point(aes(x = year, y =dam_dif_perc2025))
    ggplot(fish_dam) + geom_point(aes(x = year, y =YNET_dam_mkt_dif ))
    ggplot(fish_dam) + geom_point(aes(x = year, y =usenm_dif ))
    glimpse(exp_data_damage)


    surpass_year_data <- find_surpass_year(fish_dam, "country", "year", "cumulative_dam_perc2025", threshold = 1) %>% 
        as.data.frame() 
    surpass_year_data %>% filter(country=="irl")   
    surpass_year_data %>% filter(year_surpass==-9999)   
    
    fish_year_1perc <- surpass_year_data  %>% mutate(iso_a3=toupper(country))
    world <- ne_countries(scale = "medium", returnclass = "sf")
    fish_year_1perc_map <- left_join(world, fish_year_1perc)%>% filter(continent != "Antarctica")

    ggplot(fish_year_1perc_map)+geom_sf(aes(fill=year_surpass))+scale_fill_viridis()

## Fish Year Surpass
    
    surpass_all <- rbind(cor_year_1perc_map %>% mutate(capital = "Corals"), fish_year_1perc_map %>% mutate(capital="Fisheries & Mariculture"), 
        ports_year_1perc_map %>% mutate(capital = "Ports"), man_year_1perc_map %>% mutate(capital="Mangroves"))
        glimpse(surpass_all)    


        surpass_all$damage_category <- cut(
                            surpass_all$year_surpass,
                            breaks = c(2020,2030,2040,2050,2060,2070,2080,2090,2100,2500,999999),
                            labels = c(2020,2030,2040,2050,2060,2070,2080,2090,">2100","Benefits"),
                            include.lowest = TRUE
                        )

        # Create an elegant red gradient
        color_ramp <- colorRampPalette(c("#4B000F", "#9B2C2C", "#C53030", "#FC8181", "#FED7D7"))  # Deep wine red to soft rose

        # Assign the gradient to your specific time points
        custom_colors <- c("darkgreen",color_ramp(9))  # Create 8 colors for each year group

        names(custom_colors) <- c("Benefits","2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", ">2100")

        custom_colors
        surpass_year_plot <- ggplot(data = surpass_all %>% filter(continent != "Antarctica")) +
            geom_sf(aes(fill = damage_category),linewidth=0.01) +
            facet_wrap(~capital)+
            #scale_fill_manual(values = custom_colors, name = "Year surpassing \n$1 Trillion in Losses",na.value="transparent") +
            scale_fill_manual(values = custom_colors, name = "Year", na.value = "transparent") +
            coord_sf(crs = "+proj=robin") + # Robinson projection
            theme_void() +
            labs(title="B. Cumulative damages exceeding 1% of 2025 GDP")+
            guides(
                #fill = guide_colorbar(
                #title.position = "top",
                #title.hjust = 0.5,
                #label.position = "bottom",
                #barwidth = 20,
                #barheight = 1,
                #direction = "horizontal",
                #position = "bottom",
                #ticks.colour = "black",
                #frame.colour = "black"
                #)
            ) + labs(title="B. Cumulative damages exceeding 1% of 2025 GDP")+
            theme(
                plot.title = element_text(hjust = 0.5)
            )
            #windows()
            
            print(surpass_year_plot)
        #     #ggsave("Figures/Main2.png")

        #     surpass_all <- surpass_all %>%
        #     mutate(year_capped = ifelse(year_surpass>2100,2100,year_surpass))%>%
        #  mutate(year_surpass_transformed = log(as.integer(year_capped ) - 2019))

        #     ggplot(data = surpass_all %>% filter(continent != "Antarctica")) +
        #     geom_sf(aes(fill = as.integer(year_surpass))) +
        #     facet_wrap(~capital)+
        #     scale_fill_scico(palette="batlow",
        #         direction=-1, end=0.8,
        #         "Year surpassing \n$1 Billion in Losses",
        #         na.value="transparent")+
        #     coord_sf(crs = "+proj=robin") + # Robinson projection
        #     theme_void() +
        #     labs(fill = "Year surpassing \n$1 Billion in Losses")+
        #     guides(fill = guide_colorbar(
        #         title.position = "top",
        #         title.hjust = 0.5,
        #         label.position = "bottom",
        #         barwidth = 20,
        #         barheight = 1,direction="horizontal",position="bottom",ticks.colour = "black", frame.colour = "black"
        #         ))
        #     #windows()v2

            #ggsave("Figures/Main2.png")
            


            # surpass_year_plot <- ggplot(data = surpass_all %>% filter(continent != "Antarctica")) +
            # geom_sf(aes(fill = year_surpass_transformed)) +
            # facet_wrap(~capital) +
            # scale_fill_scico(
            #     palette = "batlow",
            #     direction = -1,
            #     end = 0.8,
            #     name = "Year",
            #     na.value = "transparent",
            #     breaks = log(c(2019, 2030, 2040, 2050, 2070, 2099) - 2025),
            #     labels = c(2020,2030,2040, 2050,2070,2100)
            # ) +
            # coord_sf(crs = "+proj=robin") + # Robinson projection
            # theme_void() +
            # guides(
            #     fill = guide_colorbar(
            #     title.position = "top",
            #     title.hjust = 0.5,
            #     label.position = "bottom",
            #     barwidth = 20,
            #     barheight = 1,
            #     direction = "horizontal",
            #     position = "bottom",
            #     ticks.colour = "black",
            #     frame.colour = "black"
            #     )
            # ) + labs(title="B. Cumulative damages exceeding 1% of 2025 GDP")+
            # theme(
            #     plot.title = element_text(hjust = 0.5)
            # )

            # surpass_year_plot
            # surpass_all %>% filter(iso_a3 == "MNG")
            # ed57 %>% filter(ed57=="osea")
            #ggsave("Figures/Main_fig2.png")
####
## Across Time

glimpse(fish_dam)
glimpse(man_dam)
glimpse(cor_dam)
glimpse(ports_dam)



fish_dam <- fish_dam %>% mutate(capital = "Fisheries & Mariculture")
man_dam <- man_dam %>% mutate(capital = "Mangroves")
cor_dam <- cor_dam %>% mutate(capital = "Corals")
ports_dam <- ports_dam %>% mutate(capital = "Ports")

merged_df <- bind_rows(fish_dam, man_dam, cor_dam, ports_dam)

# Filter data for the year 2100
end_year <- 2100


# Reshape data to long format
plot_data <- merged_df %>%
  select(year, country,YNET,pop, capital, usenm_dif, nonuse_dif, YNET_dam_mkt_dif,CPC) %>%
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

glimpse(plot_data)


# Create the ggplot
ggplot(plot_data %>% filter(year < end_year+1, value<0), aes(x = CPC, y = -value*10^3, color = capital, group = interaction(country,capital, variable))) +
  geom_line(alpha=0.5) +
  geom_point(data = plot_data %>% filter(year == end_year), 
             aes(shape = variable), size = 2,alpha=0.4) +
    geom_text_repel(data = plot_data %>% filter(year == end_year), 
             aes(label=country), size = 2) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values=Color_capitals_dark)+
  facet_wrap(~capital,scales="free")+ 
  scale_y_log10(labels = scales::dollar_format(suffix = "B")) +
  #scale_x_log10() +
  labs(title = "A. Ocean-based damages under SSP2-6.0",
       #x = "YNET",
       y = "Losses (Billion USD)",
       #color = "Capital",
       shape = "Type of Value") +
  theme_minimal()

    ggplot(plot_data %>% filter(country=="guy"), aes(x=year,y=CPC))+geom_point()

  time_damages_plot <- ggplot(plot_data %>% filter(value<0,year < end_year+1), 
            aes(x = CPC/1000, y = -1000*value, color = capital, group = interaction(country,capital, variable))) +
        geom_line(alpha=0.5) +
        geom_point(data = plot_data %>% filter(year == end_year), 
                    aes(shape = variable), size = 2,alpha=0.4) +
        geom_text_repel(data = plot_data %>% filter(year == end_year), 
                    aes(label=country), size = 2) +
        scale_shape_manual(values = shapes) +
        facet_wrap(~variable)+
        #facet_wrap(~variable,scales="free")+ 
        scale_color_manual(values=Color_capitals_black)+
  scale_y_log10(labels = scales::dollar_format(suffix = "B")) +
        #scale_x_log10() +
        labs(title = "A. Ocean-based damages under SSP2-6.0",
            x = "GDP per capita (Thousand USD)",
            y = "Damages (Billion USD)",
            color = "Blue Capital",
            shape = "Value Category") +
        theme_minimal()+ 
                    theme(
                        plot.title = element_text(hjust = 0.5)
                    )



ggarrange(time_damages_plot,surpass_year_plot,ncol=1)
ggsave("Figures/Main/Fig2_Damages_v2.jpg",dpi=300)

