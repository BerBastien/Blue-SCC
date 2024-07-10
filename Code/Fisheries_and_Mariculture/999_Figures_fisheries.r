## Load data(start)
    fisheries_df_temp_gdp <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/fisheries_Free_EtAl.csv")
    fish_tcoeff <- read.csv(file="Data/output_modules_input_rice50x/input_rice50x/fish_tcoeff.csv")
## Load data (end)


## Figure F1 (start)
      
  plot_profits_usd <- ggplot(fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year>2013))+
    geom_line(aes(y=profits_usd/10^6,x=year,color=rcp, group=interaction(rcp,country_iso3)))+
    # scale_color_manual(values = c("RCP26" = "#2ca02c", 
    #                                 "RCP45" = "#1f77b4", 
    #                                 "RCP85" = "#d62728",
    #                                 "RCP60" = "#FFA500")) + 
    scale_color_manual(values=hex_rcps)+
      theme_minimal() +
    labs(color = "RCP Scenario",y="Profits (million USD)",title="A. Fisheries Projection (MEX)")


  plot_profits_gdp <- ggplot(fisheries_df_temp_gdp %>% 
          filter(scenario == "Full Adaptation", 
                  country_iso3 == "MEX", 
                  year > 2013, 
                  !is.na(profits_usd_percGDP))) +
    geom_point(aes(y = profits_usd_percGDP, x = year, color = rcp)) +
    geom_line(aes(y = profits_usd_percGDP, x = year, color = rcp)) +
    geom_vline(xintercept = 2075, linetype = "dashed") +
    geom_segment(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation",  country_iso3 == "MEX", year == 2075,rcp %in% c("RCP45")), 
                aes(x = 2075, xend = 2075 + 5, y = profits_usd_percGDP, yend = profits_usd_percGDP+0.015, color = rcp), 
                arrow = arrow(type = "closed", length = unit(0.02, "inches"))) +
    geom_text(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation", country_iso3 == "MEX", year == 2075,rcp %in% c("RCP45")), 
              aes(y = profits_usd_percGDP+0.015, x = 2075 + 6, label = sprintf("%.4f", profits_usd_percGDP), color = rcp), 
              hjust = 0, size = 3) +
    geom_segment(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation",  country_iso3 == "MEX", year == 2075,rcp %in% c("RCP26")), 
                aes(x = 2075, xend = 2075 + 5, y = profits_usd_percGDP, yend = profits_usd_percGDP+0.01, color = rcp), 
                arrow = arrow(type = "closed", length = unit(0.02, "inches"))) +
    geom_text(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation", country_iso3 == "MEX", year == 2075,rcp %in% c("RCP26")), 
              aes(y = profits_usd_percGDP+0.01, x = 2075 + 6, label = sprintf("%.4f", profits_usd_percGDP), color = rcp), 
              hjust = 0, size = 3) +
    geom_segment(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation",  country_iso3 == "MEX", year == 2075,rcp %in% c("RCP85")), 
                aes(x = 2075, xend = 2075 - 10, y = profits_usd_percGDP, yend = profits_usd_percGDP-0.005, color = rcp), 
                arrow = arrow(type = "closed", length = unit(0.02, "inches"))) +
    geom_text(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation", country_iso3 == "MEX", year == 2075,rcp %in% c("RCP85")), 
              aes(y = profits_usd_percGDP-0.006, x = 2075 - 25, label = sprintf("%.4f", profits_usd_percGDP), color = rcp), 
              hjust = 0, size = 3) +
    geom_segment(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation",  country_iso3 == "MEX", year == 2075,rcp %in% c("RCP60")), 
                aes(x = 2075, xend = 2075 - 10, y = profits_usd_percGDP, yend = profits_usd_percGDP-0.005, color = rcp), 
                arrow = arrow(type = "closed", length = unit(0.02, "inches"))) +
    geom_text(data = fisheries_df_temp_gdp %>% filter(scenario == "Full Adaptation", country_iso3 == "MEX", year == 2075,rcp %in% c("RCP60")), 
              aes(y = profits_usd_percGDP-0.005, x = 2075 - 25, label = sprintf("%.4f", profits_usd_percGDP), color = rcp), 
              hjust = 0, size = 3) +
    
    scale_color_manual(values=hex_rcps)+
    theme_minimal() +
    labs(color = "RCP Scenario",y="Profits (%GDP\n(difference from RCP 2.6)")



  fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year==2075)

  plot_damage <- ggplot(fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX"))+
    geom_hline(aes(yintercept=0),linetype="dashed")+
    geom_point(aes(y=profit_ppDiff_from_rcp_26,x=tdif_from_rcp26,color=rcp),alpha=0.3)+
    geom_point(data=fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year==2075),
        aes(y=profit_ppDiff_from_rcp_26,x=tdif_from_rcp26,color=rcp),size=3) + 
    geom_smooth(aes(y=profit_ppDiff_from_rcp_26,x=tdif_from_rcp26),method="lm",color="darkgray")+
    scale_color_manual(values=hex_rcps)+
    theme_minimal() +
    labs(color = "RCP Scenario", x= "Temperature(°C)\n(difference from RCP 2.6)", y="Profits\n Deviation from RCP2.6 (pp of GDP)",title="B. Warming Damage")

  
  fig_f1 <- ggarrange(ggarrange(plot_profits_usd,plot_profits_gdp,ncol=1,common.legend=TRUE,legend="none",heights=c(4,3)),plot_damage,ncol=2,common.legend=TRUE,legend="bottom")

  windows()
  print(fig_f1)
  
  #ggsave("Figures/SM/fisheries/data_mex_process_FreeEtAl.png")

## Figure F1 (end)

## Figure F2

  # Get the world map in sf format
  world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
  merged_data <- left_join(world, fish_tcoeff, by = c("iso_a3" = "country_iso3"))

  # Plot
  map_port <- ggplot(data = merged_data) +
    geom_sf(aes(fill = GDP_FractionChange_perC)) +
    scale_fill_scico(palette = "batlow", limits = c(-0.00005,0.00005), oob=squish, direction=-1,na.value="transparent") + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    theme(legend.position = "right",
    #legend.title = element_blank(), # Ensure no title is displayed
    legend.text = element_text(angle = 0)) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "Damage Coefficients\n(GDP Change/Degree C)",ticks.colour = "black", frame.colour = "black")) +
    ggtitle("Fisheries Damage Function")
  #map_port

  #ggsave("Figures/SM/fisheries/coeff_FreeEtAl_fraction.png")

  #glimpse(merged_data)

  merged_data <- merged_data %>% filter(!is.na(GDP_FractionChange_perC)) %>% mutate(GDP_PercentChange_perC=100*GDP_FractionChange_perC)

  # Create categories for negative values
  neg_data <- merged_data %>%
    filter(GDP_PercentChange_perC < 0) %>%
    mutate(fish_tcoeff_category = paste0("Negative_", ntile(GDP_PercentChange_perC, 3)))
    
    merged_data %>%
    filter(GDP_PercentChange_perC < 0) %>% mutate(a=ntile(GDP_PercentChange_perC, 3)) %>% dplyr::select(a,GDP_PercentChange_perC) %>% as.data.frame()

  # Create categories for positive values
  pos_data <- merged_data %>%
    filter(GDP_PercentChange_perC > 0) %>%
    mutate(fish_tcoeff_category = paste0("Positive_", ntile(GDP_PercentChange_perC, 3)))

  # Combine the datasets back together
  merged_data <- bind_rows(neg_data, pos_data)

  # If there are any remaining NA categories, handle them as required (optional)
  merged_data <- merged_data %>% mutate(fish_tcoeff_category = replace_na(fish_tcoeff_category, "Unknown"))

  # Get quantile values for labeling
  negative_quantiles <- quantile(neg_data$GDP_PercentChange_perC, probs = seq(0, 1, length.out = 4), na.rm = TRUE)
  positive_quantiles <- quantile(pos_data$GDP_PercentChange_perC, probs = seq(0, 1, length.out = 4), na.rm = TRUE)

  # Create labels for the categories
  labels <- c(
    paste0("Negative (", round(negative_quantiles[1],5), ", ", round(negative_quantiles[2],5), "]"),
    paste0("Negative (", round(negative_quantiles[2],5), ", ", round(negative_quantiles[3],5), "]"),
    paste0("Negative (", round(negative_quantiles[3],5), ", ", round(negative_quantiles[4],5), "]"),
    paste0("Positive (", round(positive_quantiles[1],5), ", ", round(positive_quantiles[2],5), "]"),
    paste0("Positive (", round(positive_quantiles[2],5), ", ", round(positive_quantiles[3],5), "]"),
    paste0("Positive (", round(positive_quantiles[3],5), ", ", round(positive_quantiles[4],5), "]")
  )

  # Map the labels to the categories
  merged_data <- merged_data %>%
    mutate(fish_tcoeff_category = case_when(
      fish_tcoeff_category == "Negative_1" ~ labels[1],
      fish_tcoeff_category == "Negative_2" ~ labels[2],
      fish_tcoeff_category == "Negative_3" ~ labels[3],
      fish_tcoeff_category == "Positive_1" ~ labels[4],
      fish_tcoeff_category == "Positive_2" ~ labels[5],
      fish_tcoeff_category == "Positive_3" ~ labels[6],
      TRUE ~ fish_tcoeff_category
    ))
  merged_data <- merged_data %>%
  mutate(fish_tcoeff_category = factor(fish_tcoeff_category, levels = labels))


  map_fisheries <- ggplot(data = merged_data %>% dplyr::filter(continent!="Antarctica") ) +
  geom_sf(data=world  %>% dplyr::filter(continent!="Antarctica")  , color="gray26",fill="transparent")+
  geom_sf(aes(fill = fish_tcoeff_category), color="gray26") +
  scale_fill_manual(values = c( "Negative (-0.30382, -0.01178]"=hex_neg_3[1],
                                "Negative (-0.01178, -0.00195]"=hex_neg_2[1],
                                "Negative (-0.00195, -1e-05]" = hex_neg_1[1],
                                "Positive (2e-05, 0.00125]"=hex_pos_1[1],
                                "Positive (0.00125, 0.0067]"=hex_pos_2[1],
                               "Positive (0.0067, 0.05051]"=hex_pos_3[1]), 
                    name = "Damage Coefficients\n(% GDP Change/Degree C)") +
  coord_sf(crs = "+proj=robin") +  # Robinson projection
  theme_minimal() +
  theme(legend.position = "right",
    #legend.title = element_blank(), # Ensure no title is displayed
    legend.text = element_text(angle = 0))  +
  #guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title = "Fish T-Coeff\nCategories")) +
  #guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black"))+
  ggtitle("")

  # Display the map
  windows()
  print(map_fisheries)
  
  #ggsave("Figures/SM/fisheries/coeff_FreeEtAl_percentage.png")




## Figure F2 (end)

## Figures Nutrition (start)
    ## Nutrition Change (start)
      nut_proj_long_coeff <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/nut_proj_long_coeff.csv")
      
      windows()
      ggplot(nut_proj_long_coeff) +
          geom_point(aes(x=atmoT,y=Value,shape=factor(RCP),color=nutrient))+
          xlab("Global Mean Surface Temperature Increase (°C)")+
          ylab("Nutrients Change (%)") +
          theme_minimal() +
          geom_line(aes(x=atmoT,y=(atmoT*tcoeff),color=nutrient))+
          geom_ribbon(aes(x=atmoT,ymin=((atmoT-se*1.96)*tcoeff),ymax=((atmoT+se*1.96)*tcoeff),fill=nutrient),alpha=0.5) +
        guides(shape=guide_legend("RCP")) +
        labs(fill="Nutrient",color="Nutrient")+
        scale_fill_manual(values=hex_ssps)+
        scale_color_manual(values=hex_ssps) + 
        facet_wrap(~income)

      #ggsave("Figures/SM/fisheries/nut_tcoeffs.png")
    ## Nutrition Change (end)

    
    ## Risk Change Cardiovascular Omega (start)
      gbd_deaths_number_doseresponse_pop_temp_future <- 
        read.csv("Data/output_modules_input_rice50x/output_modules/fish/gbd_deaths_number_doseresponse_pop_temp_future.csv")


      
      #gbd_deaths_number_doseresponse_pop_temp_future %>% filter(year ==2020, ISO3=="MEX", nutrient=="Omega3", cause=="Cardiovascular diseases", !is.na(rcp))
      windows()
      ggplot(gbd_deaths_number_doseresponse_pop_temp_future %>% filter(year >2020, ISO3=="MEX", nutrient=="Omega3", cause=="Cardiovascular diseases", !is.na(rcp)))+
        geom_point(aes(x=year,y=-total_effect+delta_risk,color=rcp))+
        scale_color_manual(values=hex_ssps)+
        theme_minimal()+
        ylab("Relative Risk due to Omega-3 intake")+
        guides(color=guide_legend("RCP")) 
      
      #ggsave("Figures/SM/fisheries/nut_riskchange.png")
      
    ## Risk change (end)

    ## Future Deaths Global
        c25 <- c(
          "dodgerblue2", "#E31A1C", # red
          "green4",
          "#6A3D9A", # purple
          "#FF7F00", # orange "gold1",
          "skyblue2", "#FB9A99", # lt pink
          "palegreen2",
          "#CAB2D6", # lt purple
          "#FDBF6F", # lt orange
          "gray70", "khaki2",
          "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
          "darkturquoise", "green1", "yellow4", "yellow3",
          "darkorange4", "brown"
          )
      
      global_deaths <- ggplot(gbd_deaths_number_doseresponse_pop_temp_future_dep %>% filter(ssp=="SSP2",year>2020,ISO3!="LUX")) +
      geom_line(aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000, color=cause, group=interaction(cause,ISO3,nutrient)),alpha=0.1) +
          geom_point(data=gbd_deaths_number_doseresponse_pop_temp_future_dep %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"),
              aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000, color=cause, shape=nutrient, group=interaction(cause,nutrient))) +
          #xlim(c(2020,2120))+
          #geom_text_repel(data=gbd_deaths_number_doseresponse_pop_temp_future %>% filter(ssp=="SSP2",year==2100),
          #   aes(x = deaths_percapita_future*Pop.million*10^6, y=100*delta_risk, color=nutrient, label=cause),size=2.5) +
          ylab("Increase in Health Conditions (%)") +
          labs(title="World under SSP2-4.6 (2020 to 2100)",shape="Deficiency",color="Mortality Cause",y="Additional Deaths \n(per million persons)",x="GDP per capita (Thousand 2020 USD)") +
          #scale_x_continuous(trans="log2")+
          #scale_y_continuous(trans="log2")+
          theme_minimal() +
          scale_color_manual(values=c25) +
          theme(legend.position = "bottom") +
          guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                  color = guide_legend(title.position = "top", title.hjust = 0.5))
        
      #ggsave("Figures/SM/fisheries/health_all_cause_nologxy.png")

      MEX_deaths <- ggplot(gbd_deaths_number_doseresponse_pop_temp_future_dep %>% filter(ssp=="SSP2",year>2020,ISO3=="MEX")) +
          geom_line(aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000, color=cause, group=interaction(cause,ISO3,nutrient))) +
          geom_ribbon(aes(ymax = (deaths_percapita_future+deaths_percapita_future_se*1.96)*10^6, ymin = (deaths_percapita_future-deaths_percapita_future_se*1.96)*10^6, 
              x=GDPpc_2020USD/1000, fill=cause, group=interaction(cause,ISO3,nutrient)),alpha=0.3) +
          geom_point(data=gbd_deaths_number_doseresponse_pop_temp_future_dep %>% filter(ssp=="SSP2",year==2100,ISO3=="MEX"),
              aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000, color=cause, shape=nutrient, group=interaction(cause,nutrient))) +
          ylab("Increase in Health Conditions (%)") +
          labs(title="Mexico under SSP2-4.6 (2020 to 2100)",shape="Deficiency",color="Mortality Cause",fill="Mortality Cause",y="Additional Deaths \n(per million persons)",x="GDP per capita (Thousand 2020 USD)") +
          theme_minimal() +
          scale_color_manual(values=c25)  +
          scale_fill_manual(values=c25) +
          theme(legend.position = "bottom") +
          guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                  color = guide_legend(title.position = "top", title.hjust = 0.5),
                  fill = guide_legend(title.position = "top", title.hjust = 0.5))

      windows()
      ggarrange(global_deaths,MEX_deaths,ncol=2,common.legend=TRUE,legend="bottom")    
      #ggsave("Figures/SM/fisheries/health_all_cause_global_mex_nutdep.png")

    ## future Deaths Global

    ## Future Deaths by nutrient
      deaths_by_nutrient <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/deaths_by_nutrient.csv")
      windows()
      ggplot(deaths_by_nutrient %>% filter(ssp=="SSP2",year>2020,ISO3!="LUX")) +
        geom_line(aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=nutrient, group=interaction(ISO3,nutrient)),alpha=0.1) +
            geom_point(data=deaths_by_nutrient %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"),
                aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000, ,  color=nutrient, shape=nutrient, group=interaction(nutrient))) +
            labs(title="World under SSP2-4.6 (2020 to 2100)",shape="Deficiency",color="Deficiency",y="Additional Deaths \n(per million persons)",x="GDP per capita (Thousand 2020 USD)") +
            theme_minimal() +
            scale_color_manual(values=c25) +
            theme(legend.position = "bottom") +
            guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                    color = guide_legend(title.position = "top", title.hjust = 0.5))+
            scale_color_manual(values=hex_ssps)
    
      #ggsave("Figures/SM/fisheries/health_all_cause_by_nutrient_nutdep.png")
    ## Future Deaths by Nutrient

    ## Deaths by Country (start)
      
      deaths_by_country <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/deaths_by_country_Globalvsl.csv")
      #glimpse(deaths_by_country)
      windows()
      ggplot(deaths_by_country %>% filter(ssp=="SSP2",year>2020)) +
        geom_line(aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, group=interaction(ISO3)),alpha=0.1) +
        geom_ribbon(aes(ymin = (deaths_percapita_future-1.96*deaths_percapita_future_se)*10^6,ymax = (deaths_percapita_future+1.96*deaths_percapita_future_se)*10^6, x=GDPpc_2020USD/1000,  fill=R5, group=interaction(ISO3)),alpha=0.09) +
            geom_point(data=deaths_by_country %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"),
                aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5)) +
        geom_text_repel(data= deaths_by_country %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"), 
            aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, label=ISO3),size=1.7)+
            labs(title="World under SSP2-4.6 (2020 to 2100)",shape="Deficiency",fill="Region",color="Region",y="Additional Deaths \n(per million persons)",x="GDP per capita (Thousand 2020 USD)") +
            theme_minimal() +
            scale_color_manual(values=c25) +
            theme(legend.position = "bottom") +
            guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                    color = guide_legend(title.position = "top", title.hjust = 0.5))+
            scale_color_manual(values=hex_R5)+
            scale_fill_manual(values=hex_R5)
      
      #ggsave("Figures/SM/fisheries/health_all_cause_by_country_nutdep.png")

      windows()
      ggplot(deaths_by_country %>% filter(ssp=="SSP2",year>2020)) +
        geom_line(aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, group=interaction(ISO3)),alpha=0.5) +
        #geom_ribbon(aes(ymin = (deaths_percapita_future-1.96*deaths_percapita_future_se)*10^6,ymax = (deaths_percapita_future+1.96*deaths_percapita_future_se)*10^6, x=GDPpc_2020USD/1000,  fill=R5, group=interaction(ISO3)),alpha=0.09) +
         #   geom_point(data=deaths_by_country %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"),
          #      aes(y = deaths_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5)) +
        geom_text_repel(data= deaths_by_country %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"), 
            aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, label=ISO3),size=1.7)+
            labs(title="World under SSP2-4.6 (2020 to 2100)",shape="Deficiency",fill="Region",color="Region",y="Lives Saved \n(per million persons)",x="GDP per capita (Thousand 2020 USD)") +
            theme_minimal() +
            scale_color_manual(values=c25) +
            theme(legend.position = "bottom") +
            guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                    color = guide_legend(title.position = "top", title.hjust = 0.5))+
            scale_color_manual(values=hex_R5)+
            scale_fill_manual(values=hex_R5)
      #ggsave("Figures/SM/fisheries/lives_saved.png")


      windows()
      ggplot(deaths_by_country %>% filter(ssp=="SSP2",year==2021,ISO3!="LUX")) +
        geom_line(aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, group=interaction(ISO3)),alpha=0.1) +
        geom_text_repel(data = deaths_by_country %>% filter(ssp=="SSP2",year==2021,ISO3!="LUX"),
        aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, label=(ISO3))) +
        geom_ribbon(aes(ymin = (lives_saved_percapita_future-1.96*lives_saved_percapita_future_se)*10^6,ymax = (lives_saved_percapita_future+1.96*lives_saved_percapita_future_se)*10^6, x=GDPpc_2020USD/1000,  fill=R5, group=interaction(ISO3)),alpha=0.09) +
            geom_point(data=deaths_by_country %>% filter(ssp=="SSP2",year==2021,ISO3!="LUX"),
                aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5)) +
            labs(title="Lives supported by fisheries in 2021",shape="Deficiency",fill="Region",color="Region",
                y="Avoided Deaths due to fiheries and mariculture nutrients \n(per million persons)",x="GDP per capita (Thousand 2020 USD)") +
            theme_minimal() +
            scale_color_manual(values=c25) +
            theme(legend.position = "bottom") +
            guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                    color = guide_legend(title.position = "top", title.hjust = 0.5))+
            scale_color_manual(values=hex_R5)+
            scale_fill_manual(values=hex_R5) 

        windows()
        ggplot(deaths_by_country %>% filter(ssp=="SSP2",year>2020)) +
            geom_line(aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, group=interaction(ISO3)),alpha=0.5) +
            #geom_text_repel(data = deaths_by_country %>% filter(ssp=="SSP2",year==2100,ISO3!="LUX"),aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5, label=(ISO3))) +
            #geom_ribbon(aes(ymin = (lives_saved_percapita_future-1.96*lives_saved_percapita_future_se)*10^6,ymax = (lives_saved_percapita_future+1.96*lives_saved_percapita_future_se)*10^6, x=GDPpc_2020USD/1000,  fill=R5, group=interaction(ISO3)),alpha=0.09) +
                geom_point(data=deaths_by_country %>% filter(ssp=="SSP2",year==2100),
                    aes(y = lives_saved_percapita_future*10^6, x=GDPpc_2020USD/1000,  color=R5)) +
                labs(title="World under SSP2-4.6 (2020 to 2100)",shape="Deficiency",fill="Region",color="Region",
                    y="Health benefits from fusheries and mariculture \n(Avoided Deaths per million persons)",x="GDP per capita (Thousand 2020 USD)") +
                theme_minimal() +
                scale_color_manual(values=c25) +
                theme(legend.position = "bottom") +
                guides(shape = guide_legend(title.position = "top", title.hjust = 0.5),
                        color = guide_legend(title.position = "top", title.hjust = 0.5))+
                scale_color_manual(values=hex_R5)+
                scale_fill_manual(values=hex_R5) 

          windows()
          nut <- ggplot(deaths_by_country %>% filter(ssp=="SSP2",year==2020,ISO3!="ISR")) +
              geom_point(aes(size=deaths_base_percapita,y = lives_saved_percapita_future*10^6, x=Nutritional_D,  color=R5, group=interaction(ISO3)),alpha=0.5) +
              geom_text_repel(aes(x=Nutritional_D,y=lives_saved_percapita_future*10^6,label=ISO3),fill="transparent",size=2) +
              theme_minimal() +
              geom_rect(aes(xmin=0, xmax=0.125, ymin=0, ymax=400), fill=NA, color="black", linetype="dashed") +  # Add bounding box
                  scale_color_manual(values=hex_R5)+
                  scale_fill_manual(values=hex_R5) +                  
                  labs(size="Mortality Rate Baseline", x = "Nutritional Dependency on Fisheries", y = "Avoided premature deaths \nper million persons", title="Avoided premature deaths in 2100")

          zoom_nut <- ggplot(deaths_by_country %>% filter(ssp=="SSP2",year==2020,ISO3!="ISR")) +
              geom_point(aes(size=deaths_base_percapita,y = lives_saved_percapita_future*10^6, x=Nutritional_D,  color=R5, group=interaction(ISO3)),alpha=0.5) +
              geom_text_repel(aes(x=Nutritional_D,y=lives_saved_percapita_future*10^6,label=ISO3),fill="transparent",size=2) +
              theme_minimal() +
              xlim(c(0,0.125))+
              ylim(c(0,400))+
                  scale_color_manual(values=hex_R5)+
                  scale_fill_manual(values=hex_R5) +                  
                  labs(size="Mortality Rate Baseline", x = "Nutritional Dependency on Fisheries", y = "Avoided premature deaths \nper million persons", title=" (Zoomed in)")
          ggarrange(nut,zoom_nut,ncol=2,common.legend=T,legend="bottom")
          #ggsave("Figures/SM/fisheries/Deaths_dependency.png")  

    ## Deaths by Country (end)

    ## Health Damages in Dollars (start)
      deaths_by_country_vsl <- read.csv("Data/Modules/fish/Nutrition/deaths_by_country_GlobalVSL.csv")
      glimpse(deaths_by_country_vsl)
      windows()
      ggplot(deaths_by_country_vsl) +
        geom_line(aes(x=tdif,y=health_damage,color=R5,linetype=scenario,group=interaction(ssp,ISO3)))+
        geom_ribbon(aes(x=tdif,ymin=health_damage - (1.96*health_damage_se),ymax=health_damage + (1.96*health_damage_se),fill=R5,linetype=scenario,group=interaction(ssp,ISO3)),alpha=0.09)+
        geom_text_repel(data=deaths_by_country_vsl %>% filter(year==2100),aes(x=tdif,y=health_damage,color=R5,label=ISO3),max.overlaps=100,size=2.5)+
        scale_color_manual(values=hex_R5) +
        scale_fill_manual(values=hex_R5) +
        labs(y = "Health Damage \n(million 2020 USD)", linetype="Scenario", fill="Region", color="Region", x="Global Temperature Increase (°C)") +
        xlim(c(0,4))+
        #scale_y_continuous(trans="log10")
        theme_minimal()

      #ggsave("Figures/SM/fisheries/health_damage_by_country.png")  

      windows()
      ggplot(deaths_by_country_vsl %>% filter(year %in% c(seq(1:100)*5+2000)))+
          geom_point(aes(x=year,y=popweighted_average_GDPpc/1000,color=ssp,shape=ssp)) + 
          geom_line(data=deaths_by_country_vsl,
              #%>% filter(ISO3=="USA"), 
              aes(x=year,y=GDPpc_2020USD/1000,color=ssp,group=interaction(ssp,ISO3)),alpha=0.1) +
              guides(color="none") +
              theme_minimal()+
              labs(x="year",y="GDP per capita (Thousand 2020 USD)",shape="Scenario")
        #ggsave("Figures/SM/fisheries/MeanGDP_pc.png")  

      ggplot(deaths_by_country_vsl %>% filter(ISO3=="MEX",year %in% c(seq(1:100)*5+2020)))+
          geom_line(aes(x=year,y=health_benefits/Pop.million/GDP_2020USD,color=ssp,group=interaction(ssp,ISO3))) +
          #geom_ribbon(aes(x=year,ymin=health_benefits_percGDP-(health_benefits_percGDP_se*1.96),ymax=health_benefits_percGDP+(health_benefits_percGDP_se*1.96),fill=ssp,group=interaction(ssp,ISO3)),alpha=0.09) +
          #geom_line(data=deaths_by_country_vsl,
              #%>% filter(ISO3=="USA"), 
              #aes(x=year,y=GDPpc_2020USD/1000,color=ssp,group=interaction(ssp,ISO3)),alpha=0.1) +
              #guides(color="none") +
              scale_color_manual(values=hex_ssps) +
              scale_fill_manual(values=hex_ssps) +
              theme_minimal()+
              labs(x="year",y="Health benefits  (% of GDP)",color="Scenario",fill="Scenario")
      
      
    
    fisheries_df_iso <- read.csv(file="Data/Modules/fish/Nutrition/health_benefits_tcoeff_GlobalVSL.csv")
    glimpse(fisheries_df_iso)
    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, fisheries_df_iso, by = c("iso_a3" = "ISO3"))

    # Plot
    dam <- ggplot(data = merged_data %>% filter(continent != "Antarctica")) +
    geom_sf(aes(fill = HealthBenefit_PercentageGDP_intercept )) +
    scale_fill_scico(palette = "lajolla", oob=squish, na.value=NA,direction=1) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Coral cover change \n(% change/C)")
    dam
    #ggsave("Figures/SM/corals/coral_countrymap_damage.png",dpi=600) 

    quantile_breaks <- quantile(merged_data$HealthBenefit_PercentageGDP_perDegreeC, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

    labels2 <- c(
        paste0("<", round(quantile_breaks[2], 2)),
        paste0("(", round(quantile_breaks[2], 2), ",", round(quantile_breaks[3], 2), "]"),
        paste0("(", round(quantile_breaks[3], 2), ",", round(quantile_breaks[4], 2), "]"),
        paste0(">", round(quantile_breaks[4], 2))
    )

    merged_data$damage_category <- cut(
        merged_data$HealthBenefit_PercentageGDP_perDegreeC,
        breaks = quantile_breaks,
        labels = labels2,
        include.lowest = TRUE
    )

    fig_c5 <- ggplot(data = merged_data %>% filter(continent != "Antarctica")) +
    geom_sf(aes(fill = damage_category )) +
    scale_fill_manual(values = custom_colors, name = "Health Damages \n(% of GDP-equivalent \n per Degree C)",na.value="transparent") +
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Health Damages \n(% change/C)")
    windows()
    
    print(fig_c5)
    ## Health Damages in Dollars (end)

## figures Nutrition (end)
