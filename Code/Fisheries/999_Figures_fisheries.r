#Figures Fisheries

    fisheries_df_temp_gdp <- read.csv("Data/modules/fish/Statistical/fisheries_Free_EtAl.csv")


    
plot_profits_usd <- ggplot(fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year>2013))+
geom_line(aes(y=profits_usd,x=year,color=rcp, group=interaction(rcp,country_iso3)))+
scale_color_manual(values = c("RCP26" = "#2ca02c", 
                                "RCP45" = "#1f77b4", 
                                "RCP85" = "#d62728",
                                "RCP60" = "#FFA500")) + 
  theme_minimal() +
  labs(color = "RCP Scenario",y="Profits (USD)",title="A. Fisheries Projection (MEX)")


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
  scale_color_manual(values = c("RCP26" = "#2ca02c", 
                                "RCP45" = "#1f77b4", 
                                "RCP85" = "#d62728",
                                "RCP60" = "#FFA500")) + 
  theme_minimal() +
  labs(color = "RCP Scenario",y="Profits (%GDP)")


plot_profits_gdp

fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year==2075)

plot_damage <- ggplot(fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX"))+
geom_point(aes(y=profit_ppDiff_from_rcp_26,x=tdif_from_rcp26,color=rcp),alpha=0.3)+
geom_point(data=fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year==2075),
    aes(y=profit_ppDiff_from_rcp_26,x=tdif_from_rcp26,color=rcp),size=3) + 
geom_smooth(aes(y=profit_ppDiff_from_rcp_26,x=tdif_from_rcp26),method="lm",color="darkgray")+
  scale_color_manual(values = c("RCP26" = "#2ca02c", 
                                "RCP45" = "#1f77b4", 
                                "RCP85" = "#d62728",
                                "RCP60" = "#FFA500"))  + 

  theme_minimal() +
  labs(color = "RCP Scenario", x= "Temperature: Deviation from RCP2.6 (°C)", y="Profits: Deviation from RCP2.6 (pp of GDP)",title="B. Warming Damage")

ggarrange(ggarrange(plot_profits_usd,plot_profits_gdp,ncol=1,common.legend=TRUE,legend="none",heights=c(4,3)),plot_damage,ncol=2,common.legend=TRUE,legend="bottom")


ggsave("Figures/SM/fisheries/data_mex_process_FreeEtAl.png")



load(file="Data/Modules/fish/Statistical/fish_tcoeff_FreeEtAl.Rds") #fish_tcoeff
glimpse(fish_tcoeff)

fish_tcoeff  <- fish_tcoeff %>% dplyr::rename(GDP_FractionChange_perC=tcoeff,GDP_FractionChange_perC_se=se)
    write.csv(fish_tcoeff,file="Data/intermediate_output/fish_tcoeff_v3.csv")
    
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
map_port

ggsave("Figures/SM/fisheries/coeff_FreeEtAl_fraction.png")
