
# Load files (start)
  port_ssp<- read.csv("Data/output_modules_input_rice50x/output_modules/ports/ports_ssps_rcps.csv")
    load(file="Data/output_modules_input_rice50x/output_modules/ports/ports_tcoeff.Rds")
    port_locations_file <- 'Data/input_modules/ports/nodes_maritime.gpkg'
    library(scales)
# Load files (end)

## Fig P1 (start)

  density_RCP_ssp <- ggplot(data=port_ssp %>%
      group_by(iso3, RCP, SSP) %>% 
      summarise(risk_change_percGDP = sum(risk_change_percGDP,na.rm=TRUE),GDP_ppp_2050=first(GDP_ppp_2050)),
    aes(x = (GDP_ppp_2050/10^12), y = risk_change_percGDP)) +
    geom_hline(aes(yintercept=0),linetype="dashed")+
    geom_density_2d_filled(contour_var = "ndensity",aes(alpha=..level..,fill=RCP),bins=4,size=1)+
    geom_point(data=port_ssp %>%
      group_by(iso3, RCP, SSP) %>% 
      summarise(risk_change_percGDP = sum(risk_change_percGDP,na.rm=TRUE),GDP_ppp_2050=first(GDP_ppp_2050))%>%
                  ungroup() %>%
                  group_by(RCP, SSP) %>% 
      summarise(risk_change_percGDP = median(risk_change_percGDP,na.rm=TRUE),GDP_ppp_2050=mean(GDP_ppp_2050,na.rm=T)),
                  aes(x = (GDP_ppp_2050/10^12), y = risk_change_percGDP,shape=SSP) )+
    scale_alpha_discrete(range = c(0,0.6,0.9,1),guide = guide_none()) +
    facet_wrap(~ RCP) +
    scale_fill_manual(values = c("RCP26"=hex_rcp26,"RCP45"=hex_rcp45,"RCP85"=hex_rcp85)) +
    labs(
      title = "C. Increase in Risk",
      x = "GDP in 2050 (2020 trill USD)",
      y = "Change in economic value at risk \n(pp of GDP)"
    ) +
    coord_cartesian(ylim = c(-0.1, 0.4)) +
    theme_minimal()+
    scale_x_continuous(
        trans = "log10",lim=c(0.5,10^5)/1000,
        labels = dollar_format(prefix = "$", big.mark = ",")
      )

  density_RCP_ssp 
  
  port_ssp <- port_ssp %>% left_join(regions %>% mutate(iso3=countrycode),by="iso3")

  pr_total_present <- ggplot( )+
    geom_point(data=port_ssp %>% group_by(iso3,RCP,SSP) %>% 
        summarise(GDP_ppp_2022=GDP_ppp_2022/10^12,rb=sum(risk_base_perc),R5=R5)%>% slice(1),
        aes(x=(GDP_ppp_2022*1.087),y=rb,color=R5)) + 
    geom_text_repel(data= port_ssp %>% group_by(iso3) %>% filter(RCP=="RCP45", SSP=="SSP2") %>%
        summarise(GDP_ppp_2022=1.087*GDP_ppp_2022/10^12,rb=sum(risk_base_perc),R5=R5 )%>% slice(1),
        aes(x=(GDP_ppp_2022),y=rb,label=iso3,color=R5)) + 
    theme_bw()+
    xlab("GDP in 2022 (2020 trill USD)")+
    scale_color_manual(values=hex_R5)+
    scale_y_continuous(trans="log2",lim=c(0.01,16)) +
    #scale_x_continuous(trans="log10",lim=c(0.5,10^5)) +
    scale_x_continuous(
        trans = "log10",lim=c(0.5,10^5)/1000,
        labels = dollar_format(prefix = "$", big.mark = ",")
      )+
    ylab("Economic value at risk\n(% of GDP)")+
    ggtitle("A. Present Risk")
 



  pr_total_future_1scen <- ggplot( )+
    geom_point(data=port_ssp %>% filter(RCP=="RCP45",SSP=="SSP2") %>% group_by(iso3) %>% 
        summarise(GDP_ppp_2022=GDP_ppp_2050/10^12,rb=sum(risk_percGDP_2050),R5=R5 ),
        aes(x=(GDP_ppp_2022*1.087),y=rb,color=R5)) + #1.087 deflator 2015 to 2020
    geom_text_repel(data= port_ssp %>% group_by(iso3) %>% filter(RCP=="RCP45", SSP=="SSP2") %>%
        summarise(GDP_ppp_2022=GDP_ppp_2050/10^12,rb=sum(risk_percGDP_2050), R5=R5 )%>% slice(1),
        aes(x=(GDP_ppp_2022*1.087),y=rb,label=iso3,color=R5)) + 
    theme_bw()+
    xlab("GDP in 2050 (2020 trill USD)")+
      scale_color_manual(values=hex_R5)+
    ylab("Present Value at Risk in 2050 \n(% of GDP)")+
    scale_y_continuous(trans="log2",lim=c(0.01,16)) +
    #scale_x_continuous(trans="log10",lim=c(0.5,10^5)) +
    scale_x_continuous(
        trans = "log10",lim=c(0.5,10^5)/1000,
        labels = dollar_format(prefix = "$", big.mark = ",")
      )+
    ggtitle("B. Future Risk")+
    theme(axis.title.y = element_blank(),  # Removes y-axis label
          axis.text.y = element_blank()) 
  


  fig_p1 <- ggarrange(ggarrange(pr_total_present,pr_total_future_1scen,ncol=2,align="h",widths=c(4,3),
  common.legend=TRUE,legend="bottom"),
      density_RCP_ssp,ncol=1)

  windows()
  print(fig_p1)
  #ggsave("Figures/SM/ports/present_future_risk.png")


## Fig P1 (end)

## Fig P2 (start)


  # Panel A
    GDP_ppp_2050_mex <- port_ssp %>%
      filter(iso3 == "MEX", SSP == "SSP2") %>%
      summarise(GDP_ppp_2050_mex = first(GDP_ppp_2050) / 10^9) %>%
      pull(GDP_ppp_2050_mex)


    pr_dif_future <- ggplot( )+ 
      geom_smooth(data=port_ssp%>% group_by(iso3,RCP)  %>% filter(SSP=="SSP2") %>%
          summarise(GDP_ppp_2050=GDP_ppp_2050/10^9,rb=sum(risk_change_percGDP,na.rm=TRUE)),
          aes(x=GDP_ppp_2050,y=rb,color=RCP,fill=RCP)) + 
      scale_x_log10()+
      theme_bw()+
      xlab("GDP in 2050 (2015 Bill USD)")+
      scale_color_manual(values=hex_rcps)+
      scale_fill_manual(values=hex_rcps)+
      #scale_color_scico_d(palette="batlow", begin=0.1, end=0.8)+
      #scale_fill_scico_d(palette="batlow", begin=0.1, end=0.8)+
      geom_vline(xintercept = GDP_ppp_2050_mex, linetype = "dotted", color = "blue") + # Add the vertical line for Mexico's GDP  
      geom_text(aes(x = GDP_ppp_2050_mex+2500, y = 3, label = "MEX"), vjust = -0.5, color = "blue") + # Label for the line
      #ylim(c(-0.5,1))+
      geom_hline(aes(yintercept=0),linetype="dashed")+
      ylab("Change of Impacts\n (pp of GDP)")+
      ggtitle("A. Distribution of Impacts") 





  #Panel B
    glimpse(port_ssp)
    linear_damage_country <- ggplot()+
      geom_point(data = port_ssp %>% filter(iso3=="MEX")%>% group_by(RCP,SSP)   %>%
          summarise(GDP_ppp_2050=GDP_ppp_2050/10^9,risk_change=sum(risk_change_percGDP),tdif=mean(tdif)),
          aes(x=tdif,y=risk_change,color=RCP,shape=SSP) ) +
          geom_smooth(data = port_ssp %>%
                      filter(iso3 == "MEX") %>%
                      group_by(RCP, SSP) %>%
                      summarise(GDP_ppp_2050 = GDP_ppp_2050 / 10^9,
                                risk_change = sum(risk_change_percGDP),
                                tdif = mean(tdif), .groups = 'drop'), # Data for geom_smooth
                    aes(x = tdif, y = risk_change),
                    method = "lm",
                    formula = y ~ x - 1, # This forces the line through the origin
                    #se = FALSE, # Set to FALSE if you don't want the confidence interval shaded
                    color = "black")+
      geom_hline(aes(yintercept=0),linetype="dashed")+
      scale_color_manual(values=hex_rcps)+
      #scale_fill_manual(values=hex_rcps)+
      #scale_color_scico_d(palette="batlow", begin=0.1, end=0.8) + 
      theme_bw() + ylab("Change of Impacts\n (pp of GDP)") + 
      labs(title="B. Impacts in Mexico in 2050") + xlab("Change in GMST w.r.t. Baseline")+
        guides(color = FALSE) # This removes the color legend


    plot_effect_ports <- ggarrange(pr_dif_future,linear_damage_country,ncol=1,legend="right")

  #Panel C original (start)
 
    ports_tcoeff  <- ports_tcoeff %>% dplyr::rename(GDP_FractionChange_perC = tcoeff, 
    GDP_FractionChange_perC_se = se)
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    merged_data <- left_join(world, ports_tcoeff, by = c("iso_a3" = "iso3"))

    # Plot
    map_port <- ggplot(data = merged_data) +
      geom_sf(aes(fill = -GDP_FractionChange_perC*100)) +
      scale_fill_scico(palette = "lajolla", limits = c(0, 0.5), oob=squish,direction=-1) + 
      coord_sf(crs = "+proj=robin") + 
      theme_minimal() +
      theme(legend.position = "bottom",
          legend.text = element_text(angle = 0)) +
      guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "Ports Damage\n(%GDP/Degree C)")) +
      ggtitle("C. Damage Function Coefficients")

    fig_p2 <- ggarrange(plot_effect_ports,map_port,ncol=2)
    windows()
    print(fig_p2)
    glimpse(merged_data)
    ED_Table4_ports <- merged_data %>%st_drop_geometry(merged_data)%>% select(iso_a3,GDP_FractionChange_perC,GDP_FractionChange_perC_se)
    #write.csv(ED_Table4_ports,file="ExtendedData\\ED_Table4_ports.csv")
        
    #ggsave("Figures/SM/ports/coefficients.png")
  #Panel C original (end)

  
  #Panel C with ports (start)

    port_locations <- st_read(port_locations_file)
    port_locations <- port_locations %>%  left_join(ports_tcoeff, by = "iso3")


    map2_port <- ggplot(data = port_locations %>% filter(infra=="port")) +
      theme(legend.position = "bottom")+
      geom_sf(data = merged_data %>% filter(continent!="Antarctica"), aes(fill = -GDP_FractionChange_perC*100),color="lightgray") +
        scale_fill_scico(palette = "lajolla", limits = c(0, 0.5), oob=squish,direction=-1,na.value="transparent") + 
        #geom_sf(data = merged_data %>% filter(continent!="Antarctica"),fill = "transparent",color="gray") +
          guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "Ports Damage\n(%GDP/Degree C)")) +
          ggtitle("C. Damage Function Coefficients")+
          geom_sf(color="blue",alpha=0.1, size = 1) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(angle = 0)) +
            coord_sf(crs = "+proj=robin")  

    fig_p2_v2 <- ggarrange(plot_effect_ports,map2_port,ncol=2)

    windows()
    print(fig_p2_v2)
    #ggsave("Figures/SM/ports/coefficients_v2.png")
  
  #Panel C with ports (end)

  
  #Panel C with quantiles (start)

    # quantile_breaks <- quantile(merged_data$GDP_FractionChange_perC*100, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

    #   labels2 <- c(
    #       paste0("<", round(quantile_breaks[2], 2)),
    #       paste0("(", round(quantile_breaks[2], 2), ",", round(quantile_breaks[3], 2), "]"),
    #       paste0("(", round(quantile_breaks[3], 2), ",", round(quantile_breaks[4], 2), "]"),
    #       paste0(">", round(quantile_breaks[4], 2))
    #   )

    #   merged_data$damage_category <- cut(
    #       merged_data$GDP_FractionChange_perC*100,
    #       breaks = quantile_breaks,
    #       labels = labels2,
    #       include.lowest = TRUE
    #   )


      
    # map2_port <- ggplot(data = merged_data %>% filter(continent!="Antarctica")) +
    #   geom_sf(aes(fill = damage_category )) +
    #   scale_fill_manual(values = custom_colors, name = "Cover damage \n(% per Degree C)",na.value="transparent", drop = TRUE) +
    #   coord_sf(crs = "+proj=robin") + # Robinson projection
    #   theme_minimal() +
    #   theme(legend.position = "bottom",
    #         legend.text = element_text(angle = 45)) +
    #         coord_sf(crs = "+proj=robin")  



    
    merged_data <- merged_data %>% filter(!is.na(GDP_FractionChange_perC)) %>% mutate(GDP_PercentChange_perC=100*GDP_FractionChange_perC)


    # Create categories for negative values
    neg_data <- merged_data %>%
      filter(GDP_PercentChange_perC < 0) %>%
      mutate(port_tcoeff_category = paste0("Negative_", ntile(GDP_PercentChange_perC, 3)))
      
      merged_data %>%
      filter(GDP_PercentChange_perC < 0) %>% mutate(a=ntile(GDP_PercentChange_perC, 3)) %>% dplyr::select(a,GDP_PercentChange_perC) %>% as.data.frame()

    # Create categories for positive values
    pos_data <- merged_data %>%
      filter(GDP_PercentChange_perC > 0) %>%
      mutate(port_tcoeff_category = paste0("Positive_", ntile(GDP_PercentChange_perC, 3)))

      merged_data %>%
      filter(GDP_PercentChange_perC > 0) %>%
      dplyr::select(name_en)

    # Combine the datasets back together
    merged_data <- bind_rows(neg_data, pos_data)

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
      mutate(port_tcoeff_category = case_when(
        port_tcoeff_category == "Negative_1" ~ labels[1],
        port_tcoeff_category == "Negative_2" ~ labels[2],
        port_tcoeff_category == "Negative_3" ~ labels[3],
        port_tcoeff_category == "Positive_1" ~ labels[4],
        port_tcoeff_category == "Positive_2" ~ labels[5],
        port_tcoeff_category == "Positive_3" ~ labels[6],
        TRUE ~ port_tcoeff_category
      ))
    
    merged_data <- merged_data %>%
    mutate(port_tcoeff_category = factor(port_tcoeff_category, levels = labels))


    map2_port <- ggplot(data = merged_data %>% dplyr::filter(continent!="Antarctica") ) +
    geom_sf(data=world  %>% dplyr::filter(continent!="Antarctica")  , color="gray26",fill="transparent")+
    geom_sf(aes(fill = port_tcoeff_category), color="gray26") +
    scale_fill_manual(values = c( "Negative (-6.54074, -0.21241]"=hex_neg_3[1],
                                  "Negative (-0.21241, -0.08083]"=hex_neg_2[1],
                                  "Negative (-0.08083, -0.00177]" = hex_neg_1[1],
                                  "Positive (0.00295, 0.02032]"=hex_pos_1[1],
                                  "Positive (0.02032, 0.08329]"=hex_pos_2[1],
                               "Positive (0.08329, 0.50383]"=hex_pos_3[1]), 
                      name = "Damage Coefficients\n(% GDP Change/Degree C)") +
    coord_sf(crs = "+proj=robin") +  # Robinson projection
    theme_minimal() +
    theme(legend.position = "right",
      #legend.title = element_blank(), # Ensure no title is displayed
      legend.text = element_text(angle = 0))  +
    #guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title = "Fish T-Coeff\nCategories")) +
    #guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black"))+
    ggtitle("")

    fig_p2_v2 <- ggarrange(plot_effect_ports,map2_port,ncol=2)
  
  #Panel C with quantiles (end)


  windows()
  print(fig_p2_v2)
  #ggsave("Figures/SM/ports/coefficients_v3.png")
## Fig P2 (end)


## Figure comparing ports and fisheries

fisheries_df_temp_gdp <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/fisheries_Free_EtAl.csv")

plot_profits_usd <- ggplot(fisheries_df_temp_gdp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX", year>2013))+
    geom_line(aes(y=profits_usd/10^6,x=year,color=rcp, group=interaction(rcp,country_iso3)))+
    # scale_color_manual(values = c("RCP26" = "#2ca02c", 
    #                                 "RCP45" = "#1f77b4", 
    #                                 "RCP85" = "#d62728",
    #                                 "RCP60" = "#FFA500")) + 
    scale_color_manual(values=hex_rcps)+
      theme_minimal() +
    labs(color = "RCP Scenario",y="Profits (million USD)",title="A. Fisheries Projection (MEX)")

    
    fish_tcoeff <- read.csv(file="Data/output_modules_input_rice50x/input_rice50x/fish_tcoeff.csv")

    GDP_FractionChange_perC

    ###############
    #################
    ###############


    ED_Table4_ports <- read.csv(file="ExtendedData\\ED_Table4_ports.csv")
    ED_Table5_fisheries <- read.csv(file="ExtendedData\\ED_Table5_fisheries.csv")
    glimpse(ED_Table4_ports)
    glimpse(ED_Table5_fisheries)
    ports_fi <- ED_Table4_ports %>% 
      mutate(ports_coef = GDP_FractionChange_perC) %>% dplyr::select(-GDP_FractionChange_perC) %>% left_join(ED_Table5_fisheries,by="iso_a3")%>% 
      mutate(fish_coef = GDP_FractionChange_perC)
    
    library(ggplot2)
library(ggrepel)

ggplot(ports_fi, aes(x = fish_coef * 100, y = ports_coef * 100)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
  geom_text_repel(aes(label = iso_a3), size = 3) +
  theme_minimal() +
  labs(
    x = "Fisheries damage coefficient (% GDP per °C)",
    y = "Ports damage coefficient (% GDP per °C)",
    title = "Comparison of Fisheries vs Ports Damage Coefficients"
  )
ggsave("Figures/SM/ports/ports_fish_comparison.png")

