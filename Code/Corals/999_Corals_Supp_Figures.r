## Supplementary Figures Corals Module


## Load Data (start)
    
    #Fig C1
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/corals_tcoeff.Rds") #corals_tcoeff
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/corals_temp_unique.Rds") #corals_temp_unique

    #Fig C2
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/coral_areas_gulf.Rds") #coral_areas_gulf
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/coral_temp_gulf.Rds") #coral_areas_gulf

    #Fig C3
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/coral_areas_keys.Rds")#coral_areas_keys
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/coral_areas_keys_single.Rds")#coral_areas_keys_single
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/coral_areas_keys_single_joined.Rds")#coral_areas_keys_single_joined
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/coral_temp_keys.Rds")#coral_temp_keys
    
    #Fig C4
    load(file="Data\\output_modules_input_rice50x\\output_modules\\corals/corals_area_coeff_sf.Rds") #corals_area_coeff_sf

    #Fig C6
    coefs_with_vcov <- read.csv(file="Data\\output_modules_input_rice50x\\input_rice50x\\coral_GDPdam_coefficients.csv")
    ssp_corals_growth <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\corals/ssp_corals_growth.csv")
    market_coefficients_by_country3 <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\corals\\market_coefficients_by_country3.csv")

    
    corals_df_iso <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\corals_area_damage_value.csv")
    
    continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
                    st_transform(st_crs(corals_area_coeff_sf))  

        

## Load Data (end)

## Themes (start)
    my_theme <- function() {
        theme_minimal() +
            theme(panel.background = element_rect(fill = "black", color = NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank()#,
                #legend.position = "right",
                #legend.background = element_rect(fill = "black", color = NA),
                #legend.text = element_text(size = 12, color = "white"),
                #legend.title = element_text(size = 14, color = "white"),
                #plot.title = element_text(size = 22, color = "white", face = "bold")
                )
        }
        pal1 <- viridis(10, option = "plasma")

        pal1_heat <- heat.colors(10)
        pal2 <- rev(viridis(10, option = "RdBu"))
        pal1 <- viridis(100)
        #colors <- colorRampPalette(pal1)(length(breaks) - 1)
## Themes (end)

## Fig. S1 (start)
    individual_coral_change <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)), alpha = 0.05)+
    theme_bw()+
    guides(color="none")+
    scale_color_manual(values=c(hex_rcp45, hex_rcp85))+
    geom_text(aes(x=1,y=1.5,label="RCP4.5 - 2050"),color=hex_rcp45,size=3) +
    geom_text(aes(x=1.5,y=-2,label="RCP8.5 - 2050"),color=hex_rcp85,size=3)+
    geom_text(aes(x=2.1,y=-5,label="RCP4.5 - 2100"),color=hex_rcp45,size=3)+
    geom_text(aes(x=3.5,y=-12,label="RCP8.5 - 2100"),color=hex_rcp85,size=3) +
    xlab("Temperature increase from 1997-2018 baseline")+
    xlim(c(0,4.5))+
    ylab("Coral Cover Change (%)")

    legend_plot <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)))+
    theme_bw()+
    guides(color=guide_legend("Scenario"))+
    scale_color_manual(values=c(hex_rcp45, hex_rcp85))

    leg <- get_legend(legend_plot)

    density_coral_coeff <- ggplot(corals_tcoeff, aes(x = tcoeff, y = pval)) +
        geom_hex() +
        scale_y_log10() +  # Log scale on the y-axis
        labs(x = "GMST Coefficient (% loss per degree C)", y = "P value") +
        scale_fill_scico(palette="berlin")+theme_bw()

    density_coral_coeff_cover <- ggplot(corals_tcoeff, aes(x = tcoeff, y = living_coral_cover*100)) +
        geom_hex() +
        labs(x = "GMST Coefficient (Damages per degree C)", y = "Present Percentage of the Reef \nCovered by Living Corals") +
        scale_fill_scico(palette="berlin")+theme_bw()
    
    
    fig_c1 <- ggarrange(ggarrange(individual_coral_change,leg,ncol=2,widths=c(4,1)),density_coral_coeff_cover)
    
    windows()
    print(fig_c1)
    Sys.sleep(0.5)  # Add a short pause to ensure the plot is rendered when running source
    
    #ggsave("Figures/SM/corals/Coeff_CoralCover_PresentCover.png",dpi=600)


## Fig. S1 (end)

## Fig. S2 (start)
    # Define the coordinates of the Gulf of Mexico polygon
        gulf_coords <- matrix(c(
        -90, 18,
        -78, 18,
        -78, 28,
        -90, 28,
        -90, 18
        ), ncol = 2, byrow = TRUE)

        gulf_polygon <- st_polygon(list(gulf_coords))

        # Get the bounding box of the Gulf of Mexico polygon
        gulf_bbox <- st_bbox(gulf_polygon)

    coral_areas_temp <- ggplot() +
        geom_sf(data = coral_areas_gulf, aes(fill = log10(GIS_AREA_K)), size=5,color = NA)+
        scale_fill_viridis(name = expression(paste("Area (km"^2, ")")), 
            guide = guide_colorbar(nbin = 50),labels = c("1e-6","1e-4","0.01","1","100")) +
        geom_sf(data = coral_temp_gulf, aes(color = tcoeff), size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(% change/C)", guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Coral Area and Cover Change") +
        coord_sf(xlim = c(gulf_bbox["xmin"], gulf_bbox["xmax"]),
                ylim = c(gulf_bbox["ymin"], gulf_bbox["ymax"])) +
        my_theme()
        
    
    pal1 <- viridis(100)
    colors <- colorRampPalette(pal1)
        
    count_areas <- ggplot(coral_areas_gulf[which(!is.na(coral_areas_gulf$area_group)),], aes(x = GIS_AREA_K, fill = factor(area_group))) +
        geom_histogram(bins = 50, color = "black") +
        scale_fill_manual(values = colors(10),guide="none") +
        scale_x_log10() +
        labs(x = "Area (km2)", y = "Count", fill = "Area Group") +
        theme_minimal()
    count_areas    
    
    pal1 <- heat.colors(10)
    colors <- colorRampPalette(pal1)
    
    count_temp <- ggplot(coral_temp_gulf[which(!is.na(coral_temp_gulf$coef_group)),], aes(x = tcoeff, fill = factor(coef_group))) +
        geom_histogram(bins = 50, color = "black") +
        scale_fill_manual(values = colors(10),guide="none") +
        labs(x = "Cover damage \n(% change/C)", y = "Count", fill = "Cover change") +
        theme_minimal()
    fig_c2 <- ggarrange(coral_areas_temp,ggarrange(count_areas,count_temp,nrow=2,ncol=1,legend="none"),ncol=2,widths=c(2,1))
    windows()
    print(fig_c2)
    Sys.sleep(0.5)  # Add a short pause to ensure the plot is rendered when running source
    #ggsave("Figures/SM/corals/CoralChange_temp_area.png",dpi=600)
## Fig. S2 (end)

## Fig. S3 (start)
    coral_areas_temp_florida <- ggplot() +
        geom_sf(data = coral_areas_keys, aes(fill = (GIS_AREA_K)), color = NA)+
        scale_fill_viridis(name = expression(paste("Area (km"^2, ")")), guide = guide_colorbar(nbin = 50))+#,labels = c("1e-6","1e-4","0.01","1","100")) +
        geom_sf(data = coral_temp_keys, aes(color = tcoeff), size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(% change/C)", guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Coral Area and Cover Change in Florida Keys") +
        coord_sf(xlim = c(-80.5, -80.2),
                ylim = c(24.9, 25.3)) +
        my_theme()

    coral_areas_temp_florida <- ggplot() +
        geom_sf(data = coral_areas_keys_single, aes(fill = (area_km2)), color = NA)+
        scale_fill_viridis(name = expression(paste("Area (km"^2, ")")), guide = guide_colorbar(nbin = 50))+#,labels = c("1e-6","1e-4","0.01","1","100")) +
        geom_sf(data = coral_temp_keys, aes(color = tcoeff), size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = expression(paste("Cover damage \n(% change/C)")), guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Coral Cover Polygons\nand Surveys Location") +
        coord_sf(xlim = c(-80.5, -80.2),
                ylim = c(24.9, 25.3)) +
        my_theme()+
        guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5),
        color = guide_legend(title.position = "top", title.hjust = 0.5)) + 
        theme(legend.position="bottom")

    coral_areas_temp_florida2 <- ggplot() +
        geom_sf(data = coral_areas_keys_single_joined, aes(fill = surveys), color = NA)+
        geom_sf(data = coral_temp_keys, color = "white", size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = expression(paste("Cover damage (pp/T"^2, ")")), guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Number of Surveys \nwithin Polygons") +
        coord_sf(xlim = c(-80.5, -80.2),
                ylim = c(24.9, 25.3)) +
        my_theme()+
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) + 
        theme(legend.position="bottom")
        

    coral_areas_temp_florida3 <- ggplot() +
        geom_sf(data = coral_areas_keys_single_joined, aes(fill = mean_coef), color = NA)+
        scale_fill_gradientn(colors = pal1_heat[1:7],name = expression(paste("Cover damage \n(% change/C)")), guide = guide_colorbar(nbin = 50))+#,labels = c("1e-6","1e-4","0.01","1","100")) +
        #geom_sf(data = coral_temp_keys,color = "white", size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = expression(paste("Cover damage \n(% change/C)")), guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Mean Damage Coefficient\nwithing Coral Polygons") +
        coord_sf(xlim = c(-80.5, -80.2),
                ylim = c(24.9, 25.3)) +
        my_theme()+
        guides(fill = guide_colorbar(title.position = "top", barwidth =10, title.hjust = 0.5)) + 
        theme(legend.position="bottom")
    fig_c3 <- ggarrange(coral_areas_temp_florida,
            coral_areas_temp_florida2,
            coral_areas_temp_florida3,ncol=3,legend="bottom",align="h")
        
    windows()
    print(fig_c3)
    Sys.sleep(0.5)  # Add a short pause to ensure the plot is rendered when running source

    #ggsave("Figures/SM/corals/meandamage.png",dpi=600)



## Fig. C3 (end)

## Fig. C4 (start)

        
    continents <- continents[continents$continent != "Antarctica", ]
    data_robinson <- st_transform(corals_area_coeff_sf, "+proj=robin")
        ggplot(data = data_robinson, aes(fill = mean_coef)) +
        geom_sf(aes(color=mean_coef), size=2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(% change/C)", guide = "legend") +
        scale_fill_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(% change/C)", 
                            guide = guide_colorbar(nbin = 50, title.position = "top", title.hjust = 0.5,barwidth = 300, barheight = 300)) +
        geom_sf(data = continents, fill = "gray", color = NA, alpha = 0.2) +
        labs(title = "Effect of 1C increase in coral cover") +
        my_theme()+ 
        theme(legend.position="bottom") +
        guides(fill = guide_colorbar(title.position = "top",position = "bottom", title.hjust = 0.5,barwidth = 300, barheight = 300), 
            color = guide_colorbar(position = "bottom"), fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) + 
    theme(
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"), # Adjust the height of the legend key
        legend.key.width = unit(1.5, "cm")  # Adjust the width of the legend key
    )
    #ggsave("Figures/SM/corals/Effect1C_coralcover_size2.png",dpi=600) 

    quantile_breaks <- quantile(data_robinson$mean_coef, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

    # Create labels for the ranges
    labels2 <- c(
        paste0("<", round(quantile_breaks[2], 2)),
        paste0("(", round(quantile_breaks[2], 2), ",", round(quantile_breaks[3], 2), "]"),
        paste0("(", round(quantile_breaks[3], 2), ",", round(quantile_breaks[4], 2), "]"),
        paste0(">", round(quantile_breaks[4], 2))
    )

    # Create the categorical variable with the new labels
    data_robinson$damage_category <- cut(
        data_robinson$mean_coef,
        breaks = quantile_breaks,
        labels = labels2,
        include.lowest = TRUE
    ) 
    custom_colors <- c(hex_maxdam, hex_highdam, hex_meddam, hex_smalldam)
    
    map_cat <- ggplot(data = data_robinson, aes(fill = damage_category)) +
        geom_sf(aes(color = damage_category), size = 2) +
        scale_color_manual(values = custom_colors, name = "Cover damage (% per Degree C)") +
        scale_fill_manual(values = custom_colors, name = "Cover damage (% per Degree C)") +
        geom_sf(data = continents, fill = "gray", color = NA, alpha = 0.2) +
        labs(title = "") +
        my_theme() + 
        theme(
            legend.position = "bottom",
            legend.key.height = unit(1, "cm"), # Adjust the height of the legend key
            legend.key.width = unit(1.5, "cm")  # Adjust the width of the legend key
        ) +
        guides(
            fill = guide_legend(title.position = "top", title.hjust = 0.5),
            color = guide_legend(title.position = "top", title.hjust = 0.5)
        )
    #glimpse(data_robinson)
    #geospatial_coral_polygons <- data_robinson
    #save(geospatial_coral_polygons, file="Data/output_modules_input_rice50x/output_modules/corals/geospatial_coral_polygons.rdat")        
    #ggsave("Figures/SM/corals/Effect1C_coralcover_categories.png",dpi=600) 

    
    
    histogram_corals <- ggplot(data_robinson, aes(x = mean_coef, fill = damage_category)) +
        geom_histogram(binwidth = (max(data_robinson$mean_coef) - min(data_robinson$mean_coef)) / 30, color = "black", position = "identity") +
        scale_fill_manual(values = custom_colors, name = "Cover damage \n(% change/C)") +
        labs(title = "",
            x = "",
            y = "# of sites") +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            legend.key.height = unit(1, "cm"),  # Adjust the height of the legend key
            legend.key.width = unit(1.5, "cm"),  # Adjust the width of the legend key
            axis.text = element_text(color = "white"),  # Set axis numbers to white
            axis.title = element_text(color = "white")  # Set axis titles to white
        ) +
        guides(
            fill = FALSE
            )
        
    # Convert histogram plot to a grob
    hist_grob <- ggplotGrob(histogram_corals)    

    combined_plot <- map_cat +
        annotation_custom(
            grob = hist_grob,
            xmin = -13000000, xmax = -2000000,  # Adjust these values based on where you want to place the histogram
            ymin = -8000000, ymax = 400000
        )

    # Display the combined plot
    windows()
    print(combined_plot)
    Sys.sleep(0.5)  # Add a short pause to ensure the plot is rendered when running source
    #ggsave("Figures/SM/corals/Effect1C_coralcover_categories_histogram.png",dpi=600) 

## Fig. C4 (end)

##  Fig C5
    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, corals_df_iso, by = c("iso_a3" = "countrycode"))

    # Plot
    dam <- ggplot(data = merged_data %>% filter(continent != "Antarctica")) +
    geom_sf(aes(fill = DamCoef_changeperC*100 )) +
    scale_fill_scico(palette = "lajolla", oob=squish, na.value=NA,direction=1) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Coral cover change \n(% change/C)")
    dam
    #ggsave("Figures/SM/corals/coral_countrymap_damage.png",dpi=600) 

    quantile_breaks <- quantile(merged_data$DamCoef_changeperC, probs = seq(0, 1, length.out = 5), na.rm = TRUE)

    labels2 <- c(
        paste0("<", round(quantile_breaks[2], 2)),
        paste0("(", round(quantile_breaks[2], 2), ",", round(quantile_breaks[3], 2), "]"),
        paste0("(", round(quantile_breaks[3], 2), ",", round(quantile_breaks[4], 2), "]"),
        paste0(">", round(quantile_breaks[4], 2))
    )

    merged_data$damage_category <- cut(
        merged_data$DamCoef_changeperC,
        breaks = quantile_breaks,
        labels = labels2,
        include.lowest = TRUE
    )

    fig_c5 <- ggplot(data = merged_data %>% filter(continent != "Antarctica")) +
    geom_sf(aes(fill = damage_category )) +
    scale_fill_manual(values = custom_colors, name = "Cover damage \n(% per Degree C)",na.value="transparent") +
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Coral cover change \n(% change/C)")
    windows()
    print(fig_c5)
    Sys.sleep(0.5)  # Add a short pause to ensure the plot is rendered when running source
    #ggsave("Figures/SM/corals/coral_countrymap_cat.png",dpi=600) 

    # Custom colors for the categories
    
    # area <- ggplot(data = merged_data) +
    # geom_sf(aes(fill = area_km2_t0)) +
    # scale_fill_scico(palette = "batlow", trans = "log10", oob = squish, na.value = NA, direction = -1) +
    # coord_sf(crs = "+proj=robin") +
    # theme_minimal() +
    # labs(fill = "Coral Cover (km2)")

    
    # ggarrange(area,dam)
    # ggsave("Figures/SM/corals/coral_countrymap.png",dpi=600) 
##  Fig C5 (end)


## Fig. C6


    ssp_corals_growth <- ssp_corals_growth%>% left_join(regions,by="countrycode")
    market_coefficients_by_country3 <- market_coefficients_by_country3%>% left_join(regions,by="countrycode")
    
    Market_Value <- ggplot(ssp_corals_growth %>% filter(scenario=="SSP2")) + 
        geom_line(aes(x=year,y=Market_Use_Values_Undamaged_percGDP,color=R5,linetype=scenario,group=countrycode)) + 
        geom_text(data= ssp_corals_growth %>% filter(scenario=="SSP2", year==2100), aes(x=year,y=Market_Use_Values_Undamaged_percGDP,color=R5,label=countrycode)) +
        scale_color_manual(values=hex_R5)+
        ggtitle("Corals Market Revenue\n Under SSP2 (No Climate Impacts)") + xlab("Year") + ylab("Market Benefits From Corals (% GDP)") + theme_bw() + guides(color=FALSE, linetype=FALSE)
    
    
    coral_dam_plot2 <- ggplot(market_coefficients_by_country3%>% filter(scenario=="SSP2")) + 
        geom_point(aes(x=temp,y=fraction_damaged*100 ,color=R5)) +
        geom_text_repel(data=market_coefficients_by_country3%>% filter(scenario=="SSP2") %>% filter(year==2100), aes(x=temp+0.02,y=fraction_damaged*100 ,color=countrycode,label=countrycode)) +
        geom_line(aes(x=temp,y=100*FractionChangeGDP_perC*temp ,color=R5,group=countrycode))+
        geom_ribbon(aes(x=temp,ymin=100*(FractionChangeGDP_perC-FractionChangeGDP_perC_se*1.645)*temp, 
        ymax=100*(FractionChangeGDP_perC+FractionChangeGDP_perC_se*1.645)*temp ,fill=R5),alpha=0.2)+        
        theme_bw() + 
        guides(color=FALSE,fill=FALSE) + 
        scale_color_manual(values=hex_R5)+
        scale_fill_manual(values=hex_R5)+
        xlab("Temperature Increase under RCP7") + 
        ylab("Market Damages (% GDP)")
    

    coral_benefits_2100 <- ggplot(ssp_corals_growth %>% filter(scenario=="SSP2")) + 
        geom_hline(aes(yintercept=100),linetype=2)  +
        geom_hline(aes(yintercept=1),linetype=2)  +
        geom_text(aes(x=100,y=1.5,label="1%")) + 
        geom_text(aes(x=100,y=150,label="100%")) + 
        geom_text(data= ssp_corals_growth %>% filter(scenario=="SSP2", year==2100), 
            aes(x=GDP.billion2005USDperYear,y=Market_Use_Values_Undamaged_percGDP,color=R5,label=countrycode)) + 
            scale_y_continuous(trans="log10") + 
            scale_x_continuous(trans="log10") + 
            scale_color_manual(values=hex_R5)+
            theme_bw() +
            guides(color=FALSE) + xlab("GDP in 2100 (2005 Int USD)") + ylab("Market Benefits From Corals (% GDP)") + ggtitle("Market benefits in 2100\n Under SSP2")
    fig_c6 <- ggarrange(ggarrange(Market_Value,coral_benefits_2100),coral_dam_plot2,ncol=1)

    windows()
    print(fig_c6)
    Sys.sleep(0.5)  # Add a short pause to ensure the plot is rendered when running source
    #ggsave("Figures/SM/corals/DamageFunction_linear.png",dpi=600) 

## Fig. C6