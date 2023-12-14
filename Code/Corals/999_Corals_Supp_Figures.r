## Supplementary Figures Corals Module

## Libararies 

        library(ggplot2)
        library(sf)
        library(viridis)
        library(scico)
        library(rnaturalearth)
        library(scales)

## Load Data (start)
    
    #Fig S1
    load(file="Data/Modules/Corals/corals_tcoeff.Rds") #corals_tcoeff
    load(file="Data/Modules/Corals/corals_temp_unique.Rds") #corals_temp_unique

    #Fig S2
    load(file="Data/Modules/Corals/coral_areas_gulf.Rds") #coral_areas_gulf

    #Fig S3
    load(file="Data/Modules/Corals/coral_areas_keys.Rds")#coral_areas_keys
    load(file="Data/Modules/Corals/coral_areas_keys_single.Rds")#coral_areas_keys_single
    load(file="Data/Modules/Corals/coral_areas_keys_single_joined.Rds")#coral_areas_keys_single_joined
    load(file="Data/Modules/Corals/coral_temp_keys.Rds")#coral_temp_keys
    
    #Fig S4
    load(file="Data/modules/corals/corals_area_coeff_sf.Rds") #corals_area_coeff_sf
    
    
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
        colors <- colorRampPalette(pal1)(length(breaks) - 1)
## Themes (end)

## Fig. S1 (start)
    individual_coral_change <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc*0.01,color=scenario,group=interaction(scenario,uniqueplace)), alpha = 0.05)+
    theme_bw()+
    guides(color="none")+
    scale_color_manual(values=c("#0072B2", "#D55E00"))+
    geom_text(aes(x=1,y=1.5*0.01,label="RCP4.5 - 2050"),color="#0072B2",size=3) +
    geom_text(aes(x=1.5,y=-2*0.01,label="RCP8.5 - 2050"),color="#D55E00",size=3)+
    geom_text(aes(x=2.1,y=-5*0.01,label="RCP4.5 - 2100"),color="#0072B2",size=3)+
    geom_text(aes(x=3.5,y=-12*0.01,label="RCP8.5 - 2100"),color="#D55E00",size=3) +
    xlab("Temperature increase from 1997-2018 baseline")+
    xlim(c(0,4.5))+
    ylab("Coral Cover Change (fraction)")

    legend_plot <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)))+
    theme_bw()+
    guides(color=guide_legend("Scenario"))+
    scale_color_manual(values=c("#0072B2", "#D55E00"))

    leg <- get_legend(legend_plot)

    density_coral_coeff <- ggplot(corals_tcoeff, aes(x = tcoeff*0.01, y = pval)) +
        geom_hex() +
        scale_y_log10() +  # Log scale on the y-axis
        labs(x = "GMST Coefficient (fraction cover per degree C)", y = "P value") +
        scale_fill_scico(palette="berlin")+theme_bw()

    density_coral_coeff_cover <- ggplot(corals_tcoeff, aes(x = tcoeff*0.01, y = living_coral_cover)) +
        geom_hex() +
        labs(x = "GMST Coefficient (Damages per degree C)", y = "Present Fraction of the Reef \nCovered by Living Corals") +
        scale_fill_scico(palette="berlin")+theme_bw()
        density_coral_coeff_cover
    
    ggarrange(ggarrange(individual_coral_change,leg,ncol=2,widths=c(4,1)),density_coral_coeff_cover)
    
    #ggsave("Figures/SM/corals/Coeff_CoralCover_PresentCover.png",dpi=600)


## Fig. S1 (end)

## Fig. S2 (start)
    coral_areas_temp <- ggplot() +
        geom_sf(data = coral_areas_gulf, aes(fill = log10(GIS_AREA_K)), size=5,color = NA)+
        scale_fill_viridis(name = expression(paste("Area (km"^2, ")")), 
            guide = guide_colorbar(nbin = 50),labels = c("1e-6","1e-4","0.01","1","100")) +
        geom_sf(data = coral_temp_gulf, aes(color = tcoeff*0.01), size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(fraction change/C)", guide = "legend") +
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
    
    count_temp <- ggplot(coral_temp_gulf[which(!is.na(coral_temp_gulf$coef_group)),], aes(x = tcoeff*0.01, fill = factor(coef_group))) +
        geom_histogram(bins = 50, color = "black") +
        scale_fill_manual(values = colors(10),guide="none") +
        labs(x = "Cover damage \n(fraction change/C)", y = "Count", fill = "Cover change") +
        theme_minimal()

    ggarrange(coral_areas_temp,ggarrange(count_areas,count_temp,nrow=2,ncol=1,legend="none"),ncol=2,widths=c(2,1))
    #ggsave("Figures/SM/corals/CoralChange_temp_area.png",dpi=600)
## Fig. S2 (end)

## Fig. S3 (start)
    coral_areas_temp_florida <- ggplot() +
        geom_sf(data = coral_areas_keys, aes(fill = (GIS_AREA_K)), color = NA)+
        scale_fill_viridis(name = expression(paste("Area (km"^2, ")")), guide = guide_colorbar(nbin = 50))+#,labels = c("1e-6","1e-4","0.01","1","100")) +
        geom_sf(data = coral_temp_keys, aes(color = tcoeff*0.01), size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(fraction change/C)", guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Coral Area and Cover Change in Florida Keys") +
        coord_sf(xlim = c(-80.5, -80.2),
                ylim = c(24.9, 25.3)) +
        my_theme()

    coral_areas_temp_florida <- ggplot() +
        geom_sf(data = coral_areas_keys_single, aes(fill = (area_km2)), color = NA)+
        scale_fill_viridis(name = expression(paste("Area (km"^2, ")")), guide = guide_colorbar(nbin = 50))+#,labels = c("1e-6","1e-4","0.01","1","100")) +
        geom_sf(data = coral_temp_keys, aes(color = tcoeff*0.01), size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = expression(paste("Cover damage \n(fraction change/C)")), guide = "legend") +
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
        geom_sf(data = coral_areas_keys_single_joined, aes(fill = mean_coef*0.01), color = NA)+
        scale_fill_gradientn(colors = pal1_heat[1:7],name = expression(paste("Cover damage \n(fraction change/C)")), guide = guide_colorbar(nbin = 50))+#,labels = c("1e-6","1e-4","0.01","1","100")) +
        #geom_sf(data = coral_temp_keys,color = "white", size = 2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = expression(paste("Cover damage \n(fraction change/C)")), guide = "legend") +
        geom_sf(data = continents, fill = "white", color = "gray", alpha = 0.5) +
        labs(title = "Mean Damage Coefficient\nwithing Coral Polygons") +
        coord_sf(xlim = c(-80.5, -80.2),
                ylim = c(24.9, 25.3)) +
        my_theme()+
        guides(fill = guide_colorbar(title.position = "top", barwidth =10, title.hjust = 0.5)) + 
        theme(legend.position="bottom")
        
        ggarrange(coral_areas_temp_florida,
            coral_areas_temp_florida2,
            coral_areas_temp_florida3,ncol=3,legend="bottom",align="h")

    #ggsave("Figures/SM/corals/meandamage.png",dpi=600)



## Fig. S3 (end)

## Fig. S4 (start)

        data_robinson <- st_transform(corals_area_coeff_sf, "+proj=robin")
        ggplot(data = data_robinson, aes(fill = mean_coef*0.01)) +
        geom_sf(aes(color=mean_coef*0.01), size=2) +
        scale_color_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(fraction change/C)", guide = "legend") +
        scale_fill_gradientn(colors = pal1_heat[1:7], name = "Cover damage \n(fraction change/C)", 
                            guide = guide_colorbar(nbin = 50, title.position = "top", title.hjust = 0.5,barwidth = 300, barheight = 300)) +
        geom_sf(data = continents, fill = "gray", color = NA, alpha = 0.2) +
        labs(title = "Effect of 1C increase in coral cover") +
        my_theme()+ 
        theme(legend.position="bottom") +
        guides(fill = guide_colorbar(title.position = "top",position = "bottom", title.hjust = 0.5,barwidth = 300, barheight = 300), 
            color = guide_colorbar(position = "none")) + 
    theme(
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"), # Adjust the height of the legend key
        legend.key.width = unit(1.5, "cm")  # Adjust the width of the legend key
    )

        ggsave("Figures/SM/corals/Effect1C_coralcover_size2.png",dpi=600) 

        
## Fig. S4 (end)

##  
    corals_df_iso <- read.csv(file="Data/intermediate_output/corals_area_damage_value.csv")
    glimpse(corals_df_iso)
    
    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, corals_df_iso, by = c("iso_a3" = "countrycode"))

    glimpse(merged_data )

    # Plot
    dam <- ggplot(data = merged_data) +
    geom_sf(aes(fill = DamCoef_changeperC )) +
    scale_fill_scico(palette = "lajolla", oob=squish, na.value=NA,direction=-1) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Cover Cover Change \n(fraction change/C)")
    dam
    ggsave("Figures/SM/corals/coral_countrymap_damage.png",dpi=600) 


    area <- ggplot(data = merged_data) +
    geom_sf(aes(fill = area_km2_t0)) +
    scale_fill_scico(palette = "batlow", trans = "log10", oob = squish, na.value = NA, direction = -1) +
    coord_sf(crs = "+proj=robin") +
    theme_minimal() +
    labs(fill = "Coral Cover (km2)")

    
    ggarrange(area,dam)
    ggsave("Figures/SM/corals/coral_countrymap.png",dpi=600) 


