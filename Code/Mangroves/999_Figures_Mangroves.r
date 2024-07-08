

# Figure 3


    mangrove_tcoeff <- read.csv(file="Data/output_modules_input_rice50x/input_rice50x/mangrove_area_coefficients_sq.csv")

    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, mangrove_tcoeff, by = c("iso_a3" = "countrycode"))
    glimpse(merged_data)
    merged_data$Damage_at1C <- merged_data$FractionChange_perC + merged_data$FractionChange_perC_sq
    merged_data$Damage_at1C_distortion <- -merged_data$Damage_at1C*10000
    
    windows()
    # Plot
    ggplot(data = merged_data) +
    geom_sf(aes(fill = 100*(Damage_at1C))) +
    scale_fill_scico(palette = "vik", oob=squish,midpoint=0,limits=c(-2,2), 
                     na.value="transparent",direction=-1) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Mangroves Impact at \n1C Warming (% Cover)")
    
    #ggsave("Figures/SM/mangroves/Map_Mangroves_Coef.png",dpi=600)
    library(cartogram)
    projected_data <- st_transform(merged_data, crs = "+proj=robin")
    projected_data$Damage_at1C_exp <- (1000*projected_data$Damage_at1C)^2
    #summary(projected_data$Damage_at1C_distortion)
    #summary(projected_data$Damage_at1C_exp)
    #cartogram_data <- cartogram_cont(projected_data, weight = "Damage_at1C_exp)")
    

    # Plot the transformed data
    ggplot(data = cartogram_data) +
        geom_sf(aes(fill = 100*(Damage_at1C))) +
        scale_fill_scico(palette = "vik", oob=squish, midpoint=0, limits=c(-2, 2), 
                        na.value="transparent", direction=-1) +
        coord_sf(crs = "+proj=robin") + # Robinson projection
        theme_minimal() +
        labs(fill = "Mangroves Impact at \n1C Warming (% Cover)")



### Market Figure

        weighted_avg_benefits_prov2 <- read.csv(file="Data\\output_modules_input_rice50x\\output_modules\\weighted_avg_benefits_prov2.csv")
        weighted_avg_benefits_prov2 <- weighted_avg_benefits_prov2%>% left_join(regions,by="countrycode")
        
        mangrove_dam_plot <- ggplot(weighted_avg_benefits_prov2 %>% filter(countrycode %in% countries_in_ssps)) + 
            geom_point(aes(x=temp,y=fraction_damaged,color=R5)) +
            #geom_text(data=weighted_avg_benefits_prov2 %>% filter(year==2100), aes(x=temp+0.1,y=percentage_damaged/100,color=countrycode,label=countrycode)) +
            scale_color_manual(values=hex_R5)+
            geom_line(aes(x=temp,y=GDPDam_perC*temp + GDPDam_perC_sq*temp^2,color=R5, group=interaction(countrycode,R5)))+
            geom_ribbon(aes(x=temp,ymin=(GDPDam_perC-GDPDam_perC_se_adj*1.96)*temp + (GDPDam_perC_sq-GDPDam_perC_sq_se_adj*1.96)*temp^2, 
            ymax=(GDPDam_perC+GDPDam_perC_se_adj*1.96)*temp + (GDPDam_perC_sq+GDPDam_perC_sq_se_adj*1.96)*temp^2,fill=R5,group=interaction(countrycode,R5)),alpha=0.2)+        
            theme_bw() + 
            guides(color=FALSE,fill=FALSE) + 
            scale_fill_manual(values=hex_R5)+
            xlab("Temperature Increase under RCP7") + 
            ylab("Market Damages (% GDP)")

       
        market_ben_mangroves <- ggplot(weighted_avg_benefits_prov2 %>% filter(countrycode %in% countries_in_ssps)) + 
            geom_line(aes(x=year,y=100*(100*weighted_avg_benefit_perha) * MangroveArea_2020_km2/GDP_SSP2,color=R5,group=interaction(countrycode,R5))) +
            theme_bw() +
            scale_color_manual(values=hex_R5)+
            guides(color=FALSE) +
            geom_text_repel(data = weighted_avg_benefits_prov2 %>% filter(year==2100),aes(x=year,y=100*(100*weighted_avg_benefit_perha) * MangroveArea_2020_km2/GDP_SSP2,color=countrycode,label=countrycode))+
            xlab("Year") + ylab("Market Benefits From Mangroves (%GDP)")

        
        market_ben_mangroves_2100 <- ggplot(weighted_avg_benefits_prov2 %>% filter(year==2100,countrycode %in% countries_in_ssps)) + 
            geom_text(aes(x=10^11,y=140,label="100%"))+
            geom_text(aes(x=10^11,y=14,label="10%"))+
            geom_text(aes(x=10^11,y=1.2,label="1%"))+
            geom_hline(aes(yintercept=1),linetype=2)+
            geom_hline(aes(yintercept=10),linetype=2)+
            geom_hline(aes(yintercept=100),linetype=2)+
            scale_color_manual(values=hex_R5)+
            geom_text(data = weighted_avg_benefits_prov2 %>% filter(year==2100,countrycode %in% countries_in_ssps),
                aes(x=GDP_SSP2,y=100*(100*weighted_avg_benefit_perha) * MangroveArea_2020_km2/GDP_SSP2,color=R5,label=countrycode))+
            xlab("GDP in 2100 (2020 Int USD)") + ylab("Market Benefits From Mangroves (%GDP)")+
            scale_y_continuous(trans="log10")+
            scale_x_continuous(trans="log10")+
            theme_bw()+
            guides(color=FALSE)


        windows()
        ggarrange( ggarrange(market_ben_mangroves , market_ben_mangroves_2100        ), mangrove_dam_plot,ncol=1)        
        #ggsave("Figures//all_figures//mangroves//Market_Dam.png")