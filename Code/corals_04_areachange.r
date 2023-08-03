
        Temp_ssps <- read_excel("Data/Temp_ssps_ndc.xlsx", sheet = "data")
        names(Temp_ssps)[1] <- "year" 

        coral_sf <- st_read("Data/corals/coral_area_with_mean_t2coeff_filtered.shp")
        glimpse(coral_sf)
        iso_distance<-unique(distance_corals$iso)

        ID <- unique(coral_sf$id_1)

        
       years <- 2017 + seq(1:(dim(Temp_ssps)[1]-3))
       temp=Temp_ssps$RCP6[c(4:86)]

        for (j in 1:length(ID)){
                coral_sf_id <- coral_sf[which(coral_sf$id_1==ID[j]),]
                area_change <- data.frame(year=years, temp=temp, area=NA, ID=ID[j])
                area_change$area[1] <- coral_sf_id$area_km2
                    for (n in 2:length(temp)){
                        
                        area_change$area[n] <- area_change$area[n-1]*
                                (100+coral_sf_id$mean_coef*temp[n]^2)/100
                    }
                if(j==1){
                    coral_area_change <- area_change
                }else{
                    coral_area_change <- bind_rows(coral_area_change,area_change)
                }
                print(j)
        }

        #write.csv(coral_area_change,"Data/corals/coral_area_ID.csv")
        coral_area_change <- read.csv("Data/corals/coral_area_ID.csv")
        
        glimpse(coral_area_change)
        coral_area_change <- coral_area_change %>%
            group_by(ID) %>%
            arrange(year) %>%
            mutate(area_change_ratio =  area/ dplyr::lag(area) - 1)

        coral_area_change <- coral_area_change %>%
            filter(year == 2018) %>%
            select(ID, area) %>%
            rename(area_2018 = area) %>%
            inner_join(coral_area_change, by = "ID")
        

        coral_area_change <- coral_area_change %>%
            group_by(ID) %>%
            arrange(year) %>%
            mutate(area_loss_pct = 100 * (area/ area_2018 - 1))


        decades <- 2010+10*seq(1:9)
        ggplot(data=coral_area_change[which(coral_area_change$year %in% decade),],
            aes(x=factor(year),y=area_loss_pct,fill=temp))+
        geom_boxplot() +
        theme_bw()+
        xlab("Year")+
        ylab("Coral cover change (%)")+
        scale_fill_viridis(name = "Temp. [°C]", option = "C",direction=1)+
        ggtitle("Climate impacts on corals (RCP 6)")

        #ggsave("Figures/corals/coral_areachange.png",dpi=600)



        
            oc_ssp<- read.csv("Data/oc_ssp_allcountries.csv")
            Yblue <- read.csv("Data/Yblue_growth_v0_2.csv")
            glimpse(coral_area_change)
            distance_corals <- read.csv("Data/corals/distance_corals.csv")
            glimpse(distance_corals)
            
            distance_corals$distance[which(distance_corals$distance==0)] <- 1
            countries_with_corals <- unique(distance_corals$iso[which(distance_corals$distance==1)])
            unique(distance_corals$iso)

            

            
            coral_area_change_merged <- coral_area_change[which(coral_area_change$year==2100),] %>%
            inner_join(distance_corals, by = c("ID" = "id")) 

            min(coral_area_change_merged$distance)
            coral_area_change_merged$inv_dist <- 1/((coral_area_change_merged$distance/1000))

            countries_with_corals <- unique(coral_area_change_merged$iso[which(coral_area_change_merged$distance==1)])
            '%notin%' <- Negate('%in%')
            ggplot(coral_area_change_merged[coral_area_change_merged$iso %notin% countries_with_corals,],
                aes(x=iso,y=inv_dist)) +
                scale_y_continuous(trans="log")+
                #aes(x=iso,y=distance/1000)) +
                geom_violin()
                #geom_boxplot()

            
            glimpse(coral_area_change_merged)
            
            group_by(iso) %>%
            mutate(inverse_distance = 1 / distance^2,
                    sum_dist = sum(inverse_distance),
                    weighted_avg_area_loss_pct_inverse_distance = area_2018 * inverse_distance) %>%
            summarize(avg_area_loss_pct_2100 = mean(x=area_loss_pct, na.rm = TRUE),
                        w_avg_area_loss_pct_2100 = weighted.mean(x=area_loss_pct, w=1/distance^2, na.rm = TRUE),
                        mean_distance = mean(distance, na.rm = TRUE),
                        min_distance = min(distance, na.rm = TRUE),
                        #area_2018 = area_2018,
                        area_w_dist_m = weighted.mean(x=area_2018, w=1/distance^2, na.rm = TRUE),
                        area_w_dist_s = sum(weighted_avg_area_loss_pct_inverse_distance, na.rm = TRUE)) %>%
            ungroup()


            #glimpse(oc_ssp[which(oc_ssp$t==2020),])
            #glimpse(coral_area_change_aggregated)
            #glimpse(Yblue[which(Yblue$year==2020),])
            #Yblue$GDP[which(Yblue$year==2018)]
            #install.packages("WDI")
            #library(WDI)

            # Download GDP and Population data for 2022 (use latest available year if 2022 data is not available)
            #gdp_pop_data <- WDI(country = "all", indicator = c("NY.GDP.MKTP.CD", "SP.POP.TOTL"), start = 2021, end = 2021)

            # Rename the columns for better readability
            #colnames(gdp_pop_data) <- c("country","iso2", "iso",  "year", "gdp", "pop")
            
            #glimpse(gdp_pop_data)

            coral_area_change_aggregated <- merge(coral_area_change_aggregated,gdp_pop_data,by="iso")
            glimpse(coral_area_change_aggregated)



        #ggplot(coral_area_change_aggregated, aes(x = mean_distance/1000, y = w_avg_area_loss_pct_2100, size = gdp/pop, label = iso)) +
        ggplot(coral_area_change_aggregated, aes(x = area_w_dist_s/1000, y = w_avg_area_loss_pct_2100, size = gdp/pop, label = iso)) +
        #ggplot(coral_area_change_aggregated, 
         #   aes(x = area_w_dist_s, y = w_avg_area_loss_pct_2100, size = (gdp/pop), label = iso)) +
            geom_point(alpha = 0.7, color = "seagreen4") +
            scale_size_continuous(range = c(1, 15)) +
            # labs(title = "Coral Area Loss in 2100",
            #     x = "Mean Distance to Coral Sites (Km)",
            #     y = "Average Coral Area Loss Percentage") +
            #labs(title = "Coral Area Loss in 2100",
            #    x = "Nearby coral cover (km2)",
            #    y = "Average Coral Area Loss Percentage") +
            theme_minimal() +
            scale_x_continuous(trans="log10")+
            #theme(legend.position = "none") +
            geom_text(aes(label = iso), size = 3, hjust = -0.1, vjust = 0.5, check_overlap = TRUE)



