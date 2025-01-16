

    load(file="Data/output_modules_input_rice50x/output_modules/corals/corals_area_coeff_sf.Rds") #corals_area_coeff_sf
    load(file="Data/output_modules_input_rice50x/output_modules/corals/coral_country.Rds") 
    glimpse(corals_area_coeff_sf )
    c_coef <- corals_area_coeff_sf %>% st_drop_geometry()
    c_country <- coral_country  %>% st_drop_geometry()
    c_country <- c_country %>% dplyr::select(id,countrycode) %>% group_by(id) %>% slice(1) %>% ungroup() 
    corals_df <- merge(c_coef,c_country,by="id",all.x=T,all.y=F)
    

        
   
    #save(corals_df,file="Data/output_modules_input_rice50x/output_modules/corals/corals_df.Rds")
    #write.csv(corals_df,file="Data/output_modules_input_rice50x/output_modules/corals/corals_df.csv")
    
       
    ## Get Mean Coefficient by Country
        corals_df_iso <- corals_df %>% #filter(ssp=="SSP1" & year==2020) %>%
                        group_by(countrycode) %>%
                        summarise(sum_area_km2=sum(area_km2),
                        weighted_mean_coeff = sum(mean_coef * area_km2) / sum(area_km2),
                        weighted_mean_se = sum(mean_se * area_km2) / sum(area_km2),
                        sum_area_cover_km2=sum(area_km2*mean_cover),
                        weighted_mean_coeff_cover = sum(mean_coef * area_km2*mean_cover) / sum(area_km2*mean_cover))

    ## Get GDP per Capita and Adjust Values According to income elasticity to WTP (start)
        gdp_data <- WDI(indicator = "NY.GDP.PCAP.KD", start = 2020, end = 2020, extra = TRUE)
        gdp_data_clean <- gdp_data %>%
        dplyr::select(iso3c, NY.GDP.PCAP.KD) %>%
        rename(countrycode = iso3c, gdp = NY.GDP.PCAP.KD)

        corals_df_iso_with_gdp <- merge(corals_df_iso, gdp_data_clean, by = "countrycode", all.x = TRUE)
        glimpse(corals_df_iso_with_gdp)
        corals_df_iso_with_gdp %>% filter(countrycode=="MDV")

        corals_df_iso_with_gdp %>% filter(is.na(gdp))
        corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="ERI")] <- 566
        corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="TWN")] <- 28570
        corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="VEN")] <- 1570

        # gamma <- 0.79 #Drupp 2023
        # se_gamma <- 0.09 #Drupp 2023
        corals_df_iso_with_gdp$gamma_um <- 0.129 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_um_se <- 0.062 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_unm <- 0.222 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_unm_se <- 0.058 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_nu <- 0.243 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_nu_se <- 0.068 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% filter(!is.na(countrycode))
        mean_gdp <- mean(corals_df_iso_with_gdp$gdp, na.rm = TRUE)
        corals_df_iso_with_gdp$GDP_asPercentage <- (corals_df_iso_with_gdp$gdp / mean_gdp)
        corals_df_iso_with_gdp$adjustment_factor_um <-1+((corals_df_iso_with_gdp$gdp / mean_gdp)-1)* corals_df_iso_with_gdp$gamma_um
        corals_df_iso_with_gdp$adjustment_factor_unm <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* corals_df_iso_with_gdp$gamma_unm
        corals_df_iso_with_gdp$adjustment_factor_nu <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* corals_df_iso_with_gdp$gamma_nu
        #corals_df_iso_with_gdp$adjustment_factor_se <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* se_gamma



        corals_df_iso_with_gdp$muV_value_perkm2year <- 100*(741+18514+34) * corals_df_iso_with_gdp$adjustment_factor_um #Total Monetary Value of the Bundle of: Food, Raw Materials, Ornamental (Int$/ha/year) in Brander et al (2024), multiplying by 100 to pass it to (Int$/km/year)
        corals_df_iso_with_gdp$nuV_value_perkm2year <- 100*(2+14369+4078+3418+1551+5580+6271+917) * corals_df_iso_with_gdp$adjustment_factor_unm#Total Monetary Value of the Bundle of: climate reg, moderation of extreme events, waste tratment, erosion prevention, maintenanco of soil fertility, aesthetic information, recreation, inspiration (Int$/km2/year) in Brander et al (2024)
        corals_df_iso_with_gdp$nV_value_perkm2year <- 100*(1385+9432+18793)* corals_df_iso_with_gdp$adjustment_factor_nu#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)

        corals_df_iso_with_gdp$muV_value_perkm2year_se <-  100*(21783)* corals_df_iso_with_gdp$adjustment_factor_um#Total Monetary Value of the Bundle of Provisioning Ecosystem Services for Coral Reefs (Int$/km2/year)
        corals_df_iso_with_gdp$nuV_value_perkm2year_se <- 100*(51218)  * corals_df_iso_with_gdp$adjustment_factor_unm#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
        corals_df_iso_with_gdp$nV_value_perkm2year_se <- 100*(56536) * corals_df_iso_with_gdp$adjustment_factor_nu#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)

        corals_df_iso_with_gdp <- corals_df_iso_with_gdp  %>% dplyr::select(-sum_area_cover_km2,weighted_mean_coeff,gdp,adjustment_factor)
        glimpse(corals_df_iso_with_gdp)
        corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% rename(area_km2_t0=sum_area_cover_km2, DamCoef_changeperC=weighted_mean_coeff,DamCoef_changeperC_se=weighted_mean_se)
        corals_df_iso_with_gdp$DamCoef_changeperC <- corals_df_iso_with_gdp$DamCoef_changeperC*0.01
        corals_df_iso_with_gdp$DamCoef_changeperC_se <- corals_df_iso_with_gdp$DamCoef_changeperC_se*0.01
    

        def_mult <- deflator_data %>% 
        summarize(def_2005_to_2020 =NY.GDP.DEFL.ZS[year==2020]/NY.GDP.DEFL.ZS[year==2005])

        corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% 
        #dplyr::select(-X) %>% 
        rename(CoralArea_2020_km2=area_km2_t0) %>% 
        mutate(units = "Int2020$_perkm2_peryear") 

        corals_df_iso_with_gdp_clean <- corals_df_iso_with_gdp %>% 
            dplyr::select(-weighted_mean_coeff_cover,gdp,GDP_asPercentage,muV_value_perkm2year,nuV_value_perkm2year,nV_value_perkm2year,muV_value_perkm2year_se,nuV_value_perkm2year_se,nV_value_perkm2year_se, 
            gamma_um, gamma_um_se,gamma_unm,gamma_unm_se,gamma_nu,gamma_nu_se)
        #write.csv(corals_df_iso_with_gdp_clean,file="Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv")
        glimpse(corals_df_iso_with_gdp )
    
    ## Get GDP per Capita and Adjust Values According to income elasticity to WTP (end)

    #corals_df_iso_with_gdp <- read.csv(file="Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv")
    

    ### Market Damage Function (start)
            
        ssp_corals <- ssp_gdp %>% left_join(corals_df_iso_with_gdp,by="countrycode")
        ssp_corals <- ssp_corals %>% left_join(ssp_pop,by=c("countrycode","scenario","year"))
        ssp_corals <- ssp_corals %>% left_join(ssp_temp_long %>% dplyr::select(-scenario),by="year")
        glimpse(ssp_corals)    
        glimpse(ssp_temp_long)
        levels(factor(ssp_temp_long$scenario))
        
        ## Project Market Values of Corals
        ssp_corals_growth <- ssp_corals %>% 
            ## GDP per Capita:
            mutate( GDPpc_2020IntUSD = (def_mult[[1]]* GDP.billion2005USDperYear*10^9) / (Pop.million*10^6)) %>% #Cumulative Inflation from 2005 Int Dollars to 2020 Int Dollars, https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG 
            group_by(scenario,countrycode) %>% 
            ## Calculating GDPpc Growth for WTP Income Elasticity
            mutate(GDP_growth = GDPpc_2020IntUSD / lag(GDPpc_2020IntUSD) - 1) %>% 
            mutate(Adjustment= ifelse(year == 2020, 0, GDP_growth * 0.129)) %>% # From Brander et al 2024, S.E. = 0.062
            filter(year >2019)  %>% 
            mutate(Adjustment_cum  = cumsum(Adjustment)) %>% 
            ## Adjusting the Per-Area Values Using GDPpc Growth
            mutate(muV_value_perkm2year_adjusted = muV_value_perkm2year* (1+Adjustment_cum), 
            nuV_value_perkm2year_adjusted = nuV_value_perkm2year * (1+Adjustment_cum), 
            nV_value_perkm2year_adjusted = nV_value_perkm2year * (1+Adjustment_cum), 
            area = CoralArea_2020_km2 *(1+DamCoef_changeperC*temp) ) %>% 
            mutate(area = ifelse(area>0,area,0)) %>%
            ## Calculate future market values
            mutate(Market_Use_Values_Undamaged = muV_value_perkm2year_adjusted * CoralArea_2020_km2, 
            Market_Use_Values_Undamaged_percGDP = 100 * muV_value_perkm2year_adjusted * CoralArea_2020_km2 / (def_mult[[1]]*  GDP.billion2005USDperYear*10^9), 
            Market_Use_Values_Damaged_percGDP = 100 * muV_value_perkm2year_adjusted * (area ) / (def_mult[[1]]* GDP.billion2005USDperYear*10^9),
            nuV_Undamaged_percGDP = 100 * nuV_value_perkm2year_adjusted * CoralArea_2020_km2 / (def_mult[[1]]* GDP.billion2005USDperYear*10^9), 
            nV_Undamaged_percGDP = 100 * nV_value_perkm2year_adjusted * CoralArea_2020_km2 / (def_mult[[1]]* GDP.billion2005USDperYear*10^9) ) %>% 
            # Calculte Damages
            mutate(Damages_percGDP = Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP, 
            fraction_damaged = (Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP)/100, 
            lambda =  muV_value_perkm2year_adjusted * CoralArea_2020_km2 *DamCoef_changeperC*temp/ (def_mult[[1]]* GDP.billion2005USDperYear*10^9))
        glimpse(ssp_corals_growth)
        ggplot(ssp_corals_growth %>% filter(countrycode=="MDV"))+geom_line(aes(x=temp, y=area))

        market_coefficients_by_country <- ssp_corals_growth %>% filter(!is.na(fraction_damaged)) %>%
                group_by(countrycode) %>%
                do({
                    model_market_loss <- lm(fraction_damaged  ~ 0 + temp , data = .)
                    data.frame(FractionChangeGDP_perC = coef(model_market_loss)["temp"],
                    FractionChangeGDP_perC_se = summary(model_market_loss)$coefficients[2],
                    variance = summary(model_market_loss)$sigma^2,
                    r_sq = summary(model_market_loss)$r.squared
                    )
                }) %>% ungroup()
        
        market_coefficients_by_country2 <- ssp_corals_growth %>% left_join(market_coefficients_by_country,by="countrycode")
        # glimpse(market_coefficients_by_country2)
        
        ssp_corals_growth <- ssp_corals %>%
            ## GDP per Capita:
            mutate(
                GDPpc_2020IntUSD = (def_mult[[1]] * GDP.billion2005USDperYear * 10^9) / (Pop.million * 10^6)
            ) %>% # Cumulative Inflation from 2005 Int Dollars to 2020 Int Dollars, https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG
            group_by(scenario, countrycode) %>%
            ## Calculating GDPpc Growth for WTP Income Elasticity
            mutate(
                GDP_growth = GDPpc_2020IntUSD / lag(GDPpc_2020IntUSD) - 1,
                Adjustment = ifelse(year == 2020, 0, GDP_growth * 0.129),
                Adjustment_se = ifelse(year == 2020, 0, GDP_growth * 0.062) # SE of Brander 0.062
            ) %>% 
            filter(year > 2019) %>%
            mutate(
                Adjustment_cum = cumsum(Adjustment),
                Adjustment_cum_se = sqrt(cumsum(Adjustment_se^2)) # Propagating cumulative SE
            ) %>%
            ## Adjusting the Per-Area Values Using GDPpc Growth
            mutate(
                muV_value_perkm2year_adjusted = muV_value_perkm2year * (1 + Adjustment_cum),
                muV_value_perkm2year_adjusted_se = sqrt( (muV_value_perkm2year_se* (1+Adjustment_cum))^2 +  (muV_value_perkm2year * Adjustment_cum_se)^2),
                nuV_value_perkm2year_adjusted = nuV_value_perkm2year * (1 + Adjustment_cum),
                nV_value_perkm2year_adjusted = nV_value_perkm2year * (1 + Adjustment_cum),
                area = CoralArea_2020_km2 * (1 + DamCoef_changeperC * temp)
            ) %>% 
            mutate(area = ifelse(area > 0, area, 0)) %>%
            ## Calculate future market values
            mutate(
                Market_Use_Values_Undamaged = muV_value_perkm2year_adjusted * CoralArea_2020_km2,
                Market_Use_Values_Undamaged_se = muV_value_perkm2year_adjusted_se * CoralArea_2020_km2, # SE for market values
                Market_Use_Values_Undamaged_percGDP = 100 * muV_value_perkm2year_adjusted * CoralArea_2020_km2 / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9),
                Market_Use_Values_Undamaged_percGDP_se = 100 * Market_Use_Values_Undamaged_se / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9), # SE for percentage GDP
                Market_Use_Values_Damaged_percGDP = 100 * muV_value_perkm2year_adjusted * (area) / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9),
                nuV_Undamaged_percGDP = 100 * nuV_value_perkm2year_adjusted * CoralArea_2020_km2 / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9),
                nV_Undamaged_percGDP = 100 * nV_value_perkm2year_adjusted * CoralArea_2020_km2 / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9)
            ) %>%
            # Calculate Damages
            mutate(
                Damages_percGDP = Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP,
                fraction_damaged = (Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP) / 100,
                lambda = muV_value_perkm2year_adjusted * CoralArea_2020_km2 * DamCoef_changeperC * temp / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9),
                lambda_se = lambda * sqrt(
                (muV_value_perkm2year_adjusted_se * CoralArea_2020_km2 * DamCoef_changeperC * temp / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9))^2 +
                (muV_value_perkm2year_adjusted * CoralArea_2020_km2 * DamCoef_changeperC_se * temp / (def_mult[[1]] * GDP.billion2005USDperYear * 10^9))^2
                ) # Propagating SE for lambda
            )

        #market_coefficients_by_country2 <- ssp_corals_growth %>% left_join(market_coefficients_by_country,by="countrycode")
            market_coefficients_by_country3 <- ssp_corals_growth %>%
            group_by(countrycode) %>%
            mutate(
                FractionChangeGDP_perC = mean(lambda),
                FractionChangeGDP_perC_se_adj =sqrt(
                    (var(lambda) / n()) + mean(lambda_se^2)
                    ) 
            )
        
        glimpse(market_coefficients_by_country3)


        ggplot(market_coefficients_by_country3%>% filter(scenario=="SSP2")) + 
        geom_point(aes(x=temp,y=fraction_damaged*100)) +
        geom_text_repel(data=market_coefficients_by_country3%>% filter(scenario=="SSP2") %>% filter(year==2100), aes(x=temp+0.02,y=fraction_damaged*100 ,color=countrycode,label=countrycode)) +
        geom_line(aes(x=temp,y=100*FractionChangeGDP_perC*temp,group=countrycode))+
        geom_ribbon(aes(x=temp,ymin=100*(FractionChangeGDP_perC+FractionChangeGDP_perC_se_adj *1.946)*temp, 
        ymax=100*(FractionChangeGDP_perC-FractionChangeGDP_perC_se_adj *1.946)*temp, group=countrycode), fill="red")+        
        theme_bw() + 
        guides(color=FALSE,fill=FALSE) + 
        #scale_color_manual(values=hex_R5)+
        #scale_fill_manual(values=hex_R5)+
        xlab("Temperature Increase under RCP7") + 
        ylab("Market Damages (% GDP)")


        # market_coefficients_by_country3 <- market_coefficients_by_country2 %>% 
        #     mutate(
        #         SE_dam = fraction_damaged * DamCoef_changeperC_se/DamCoef_changeperC) %>% 
        #         mutate(FractionChangeGDP_perC_se_adj = FractionChangeGDP_perC_se * ((variance + SE_dam^2)/(variance))^0.5)
        
        # coefs_with_vcov <- market_coefficients_by_country3 %>% ungroup() %>% filter(year==2100,scenario=="SSP1") %>% dplyr::select(countrycode,FractionChangeGDP_perC,FractionChangeGDP_perC_se_adj) %>% as.data.frame()
        # glimpse(coefs_with_vcov)
        # coefs_with_vcov %>% filter(countrycode=="MDV")
       
        # write.csv(coefs_with_vcov,file="Data\\output_modules_input_rice50x\\input_rice50x\\coral_GDPdam_coefficients.csv")
        # write.csv(ssp_corals_growth,file="Data\\output_modules_input_rice50x\\output_modules\\corals\\ssp_corals_growth.csv")
        # write.csv(market_coefficients_by_country3,file="Data\\output_modules_input_rice50x\\output_modules\\corals\\market_coefficients_by_country3.csv")
        ED_Table1_coralscoefs <- market_coefficients_by_country3 %>% filter(year==2020) %>% select(countrycode,CoralArea_2020_km2,DamCoef_changeperC,DamCoef_changeperC_se)
        #write.csv(ED_Table1_coralscoefs,file="ExtendedData\\ED_Table1_coralscoefs.csv")
        
    ### Market Damagfe Function (end)


     ## Maldives
            glimpse(corals_temp_unique_sf_wgs84)
            temp_cover_maldives <- corals_temp_unique_sf_wgs84 %>% filter(uniqueplace %in% unique_place_maldives)
            new_rows <- temp_cover_maldives %>%
                group_by(uniqueplace) %>%
                summarize(
                scenario = first(scenario), # Retain any scenario value, if consistent
                tdif = 0,
                year = NA, # No specific year for this artificial point, adjust as needed
                cover_change_perc = 0,
                cover = first(cover), # Retain the current cover value
                geometry = first(geometry) # Retain the current geometry
                )

                # Add the new rows to the original dataframe
                temp_cover_maldives_updated <- bind_rows(temp_cover_maldives, new_rows)
            glimpse(temp_cover_maldives_updated)
            ggplot(temp_cover_maldives_updated) + geom_line(aes(x=tdif,y=cover*(1+cover_change_perc/100),group=uniqueplace)) + geom_point(aes(x=tdif,y=cover*(1+cover_change_perc/100),color=scenario))
            load("Data/output_modules_input_rice50x/output_modules/corals/coral_areas_maldives_single_joined.Rds")
            glimpse(coral_areas_maldives_single)
            glimpse(coral_areas_maldives_single_joined)
            ggplot(coral_areas_maldives_single_joined, aes(x=area_km2*cover,y=mean_coef))+geom_point()        
            ggplot(coral_areas_maldives_single_joined, aes(x=area_km2,y=mean_coef))+geom_point()+theme_minimal()+labs(x="Coral reef area (km2)",y="Cover Loss per Degree Celsius (%)")
            #ggsave("Figures/SM/corals/maldives_coef_area.png",dpi=600)
            ggplot(coral_areas_maldives_single_joined, aes(x=area_km2,y=cover*100))+geom_point()+theme_minimal()+labs(x="Coral reef area (km2)",y="Cover of living coral (%)")
            # ggsave("Figures/SM/corals/maldives_cover_area.png",dpi=600)

            gdp_data_clean %>% filter(countrycode=="MDV")
            corals_df_iso_with_gdp %>% filter(countrycode=="MDV")
            ssp_corals_growth %>% filter(countrycode=="MDV") %>% dplyr::select(scenario,year,CoralArea_2020_km2,DamCoef_changeperC,sum_area_cover_km2,gamma_um,GDP.billion2005USDperYear,Pop.million)
            temp_mdv <- ssp_corals_growth %>% filter(countrycode=="MDV") %>% dplyr::select(scenario,year,GDP.billion2005USDperYear,Pop.million,muV_value_perkm2year_adjusted,temp) %>% ungroup()
            coral_areas_maldives_single_joined$year <- 2020
            glimpse(coral_areas_maldives_single_joined)
            glimpse(temp_mdv)

            unique_ids <- coral_areas_maldives_single_joined %>%
                select(ID) %>%
                distinct()

                # Repeat the second dataframe for each ID
                temp_mdv_expanded <- temp_mdv %>%
                crossing(unique_ids)  # Cross-join to create all combinations of ID and temp_mdv rows

                glimpse(temp_mdv_expanded )

                # Merge the datasets by ID and year
                series_mdv <- temp_mdv_expanded %>%
                    left_join(coral_areas_maldives_single_joined, by = c("ID", "year"))%>%
                    group_by(ID) %>% 
                    mutate(mean_coef=mean_coef[year==2020][1], 
                    area_km2 = area_km2[year==2020][1], cover = cover[year==2020][1]) %>% 
                    mutate(damage = mean_coef * temp)    %>% ungroup()

                # Inspect the merged result
                glimpse(merged_data)
                glimpse(series_mdv)
            cool_colors <- scale_color_manual(
            values = c("SSP1" = "#1f78b4", "SSP2" = "#33a02c", "SSP3" = "#6a3d9a", "SSP4" = "#e31a1c", "SSP5" = "#ff7f00")
            )

            ggplot(series_mdv%>% filter(scenario=="SSP1")) + geom_line(aes(y=(1+damage/100)*area_km2*cover,x=temp,group=interaction(scenario,ID)))+theme_minimal()+labs(x="Temperature change from 2020 (RCP4.5)",y="Living Coral Area (km2)")
            #ggsave("Figures/SM/corals/maldives_cover_decrease.png",dpi=600)
            ggplot(series_mdv) + geom_line(aes(y=muV_value_perkm2year_adjusted,x=(1+damage/100)*area_km2*cover,color=scenario,group = interaction(ID, scenario)), alpha=0.7) + labs(x="Living coral area under RCP 4.5",y="Marginal Market Value ($/km2)")+theme_minimal()+cool_colors
            #ggsave("Figures/SM/corals/maldives_market_marginal_value.png",dpi=600)
            
            # Plot the data
            ggplot(series_mdv, aes(
            x = (1+damage/100)*area_km2*cover,
            y = 100*muV_value_perkm2year_adjusted*(1+damage/100)*area_km2*cover/(GDP.billion2005USDperYear*10^9), # Adjust y-axis variable
            color = scenario, # Scenarios like SSP1, SSP2, etc.
            group = interaction(ID, scenario) # Grouping by ID and scenario
            )) +
            geom_line(size = 1, alpha=0.8) + # Line for each ID and scenario
             geom_point(aes(shape = as.factor(year)), 
             data = subset(series_mdv, year %in% c(2020, 2100)), 
             size = 1.5) + # Markers for 2020 and 2100
            cool_colors + # Apply custom color palette
            scale_shape_manual(
                values = c("2020" = 17, "2100" = 16), # Triangle for 2020, Circle for 2100
                name = "Year" # Legend title for shapes
            ) +
              labs(
                title = "Corals reefs in Maldives",
                x = "Percentage of reef covered by living coral",
                y = "Market Benefits from Corals (% GDP)"
            ) +
            theme_minimal() +
            theme(legend.position = "right")
            ggsave("Figures/SM/corals/maldives_market_total_value_percentGDP.png",dpi=600)

            ggplot(series_mdv, aes(
            x = (1+damage/100)*area_km2*cover,
            y = 100*muV_value_perkm2year_adjusted*(1+damage/100)*area_km2*cover, # Adjust y-axis variable
            color = scenario, # Scenarios like SSP1, SSP2, etc.
            group = interaction(ID, scenario) # Grouping by ID and scenario
            )) +
            geom_line(size = 1, alpha=0.8) + # Line for each ID and scenario
             geom_point(aes(shape = as.factor(year)), 
             data = subset(series_mdv, year %in% c(2020, 2100)), 
             size = 1.5) + # Markers for 2020 and 2100
            cool_colors + # Apply custom color palette
            scale_shape_manual(
                values = c("2020" = 17, "2100" = 16), # Triangle for 2020, Circle for 2100
                name = "Year" # Legend title for shapes
            ) +
              labs(
                title = "Corals reefs in Maldives",
                x = "Percentage of reef covered by living coral",
                y = "Market Benefits from Corals ($)"
            ) +
            theme_minimal() +
            theme(legend.position = "right")
            ggsave("Figures/SM/corals/maldives_market_total_value_usd.png",dpi=600)
            #ggplot(coral_areas_maldives_single_joined, aes(x=area_km2*cover,y=mean_coef))+geom_point()  

            series_mdv_total <- series_mdv %>% group_by(scenario,year) %>% 
                summarize(GDP.billion2005USDperYear = mean(GDP.billion2005USDperYear),
                area_km2 = sum(area_km2), 
                cover = mean(cover), 
                muV_value_perkm2year_adjusted = mean(muV_value_perkm2year_adjusted), 
                damage = mean(damage)) %>% 
                mutate(livingcoral = area_km2*cover) %>% as.data.frame()
            
            total_mkt_maldives <- ggplot(series_mdv_total, aes(
            x = (1+damage/100)*area_km2,
            y = muV_value_perkm2year_adjusted*(1+damage/100)*area_km2*cover, # Adjust y-axis variable
            color = scenario, # Scenarios like SSP1, SSP2, etc.
            group = interaction(scenario) # Grouping by ID and scenario
            )) +
            geom_line(size = 1, alpha=0.8) + # Line for each ID and scenario
             geom_point(aes(shape = as.factor(year)), 
             data = subset(series_mdv_total, year %in% c(2020, 2100)), 
             size = 1.5) + # Markers for 2020 and 2100
            cool_colors + # Apply custom color palette
            scale_shape_manual(
                values = c("2020" = 17, "2100" = 16), # Triangle for 2020, Circle for 2100
                name = "Year" # Legend title for shapes
            ) +
              labs(
                title = "Corals reefs in Maldives",
                x = "Living coral (km2)",
                y = "Market Benefits from Corals ($)"
            ) +
            theme_minimal() +
            theme(legend.position = "right")

            total_mkt_maldives_gdp <- ggplot(series_mdv_total, aes(
            x = (1+damage/100)*area_km2*cover,
            y = 100*muV_value_perkm2year_adjusted*(1+damage/100)*area_km2/(GDP.billion2005USDperYear*10^9), # Adjust y-axis variable
            color = scenario, # Scenarios like SSP1, SSP2, etc.
            group = interaction(scenario) # Grouping by ID and scenario
            )) +
            geom_line(size = 1, alpha=0.8) + # Line for each ID and scenario
             geom_point(aes(shape = as.factor(year)), 
             data = subset(series_mdv_total, year %in% c(2020, 2100)), 
             size = 1.5) + # Markers for 2020 and 2100
            cool_colors + # Apply custom color palette
            scale_shape_manual(
                values = c("2020" = 17, "2100" = 16), # Triangle for 2020, Circle for 2100
                name = "Year" # Legend title for shapes
            ) +
              labs(
                title = "Corals reefs in Maldives",
                x = "Living coral (km2)",
                y = "Market Benefits from Corals (%GDP)"
            ) +
            theme_minimal() +
            theme(legend.position = "right")

            ggarrange(total_mkt_maldives,total_mkt_maldives_gdp)
            ggsave("Figures/SM/corals/TOTAL_maldives_market_total_value_usd.png",dpi=600)
            
            series_mdv %>% filter(year==2021) %>% select(mean_coef)
            #corals_df <- read.csv(file="Data/output_modules_input_rice50x/output_modules/corals/corals_df.csv")
                #glimpse(corals_df)
            #corals_df %>% filter(countrycode=="MDV") %>% summarize(area=sum(area_km2,na.rm=T))
        ## Maldives
    
        

