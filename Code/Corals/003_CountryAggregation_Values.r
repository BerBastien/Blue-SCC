

    load(file="Data/output_modules_input_rice50x/output_modules/corals/corals_area_coeff_sf.Rds") #corals_area_coeff_sf
    load(file="Data/output_modules_input_rice50x/output_modules/corals/coral_country.Rds") 
    
    c_coef <- corals_area_coeff_sf %>% st_drop_geometry()
    c_country <- coral_country  %>% st_drop_geometry()
    c_country <- c_country %>% dplyr::select(id,countrycode) %>% group_by(id) %>% slice(1) %>% ungroup() 
    corals_df <- merge(c_coef,c_country,by="id",all.x=T,all.y=F)
    
   
    #save(corals_df,file="Data/output_modules_input_rice50x/output_modules/corals/corals_df.Rds")
    #write.csv(corals_df,file="Data/output_modules_input_rice50x/output_modules/corals/corals_df.csv")
    
    #corals_df <- read.csv(file="Data/output_modules_input_rice50x/output_modules/corals/corals_df.csv")

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

        corals_df_iso_with_gdp %>% filter(is.na(gdp))
        corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="ERI")] <- 566
        corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="TWN")] <- 28570
        corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="VEN")] <- 1570

        # gamma <- 0.79 #Drupp 2023
        # se_gamma <- 0.09 #Drupp 2023
        corals_df_iso_with_gdp$gamma_um <- 0.129 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_um_se <- 0.062 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_unm <- 0.129 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_unm_se <- 0.222 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_nu <- 0.243 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp$gamma_nu_se <- 0.068 #As given by ESDV Analysis specific to corals, same data as in Brander et al (2024)
        corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% filter(!is.na(countrycode))
        mean_gdp <- mean(corals_df_iso_with_gdp$gdp, na.rm = TRUE)
        corals_df_iso_with_gdp$GDP_asPercentage <- (corals_df_iso_with_gdp$gdp / mean_gdp)
        corals_df_iso_with_gdp$adjustment_factor_um <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* corals_df_iso_with_gdp$gamma_um
        corals_df_iso_with_gdp$adjustment_factor_unm <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* corals_df_iso_with_gdp$gamma_unm
        corals_df_iso_with_gdp$adjustment_factor_nu <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* corals_df_iso_with_gdp$gamma_nu
        #corals_df_iso_with_gdp$adjustment_factor_se <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))* se_gamma



        corals_df_iso_with_gdp$muV_value_perkm2year <- 100*(741+18514+34) * corals_df_iso_with_gdp$adjustment_factor_um #Total Monetary Value of the Bundle of: Food, Raw Materials, Ornamental (Int$/km2/year) in Brander et al (2024)
        corals_df_iso_with_gdp$nuV_value_perkm2year <- 100*(2+14369+4078+3418+1551+5580+6271+917) * corals_df_iso_with_gdp$adjustment_factor_unm#Total Monetary Value of the Bundle of: climate reg, moderation of extreme events, waste tratment, erosion prevention, maintenanco of soil fertility, aesthetic information, recreation, inspiration (Int$/km2/year) in Brander et al (2024)
        corals_df_iso_with_gdp$nV_value_perkm2year <- 100*(1385+9432+18793)* corals_df_iso_with_gdp$adjustment_factor_nu#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)

        corals_df_iso_with_gdp$muV_value_perkm2year_se <-  100*(21783)* corals_df_iso_with_gdp$adjustment_factor_um#Total Monetary Value of the Bundle of Provisioning Ecosystem Services for Coral Reefs (Int$/km2/year)
        corals_df_iso_with_gdp$nuV_value_perkm2year_se <- 100*(51218)  * corals_df_iso_with_gdp$adjustment_factor_unm#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
        corals_df_iso_with_gdp$nV_value_perkm2year_se <- 100*(56536) * corals_df_iso_with_gdp$adjustment_factor_nu#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)

        corals_df_iso_with_gdp <- corals_df_iso_with_gdp  %>% dplyr::select(-sum_area_cover_km2,weighted_mean_coeff,gdp,adjustment_factor)
        glimpse(corals_df_iso_with_gdp)
        corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% rename(area_km2_t0=sum_area_km2, DamCoef_changeperC=weighted_mean_coeff,DamCoef_changeperC_se=weighted_mean_se)
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

    corals_df_iso_with_gdp <- read.csv(file="Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv")
    

    ### Market Damage Function (start)
            
        ssp_corals <- ssp_gdp %>% left_join(corals_df_iso_with_gdp,by="countrycode")
        ssp_corals <- ssp_corals %>% left_join(ssp_pop,by=c("countrycode","scenario","year"))
        ssp_corals <- ssp_corals %>% left_join(ssp_temp_long %>% dplyr::select(-scenario),by="year")
        glimpse(ssp_corals)    
        
        ## Project Market Values of Corals
        ssp_corals_growth <- ssp_corals %>% 
            ## GDP per Capita:
            mutate( GDPpc_2020IntUSD = (def_mult[[1]]* GDP.billion2005USDperYear*10^9) / (Pop.million*10^6)) %>% #Cumulative Inflation from 2005 Int Dollars to 2020 Int Dollars, https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG 
            group_by(scenario,countrycode) %>% 
            ## Calculating GDPpc Growth for WTP Income Elasticity
            mutate(GDP_growth = GDPpc_2020IntUSD / lag(GDPpc_2020IntUSD) - 1) %>% 
            mutate(Adjustment= ifelse(year == 2020, 0, GDP_growth * 0.129)) %>% # From Brander et al 2024
            filter(year >2019)  %>% 
            mutate(Adjustment_cum  = cumsum(Adjustment)) %>% 
            ## Adjusting the Per-Area Values Using GDPpc Growth
            mutate(muV_value_perkm2year_adjusted = muV_value_perkm2year * (1+Adjustment_cum), 
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
        
        
        market_coefficients_by_country3 <- market_coefficients_by_country2 %>% group_by(countrycode) %>% 
            mutate(FractionChangeGDP_perC =mean(lambda), 
            FractionChangeGDP_perC_se_adj = mean(lambda)*DamCoef_changeperC_se/DamCoef_changeperC)
        #  ggplot(validation, aes(x=coef,y=lambda)) + geom_point()       
        #  ggplot(validation, aes(x=lambda,y=lambda_se)) + geom_point()       

        # market_coefficients_by_country3 <- market_coefficients_by_country2 %>% 
        #     mutate(
        #         SE_dam = fraction_damaged * DamCoef_changeperC_se/DamCoef_changeperC) %>% 
        #         mutate(FractionChangeGDP_perC_se_adj = FractionChangeGDP_perC_se * ((variance + SE_dam^2)/(variance))^0.5)
        
        coefs_with_vcov <- market_coefficients_by_country3 %>% ungroup() %>% filter(year==2100,scenario=="SSP1") %>% dplyr::select(countrycode,FractionChangeGDP_perC,FractionChangeGDP_perC_se_adj) %>% as.data.frame()
       
        write.csv(coefs_with_vcov,file="Data\\output_modules_input_rice50x\\input_rice50x\\coral_GDPdam_coefficients.csv")
        write.csv(ssp_corals_growth,file="Data\\output_modules_input_rice50x\\output_modules\\corals\\ssp_corals_growth.csv")
        write.csv(market_coefficients_by_country3,file="Data\\output_modules_input_rice50x\\output_modules\\corals\\market_coefficients_by_country3.csv")
 
    ### Market Damagfe Function (end)
        

