#setup 

    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales','ggrepel')
    lapply(x, require, character.only = TRUE)
    
    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\'
    
#setup 


    load(file="Data/modules/corals/corals_area_coeff_sf.Rds") #corals_area_coeff_sf
    load(file="Data/modules/corals/floodedArea_corals.Rds") 
    load(file="Data/modules/corals/corals_pop_ssp1_5.Rds") 
    load(file="Data/modules/corals/coral_country.Rds") 
    glimpse(corals_area_coeff_sf)
    glimpse(floodedArea_corals)
    glimpse(corals_pop_ssp)

    c_coef <- corals_area_coeff_sf %>% st_drop_geometry()
    c_pop <- corals_pop_ssp %>% st_drop_geometry()
    c_flood <- floodedArea_corals %>% ungroup() %>% st_drop_geometry() %>% st_drop_geometry() %>% dplyr::select(-geometry)
    c_country <- coral_country  %>% st_drop_geometry()
    c_country <- c_country %>% dplyr::select(id,countrycode) %>% group_by(id) %>% slice(1) %>% ungroup() 


    glimpse(c_country)
    corals_df <- merge(c_pop,c_coef,by="id",all=T)
    corals_df <- merge(corals_df,c_flood,by="id",all=T)
    corals_df <- merge(corals_df,c_country,by="id",all.x=T,all.y=F)
    glimpse(corals_df)

    #save(corals_df,file="Data/intermediate_output/corals_df.Rds")
    #write.csv(corals_df,file="Data/intermediate_output/corals_df.csv")
    
    #corals_df <- read.csv(file="Data/intermediate_output/corals_df.csv")

    corals_df_iso <- corals_df %>% filter(ssp=="SSP1" & year==2020) %>%
                    group_by(countrycode) %>%
                    summarise(sum_area_km2=sum(area_km2),
                    weighted_mean_coeff = sum(mean_coef * area_km2) / sum(area_km2),
                    weighted_mean_se = sum(mean_se * area_km2) / sum(area_km2),
                    sum_area_cover_km2=sum(area_km2*mean_cover),
                    weighted_mean_coeff_cover = sum(mean_coef * area_km2*mean_cover) / sum(area_km2*mean_cover))

    glimpse(corals_df_iso)
    library(WDI)
    library(dplyr)
    gdp_data <- WDI(indicator = "NY.GDP.PCAP.KD", start = 2020, end = 2020, extra = TRUE)
    glimpse(gdp_data)
    gdp_data_clean <- gdp_data %>%
    dplyr::select(iso3c, NY.GDP.PCAP.KD) %>%
    rename(countrycode = iso3c, gdp = NY.GDP.PCAP.KD)

    corals_df_iso_with_gdp <- merge(corals_df_iso, gdp_data_clean, by = "countrycode", all.x = TRUE)
    glimpse(corals_df_iso_with_gdp)

    corals_df_iso_with_gdp %>% filter(is.na(gdp))
    corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="ERI")] <- 566
    corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="TWN")] <- 28.57 *1000
    corals_df_iso_with_gdp$gdp[which(corals_df_iso_with_gdp$countrycode=="VEN")] <- 1570

    
    corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% filter(!is.na(countrycode))
    mean_gdp <- mean(corals_df_iso_with_gdp$gdp, na.rm = TRUE)
    corals_df_iso_with_gdp$GDP_asPercentage <- (corals_df_iso_with_gdp$gdp / mean_gdp)
    corals_df_iso_with_gdp$adjustment_factor <- ((corals_df_iso_with_gdp$gdp) / (mean_gdp))*0.37

    
    corals_df_iso_with_gdp$muV_value_perkm2year <- (55724-33048)*100 * corals_df_iso_with_gdp$adjustment_factor#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    corals_df_iso_with_gdp$nuV_value_perkm2year <- 268935*100 * corals_df_iso_with_gdp$adjustment_factor#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    #corals_df_iso_with_gdp$habitat_value_perkm2year <- 16210*100 * corals_df_iso_with_gdp$adjustment_factor#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    #corals_df_iso_with_gdp$cultural_value_perkm2year <- 108837*100 * corals_df_iso_with_gdp$adjustment_factor#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)
    corals_df_iso_with_gdp$nV_value_perkm2year <- (27600*100 )* corals_df_iso_with_gdp$adjustment_factor#Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/km2/year)

    glimpse(corals_df_iso_with_gdp)
    ggplot(data = corals_df_iso_with_gdp, aes(x=log(gdp),y=(muV_value_perkm2year*sum_area_km2)))+geom_text(aes(label=countrycode))
    
    #save(corals_df,file="Data/intermediate_output/corals_df.Rds")
    #write.csv(corals_df,file="Data/intermediate_output/corals_df.csv")

    corals_df_iso_with_gdp <- corals_df_iso_with_gdp  %>% dplyr::select(-sum_area_cover_km2,weighted_mean_coeff,gdp,adjustment_factor)
    glimpse(corals_df_iso_with_gdp)
    corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% rename(area_km2_t0=sum_area_km2, DamCoef_changeperC=weighted_mean_coeff,DamCoef_changeperC_se=weighted_mean_se)
    corals_df_iso_with_gdp$DamCoef_changeperC <- corals_df_iso_with_gdp$DamCoef_changeperC*0.01
    corals_df_iso_with_gdp$DamCoef_changeperC_se <- corals_df_iso_with_gdp$DamCoef_changeperC_se*0.01
    write.csv(corals_df_iso_with_gdp,file="Data/intermediate_output/corals_area_damage_value_v4.csv")
    corals_df_iso_with_gdp <- read.csv("Data/intermediate_output/corals_area_damage_value_v4.csv")
    
    corals_df_iso_with_gdp <- corals_df_iso_with_gdp %>% 
    dplyr::select(-X) %>% 
    rename(CoralArea_2020_km2=area_km2_t0) %>% 
    mutate(muV_value_perkm2year = muV_value_perkm2year*1.478,
    nuV_value_perkm2year = nuV_value_perkm2year*1.478,
    nV_value_perkm2year = nV_value_perkm2year*1.478, 
    units = "Int2020$_perkm2_peryear") #Cumulative Inflation from 2005 Int Dollars to 2020 Int Dollars, https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG 
    glimpse(corals_df_iso_with_gdp)
    write.csv(corals_df_iso_with_gdp,file="Data/intermediate_output/corals_area_damage_value_v4.csv")
    corals_df_iso_with_gdp <- read.csv(file="Data/intermediate_output/corals_area_damage_value_v4.csv")
    glimpse(corals_df_iso_with_gdp)


    ### Market Damagfe Fucntion
        
    library(stringr)
    ssp_gdp <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv')
    ssp_pop <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_pop.csv')
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP245_magicc_202303021423.csv")
    glimpse(ssp_temp)
    ssp_temp %>% dplyr::select(scenario)
    ssp_gdp$countrycode <- ssp_gdp$ISO3
    ssp_pop$countrycode <- ssp_pop$ISO3
    ssp_temp_long <- ssp_temp %>%
        tidyr::pivot_longer(
            cols = starts_with("X"),
            names_to = "year",
            values_to = "value"
        ) %>%
        mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")



    ssp_temp_long$temp2020 <- ssp_temp_long %>% filter(year==2020) %>% dplyr::select(value) %>% unlist()
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2020

    glimpse(ssp_gdp)
    glimpse(corals_df_iso_with_gdp)
    glimpse(ssp_pop)
    glimpse(ssp_temp_long)
    ssp_corals <- ssp_gdp %>% left_join(corals_df_iso_with_gdp,by="countrycode")
    glimpse(ssp_corals)
    ssp_corals <- ssp_corals %>% left_join(ssp_pop,by=c("countrycode","scenario","year"))
    ssp_corals <- ssp_corals %>% left_join(ssp_temp_long %>% dplyr::select(-scenario),by="year")

    glimpse(ssp_corals)
    ssp_corals %>% filter(ISO3.x=="MEX",scenario=="SSP1",year==2010)
    
    ssp_corals_growth <- ssp_corals %>% 
    ## GDP per Capita:
    mutate( GDPpc_2020IntUSD = (1.478 * GDP.billion2005USDperYear*10^9) / (Pop.million*10^6)) %>% #Cumulative Inflation from 2005 Int Dollars to 2020 Int Dollars, https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG 
    group_by(scenario,countrycode) %>% 
    ## Calculating GDPpc Growth for WTP Income Elasticity
    mutate(GDP_growth = GDPpc_2020IntUSD / lag(GDPpc_2020IntUSD) - 1) %>% 
    mutate(Adjustment= ifelse(year == 2020, 0, GDP_growth * 0.79)) %>% # From Drupp 2024 
    filter(year >2019)  %>% 
    mutate(Adjustment_cum  = cumsum(Adjustment)) %>% 
    ## Adjusting the Per-Area Values Using GDPpc Growth
    mutate(muV_value_perkm2year_adjusted = muV_value_perkm2year * (1+Adjustment_cum), 
    nuV_value_perkm2year_adjusted = nuV_value_perkm2year * (1+Adjustment_cum), 
    nV_value_perkm2year_adjusted = nV_value_perkm2year * (1+Adjustment_cum), 
    area = CoralArea_2020_km2 *(1+DamCoef_changeperC*temp) ) %>% 
    mutate(area = ifelse(area>0,area,0)) %>%
    mutate(Market_Use_Values_Undamaged = muV_value_perkm2year_adjusted * CoralArea_2020_km2, 
    Market_Use_Values_Undamaged_percGDP = 100 * muV_value_perkm2year_adjusted * CoralArea_2020_km2 / (1.478 * GDP.billion2005USDperYear*10^9), 
     Market_Use_Values_Damaged_percGDP = 100 * muV_value_perkm2year_adjusted * (area ) / (1.478 * GDP.billion2005USDperYear*10^9)) %>% 
    mutate(Damages_percGDP = Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP, 
    fraction_damaged = (Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP)/100)

    ssp_corals_growth %>% filter(year==2100,countrycode=="MDV") %>% as.data.frame()


    glimpse(ssp_corals_growth)    


    ## Simulation
        library(dplyr)
        library(purrr)

        # Assuming ssp_corals is your initial dataset and is already loaded

        # Function to simulate `area` and calculate `fraction_damaged`
        simulate_data <- function(df) {
        simulations <- map_dfr(seq_len(10), ~ {
            # Simulate `area` by perturbing `DamCoef_changeperC` within its standard error
            simulated_DamCoef_changeperC <- rnorm(n = 1, mean = df$DamCoef_changeperC, sd = df$DamCoef_changeperC_se)
            simulated_area <- df$CoralArea_2020_km2 * (1 + simulated_DamCoef_changeperC * df$temp)
            simulated_area <- max(simulated_area, 0)  # Ensure area is not negative

            # Calculate `fraction_damaged`
            Market_Use_Values_Undamaged <- df$muV_value_perkm2year_adjusted * df$CoralArea_2020_km2
            Market_Use_Values_Undamaged_percGDP <- 100 * Market_Use_Values_Undamaged / df$GDP_adjusted
            Market_Use_Values_Damaged <- df$muV_value_perkm2year_adjusted * simulated_area
            Market_Use_Values_Damaged_percGDP <- 100 * Market_Use_Values_Damaged / df$GDP_adjusted
            Damages_percGDP <- Market_Use_Values_Damaged_percGDP - Market_Use_Values_Undamaged_percGDP
            fraction_damaged <- Damages_percGDP / 100
            
            # Return a dataframe for this simulation
            tibble(simulation_id = .x,
                area = simulated_area,
                fraction_damaged = fraction_damaged)
        })
        
        return(simulations)
        }

        # Enhanced dataset generation
        glimpse(ssp_corals)
        ssp_corals_growth <- ssp_corals %>%
        mutate( GDPpc_2020IntUSD = (1.478 * GDP.billion2005USDperYear*10^9) / (Pop.million*10^6)) %>% #Cumulative Inflation from 2005 Int Dollars to 2020 Int Dollars, https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG 
        mutate(GDP_adjusted = 1.478 * GDP.billion2005USDperYear * 10^9 / (Pop.million * 10^6)) %>%
        group_by(scenario, countrycode) %>%
        mutate(GDP_growth = GDPpc_2020IntUSD / lag(GDPpc_2020IntUSD, default = first(GDPpc_2020IntUSD)) - 1,
                Adjustment = ifelse(year == 2020, 0, GDP_growth * 0.79),
                Adjustment_cum = cumsum(Adjustment),
                muV_value_perkm2year_adjusted = muV_value_perkm2year * (1 + Adjustment_cum),
                nuV_value_perkm2year_adjusted = nuV_value_perkm2year * (1 + Adjustment_cum),
                nV_value_perkm2year_adjusted = nV_value_perkm2year * (1 + Adjustment_cum)) %>%
        ungroup() %>%
        nest(data = c(muV_value_perkm2year_adjusted, nuV_value_perkm2year_adjusted, nV_value_perkm2year_adjusted,
                        CoralArea_2020_km2, temp, DamCoef_changeperC, DamCoef_changeperC_se, GDP_adjusted)) %>%
        mutate(simulations = map(data, ~ simulate_data(.))) %>%
        select(-data) %>%
        unnest(simulations)

        # Print and inspect the results
        print(ssp_corals_growth)



        ### Other Sim
            library(dplyr)
            library(tidyr)
            library(purrr)
            library(broom)

            set.seed(123)  # Ensure reproducibility

            # Number of simulations
            n_sim <- 10

            # Function to simulate 'area' based on uncertainty in 'DamCoef_changeperC'
            simulate_area <- function(df, n_sim) {
            replicate(n_sim, {
                df %>%
                mutate(area = CoralArea_2020_km2 * (1 + rnorm(n(), mean = DamCoef_changeperC, sd = DamCoef_changeperC_se) * temp),
                        area = ifelse(area > 0, area, 0))
            }, simplify = FALSE) %>% 
            bind_rows(.id = "sim_id")
            }

            # Applying simulation and fitting models by country
            results <- ssp_corals %>%
            group_by(countrycode) %>%
            nest() %>%
            mutate(simulations = map(data, ~ simulate_area(., n_sim))) %>%
            unnest(simulations) %>%
            mutate(model = map(data, ~ lm(fraction_damaged ~ poly(temp, 2), data = .))) %>%
            mutate(coefs = map(model, tidy)) %>%
            select(countrycode, sim_id, coefs) %>%
            unnest(coefs)

            # Summarizing results by country
            summary_results <- results %>%
            group_by(countrycode, term) %>%
            summarise(mean_estimate = mean(estimate), sd_estimate = sd(estimate), .groups = 'drop')

            # Optionally, calculate variance-covariance matrix for each country
            var_cov_by_country <- results %>%
            group_by(countrycode) %>%
            summarise(cov_matrix = list(cov(select(., estimate))), .groups = 'drop')

            print(summary_results)
            print(var_cov_by_country)
        ### Other Sim

    ## Simulation

    Market_Value <- ggplot(ssp_corals_growth %>% filter(scenario=="SSP2")) + 
    geom_line(aes(x=year,y=Market_Use_Values_Undamaged_percGDP,color=countrycode,linetype=scenario)) + 
    geom_text(data= ssp_corals_growth %>% filter(scenario=="SSP2", year==2100), aes(x=year,y=Market_Use_Values_Undamaged_percGDP,color=countrycode,label=countrycode)) +
    ggtitle("Corals Market Revenue\n Under SSP2 (No Climate Impacts)") + xlab("Year") + ylab("Market Benefits From Corals (% GDP)") + theme_bw() + guides(color=FALSE, linetype=FALSE)

    ggplot(ssp_corals_growth %>% filter(scenario=="SSP2")) + 
    geom_line(aes(x=year,y=Damages_percGDP,color=countrycode,linetype=scenario)) + 
    geom_text(data= ssp_corals_growth %>% filter(scenario=="SSP2", year==2100), aes(x=year,y=Damages_percGDP,color=countrycode,label=countrycode))

    
    coral_dam_plot <- ggplot(ssp_corals_growth %>% filter(scenario=="SSP2"), aes(x = temp, y = Damages_percGDP, color = countrycode)) + 
        geom_point(aes(linetype = scenario)) +  # Plot points
        geom_smooth(method = "lm", formula = y ~ x + I(x^2)+ 0, se = TRUE, linetype = "solid") +  # Add linear model through origin
        geom_text_repel(data = ssp_corals_growth %>% filter(scenario == "SSP2", year == 2100), aes(x = temp + 0.05, y = Damages_percGDP, label = countrycode)) + 
        xlab("Temperature (C)") + 
        ylab("Market Damages (% GDP)") + 
        theme_bw() + guides(color=FALSE, linetype=FALSE) +
    ggtitle("Damages on Market Benefits from Corals Under RCP4.5") 

    ggarrange(Market_Value, coral_dam_plot)
    
    model_market_loss <- lm(fraction_damaged  ~ 0 + temp, data = ssp_corals_growth %>% filter(!is.na(fraction_damaged)) )
    summary(model_market_loss)$coefficients[2]
    vcov(model_market_loss)[2]    
    
    market_coefficients_by_country <- ssp_corals_growth %>% filter(!is.na(fraction_damaged)) %>%
            group_by(countrycode) %>%
            do({
                model_market_loss <- lm(fraction_damaged  ~ 0 + temp + I(temp^2), data = .)
                data.frame(FractionChangeGDP_perC = coef(model_market_loss)["temp"],
                FractionChangeGDP_perC_se = summary(model_market_loss)$coefficients[3],
                FractionChangeGDP_perC_sq = summary(model_market_loss)$coefficients[2],
                FractionChangeGDP_perC_sq_se = summary(model_market_loss)$coefficients[4],
                variance = summary(model_market_loss)$sigma^2,
                cov_t_t2 = vcov(model_market_loss)[2]    ,

                r_sq = summary(model_market_loss)$r.squared
                )
            }) %>% ungroup()

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
    
    glimpse(market_coefficients_by_country)
    
    market_coefficients_by_country2 <- ssp_corals_growth %>% left_join(market_coefficients_by_country,by="countrycode")
    glimpse(market_coefficients_by_country2)
    
    market_coefficients_by_country3 <- market_coefficients_by_country2 %>% 
         mutate(#var_AreaLoss_approx =  (temp^2) * (DamCoef_changeperC_se)^2, 
            SE_dam = fraction_damaged * DamCoef_changeperC_se/DamCoef_changeperC) %>% 
            mutate(FractionChangeGDP_perC_se_adj = FractionChangeGDP_perC_se * ((variance + SE_dam^2)/(variance))^0.5, 
            FractionChangeGDP_perC_sq_se_adj = FractionChangeGDP_perC_sq_se * ((variance + SE_dam^2)/(variance))^0.5, 
            cov_t_t2_adj = cov_t_t2* ((variance + SE_dam^2)/(variance))^0.5)

    market_coefficients_by_country3 <- market_coefficients_by_country2 %>% 
         mutate(#var_AreaLoss_approx =  (temp^2) * (DamCoef_changeperC_se)^2, 
            SE_dam = fraction_damaged * DamCoef_changeperC_se/DamCoef_changeperC) %>% 
            mutate(FractionChangeGDP_perC_se_adj = FractionChangeGDP_perC_se * ((variance + SE_dam^2)/(variance))^0.5)

    glimpse(market_coefficients_by_country3)

         #mutate(var_ValueLoss_contribution = (FractionChangeGDP_perC)^2 * var_AreaLoss_approx) %>%
         #mutate(FractionChangeGDP_perC_se= FractionChangeGDP_perC_se+var_ValueLoss_contribution, 
         #FractionChangeGDP_perC_sq_se= FractionChangeGDP_perC_sq_se+var_ValueLoss_contribution)
    market_coefficients_by_country3 %>% filter(scenario=="SSP2",countrycode=="MEX") %>% dplyr::select(FractionChangeGDP_perC)

    ggplot(market_coefficients_by_country3%>% filter(scenario=="SSP2")) + 
        geom_point(aes(x=FractionChangeGDP_perC_se,y=FractionChangeGDP_perC_se_adj ,color=countrycode)) +
        geom_abline(aes(slope=1,intercept=0))

    coral_dam_plot2 <- ggplot(market_coefficients_by_country3%>% filter(scenario=="SSP2")) + 
        geom_point(aes(x=temp,y=fraction_damaged ,color=countrycode)) +
        geom_text_repel(data=market_coefficients_by_country3%>% filter(scenario=="SSP2") %>% filter(year==2100), aes(x=temp+0.02,y=fraction_damaged ,color=countrycode,label=countrycode)) +
        geom_line(aes(x=temp,y=FractionChangeGDP_perC*temp+FractionChangeGDP_perC_sq*temp^2 ,color=countrycode))+
        geom_ribbon(aes(x=temp,ymin=(FractionChangeGDP_perC-FractionChangeGDP_perC_se_adj*1.645)*temp+(FractionChangeGDP_perC_sq-FractionChangeGDP_perC_sq_se_adj*1.645)*temp^2, 
        ymax=(FractionChangeGDP_perC+FractionChangeGDP_perC_se_adj*1.645)*temp + (FractionChangeGDP_perC_sq+FractionChangeGDP_perC_sq_se_adj*1.645)*temp^2,fill=countrycode),alpha=0.2)+        
        theme_bw() + 
        guides(color=FALSE,fill=FALSE) + 
        xlab("Temperature Increase under RCP7") + 
        ylab("Market Damages (Fraction GDP)")
    coral_dam_plot2

    
    coral_dam_plot2 <- ggplot(market_coefficients_by_country3%>% filter(scenario=="SSP2")) + 
        geom_point(aes(x=temp,y=fraction_damaged ,color=countrycode)) +
        geom_text_repel(data=market_coefficients_by_country3%>% filter(scenario=="SSP2") %>% filter(year==2100), aes(x=temp+0.02,y=fraction_damaged ,color=countrycode,label=countrycode)) +
        geom_line(aes(x=temp,y=FractionChangeGDP_perC*temp ,color=countrycode))+
        geom_ribbon(aes(x=temp,ymin=(FractionChangeGDP_perC-FractionChangeGDP_perC_se_adj*1.645)*temp, 
        ymax=(FractionChangeGDP_perC+FractionChangeGDP_perC_se_adj*1.645)*temp ,fill=countrycode),alpha=0.2)+        
        theme_bw() + 
        guides(color=FALSE,fill=FALSE) + 
        xlab("Temperature Increase under RCP7") + 
        ylab("Market Damages (Fraction GDP)")
    coral_dam_plot2
    
    
    coefs_with_vcov <- market_coefficients_by_country3 %>% filter(year==2100,scenario=="SSP1") %>% dplyr::select(countrycode,FractionChangeGDP_perC,FractionChangeGDP_perC_se_adj,FractionChangeGDP_perC_sq,FractionChangeGDP_perC_sq_se_adj,cov_t_t2_adj) %>% as.data.frame()
    coefs_with_vcov <- market_coefficients_by_country3 %>% ungroup() %>% filter(year==2100,scenario=="SSP1") %>% dplyr::select(countrycode,FractionChangeGDP_perC,FractionChangeGDP_perC_se_adj) %>% as.data.frame()
    glimpse(coefs_with_vcov)

        
        write.csv(coefs_with_vcov,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\coral_GDPdam_coefficients_v4_May2024.csv")

    ggarrange(ggarrange(Market_Value,coral_benefits_2100),coral_dam_plot2,ncol=1)
    coral_benefits_2100 <- ggplot(ssp_corals_growth %>% filter(scenario=="SSP2")) + 
    geom_hline(aes(yintercept=100),linetype=2)  +
    geom_hline(aes(yintercept=1),linetype=2)  +
    geom_text(aes(x=100,y=1.5,label="1%")) + 
    geom_text(aes(x=100,y=150,label="100%")) + 
    geom_text(data= ssp_corals_growth %>% filter(scenario=="SSP2", year==2100), 
        aes(x=GDP.billion2005USDperYear,y=Market_Use_Values_Undamaged_percGDP,color=countrycode,label=countrycode)) + 
        scale_y_continuous(trans="log10") + 
        scale_x_continuous(trans="log10") + 
        theme_bw() +
        guides(color=FALSE) + xlab("GDP in 2100 (2005 Int USD)") + ylab("Market Benefits From Corals (% GDP)") + ggtitle("Market benefits in 2100\n Under SSP2")

    ggarrange(ggarrange(Market_Value,coral_benefits_2100),coral_dam_plot2,ncol=1)
    ggsave(file="Figures//all_figures//corals//DamageFunction_linear.png")


#### END




    # mean_coefs <-  market_coefficients_by_country3 %>% 
    #                                     left_join(market_coefficients_by_country2 %>% filter(scenario=="SSP2") %>%  summarise(mean_FractionChangeGDP_perC_se = mean(FractionChangeGDP_perC_se,na.rm=TRUE), 
    #                                     mean_FractionChangeGDP_perC_sq_se = mean(FractionChangeGDP_perC_sq_se,na.rm=TRUE)),by="countrycode")
    # glimpse(mean_coefs)
    # #glimpse(ssp_corals_growth)
    # #ssp_corals_growth2 <- ssp_corals_growth %>% left_join(market_coefficients_by_country4,by="countrycode")

    #     coral_dam_plot2 <- ggplot(mean_coefs%>% filter(scenario.x=="SSP2")) + 
    #     geom_point(aes(x=temp,y=fraction_damaged ,color=countrycode)) +
    #     geom_text_repel(data=mean_coefs%>% filter(scenario.x=="SSP2") %>% filter(year==2100), aes(x=temp+0.02,y=fraction_damaged ,color=countrycode,label=countrycode)) +
    #     geom_line(aes(x=temp,y=FractionChangeGDP_perC*temp+FractionChangeGDP_perC_sq*temp^2 ,color=countrycode))+
    #     geom_ribbon(aes(x=temp,ymin=(FractionChangeGDP_perC-mean_FractionChangeGDP_perC_se*1.645)*temp+(FractionChangeGDP_perC_sq-mean_FractionChangeGDP_perC_sq_se*1.645)*temp^2, 
    #     ymax=(FractionChangeGDP_perC+mean_FractionChangeGDP_perC_se*1.645)*temp + (FractionChangeGDP_perC_sq+mean_FractionChangeGDP_perC_sq_se*1.645)*temp^2,fill=countrycode),alpha=0.2)+        
    #     theme_bw() + 
    #     guides(color=FALSE,fill=FALSE) + 
    #     xlab("Temperature Increase under RCP7") + 
    #     ylab("Market Damages (% GDP)")
    # coral_dam_plot2

    # market_coefficients_by_country2 <- mean_coefs %>% filter(year==2100,scenario.x=="SSP1") %>% dplyr::select(countrycode,FractionChangeGDP_perC,FractionChangeGDP_perC_se,FractionChangeGDP_perC_sq,FractionChangeGDP_perC_sq_se) %>% as.data.frame()
    # glimpse(market_coefficients_by_country2)

        
    #     write.csv(market_coefficients_by_country2,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\coral_GDPdam_coefficients_v2April2024.csv")

    # ggarrange(ggarrange(Market_Value,coral_benefits_2100),coral_dam_plot2,ncol=1)
    
    # ggsave(file="Figures//all_figures//corals//DamageFunction.png")


    # ggarrange(coral_dam_plot)

    # glimpse(ssp_corals_growth)
    # coral_benefits_2100 <- ggplot(ssp_corals_growth %>% filter(scenario=="SSP2")) + 
    # geom_hline(aes(yintercept=100),linetype=2)  +
    # geom_hline(aes(yintercept=1),linetype=2)  +
    # geom_text(aes(x=100,y=1.5,label="1%")) + 
    # geom_text(aes(x=100,y=150,label="100%")) + 
    # geom_text(data= ssp_corals_growth %>% filter(scenario=="SSP2", year==2100), 
    #     aes(x=GDP.billion2005USDperYear,y=Market_Use_Values_Undamaged_percGDP,color=countrycode,label=countrycode)) + 
    #     scale_y_continuous(trans="log10") + 
    #     scale_x_continuous(trans="log10") + 
    #     theme_bw() +
    #     guides(color=FALSE) + xlab("GDP in 2100 (2005 Int USD)") + ylab("Market Benefits From Corals (% GDP)") + ggtitle("Market benefits in 2100\n Under SSP2")

    # ggarrange(ggarrange(Market_Value,coral_benefits_2100),coral_dam_plot,ncol=1)
    # ggsave(file="Figures//all_figures//corals//DamageFunction.png")

    # ssp_corals_growth %>% filter(scenario=="SSP2",countrycode=="MDV") %>% dplyr::select(scenario,GDP.billion2005USDperYear,Market_Use_Values_Undamaged,Market_Use_Values_Undamaged_percGDP,CoralArea_2020_km2) %>% as.data.frame()


    # benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)  
    # glimpse(benefit_ssp)
    # ### Market Damage Function
    
    # # ggplot(data = corals_df_iso_with_gdp, aes(x=CoralArea_2020_km2,y=muV_value_perkm2year*CoralArea_2020_km2))+
    # # geom_point()+
    # # geom_text(aes(x=CoralArea_2020_km2,y=muV_value_perkm2year*CoralArea_2020_km2,label=countrycode))

    # # ggplot(data = corals_df_iso_with_gdp, aes(x=area_km2_t0,y=55724*100*area_km2_t0))+
    # # geom_point()+
    # # geom_text(aes(x=area_km2_t0,y=55724*100*area_km2_t0,label=countrycode))

    # # corals_df_iso_with_gdp$area_km2_t0[which(corals_df_iso_with_gdp$countrycode=="AUS")]*55724*100
    # # corals_df_iso_with_gdp$area_km2_t0[which(corals_df_iso_with_gdp$countrycode=="AUS")]*corals_df_iso_with_gdp$muV_value_perkm2year[which(corals_df_iso_with_gdp$countrycode=="AUS")]
    # # corals_df_iso_with_gdp$area_km2_t0[which(corals_df_iso_with_gdp$countrycode=="AUS")]
    # # living_corals <- corals_df %>% filter(year==2020 & ssp=="SSP1") %>%
    # # summarise(sum(area_km2 * mean_cover))

    # # coral_reefs <- corals_df %>% filter(year==2020 & ssp=="SSP1") %>%
    # # summarise(sum(area_km2)) #Result =  455806.9 --- Comparable with data from https://biodiversitya-z.org/content/warm-water-coral-reef that estimates:  coral reefs occupy an area of only 260,000 - 600,000 km2, less than 0.1% of the Earth’s surface, or 0.2% of the ocean’s surface



    # # v_c <- 352915 #Total Monetary Value of the Bundle of Ecosystem Services for Coral Reefs (Int$/ha/year)
    # # coral_reefs_value <- v_c * coral_reefs *100
    # # coral_reefs_value_trill <- coral_reefs_value / 10^12  # Result=16 trillion, 6 times higher than the reported in Summary for Policymakers – Status of Coral Reefs of the World: 2020

    # # living_coral_value_trill <- v_c * living_corals *100 / 10^12 # Result = 6 trill, 2 times higher than the reported in Summary for Policymakers – Status of Coral Reefs of the World: 2020


