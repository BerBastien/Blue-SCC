x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","lfe","rnaturalearth","scales","readxl","broom")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")
datadir <- "C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\modules"

## Retreiving (Biophysical) Damage Functions
    ## Temp SSPs

        T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
        T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
        T_ssp126 <- read.csv("Data/scenarios/SSP126_magicc_202308040902.csv")
        T_ssp460 <- read.csv("Data/scenarios/SSP460_magicc_202402051249.csv")


            temp <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
            temp$year <- as.integer(sub('X', '', temp$year))
            names(temp)[1] <- "temp"
            temp$scenario <- "RCP45"

            temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
            temp2$year <- as.integer(sub('X', '', temp2$year))
            names(temp2)[1] <- "temp"
            temp2$scenario <- "RCP85"


            temp3 <- data.frame(temp = t(T_ssp126[7,c(13:length(T_ssp126))]), year = names(T_ssp126[17,c(13:length(T_ssp126))]))
            glimpse(temp3)
            temp3$year <- as.integer(sub('X', '', temp3$year))
            names(temp3)[1] <- "temp"
            temp3$scenario <- "RCP26"
            
            temp4 <- data.frame(temp = t(T_ssp460[16,c(13:length(T_ssp460))]), year = names(T_ssp460[17,c(13:length(T_ssp460))]))
            temp4$year <- as.integer(sub('X', '', temp4$year))
            names(temp4)[1] <- "temp"
            temp4$scenario <- "RCP60"

            temp <- rbind(temp,temp2,temp3,temp4)
            
                temp <-temp %>% group_by(scenario) %>%
                    filter(year > 1996, year<2021) %>%
                    mutate(t_96_20 = mean(temp)) %>%
                    filter(year==2020) %>%
                    mutate(t_20 = temp) %>% 
                dplyr::select(t_96_20,scenario,t_20) %>%
                    inner_join(temp, by = c("scenario"))

                temp$tdif <- temp$temp - temp$t_96_20
                temp$tdif20 <- temp$temp - temp$t_20
                glimpse(temp)

            tdif_rcp26 <- temp %>%
                filter(scenario == "RCP26") %>%
                select(year, tdif_rcp26= tdif, -scenario)

                temp <- temp %>%
                left_join(tdif_rcp26 , by = "year")%>% select(-scenario.y) %>% dplyr::rename(scenario=scenario.x)
                glimpse(temp)

                temp <- temp %>%
                mutate(tdif_from_rcp26 = tdif - tdif_rcp26)
                ggplot(temp)+geom_line(aes(x=year,y=temp,color=scenario))
    ## Temp SSPs

    ## Ports

        ## Read Ports
            ports_b85 <- read.csv(file.path(datadir, "\\ports\\iso3_downtime_risk_rcp85_mid.csv"))
            glimpse(ports_b85)
            ports_b26 <- read.csv(file.path(datadir, "\\ports\\iso3_downtime_risk_rcp26_mid.csv"))
            glimpse(ports_b26)
            ports_b45 <- read.csv(file.path(datadir, "\\ports\\iso3_downtime_risk_rcp45_mid.csv"))
            glimpse(ports_b45)
            ports_bpr <- read.csv(file.path(datadir, "\\ports\\iso3_downtime_risk_present.csv"))
            glimpse(ports_bpr)
        ## Read Ports

        ## Merge Ports and get Percentgae Change

            ports_downtime <- ports_bpr %>% dplyr::select(iso3,total_risk_trade) %>% 
                mutate(year = 2020, scenario="null") %>% 
                bind_rows(ports_b85 %>% dplyr::select(iso3,total_risk_trade) %>% 
                mutate(year = 2050, scenario="RCP85")) %>% 
                bind_rows(ports_b26 %>% dplyr::select(iso3,total_risk_trade) %>% 
                mutate(year = 2050, scenario="RCP26")) %>% 
                bind_rows(ports_b45 %>% dplyr::select(iso3,total_risk_trade) %>% 
                mutate(year = 2050, scenario="RCP45"))

            ports_downtime <-  ports_downtime %>% group_by(iso3) %>% 
                mutate(perc_change = 100*total_risk_trade / total_risk_trade[year==2020]-100, 
                uptime_perc  = 100*(365 - total_risk_trade)/365 - 100)
            #glimpse(ports_downtime)
                    
                    ggplot(ports_downtime)+
                    geom_point(aes(x=year,y=uptime_perc))
        ## Merge Ports and get Percentgae Change

        ## Merge with Temp

            ports_downtime_temp <- merge(ports_downtime,temp %>% filter(year==2050) %>% dplyr::select(-year),by="scenario",all=F)
            glimpse(ports_downtime_temp)



                # ggplot(ports_downtime_temp)+
                # geom_point(aes(x=tdif,y=uptime_perc))



                ggplot(ports_downtime_temp)+
                geom_point(aes(x=tdif,y=uptime_perc))
        ## Merge with Temp

        ## Get Ports Downtime Temp Coefficients
            results_ports_coef_df <- ports_downtime_temp %>%
                group_by(iso3) %>%
                do(
                    #tidy(lm(perc_change ~ 0 + tdif, data = .))
                    tidy(lm(uptime_perc ~ 0 + tdif, data = .))
                ) %>%
                filter(term == "tdif") %>%
                select(iso3, estimate, std.error)
            
            glimpse(results_ports_coef_df)

            ports_downtime_baseline <-  ports_downtime %>% filter(year==2020) %>% 
                mutate(total_risk_trade_base = total_risk_trade) %>% select(iso3,total_risk_trade_base)

            results_ports_coef_df <- results_ports_coef_df %>% left_join(ports_downtime_baseline,by="iso3")
        ## Get Ports Downtime Temp Coefficients
    ## Ports

    ## Fisheries
        ## Read Fisheries
            fish_b <- read.csv(file.path(datadir, "\\fish\\Statistical\\total_catch_profit_timeseries_withBiomass.csv"))
            glimpse(fish_b)
            fish_b %>% filter(rcp=="RCP 6.0",country_iso3=="IND",scenario=="Full Adaptation",year==2012)
            
            fish_full <- fish_b %>% filter(scenario=="Full Adaptation")
            glimpse(fish_rcp6_full)
            ggplot(fish_b  %>% filter(country_iso3=="IND",scenario=="Full Adaptation",year<2030))+
                geom_line(aes(x=year,y=biomass_mt,color=rcp,linetype=scenario))

            fish_b  %>% filter(country_iso3=="IND",scenario=="Full Adaptation",year==2030)

        ## Read Fisheries

        ## Merge with Temp
            fish_df <- fish_full %>%
                mutate(across(where(is.character), toupper)) %>%
                mutate(rcp = gsub(" ", "", rcp), # Remove spaces
                    rcp = gsub("\\.", "", rcp))
            names(temp)[2] <- "rcp"
            
            fish_df_temp <- fish_df  %>%
                                    left_join(temp %>% group_by(rcp,year) %>% summarise(tdif_from_rcp26=mean(tdif_from_rcp26,na.rm=T)),by=c("rcp", "year"))



            fish_df_temp %>%
                filter(rcp == "RCP26", country_iso3=="ALB") 
                
                fish_df_temp_baseline <- fish_df_temp %>%
                filter(rcp == "RCP26") %>%
                select(country_iso3, year, biomass_mt_baseline = biomass_mt, catch_mt_baseline= catch_mt)
                
                glimpse(fish_df_temp_baseline)
            fish_df_temp_baseline%>% filter(country_iso3=="ALB",year==2012)


            fish_df_temp <- fish_df_temp %>% 
                left_join(fish_df_temp_baseline, by = c("country_iso3", "year"))

            fish_df_temp %>% filter(rcp=="RCP60",country_iso3=="ALB",scenario=="FULL ADAPTATION",year==2042)

            glimpse(fish_df_temp)
        ## Merge with Temp

        ## Getting the difference in catch
            
            fish_df_temp <- fish_df_temp %>% mutate(biomass_mt_diff_from_rcp_26 = biomass_mt - biomass_mt_baseline,
                catch_mt_diff_from_rcp_26 = catch_mt - catch_mt_baseline,
                biomass_mt_percDiff_from_rcp_26 =100* biomass_mt / biomass_mt_baseline-100,
                catch_mt_percDiff_from_rcp_26 =100* catch_mt / catch_mt_baseline-100)
        ## Getting the difference in catch

        ## Estimating linear Model

            results_fish_coef_df <- fish_df_temp %>% 
                    group_by(country_iso3) %>% 
                    filter(!is.na(tdif_from_rcp26) & !is.na(country_iso3) & !is.na(catch_mt_percDiff_from_rcp_26)) %>%
                    do(
                        tidy(lm(catch_mt_percDiff_from_rcp_26 ~ 0 + tdif_from_rcp26, data = .))
                    ) %>%
                    filter(term == "tdif_from_rcp26") %>%
                    select(country_iso3, estimate, std.error)
                
            glimpse(fish_df_temp_baseline)

            fish_df_temp_baseline_2020 <- fish_df_temp_baseline %>% filter(year==2020)
            glimpse(fish_df_temp_baseline_2020)
            
            results_fish_coef_df <- results_fish_coef_df %>% left_join(fish_df_temp_baseline_2020,by="country_iso3")
            glimpse(results_fish_coef_df)

            glimpse(fish_df_temp)
            ggplot(fish_df_temp %>% filter(country_iso3!="IND",!is.na(country_iso3)))+
            geom_line(aes(x=tdif_from_rcp26,y=catch_mt_percDiff_from_rcp_26,color=country_iso3)) + 
            geom_text(aes(x=tdif_from_rcp26,y=catch_mt_percDiff_from_rcp_26,color=country_iso3,label=country_iso3))

            fish_df_temp %>% filter(catch_mt_percDiff_from_rcp_26>10000, year==2012) %>% group_by(country_iso3) %>% as.data.frame()

        ## Estimating linear Model
    ## Fisheries

    ## Corals
        corals_coef <- read.csv(file="Data/intermediate_output/corals_area_damage_value_v4.csv")
        glimpse(corals_coef)
    ## Corals


    ## Mangroves
        mangroves_coefficients_sq <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v5April2024.csv")
        glimpse(mangroves_coefficients_sq)
    ## Mangroves

## Retreiving Damage Functions

library(dplyr)

    M_weighted_means <- mangroves_coefficients_sq %>%
    select(FractionChange_perC, FractionChange_perC_se, FractionChange_perC_sq, FractionChange_perC_sq_se, MangroveArea_2020_km2) %>%
    summarise(
        weighted_mean_estimate = sum(FractionChange_perC * MangroveArea_2020_km2) / sum(MangroveArea_2020_km2),
        weighted_std_error = sum(FractionChange_perC_se * MangroveArea_2020_km2) / sum(MangroveArea_2020_km2),
        weighted_mean_estimate_sq = sum(FractionChange_perC_sq * MangroveArea_2020_km2) / sum(MangroveArea_2020_km2),
        weighted_std_error_sq = sum(FractionChange_perC_sq_se * MangroveArea_2020_km2) / sum(MangroveArea_2020_km2), 
        p75_estimate = quantile(FractionChange_perC, 0.75, na.rm = TRUE) , 
        p25_estimate = quantile(FractionChange_perC, 0.25, na.rm = TRUE) , 
        p75_estimate_sq = quantile(FractionChange_perC_sq, 0.75, na.rm = TRUE) , 
        p25_estimate_sq = quantile(FractionChange_perC_sq, 0.25, na.rm = TRUE) 
    )

    print(M_weighted_means)

    C_weighted_means <- corals_coef %>%
    select(DamCoef_changeperC, DamCoef_changeperC_se, CoralArea_2020_km2) %>%
    summarise(
        weighted_mean_estimate = sum(DamCoef_changeperC * CoralArea_2020_km2) / sum(CoralArea_2020_km2),
        weighted_std_error = sum(DamCoef_changeperC_se * CoralArea_2020_km2) / sum(CoralArea_2020_km2), 
        p75_estimate = quantile(DamCoef_changeperC, 0.75, na.rm = TRUE) , 
        p25_estimate = quantile(DamCoef_changeperC, 0.25, na.rm = TRUE) 
    )

    print(C_weighted_means)

    P_weighted_means <- results_ports_coef_df %>% ungroup() %>% 
    summarise(
        #weighted_mean_estimate = sum(estimate * total_risk_trade_base) / sum(total_risk_trade_base),
        weighted_mean_estimate = mean(estimate),
        weighted_std_error = sum(std.error * total_risk_trade_base) / sum(total_risk_trade_base), 
        p75_estimate = quantile(estimate, 0.75, na.rm = TRUE) , 
        p25_estimate = quantile(estimate, 0.25, na.rm = TRUE) 
    )

    print(P_weighted_means)
    glimpse(results_fish_coef_df )
    F_weighted_means <- results_fish_coef_df %>% ungroup() %>% filter(country_iso3!="IND") %>% 
    summarise(
        weighted_mean_estimate = sum(estimate * catch_mt_baseline) / sum(catch_mt_baseline),
        weighted_std_error = sum(std.error * catch_mt_baseline) / sum(catch_mt_baseline) , 
        p75_estimate = quantile(estimate, 0.75, na.rm = TRUE) , 
        p25_estimate = quantile(estimate, 0.25, na.rm = TRUE) 
    )

    print(F_weighted_means)


temp_D <- temp %>% 
mutate(Dam_central_corals = tdif20*C_weighted_means$weighted_mean_estimate, 
        #Dam_min_corals = tdif20* (C_weighted_means$weighted_mean_estimate - C_weighted_means$weighted_std_error*1.64), 
        #Dam_max_corals= tdif20* (C_weighted_means$weighted_mean_estimate + C_weighted_means$weighted_std_error*1.64),
        Dam_min_corals = tdif20* (C_weighted_means$p25_estimate), 
        Dam_max_corals= tdif20* (C_weighted_means$p75_estimate),
        
        Dam_central_ports = tdif20*P_weighted_means$weighted_mean_estimate/100, 
        #Dam_min_ports_min = tdif20* (P_weighted_means$weighted_mean_estimate - P_weighted_means$weighted_std_error*1.64) /100, 
        #Dam_max_ports = tdif20* (P_weighted_means$weighted_mean_estimate + P_weighted_means$weighted_std_error*1.64)/100, 
        Dam_max_ports = tdif20* (P_weighted_means$p25_estimate)/100, 
        Dam_min_ports= tdif20* (P_weighted_means$p75_estimate)/100,
        
        Dam_central_mangroves = tdif20*M_weighted_means$weighted_mean_estimate + (tdif20^2)*M_weighted_means$weighted_mean_estimate_sq, 
        #Dam_min_mangroves_min = tdif20* (M_weighted_means$weighted_mean_estimate - M_weighted_means$weighted_std_error*1.64), 
        #Dam_max_mangroves = tdif20* (M_weighted_means$weighted_mean_estimate + M_weighted_means$weighted_std_error*1.64), 
        Dam_min_mangroves = tdif20* (M_weighted_means$p25_estimate)+ (tdif20^2)*M_weighted_means$p25_estimate_sq, 
        Dam_max_mangroves= tdif20* (M_weighted_means$p75_estimate)+ (tdif20^2)*M_weighted_means$p75_estimate_sq,
        
        Dam_central_fisheries = tdif20*F_weighted_means$weighted_mean_estimate/100, 
        #Dam_min_fisheries = tdif20* (F_weighted_means$weighted_mean_estimate - F_weighted_means$weighted_std_error*1.64)/100, 
        #Dam_max_fisheries = tdif20* (F_weighted_means$weighted_mean_estimate + F_weighted_means$weighted_std_error*1.64)/100)
        Dam_min_fisheries = tdif20* (F_weighted_means$p25_estimate)/100, 
        Dam_max_fisheries= tdif20* (F_weighted_means$p75_estimate)/100
)
glimpse(temp_D)

temp_D_long <- temp_D %>%
  pivot_longer(
    cols = c(starts_with("Dam_central_"), starts_with("Dam_min_"), starts_with("Dam_max_")),
    names_to = c(".value", "category"), 
    names_pattern = "(.+)_(.+)" 
  )

library(forcats)

temp_D_long <- temp_D_long %>%
  mutate(category = fct_relevel(category, "mangroves", "ports", "fisheries", "corals"))
glimpse(temp_D_long)
library(scico)
library(ggsci)
library(ggthemes)
install.packages("ggthemes")
install.packages('ggthemes', dependencies = TRUE)


ggplot(temp_D_long %>% filter(scenario == "RCP60")) +
geom_line(aes(x=year,y=100*Dam_central,color=category),size=0.8)+
geom_point(data = temp_D_long %>% filter(scenario == "RCP60",year==2100),
    aes(x=year,y=100*Dam_central,color=category,shape=category))+
geom_ribbon(aes(x=year,ymin=100*Dam_min, ymax=100*Dam_max,fill=category),alpha=0.2)+
  labs(title = "Percent Change under RCP60",
       x = "Year",
       y = "Percent Change from 2020 levels",
       color = "Category",
       fill = "Category", shape = "Category") +
  theme_minimal()+
  geom_hline(aes(yintercept=0),linetype=2)+
  xlim(c(2020,2100)) +
  #scale_color_npg(labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Port Downtime Days", "fisheries" = "Tons of Fish Catch")) +
  #scale_fill_npg(labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Port Downtime Days", "fisheries" = "Tons of Fish Catch")) +
    scale_color_tableau(palette="Superfishel Stone" ,labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Port Operation Days", "fisheries" = "Tons of Fish Catch")) +
    scale_fill_tableau(palette="Superfishel Stone" ,labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Port Operation Days", "fisheries" = "Tons of Fish Catch")) +
    scale_shape_discrete(labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Port Operation Days", "fisheries" = "Tons of Fish Catch")) +
    guides(
    color = guide_legend(reverse = FALSE),
    fill = guide_legend(reverse = FALSE)
  )
  getwd()
  ggsave("Figures/BiophysicalImpacts.jpg",dpi=600)
  #scale_color_scico_d(palette = "batlow",begin=0.1,end=0.9,labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Downtime Days per Year", "fisheries" = "Tons of Fish Catch")) +
  #scale_fill_scico_d(palette = "batlow",begin=0.1,end=0.9,labels = c("corals" = "Coral Cover Area", "mangroves" = "Mangrove Cover Area", "ports" = "Downtime Days per Year", "fisheries" = "Tons of Fish Catch")) 
  




ggplot(temp_D %>% filter(rcp == "RCP60")) +
geom_line(aes(x=year,y=Dam_corals,color="corals"))+
geom_line(aes(x=year,y=Dam_fisheries,color="fisheries"))+
geom_line(aes(x=year,y=Dam_ports,color="ports"))+
geom_line(aes(x=year,y=Dam_mangroves,color="mangroves")) +
geom_ribbon(aes(x=year,ymin=Dam_corals_min, ymax=Dam_corals_max,color="corals"))+
scale_color_manual(values = category_colors) +
scale_fill_manual(values = category_colors) +
  labs(title = "Damage Estimates by Category Over Years under RCP60",
       x = "Year",
       y = "Damage Estimate",
       color = "Category") +
  theme_minimal()+
  xlim(c(2020,2100))
