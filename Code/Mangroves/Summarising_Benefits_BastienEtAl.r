#Summarising Benefits per Ha

    library(scico)
    library(tidyr)
    library(dplyr)
    library(rlang)
    library(dplyr)
    library(purrr)
    library(tidyverse)
             library("rnaturalearthdata")
             library(ggpubr)
            library("rnaturalearth")
                        library("scales")



    scen_allforcings_allES <- read.csv(file="Data/modules/mangroves/scen_allforcings_allES_ssp270.csv")
    glimpse(scen_allforcings_allES)

## Area Change
    
    
    diff_country_total <- read.csv(file="Data/modules/mangroves/diff_country_total_ssp370_brander_corrected_countries.csv")
    diff_country_total1 <- read.csv(file="Data/modules/mangroves/diff_country_total_ssp170_brander_corrected_countries.csv")
    diff_country_total2 <- read.csv(file="Data/modules/mangroves/diff_country_total_ssp270_brander_corrected_countries.csv")
    diff_country_total3 <- read.csv(file="Data/modules/mangroves/diff_country_total_ssp370_brander_corrected_countries.csv")
    diff_country_total4 <- read.csv(file="Data/modules/mangroves/diff_country_total_ssp470_brander_corrected_countries.csv")
    diff_country_total5 <- read.csv(file="Data/modules/mangroves/diff_country_total_ssp570_brander_corrected_countries.csv")

    glimpse(diff_country_total3)
    glimpse(diff_country_total2)

    diff_country_total1 %>% filter(countrycode=="NLD") 

    ssp_gdp <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv')
    glimpse(ssp_gdp)
    countries_in_ssps <- unique(ssp_gdp$ISO3)    
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP585_magicc_202310021547.csv")
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP370_magicc_202311031621.csv")

        
    ssp_gdp$countrycode <- ssp_gdp$ISO3
    
    
    ssp_temp_long <- ssp_temp %>%
        tidyr::pivot_longer(
            cols = starts_with("X"),
            names_to = "year",
            values_to = "value"
        ) %>%
        # Remove the "X" prefix from the year column and convert to numeric
        mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")



    ssp_temp_long$temp2025 <- ssp_temp_long %>% filter(year==2025) %>% dplyr::select(value) %>% unlist()
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2025
    
    diff_country_total <- rbind(diff_country_total1 %>% rename("POP_Country"="Pop_Country_ssp1","GDP_Country"="GDP_Country_ssp1","POP"="POP_SSP1","GDP"="GDP_SSP1") %>% mutate(scenario="SSP1"),
                                diff_country_total2 %>% rename("POP_Country"="Pop_Country_ssp2","GDP_Country"="GDP_Country_ssp2","POP"="POP_SSP2","GDP"="GDP_SSP2") %>% mutate(scenario="SSP2"),
                                diff_country_total3 %>% rename("POP_Country"="Pop_Country_ssp3","GDP_Country"="GDP_Country_ssp3","POP"="POP_SSP3","GDP"="GDP_SSP3") %>% mutate(scenario="SSP3"),
                                diff_country_total4 %>% rename("POP_Country"="Pop_Country_ssp4","GDP_Country"="GDP_Country_ssp4","POP"="POP_SSP4","GDP"="GDP_SSP4") %>% mutate(scenario="SSP4"),
                                diff_country_total5 %>% rename("POP_Country"="Pop_Country_ssp5","GDP_Country"="GDP_Country_ssp5","POP"="POP_SSP5","GDP"="GDP_SSP5") %>% mutate(scenario="SSP5"))
    
    benefit_ssp <- merge(ssp_gdp,diff_country_total,by=c("countrycode","year","scenario"),all=T)
    benefit_ssp <- merge(benefit_ssp ,ssp_temp_long,by=c("year"),all=F) %>% filter(year>2025)
    benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)
    glimpse(benefit_ssp)

    
    scen_arealoss_perc_both <- read.csv(file="Data/modules/mangroves/Proj_Area_Perc_onlyCC_SSP270.csv")
    grid_with_countries_ALL <- read.csv("Data/modules/mangroves/grid_with_specific_countries_FINAL.csv")
    scen_arealoss_perc_both <- scen_arealoss_perc_both %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_arealoss_perc_both$countrycode_old <-scen_arealoss_perc_both$countrycode
    scen_arealoss_perc_both$countrycode <- scen_arealoss_perc_both$iso_a3
    
    glimpse(scen_arealoss_perc_both)
    benefit_ssp2 <- benefit_ssp %>% filter(scenario.x=="SSP2")
    country_area_mangrove_2020 <- scen_arealoss_perc_both %>% filter(year==2026) %>%
                            group_by(countrycode) %>% 
                            summarize(area_2020 = sum(mangrove_area2020,na.rm=T))
    benefit_ssp2 <- merge(benefit_ssp2 ,country_area_mangrove_2020,by=c("countrycode"),all=F) 
    benefit_ssp2$frac_loss <- benefit_ssp2$diff_mangrove_area_future_loss/benefit_ssp2$area_2020

    ## LINEAR FUNCTION
        #   coefficients_by_country <- benefit_ssp2 %>% filter(!is.na(frac_loss)) %>%
        #         #mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
        #         group_by(countrycode) %>%
        #         do({
        #             model_area <- lm(frac_loss ~ 0 + temp, data = .)
        #             data.frame(coefficient_temp = coef(model_area)["temp"],
        #             coefficient_temp_se = summary(model_area)$coefficients[2],
        #             r_sq = summary(model_area)$r.squared
        #             )
        #         }) %>% ungroup()

        #     coefficients_by_country %>% filter(countrycode=="FRA")
            
        #     glimpse(coefficients_by_country)
        #     glimpse(country_area_mangrove_2020)

        #     coefficients_by_country <-  merge(coefficients_by_country,country_area_mangrove_2020,by=c("countrycode"),all=F)
        #     names(coefficients_by_country)[which(names(coefficients_by_country)=="area_2020")] <- "MangroveArea_2020_hectares" #Changing the name first
        #     coefficients_by_country$MangroveArea_2020_hectares <- coefficients_by_country$MangroveArea_2020_hectares*100 #Now converting it ti hectares
            
        #     write.csv(coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_v4March2024.csv")
        #     coefficients_by_country <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_v4March2024.csv")
            
        #     glimpse(coefficients_by_country)

        #     coefficients_by_country %>% filter(countrycode %in% c("MEX","NLD"))

        #     coefficients_by_country$MangroveArea_2020_km2 <- 0.01*coefficients_by_country$MangroveArea_2020_hectares
        #      coefficients_by_country <- coefficients_by_country %>% dplyr::select(-X,-MangroveArea_2020_hectares) %>% 
        #         rename(FractionChange_perC = coefficient_temp,
        #         FractionChange_perC_se = coefficient_temp_se)

        #     write.csv(coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_v4Mar2024.csv")
        #     coefficients_by_country_linear <- coefficients_by_country
        #     ## PLOTS
        #                 glimpse(benefit_ssp2)
        #                 glimpse(coefficients_by_country)
                
        #         c_list <- unique(benefit_ssp2$countrycode)
        #         library("ggsci")

        #         for(country_interest in c_list) {
        #             coef_area_c <- coefficients_by_country %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC) %>% as.double()
        #             se_area_c <- coefficients_by_country %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_se) %>% as.double()
                    
        #             plot <- ggplot(benefit_ssp2 %>% filter(countrycode==country_interest)) +
        #                 geom_point(aes(x=year,y=frac_loss))+
        #                 geom_line(aes(x=year,y=temp*coef_area_c)) +
        #                 geom_ribbon(aes(x=year, ymin = temp*(coef_area_c-se_area_c*1.654), ymax = temp*(coef_area_c+se_area_c*1.654)), fill = "grey80", alpha = 0.5) +
        #                 theme_minimal()+
        #                 ggtitle(paste0("Projection of values for ",country_interest))+
        #                 ylab("Mangrove Area Loss") 
        #             plot
        #             ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Area_Damage//", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

        #         }

                
        #         ggplot(intermediate_output,aes(x=year,y=use_market_perkm2))+geom_line(aes(color=countrycode)) + 
        #         geom_text(data=intermediate_output%>% filter(year==2100, countrycode=="IDN"),aes(x=year,y=use_market_perkm2,label=countrycode))

        #         benefit_ssp2 %>% filter(year==2030, countrycode=="IDN")
        #         ggplot(benefit_ssp2 %>% filter(year==2030)) + 
        #         geom_text(aes(x=frac_loss,y=area_2020, label=countrycode))

        #     ## PLOTS
    ## LINEAR FUNCTION

    ## Quadratic Function (Key: benefit_ssp2)
        coefficients_by_country_sq <- benefit_ssp2 %>% filter(!is.na(frac_loss)) %>%
            #mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
            group_by(countrycode) %>%
            do({
                model_area <- lm(frac_loss ~ 0 + temp + I(temp^2), data = .)
                data.frame(coefficient_temp = coef(model_area)["temp"],
                coefficient_temp_se = summary(model_area)$coefficients[3],
                sq_coefficient_temp = coef(model_area)["I(temp^2)"],
                sq_coefficient_temp_se = summary(model_area)$coefficients[4],
                cov_t_t2 = vcov(model_area)[2],   
                r_sq = summary(model_area)$r.squared
                )
            }) %>% ungroup()
        
        coefficients_by_country_sq <-  merge(coefficients_by_country_sq,country_area_mangrove_2020,by=c("countrycode"),all=F)
        names(coefficients_by_country_sq)[which(names(coefficients_by_country_sq)=="area_2020")] <- "MangroveArea_2020_hectares" #Changing the name first
        coefficients_by_country_sq$MangroveArea_2020_hectares <- coefficients_by_country_sq$MangroveArea_2020_hectares*100 #Now converting it ti hectares
        
        write.csv(coefficients_by_country_sq,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v5April2024.csv")
        coefficients_by_country_sq <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v5April2024.csv")

        coefficients_by_country_sq$MangroveArea_2020_km2 <- 0.01*coefficients_by_country_sq$MangroveArea_2020_hectares
        coefficients_by_country_sq <- coefficients_by_country_sq %>% dplyr::select(-MangroveArea_2020_hectares) %>% 
            rename(FractionChange_perC = coefficient_temp,
            FractionChange_perC_se = coefficient_temp_se,
            FractionChange_perC_sq = sq_coefficient_temp,
            FractionChange_perC_sq_se = sq_coefficient_temp_se)
        glimpse(coefficients_by_country_sq)

        write.csv(coefficients_by_country_sq,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v5April2024_cov.csv")
        #coefficients_by_country_linear$fun <- "linear"
        coefficients_by_country_sq$fun <- "square"
        #coefs_lin_sq <- rbind(coefficients_by_country_linear %>% dplyr::select(r_sq,fun),coefficients_by_country_sq %>% dplyr::select(r_sq,fun))
        #coefficients_by_country_sq <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v4Mar2024.csv")
        

        ## PLOTS
            c_list <- unique(benefit_ssp2$countrycode)
            library("ggsci")

            for(country_interest in c_list) {
                coef_area_c <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC) %>% as.double()
                se_area_c <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_se) %>% as.double()
                coef_area_c_sq <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_sq) %>% as.double()
                se_area_c_sq <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_sq_se) %>% as.double()
                
                plot <- ggplot(benefit_ssp2 %>% filter(countrycode==country_interest)) +
                    geom_point(aes(x=year,y=frac_loss,color=scenario.x))+
                    geom_line(aes(x=year,y=temp*coef_area_c+coef_area_c_sq*temp^2)) +
                    geom_ribbon(aes(x=year, ymin = temp*(coef_area_c-se_area_c*1.654)+(coef_area_c_sq-se_area_c_sq*1.654)*temp^2, 
                        ymax = temp*(coef_area_c+se_area_c*1.654)+(coef_area_c_sq+se_area_c_sq*1.654)*temp^2), fill = "grey80", alpha = 0.5) +
                    theme_minimal()+
                    ggtitle(paste0("Projection of values for ",country_interest))+
                    ylab("Mangrove Area Loss") 
                plot
                ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Area_Damage//AprilCoeff//AllSSPs//sq_", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

            }

            #coefs_lin_sq %>% group_by(fun) %>% summarise(mean_sq=mean(r_sq,na.rm=T), median_sq=median(r_sq,na.rm=T))

        ## PLOTS
        
    ## Quadratic Function
    

        #     library("rnaturalearthdata")
        #     library("rnaturalearth")
        #     world_ne <- ne_countries(scale = "medium", returnclass = "sf")

        #     world_ne_with_coeffs <-  merge(world_ne,coefficients_by_country,by.x="iso_a3",by.y="countrycode",all=T)
        #     glimpse(world_ne_with_coeffs)

        #    world_ne_with_coeffs %>% filter(iso_a3=="GBR")
            
        #     library("scales")
        #     gg <- ggplot(data = world_ne_with_coeffs) +
        #     geom_sf(aes(fill = FractionChange_perC)) +
        #     scale_fill_scico(palette = "lajolla",na.value="transparent",
        #         begin=0.1,end=0.9,direction=1) +
        #     labs(fill = "Area Damage \n(Fraction per Degree C)") +
            
        #     theme_bw()+
        #             coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))

        #     gg
            
        #     ggsave("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_lin_march2024.png")


## Area Change


## Prices
    
    glimpse(scen_allforcings_allES)
    weighted_avg_benefits <- scen_allforcings_allES %>% filter(year>2025) %>%
    group_by(countrycode, year,type,forcing) %>%
    mutate(gdppc = (1.59*GDP_Country_ssp2*10^9)/(Pop_Country_ssp2*10^6)) %>% # GDP is in billion 2005 USD per year, Pop is in million #Cumulative Inflation Global from 2005 to 2020: 59.1 from https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG
    mutate(benefits_perha_percGDP = (100*benefits_perha)/(1.59*GDP_Country_ssp2*10^9)) %>%
    summarize(
        weighted_avg_benefit_perha = sum(benefits_perha * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        weighted_avg_benefit_perha_percGDP = sum(benefits_perha_percGDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        gdppc = first(gdppc),
        GDP_SSP2 = first(1.59*GDP_Country_ssp2*10^9),
        .groups = "drop"
    )
    glimpse(weighted_avg_benefits)
    
    # Market damage Function
        weighted_avg_benefits_prov <- weighted_avg_benefits %>% filter(type=="provision",forcing=="both") %>%
        left_join(coefficients_by_country_sq, by="countrycode") %>%
        left_join(ssp_temp_long %>% dplyr::select(temp,year), by="year") %>%
        mutate(prov_benefits_undamaged = 100* weighted_avg_benefit_perha_percGDP * MangroveArea_2020_km2, 
                prov_benefits_damaged = 100* weighted_avg_benefit_perha_percGDP * (MangroveArea_2020_km2 * (1+FractionChange_perC*temp+FractionChange_perC_sq*temp^2)) ) %>%
        mutate(percentage_points_damaged =  prov_benefits_damaged - prov_benefits_undamaged, 
        fraction_damaged = 0.01 *(prov_benefits_damaged - prov_benefits_undamaged), 
        percentage_damaged = 100*(100* weighted_avg_benefit_perha) * MangroveArea_2020_km2*(FractionChange_perC*temp + FractionChange_perC_sq* temp^2)/GDP_SSP2 , 
        x =  100* weighted_avg_benefit_perha_percGDP*0.01 * MangroveArea_2020_km2,
        fraction_damaged_variance = (x*temp)^2 *FractionChange_perC_se^2 + (x*temp^2)^2 * FractionChange_perC_sq_se^2 )

        glimpse(weighted_avg_benefits_prov)

        market_coefficients_by_country <- weighted_avg_benefits_prov %>% filter(!is.na(fraction_damaged)) %>%
            group_by(countrycode) %>%
            do({
                model_market_loss <- lm(fraction_damaged  ~ 0 + temp + I(temp^2), data = .)
                data.frame(GDPDam_perC = coef(model_market_loss)["temp"],
                GDPDam_perC_se = summary(model_market_loss)$coefficients[3],
                GDPDam_perC_sq = summary(model_market_loss)$coefficients[2],
                GDPDam_perC_sq_se = summary(model_market_loss)$coefficients[4],
                variance = summary(model_market_loss)$sigma^2,
                cov_t_t2 = vcov(model_market_loss)[2]    ,
                r_sq = summary(model_market_loss)$r.squared
                )
            }) %>% ungroup()
        glimpse(market_coefficients_by_country)

        weighted_avg_benefits_prov2 <- weighted_avg_benefits_prov %>% left_join(market_coefficients_by_country,by="countrycode") %>% 
            mutate(GDPDam_perC_se_adj = GDPDam_perC_se * ((fraction_damaged_variance + variance) / variance )^0.5 ,
            GDPDam_perC_sq_se_adj = GDPDam_perC_sq_se * ((fraction_damaged_variance + variance) / variance )^0.5,
            cov_t_t2_adj = cov_t_t2.y * ((fraction_damaged_variance + variance) / variance )^0.5 )
        glimpse(weighted_avg_benefits_prov)
        glimpse(weighted_avg_benefits_prov2)#here 




        mangrove_dam_plot <- ggplot(weighted_avg_benefits_prov2 %>% filter(countrycode%in% countries_in_ssps)) + 
        geom_point(aes(x=temp,y=fraction_damaged,color=countrycode)) +
        #geom_text(data=weighted_avg_benefits_prov2 %>% filter(year==2100), aes(x=temp+0.1,y=percentage_damaged/100,color=countrycode,label=countrycode)) +
        geom_line(aes(x=temp,y=GDPDam_perC*temp + GDPDam_perC_sq*temp^2,color=countrycode))+
        geom_ribbon(aes(x=temp,ymin=(GDPDam_perC-GDPDam_perC_se_adj*1.96)*temp + (GDPDam_perC_sq-GDPDam_perC_sq_se_adj*1.96)*temp^2, 
        ymax=(GDPDam_perC+GDPDam_perC_se_adj*1.96)*temp + (GDPDam_perC_sq+GDPDam_perC_sq_se_adj*1.96)*temp^2,fill=countrycode),alpha=0.2)+        
        theme_bw() + 
        guides(color=FALSE,fill=FALSE) + 
        xlab("Temperature Increase under RCP7") + 
        ylab("Market Damages (% GDP)")

        mangrove_dam_plot

        glimpse(weighted_avg_benefits_prov2)
        market_coefficients_by_country <- weighted_avg_benefits_prov2 %>% filter(year==2100) %>% dplyr::select(countrycode,GDPDam_perC,GDPDam_perC_se_adj,GDPDam_perC_sq,GDPDam_perC_sq_se_adj,cov_t_t2_adj)
        glimpse(market_coefficients_by_country)
        write.csv(market_coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_GDPdam_coefficients_v3May2024_cov.csv")


        market_ben_mangroves <- ggplot(weighted_avg_benefits_prov %>% filter(countrycode %in% countries_in_ssps)) + 
        geom_line(aes(x=year,y=100*(100*weighted_avg_benefit_perha) * MangroveArea_2020_km2/GDP_SSP2,color=countrycode)) +
        theme_bw() +
        guides(color=FALSE) +
        geom_text_repel(data = weighted_avg_benefits_prov %>% filter(year==2100),aes(x=year,y=100*(100*weighted_avg_benefit_perha) * MangroveArea_2020_km2/GDP_SSP2,color=countrycode,label=countrycode))+
        xlab("Year") + ylab("Market Benefits From Mangroves (%GDP)")

        
        market_ben_mangroves_2100 <- ggplot(weighted_avg_benefits_prov %>% filter(year==2100,countrycode %in% countries_in_ssps)) + 
        geom_text(aes(x=10^11,y=140,label="100%"))+
        geom_text(aes(x=10^11,y=14,label="10%"))+
        geom_text(aes(x=10^11,y=1.2,label="1%"))+
        geom_hline(aes(yintercept=1),linetype=2)+
        geom_hline(aes(yintercept=10),linetype=2)+
        geom_hline(aes(yintercept=100),linetype=2)+
        geom_text(data = weighted_avg_benefits_prov %>% filter(year==2100,countrycode %in% countries_in_ssps),aes(x=GDP_SSP2,y=100*(100*weighted_avg_benefit_perha) * MangroveArea_2020_km2/GDP_SSP2,color=countrycode,label=countrycode))+
        xlab("GDP in 2100 (2020 Int USD)") + ylab("Market Benefits From Mangroves (%GDP)")+
        scale_y_continuous(trans="log10")+
        scale_x_continuous(trans="log10")+
        theme_bw()+
        guides(color=FALSE)


        ggarrange(market_ben_mangroves , market_ben_mangroves_2100        )
        ggarrange( ggarrange(market_ben_mangroves , market_ben_mangroves_2100        ), mangrove_dam_plot,ncol=1)        
        ggsave("Figures//all_figures//mangroves//Market_Dam.png")

        # ## MonteCarlo
        #     C_i <- unique(weighted_avg_benefits_prov$countrycode)
        #     value_coefs <- data.frame(countrycode=as.character(),GDPDam_perC=as.double(),GDPDam_perC_se=as.double() ,GDPDam_perC_sq=as.double(),GDPDam_perC_sq_se=as.double())
        #         for(c_i in C_i){
                
        #                 beta_1 <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(FractionChange_perC) %>% first() %>% as.double()
        #                 if(is.na(beta_1)){next}
        #                 beta_2 <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(FractionChange_perC_sq)%>% first()%>% as.double()
        #                 se_beta_1 <-weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(FractionChange_perC_se)%>% first()%>% as.double()
        #                 se_beta_2 <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(FractionChange_perC_sq_se)%>% first()%>% as.double()

        #                 # Number of simulations
        #                 n_sim <- 1000

        #                 # Simulate coefficients
        #                 sim_beta_1 <- rnorm(n_sim, mean = beta_1, sd = se_beta_1)
        #                 sim_beta_2 <- rnorm(n_sim, mean = beta_2, sd = se_beta_2)

        #                 tempi <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(temp)%>% as.data.frame()
        #                 GDP_SSP2i <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(GDP_SSP2)%>% as.data.frame()
        #                 if(is.na(GDP_SSP2i[[1]][1])){next}
        #                 Area_t0 <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(MangroveArea_2020_km2)%>% as.data.frame()

        #                 value_loss_results <- data.frame(countrycode=as.character(),sim=as.double(), temp=as.double(), area_loss=as.double(), value_loss=as.double())
                        
                    

        #             # Example time-varying value, Value_t (can also be simulated if variable)
        #             Value_t <- weighted_avg_benefits_prov %>% filter(countrycode == c_i ) %>% dplyr::select(weighted_avg_benefit_perha) %>% as.data.frame()

        #             for (i in 1:n_sim) {
        #             # Calculate area loss for each temperature
        #             area_loss_sim <- Area_t0*(sim_beta_1[i] * tempi + sim_beta_2[i] * tempi^2)
        #             value_loss_sim <- Value_t*100* area_loss_sim/GDP_SSP2i 
        #             value_loss_results <- rbind(value_loss_results,data.frame(countrycode=c_i,sim=i, temp=tempi, area_loss=area_loss_sim, value_loss=value_loss_sim))
        #             }
        #             names(value_loss_results) <- c("countrycode","sim", "temp", "area_loss", "value_loss")
        #             #glimpse(value_loss_results)

        #             #ggplot(value_loss_results,aes(x=temp,y=value_loss))+
        #             #geom_point()

        #             model <- lm(value_loss~0+temp+I(temp^2),data=value_loss_results)
        #             value_coefs <- rbind(value_coefs,data.frame(countrycode=c_i,GDPDam_perC=summary(model)$coef[1],GDPDam_perC_se=summary(model)$coef[3] ,GDPDam_perC_sq=summary(model)$coef[2],GDPDam_perC_sq_se=summary(model)$coef[4]))
        #         }  
        #         glimpse(value_coefs)      

        # ## MonteCarlo

         

        # weighted_avg_benefits_prov <- weighted_avg_benefits_prov %>% left_join(value_coefs,by="countrycode")
        # glimpse(weighted_avg_benefits_prov)
        # weighted_avg_benefits_prov%>%filter(countrycode=="GNB") %>% dplyr::select(GDPDam_perC)

        #  weighted_avg_benefits_prov <- weighted_avg_benefits_prov %>% 
        #  mutate(var_AreaLoss_approx =  (temp * FractionChange_perC_se)^2 + (temp^2 * FractionChange_perC_sq_se)^2) %>% 
        #  mutate(var_ValueLoss_approx = (weighted_avg_benefit_perha*100*MangroveArea_2020_km2/GDP_SSP2)^2 * var_AreaLoss_approx) %>%
        #  mutate(GDPDam_perC_se = sqrt(var_ValueLoss_approx), GDPDam_perC_sq_se = sqrt(var_ValueLoss_approx))


        # mangrove_dam_plot <- ggplot(weighted_avg_benefits_prov) + 
        # geom_point(aes(x=temp,y=percentage_damaged/100,color=countrycode)) +
        # #geom_text(data=weighted_avg_benefits_prov %>% filter(year==2100), aes(x=temp+0.1,y=percentage_damaged,color=countrycode,label=countrycode)) +
        # geom_line(aes(x=temp,y=GDPDam_perC*temp + GDPDam_perC_sq*temp^2,color=countrycode))+
        # geom_ribbon(aes(x=temp,ymin=(GDPDam_perC-se_ValueLoss_approx*1.96)*temp + (GDPDam_perC_sq-se_ValueLoss_approx*1.96)*temp^2, 
        # ymax=(GDPDam_perC+se_ValueLoss_approx*1.96)*temp + (GDPDam_perC_sq+se_ValueLoss_approx*1.96)*temp^2,fill=countrycode),alpha=0.2)+        
        # theme_bw() + 
        # guides(color=FALSE,fill=FALSE) + 
        # xlab("Temperature Increase under RCP7") + 
        # ylab("Market Damages (% GDP)")

        # mangrove_dam_plot
        # ggarrange( ggarrange(market_ben_mangroves , market_ben_mangroves_2100        ), mangrove_dam_plot,ncol=1)        
        # ggsave("Figures//all_figures//mangroves//Market_Dam.png")


        # glimpse(weighted_avg_benefits_prov)
        # market_coefficients_by_country <- weighted_avg_benefits_prov %>% filter(year==2100) %>% dplyr::select(countrycode,GDPDam_perC,GDPDam_perC_se,GDPDam_perC_sq,GDPDam_perC_sq_se)
        # write.csv(market_coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_GDPdam_coefficients_v1April2024.csv")

    # Market damage Function
    
    
    # # Summarize for "water" + "coastal"
    # water_coastal <- weighted_avg_benefits %>%
    # filter(type %in% c("water", "coastal")) %>%
    # group_by(countrycode, year) %>%
    # summarize(
    #     total_weighted_avg_benefit_wc = sum(weighted_avg_benefit, na.rm = TRUE),
    #     gdppc = first(gdppc),
    #     .groups = "drop"
    # )

    weighted_avg_benefits %>%
    filter(type %in% c("provision"), forcing=="both", countrycode=="MEX",year==2050) %>%
    group_by(countrycode, year) 
    
    # Summarize for "water" + "coastal"
    use_market <- weighted_avg_benefits %>%
    filter(type %in% c("provision"), forcing=="both") %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_use_market_perha = sum(weighted_avg_benefit_perha, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    glimpse(use_market)
    
    # Summarize for "food" + "wood"
    use_nonmarket <- weighted_avg_benefits %>%
    filter(type %in% c("regulation"), forcing=="both") %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_use_nonmarket_perha = sum(weighted_avg_benefit_perha, na.rm = TRUE),
        .groups = "drop"
    )


    levels(factor(weighted_avg_benefits$type))
    non_use <- weighted_avg_benefits %>%
    filter(type %in% c("cultural"), forcing=="both") %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_nonuse_perha = sum(weighted_avg_benefit_perha, na.rm = TRUE),
        .groups = "drop"
    )

    # Bind the two summaries together
    combined_summary <- full_join(use_market,use_nonmarket, by = c("countrycode", "year"))
    combined_summary <- full_join(combined_summary,non_use, by = c("countrycode", "year"))
    glimpse(combined_summary)

    intermediate_output <- combined_summary %>% 
    mutate(unit_benefit="Int2020$_perkm2_peryear",
    use_market_perkm2 = total_weighted_avg_benefit_use_market_perha*100,
    use_nonmarket_perkm2 = total_weighted_avg_benefit_use_nonmarket_perha*100,
    nonuse_perkm2 = total_weighted_avg_benefit_nonuse_perha*100) %>% 
    dplyr::select(-gdppc,-total_weighted_avg_benefit_nonuse_perha,-total_weighted_avg_benefit_use_nonmarket_perha,-total_weighted_avg_benefit_use_market_perha)

    glimpse(intermediate_output)
    intermediate_output %>% filter(countrycode=="AGO")
    write.csv(intermediate_output,"Data/intermediate_output/mangrove_benefits_per_km2.csv") ## This is sent to F.G. and he will do the regression

    ggplot(intermediate_output,aes(x=year,y=use_market_perkm2))+geom_line(aes(color=countrycode)) + 
    geom_text(data=intermediate_output%>% filter(year==2100, countrycode=="IDN"),aes(x=year,y=use_market_perkm2,label=countrycode))
    
    model_coefficients_use_market <- combined_summary %>%
        group_by(countrycode)%>%
        filter(gdppc > 0,total_weighted_avg_benefit_use_market_perha>0) %>%
        do(intercept = summary(lm(log(total_weighted_avg_benefit_use_market_perha*100) ~ log(gdppc), data = .))$coefficients[1],
                elasticity = summary(lm(log(total_weighted_avg_benefit_use_market_perha*100) ~ log(gdppc), data = .))$coefficients[2],
                intercept_se = summary(lm(log(total_weighted_avg_benefit_use_market_perha*100) ~ log(gdppc), data = .))$coefficients[3],
                elasticity_se = summary(lm(log(total_weighted_avg_benefit_use_market_perha*100) ~ log(gdppc), data = .))$coefficients[4]) %>%
        unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_use_market$units <- "Int2020$_perkm2_peryear"
        model_coefficients <- model_coefficients_use_market
    glimpse(model_coefficients_use_market)
    
    write.csv(model_coefficients_use_market,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\model_coefficients_use_marketv3.csv")


    model_coefficients_nonuse <- combined_summary %>%
        group_by(countrycode)%>%
        filter(gdppc > 0,total_weighted_avg_benefit_nonuse_perha>0) %>%
        do(intercept = summary(lm(log(total_weighted_avg_benefit_nonuse_perha*100) ~ log(gdppc), data = .))$coefficients[1],
                elasticity = summary(lm(log(total_weighted_avg_benefit_nonuse_perha*100) ~ log(gdppc), data = .))$coefficients[2],
                intercept_se = summary(lm(log(total_weighted_avg_benefit_nonuse_perha*100) ~ log(gdppc), data = .))$coefficients[3],
                elasticity_se = summary(lm(log(total_weighted_avg_benefit_nonuse_perha*100) ~ log(gdppc), data = .))$coefficients[4]) %>%
        unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_nonuse$units <- "Int2020$_perkm2_peryear"
        glimpse(model_coefficients_nonuse)
        glimpse(combined_summary)
    write.csv(model_coefficients_nonuse ,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\model_coefficients_nonusev3.csv")

    
    
    model_coefficients_use_nonmarket <- combined_summary %>%
        group_by(countrycode)%>%
        filter(gdppc > 0,total_weighted_avg_benefit_use_nonmarket_perha*100>0) %>%
        do(intercept = summary(lm(log(total_weighted_avg_benefit_use_nonmarket_perha*100) ~ log(gdppc), data = .))$coefficients[1],
                elasticity = summary(lm(log(total_weighted_avg_benefit_use_nonmarket_perha*100) ~ log(gdppc), data = .))$coefficients[2],
                intercept_se = summary(lm(log(total_weighted_avg_benefit_use_nonmarket_perha*100) ~ log(gdppc), data = .))$coefficients[3],
                elasticity_se = summary(lm(log(total_weighted_avg_benefit_use_nonmarket_perha*100) ~ log(gdppc), data = .))$coefficients[4]) %>%
        unnest(c(intercept,elasticity,intercept_se,elasticity_se))
        model_coefficients <- model_coefficients_use_nonmarket 
        model_coefficients_use_nonmarket$units <- "Int2020$_perkm2_peryear"
        
        glimpse(model_coefficients_use_nonmarket )



        glimpse(combined_summary)
        long_df <- combined_summary %>%
            pivot_longer(
                cols = starts_with("total_weighted_avg_benefit"),
                names_to = "category",
                values_to = "total_weighted_avg_benefit"
            ) %>%
        mutate(category = case_when(
            category == "total_weighted_avg_benefit_use_market_perha" ~ "Use Market",
            category == "total_weighted_avg_benefit_nonuse_perha" ~ "Non-use",
            category == "total_weighted_avg_benefit_use_nonmarket_perha" ~ "Use Non-market",
            TRUE ~ as.character(category) # default case
        ))
        glimpse(long_df)
        c_list <- unique(combined_summary$countrycode)
        library("ggsci")

        for(country_interest in c_list) {
            intercept_c_nu <- model_coefficients_nonuse%>% filter(countrycode==country_interest) %>% select(intercept) %>% as.double()
            elas_c_nu <- model_coefficients_nonuse%>% filter(countrycode==country_interest) %>% select(elasticity) %>% as.double()
            intercept_c_um <- model_coefficients_use_market%>% filter(countrycode==country_interest) %>% select(intercept) %>% as.double()
            elas_c_um <- model_coefficients_use_market%>% filter(countrycode==country_interest) %>% select(elasticity) %>% as.double()
            intercept_c_un <- model_coefficients_use_nonmarket%>% filter(countrycode==country_interest) %>% select(intercept) %>% as.double()
            elas_c_un <- model_coefficients_use_nonmarket%>% filter(countrycode==country_interest) %>% select(elasticity) %>% as.double()
            plot <- ggplot(long_df%>% filter(countrycode==country_interest)) +
                geom_point(aes(x=year,y=total_weighted_avg_benefit*100,color=category))+
                geom_line(aes(x=year,y=exp(intercept_c_nu[1]) * gdppc^elas_c_nu[1])) + 
                geom_line(aes(x=year,y=exp(intercept_c_um[1]) * gdppc^elas_c_um[1])) + 
                geom_line(aes(x=year,y=exp(intercept_c_un[1]) * gdppc^elas_c_un[1])) + 
                theme_minimal()+
                ggtitle(paste0("Projection of values for ",country_interest))+
                ylab("Mangrove Benefits") + 
                scale_color_npg()
            ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Value_Function//", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

        }


    write.csv(model_coefficients_use_nonmarket ,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\model_coefficients_use_nonmarketv3.csv")
############################








### Comparing Mangroves v Corals

    
    coral_area <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\corals_area_damage_value_v3.csv")
    glimpse(coral_area)
    glimpse(weighted_avg_benefits_prov)
    glimpse(intermediate_output)

    mangroves_ben <- intermediate_output %>% left_join(coefficients_by_country_sq,by="countrycode")
    glimpse(mangroves_ben)
    mangroves_ben <- mangroves_ben %>% 
    mutate(total_use = MangroveArea_2020_km2*use_nonmarket_perkm2, 
            total_nonuse  = MangroveArea_2020_km2 * nonuse_perkm2)


    mean(mangroves_ben$nonuse_perkm2,na.rm=T)/10^6
    mean(coral_area$nV_value_perkm2year)/10^6

    mean(mangroves_ben$use_market_perkm2)/10^6
    mean(coral_area$nuV_value_perkm2year)/10^6

    median(mangroves_ben$use_market_perkm2)/10^6
    median(coral_area$nuV_value_perkm2year)/10^6

total_plot <-     ggplot()+
    geom_point(data=mangroves_ben %>% filter(year==2030),aes(y=total_use/10^9,x=MangroveArea_2020_km2),color="darkcyan")+
#    geom_point(data=mangroves_ben %>% filter(year==2030),aes(y=total_nonuse,x=MangroveArea_2020_km2),color="darkcyan",shape="triangle")+
    geom_text(data=mangroves_ben %>% filter(year==2030),aes(y=total_use/10^9,x=MangroveArea_2020_km2,label=countrycode),color="darkcyan")+
    geom_point(data=coral_area,aes(x=CoralArea_2020_km2,y=nuV_value_perkm2year*CoralArea_2020_km2/10^9),color="indianred")+    
#    geom_point(data=coral_area,aes(x=CoralArea_2020_km2,y=nV_value_perkm2year*CoralArea_2020_km2),color="indianred",shape="triangle")+

    geom_text(data=coral_area,aes(x=CoralArea_2020_km2,y=nuV_value_perkm2year*CoralArea_2020_km2/10^9,label=countrycode),color="indianred")+
#    scale_x_continuous(trans="log2")+
#    scale_y_continuous(trans="log2")+
    theme_bw() +
    xlab("Area Cover")+
    ylab("Annual Ecosystem Services (Billion 2020 USD)")
    

per_area_plot <-    ggplot()+
    geom_point(data=mangroves_ben %>% filter(year==2030),aes(y=use_nonmarket_perkm2/10^6,x=MangroveArea_2020_km2),color="darkcyan")+
#    geom_point(data=mangroves_ben %>% filter(year==2030),aes(y=total_nonuse,x=MangroveArea_2020_km2),color="darkcyan",shape="triangle")+
    geom_text(data=mangroves_ben %>% filter(year==2030),aes(y=use_nonmarket_perkm2/10^6,x=MangroveArea_2020_km2,label=countrycode),color="darkcyan")+
    geom_point(data=coral_area,aes(x=CoralArea_2020_km2,y=nuV_value_perkm2year/10^6),color="indianred")+    
#    geom_point(data=coral_area,aes(x=CoralArea_2020_km2,y=nV_value_perkm2year*CoralArea_2020_km2),color="indianred",shape="triangle")+

    geom_text(data=coral_area,aes(x=CoralArea_2020_km2,y=nuV_value_perkm2year/10^6,label=countrycode),color="indianred")+
#    scale_x_continuous(trans="log2")+
#    scale_y_continuous(trans="log2")+
    theme_bw() +
    xlab("Area Cover")+
    ylab("Ecosystem Services Value per Area (Million 2020 USD/km2)")

ggarrange(per_area_plot+ggtitle("Per-Area Benefits"),total_plot+ggtitle("Total Benefits"),total_plot+scale_x_continuous(trans="log")+scale_y_continuous(trans="log")+ggtitle("Total Benefits Log Scale"),ncol=3)    
