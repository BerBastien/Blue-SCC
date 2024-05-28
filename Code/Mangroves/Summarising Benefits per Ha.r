#Summarising Benefits per Ha

    library(scico)
    library(tidyr)
    library(dplyr)
    library(rlang)
    library(dplyr)
    library(purrr)

    scen_allforcings_allES <- read.csv(file="Data/modules/mangroves/scen_allforcings_allES_ssp570.csv")
    glimpse(scen_allforcings_allES)
    unique(scen_allforcings_allES$countrycode)
    
    scen_allforcings_allES %>% filter(countrycode=="NLD", year==2020) %>% dplyr::select("gridcell_id")
    scen_allforcings_allES %>% filter(countrycode=="SUR", year==2020) %>% dplyr::select("gridcell_id")
    scen_allforcings_allES %>% filter(countrycode=="GBR", year==2020) %>% dplyr::select("gridcell_id") %>% unique()
    scen_allforcings_allES %>% filter(countrycode=="FRA", year==2020) %>% dplyr::select("gridcell_id") %>% unique()

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
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP585_magicc_202310021547.csv")
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP370_magicc_202311031621.csv")
    glimpse(ssp_temp)
    glimpse(ssp_gdp)
    unique(ssp_temp$variable)

        
    ssp_temp %>% filter(variable=="Surface Temperature")
    ssp_gdp$countrycode <- ssp_gdp$ISO3
    glimpse(diff_country_total)

    library(tidyverse)

    ssp_temp_long <- ssp_temp %>%
    tidyr::pivot_longer(
        cols = starts_with("X"),
        names_to = "year",
        values_to = "value"
    ) %>%
    # Remove the "X" prefix from the year column and convert to numeric
    mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")



    ssp_temp_long$temp2025 <- ssp_temp_long %>% filter(year==2025) %>% dplyr::select(value) %>% unlist()
    glimpse(ssp_temp_long)
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2025
    #ssp_temp_long$countrycode <- ssp_temp_long$region 

    glimpse(ssp_gdp)
    glimpse(diff_country_total1)
    diff_country_total <- rbind(diff_country_total1 %>% rename("POP_Country"="Pop_Country_ssp1","GDP_Country"="GDP_Country_ssp1","POP"="POP_SSP1","GDP"="GDP_SSP1") %>% mutate(scenario="SSP1"),
                                diff_country_total2 %>% rename("POP_Country"="Pop_Country_ssp2","GDP_Country"="GDP_Country_ssp2","POP"="POP_SSP2","GDP"="GDP_SSP2") %>% mutate(scenario="SSP2"),
                                diff_country_total3 %>% rename("POP_Country"="Pop_Country_ssp3","GDP_Country"="GDP_Country_ssp3","POP"="POP_SSP3","GDP"="GDP_SSP3") %>% mutate(scenario="SSP3"),
                                diff_country_total4 %>% rename("POP_Country"="Pop_Country_ssp4","GDP_Country"="GDP_Country_ssp4","POP"="POP_SSP4","GDP"="GDP_SSP4") %>% mutate(scenario="SSP4"),
                                diff_country_total5 %>% rename("POP_Country"="Pop_Country_ssp5","GDP_Country"="GDP_Country_ssp5","POP"="POP_SSP5","GDP"="GDP_SSP5") %>% mutate(scenario="SSP5"))
    glimpse(diff_country_total)
        glimpse(diff_country_total)
        glimpse(ssp_gdp)
        glimpse(ssp_temp_long)

    levels(factor(ssp_gdp$countrycode))    
    levels(factor(diff_country_total$countrycode))    
    benefit_ssp <- merge(ssp_gdp,diff_country_total,by=c("countrycode","year","scenario"),all=T)
    benefit_ssp <- merge(benefit_ssp ,ssp_temp_long,by=c("year"),all=F) %>% filter(year>2025)
    benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)
    glimpse(benefit_ssp)

    
    scen_arealoss_perc_both <- read.csv(file="Data/modules/mangroves/Proj_Area_Perc_both_SSP370.csv")
    grid_with_countries_ALL <- read.csv("Data/modules/mangroves/grid_with_specific_countries_FINAL.csv")
    scen_arealoss_perc_both <- scen_arealoss_perc_both %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_arealoss_perc_both$countrycode_old <-scen_arealoss_perc_both$countrycode
    scen_arealoss_perc_both$countrycode <- scen_arealoss_perc_both$iso_a3
    
    glimpse(scen_arealoss_perc_both)
    
    unique(scen_arealoss_perc_both$countrycode)
    
    glimpse(diff_country_total3)
    benefit_ssp3 <- benefit_ssp %>% filter(scenario.x=="SSP3")
    glimpse(benefit_ssp3)

    ggplot(benefit_ssp3 %>% filter(countrycode=="MEX"))+
    geom_line(aes(x=year,y=diff_mangrove_area_future_loss))
    
    country_area_mangrove_2020 <- scen_arealoss_perc_both %>% filter(year==2026) %>%
                            group_by(countrycode) %>% 
                            summarize(area_2020 = sum(mangrove_area2020,na.rm=T))

    glimpse(country_area_mangrove_2020)
    glimpse(scen_arealoss_perc_both)
    country_area_mangrove_2020 %>% filter(countrycode=="MEX")
   
    benefit_ssp3 <- merge(benefit_ssp3 ,country_area_mangrove_2020,by=c("countrycode"),all=F) 
    glimpse(benefit_ssp3)

    benefit_ssp3$frac_loss <- benefit_ssp3$diff_mangrove_area_future_loss/benefit_ssp3$area_2020

    ggplot(benefit_ssp3 %>% filter(countrycode=="MYT"))+
    geom_line(aes(x=year,y=frac_loss))
    
  coefficients_by_country <- benefit_ssp3 %>% filter(!is.na(frac_loss)) %>%
        #mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
        group_by(countrycode) %>%
        do({
            model_area <- lm(frac_loss ~ 0 + temp, data = .)
            data.frame(coefficient_temp = coef(model_area)["temp"],
            coefficient_temp_se = summary(model_area)$coefficients[2],
            r_sq = summary(model_area)$r.squared
            )
        }) %>% ungroup()

    coefficients_by_country %>% filter(countrycode=="FRA")
    
    glimpse(coefficients_by_country)
    glimpse(country_area_mangrove_2020)

    coefficients_by_country <-  merge(coefficients_by_country,country_area_mangrove_2020,by=c("countrycode"),all=F)
    names(coefficients_by_country)[which(names(coefficients_by_country)=="area_2020")] <- "MangroveArea_2020_hectares" #Changing the name first
    coefficients_by_country$MangroveArea_2020_hectares <- coefficients_by_country$MangroveArea_2020_hectares*100 #Now converting it ti hectares
    
    write.csv(coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_v4March2024.csv")
    coefficients_by_country <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_v4March2024.csv")
    
    glimpse(coefficients_by_country)

    coefficients_by_country %>% filter(countrycode %in% c("MEX","NLD"))

    coefficients_by_country$MangroveArea_2020_km2 <- 0.01*coefficients_by_country$MangroveArea_2020_hectares
     coefficients_by_country <- coefficients_by_country %>% dplyr::select(-X,-MangroveArea_2020_hectares) %>% 
        rename(FractionChange_perC = coefficient_temp,
        FractionChange_perC_se = coefficient_temp_se)

    write.csv(coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_v4Mar2024.csv")
    coefficients_by_country_linear <- coefficients_by_country
    ## PLOTS
                glimpse(benefit_ssp3)
                glimpse(coefficients_by_country)
        
        c_list <- unique(benefit_ssp3$countrycode)
        library("ggsci")

        for(country_interest in c_list) {
            coef_area_c <- coefficients_by_country %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC) %>% as.double()
            se_area_c <- coefficients_by_country %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_se) %>% as.double()
            
            plot <- ggplot(benefit_ssp3 %>% filter(countrycode==country_interest)) +
                geom_point(aes(x=year,y=frac_loss))+
                geom_line(aes(x=year,y=temp*coef_area_c)) +
                geom_ribbon(aes(x=year, ymin = temp*(coef_area_c-se_area_c*1.654), ymax = temp*(coef_area_c+se_area_c*1.654)), fill = "grey80", alpha = 0.5) +
                theme_minimal()+
                ggtitle(paste0("Projection of values for ",country_interest))+
                ylab("Mangrove Area Loss") 
            plot
            ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Area_Damage//", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

        }

        
        ggplot(intermediate_output,aes(x=year,y=use_market_perkm2))+geom_line(aes(color=countrycode)) + 
        geom_text(data=intermediate_output%>% filter(year==2100, countrycode=="IDN"),aes(x=year,y=use_market_perkm2,label=countrycode))

        benefit_ssp3 %>% filter(year==2030, countrycode=="IDN")
        ggplot(benefit_ssp3 %>% filter(year==2030)) + 
        geom_text(aes(x=frac_loss,y=area_2020, label=countrycode))

    ## PLOTS

    coefficients_by_country_sq <- benefit_ssp3 %>% filter(!is.na(frac_loss)) %>%
        #mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
        group_by(countrycode) %>%
        do({
            model_area <- lm(frac_loss ~ 0 + temp + I(temp^2), data = .)
            data.frame(coefficient_temp = coef(model_area)["temp"],
            coefficient_temp_se = summary(model_area)$coefficients[3],
            sq_coefficient_temp = coef(model_area)["I(temp^2)"],
            sq_coefficient_temp_se = summary(model_area)$coefficients[4],
            r_sq = summary(model_area)$r.squared
            )
        }) %>% ungroup()
    
    coefficients_by_country_sq <-  merge(coefficients_by_country_sq,country_area_mangrove_2020,by=c("countrycode"),all=F)
    names(coefficients_by_country_sq)[which(names(coefficients_by_country_sq)=="area_2020")] <- "MangroveArea_2020_hectares" #Changing the name first
    coefficients_by_country_sq$MangroveArea_2020_hectares <- coefficients_by_country_sq$MangroveArea_2020_hectares*100 #Now converting it ti hectares
    
    write.csv(coefficients_by_country_sq,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v4March2024.csv")
    coefficients_by_country_sq <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v4March2024.csv")
    
    glimpse(coefficients_by_country_sq)


    coefficients_by_country_sq$MangroveArea_2020_km2 <- 0.01*coefficients_by_country_sq$MangroveArea_2020_hectares
     coefficients_by_country_sq <- coefficients_by_country_sq %>% dplyr::select(-X,-MangroveArea_2020_hectares) %>% 
        rename(FractionChange_perC = coefficient_temp,
        FractionChange_perC_se = coefficient_temp_se,
        FractionChange_perC_sq = sq_coefficient_temp,
        FractionChange_perC_sq_se = sq_coefficient_temp_se)

    coefficients_by_country_sq %>% filter(countrycode=="IDN") 
    coefficients_by_country_sq %>% filter(countrycode=="BRA") 
    coefficients_by_country_sq %>% summarise(sum(MangroveArea_2020_km2,na.rm=T)) #113385.1
    25000/113385.1
    10653.16/113385.1
    
    write.csv(coefficients_by_country_sq,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v4Mar2024.csv")
    coefficients_by_country_linear$fun <- "linear"
    coefficients_by_country_sq$fun <- "square"
    coefs_lin_sq <- rbind(coefficients_by_country_linear %>% dplyr::select(r_sq,fun),coefficients_by_country_sq %>% dplyr::select(r_sq,fun))
    coefficients_by_country_sq <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v4Mar2024.csv")
    

    ## PLOTS
        c_list <- unique(benefit_ssp3$countrycode)
        library("ggsci")

        for(country_interest in c_list) {
            coef_area_c <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC) %>% as.double()
            se_area_c <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_se) %>% as.double()
            coef_area_c_sq <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_sq) %>% as.double()
            se_area_c_sq <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_sq_se) %>% as.double()
            
            plot <- ggplot(benefit_ssp3 %>% filter(countrycode==country_interest)) +
                geom_point(aes(x=year,y=frac_loss))+
                geom_line(aes(x=year,y=temp*coef_area_c+coef_area_c_sq*temp^2)) +
                geom_ribbon(aes(x=year, ymin = temp*(coef_area_c-se_area_c*1.654)+(coef_area_c_sq-se_area_c_sq*1.654)*temp^2, 
                    ymax = temp*(coef_area_c+se_area_c*1.654)+(coef_area_c_sq+se_area_c_sq*1.654)*temp^2), fill = "grey80", alpha = 0.5) +
                theme_minimal()+
                ggtitle(paste0("Projection of values for ",country_interest))+
                ylab("Mangrove Area Loss") 
            plot
            ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Area_Damage//sq_", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

        }

        ggplot(coefs_lin_sq) + 
        geom_histogram(aes(x=r_sq,fill=fun),position="dodge") + theme_minimal()
        ggsave(filename = "Figures/all_figures/mangroves/Country_Area_Damage//00_rSQ.jpg")


        coefs_lin_sq %>% group_by(fun) %>% summarise(mean_sq=mean(r_sq,na.rm=T), median_sq=median(r_sq,na.rm=T))

    ## PLOTS
    


    coefficients_by_country %>% filter(countrycode=="CUB")
    library("rnaturalearthdata")
    library("rnaturalearth")
    world_ne <- ne_countries(scale = "medium", returnclass = "sf")

    world_ne_with_coeffs <-  merge(world_ne,coefficients_by_country,by.x="iso_a3",by.y="countrycode",all=T)
    glimpse(world_ne_with_coeffs)

   world_ne_with_coeffs %>% filter(iso_a3=="GBR")
    
    library("scales")
    gg <- ggplot(data = world_ne_with_coeffs) +
    geom_sf(aes(fill = FractionChange_perC)) +
    scale_fill_scico(palette = "lajolla",na.value="transparent",
        begin=0.1,end=0.9,direction=1) +
    labs(fill = "Area Damage \n(Fraction per Degree C)") +
    
    theme_bw()+
            coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))

    gg
    
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_lin_march2024.png")


## Area Change


## Second APPROACH GDP per capita

    glimpse(scen_allforcings_allES)
    unique(scen_allforcings_allES$countrycode)
    scen_allforcings_allES %>% filter(year==2030,type=="provision",forcing=="onlyCC") %>%
    filter(countrycode=="MYT") %>% 
    summarize(area=sum(mangrove_area,na.rm=T))
    
    weighted_avg_benefits <- scen_allforcings_allES %>% filter(year>2025) %>%
    group_by(countrycode, year,type,forcing) %>%
    mutate(gdppc = (1.59*GDP_Country_ssp5*10^9)/(Pop_Country_ssp5*10^6)) %>% # GDP is in billion 2005 USD per year, Pop is in million #Cumulative Inflation Global from 2005 to 2020: 59.1 from https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG
    #mutate(gdppc = Pop_Country_ssp1*10^6) %>%
    #mutate(gdppc = GDP_Country_ssp1*10^9) %>%
    mutate(benefits_perha_percGDP = (100*benefits_perha)/(1.59*GDP_Country_ssp5*10^9)) %>%
    summarize(
        weighted_avg_benefit_perha = sum(benefits_perha * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        weighted_avg_benefit_perha_percGDP = sum(benefits_perha_percGDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    glimpse(weighted_avg_benefits)
    mean(weighted_avg_benefits$gdppc, na.rm=T)
    weighted_avg_benefits %>% filter(type=="provision",forcing=="both",countrycode=="MEX") %>% as.data.frame()
    
    glimpse(coefficients_by_country_sq)
    glimpse(ssp_temp_long)
    
    # Market damage Function
    weighted_avg_benefits_prov <- weighted_avg_benefits %>% filter(type=="provision",forcing=="both") %>%
    left_join(coefficients_by_country_sq, by="countrycode") %>%
    left_join(ssp_temp_long %>% dplyr::select(temp,year), by="year") %>%
    mutate(prov_benefits_undamaged = 100* weighted_avg_benefit_perha_percGDP * MangroveArea_2020_km2, 
            prov_benefits_damaged = 100* weighted_avg_benefit_perha_percGDP * (MangroveArea_2020_km2 * (1+FractionChange_perC*temp+FractionChange_perC_sq*temp^2)) ) %>%
    mutate(percentage_points_damaged =  prov_benefits_damaged - prov_benefits_undamaged)


    glimpse(weighted_avg_benefits_prov)   

    ggplot(weighted_avg_benefits_prov) + 
    geom_line(aes(x=year,y=percentage_points_damaged,color=countrycode)) +
    geom_text(data=weighted_avg_benefits_prov %>% filter(year==2100), aes(x=year,y=percentage_points_damaged,color=countrycode,label=countrycode))
    
    coefficients_by_country_sq <- benefit_ssp3 %>% filter(!is.na(frac_loss)) %>%
        #mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
        group_by(countrycode) %>%
        do({
            model_area <- lm(frac_loss ~ 0 + temp + I(temp^2), data = .)
            data.frame(coefficient_temp = coef(model_area)["temp"],
            coefficient_temp_se = summary(model_area)$coefficients[3],
            sq_coefficient_temp = coef(model_area)["I(temp^2)"],
            sq_coefficient_temp_se = summary(model_area)$coefficients[4],
            r_sq = summary(model_area)$r.squared
            )
        }) %>% ungroup()
    
    
    # # Summarize for "water" + "coastal"
    # water_coastal <- weighted_avg_benefits %>%
    # filter(type %in% c("water", "coastal")) %>%
    # group_by(countrycode, year) %>%
    # summarize(
    #     total_weighted_avg_benefit_wc = sum(weighted_avg_benefit, na.rm = TRUE),
    #     gdppc = first(gdppc),
    #     .groups = "drop"
    # )

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
    glimpse()
    
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

    
    write.csv(intermediate_output,"Data/modules/mangroves/benefits_per_km2.csv")

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




















    model_coefficients_use_values <- combined_summary %>%
    group_by(countrycode)%>%
    filter(gdppc > 0) %>%
    do(intercept = summary(lm(log(avg_benefit_perha) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(avg_benefit_perha) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(avg_benefit_perha) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(avg_benefit_perha) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients <- model_coefficients_use_values
    glimpse(model_coefficients_use_values)

    write.csv(model_coefficients_use_values,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_usevalue_coeffs.csv")

    # Fit log-log model for total_weighted_avg_benefit_wc
    model_coefficients_regulating_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_wc > 0, gdppc > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_wc <- model_coefficients_regulating_mangroves

    model_coefficients_provisioning_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_fw > 0, gdppc > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_fw <- model_coefficients_provisioning_mangroves
    glimpse(model_coefficients_provisioning_mangroves)
    glimpse(model_coefficients_regulating_mangroves)

    


    ## PLOT (START)
        # Select a specific country, e.g., Australia (AUS)
        country_code <- "MEX"

        # Filter the data for the selected country
        data_country <- combined_summary %>%
        filter(countrycode == country_code, avg_benefit_perha > 0, gdppc > 0)

        # Get the model coefficients for the country
        coeffs <- model_coefficients_use_values %>%
        filter(countrycode == country_code)

        # Create a sequence of gdppc values for plotting
        gdppc_range <- seq(min(data_country$gdppc), max(data_country$gdppc), length.out = 100)

        # Calculate predicted values
        predicted_values <- exp(coeffs$intercept + coeffs$elasticity * log(gdppc_range))

        # Create a data frame for the predicted curve
        predicted_data <- data.frame(gdppc = gdppc_range, avg_benefit_perha = predicted_values)

        # Plotting
        ggplot() +
        geom_point(data = data_country, aes(x = gdppc, y = avg_benefit_perha), colour = "blue") +
        geom_line(data = predicted_data, aes(x = gdppc, y = avg_benefit_perha), colour = "red") +
        labs(title = paste("Comparison of Actual Data and Estimated Curve for", country_code),
            x = "GDP per capita (gdppc)",
            y = "Total Weighted Average Benefit WC") +
        theme_minimal()
    ## PLOT (END)
## Second APPROACH

## Second APPROACH GDP per capita
    weighted_avg_benefits <- scen_allforcings_allES %>% filter(year>2025) %>%
    group_by(countrycode, year,type) %>%
    mutate(gdppc = (GDP_Country_ssp3*10^9)/(Pop_Country_ssp3*10^6)) %>%
    #mutate(gdppc = Pop_Country_ssp1*10^6) %>%
    #mutate(gdppc = GDP_Country_ssp1*10^9) %>%
    mutate(benefits_perha_percGDP = (100*benefits_perha)/(GDP_Country_ssp1*10^9)) %>%
    summarize(
        weighted_avg_benefit = sum(benefits_perha * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        weighted_avg_benefit_percGDP = sum(benefits_perha_percGDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    # Summarize for "water" + "coastal"
    water_coastal <- weighted_avg_benefits %>%
    filter(type %in% c("water", "coastal")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_wc = sum(weighted_avg_benefit_percGDP, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    # Summarize for "food" + "wood"
    food_wood <- weighted_avg_benefits %>%
    filter(type %in% c("food", "wood")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_fw = sum(weighted_avg_benefit_percGDP, na.rm = TRUE),
        .groups = "drop"
    )

    # Bind the two summaries together
    combined_summary <- full_join(water_coastal, food_wood, by = c("countrycode", "year"))
    glimpse(combined_summary)



    # Fit log-log model for total_weighted_avg_benefit_wc
    model_coefficients_regulating_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_wc > 0, gdppc > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_wc <- model_coefficients_regulating_mangroves

    model_coefficients_provisioning_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_fw > 0, gdppc > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_fw <- model_coefficients_provisioning_mangroves



    ## PLOT (START)
        # Select a specific country, e.g., Australia (AUS)
        country_code <- "MEX"

        # Filter the data for the selected country
        data_country <- combined_summary %>%
        filter(countrycode == country_code, total_weighted_avg_benefit_wc > 0, gdppc > 0)

        # Get the model coefficients for the country
        coeffs <- model_coefficients_wc %>%
        filter(countrycode == country_code)

        # Create a sequence of gdppc values for plotting
        gdppc_range <- seq(min(data_country$gdppc), max(data_country$gdppc), length.out = 100)

        # Calculate predicted values
        predicted_values <- exp(coeffs$intercept + coeffs$elasticity * log(gdppc_range))

        # Create a data frame for the predicted curve
        predicted_data <- data.frame(gdppc = gdppc_range, total_weighted_avg_benefit_wc = predicted_values)

        # Plotting
        ggplot() +
        geom_point(data = data_country, aes(x = gdppc, y = total_weighted_avg_benefit_wc), colour = "blue") +
        geom_line(data = predicted_data, aes(x = gdppc, y = total_weighted_avg_benefit_wc), colour = "red") +
        labs(title = paste("Comparison of Actual Data and Estimated Curve for", country_code),
            x = "GDP per capita (gdppc)",
            y = "Total Weighted Average Benefit WC") +
        theme_minimal()
    ## PLOT (END)
## Second APPROACH



## ThirdAPPROACH / ratios
    weighted_avg_ratios <- scen_allforcings_allES %>% filter(year>2025) %>%
    group_by(countrycode, year) %>%
    #mutate(gdppc = (GDP_Country_ssp1*10^9)/(Pop_Country_ssp1*10^6)) %>%
    #mutate(gdppc = Pop_Country_ssp1*10^6) %>%
    mutate(ratio_GDP = GDP_SSP3/(GDP_Country_ssp3*10^9), 
           ratio_Pop = POP_SSP3/(Pop_Country_ssp3*10^6)  ) %>%
    summarize(
        weighted_avg_ratio_GDP = sum(ratio_GDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        weighted_avg_ratio_Pop = sum(ratio_Pop* mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        #gdppc = first(gdppc),
        .groups = "drop"
    )

    glimpse(weighted_avg_ratios)

    ggplot(weighted_avg_ratios)+
    #geom_point(aes(x=year,y=weighted_avg_ratio_GDP,color=countrycode))
    geom_point(aes(x=year,y=weighted_avg_ratio_Pop,color=countrycode))+
    geom_text(data = weighted_avg_ratios %>% filter(year==2100),aes(x=year+10,y=weighted_avg_ratio_Pop,color=countrycode,label=countrycode))

    
    ggplot(weighted_avg_ratios)+
    geom_line(aes(x=year,y=weighted_avg_ratio_GDP,color=countrycode))+
    #geom_point(aes(x=year,y=weighted_avg_ratio_Pop,color=countrycode))+
    geom_text(data = weighted_avg_ratios %>% filter(year==2100),aes(x=year+10,y=weighted_avg_ratio_GDP,color=countrycode,label=countrycode))
    
    mean_value_perha <- mean(weighted_avg_benefits$weighted_avg_benefit_percGDP[which(weighted_avg_benefits$countrycode=="MEX")],na.rm=TRUE)
    mangrove_area_MEx <- scen_allforcings_allES %>% filter(year==2025 & countrycode=="MEX") %>% summarize(area = sum(mangrove_area,na.rm=TRUE))
    max_value_perha <- max(weighted_avg_benefits$weighted_avg_benefit_percGDP[which(weighted_avg_benefits$countrycode=="MEX")],na.rm=TRUE)
    min_value_perha <- min(weighted_avg_benefits$weighted_avg_benefit_percGDP[which(weighted_avg_benefits$countrycode=="MEX")],na.rm=TRUE)
    mean_value_perha * mangrove_area_MEx*100
    max_value_perha * mangrove_area_MEx*100
    min_value_perha * mangrove_area_MEx*100


    # # Check the resulting dataframe
    glimpse(weighted_avg_benefits)
    ggplot(weighted_avg_benefits)+
    # geom_line(aes(x=year,y=weighted_avg_benefit,col=countrycode,linetype=type))
    geom_line(aes(x=year,y=weighted_avg_benefit_percGDP,col=countrycode,linetype=type))


    # Summarize for "water" + "coastal"
    water_coastal <- weighted_avg_benefits %>%
    filter(type %in% c("water", "coastal")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_wc = sum(weighted_avg_benefit_percGDP, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    # Summarize for "food" + "wood"
    food_wood <- weighted_avg_benefits %>%
    filter(type %in% c("food", "wood")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_fw = sum(weighted_avg_benefit_percGDP, na.rm = TRUE),
        .groups = "drop"
    )

    # Bind the two summaries together
    combined_summary <- full_join(water_coastal, food_wood, by = c("countrycode", "year"))
    glimpse(combined_summary)



    # Fit log-log model for total_weighted_avg_benefit_wc
    model_coefficients_regulating_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_wc > 0, gdppc > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_wc <- model_coefficients_regulating_mangroves

    model_coefficients_provisioning_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_fw > 0, gdppc > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdppc), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_fw <- model_coefficients_provisioning_mangroves



    ## PLOT (START)
        # Select a specific country, e.g., Australia (AUS)
        country_code <- "MEX"

        # Filter the data for the selected country
        data_country <- combined_summary %>%
        filter(countrycode == country_code, total_weighted_avg_benefit_wc > 0, gdppc > 0)

        # Get the model coefficients for the country
        coeffs <- model_coefficients_wc %>%
        filter(countrycode == country_code)

        # Create a sequence of gdppc values for plotting
        gdppc_range <- seq(min(data_country$gdppc), max(data_country$gdppc), length.out = 100)

        # Calculate predicted values
        predicted_values <- exp(coeffs$intercept + coeffs$elasticity * log(gdppc_range))

        # Create a data frame for the predicted curve
        predicted_data <- data.frame(gdppc = gdppc_range, total_weighted_avg_benefit_wc = predicted_values)

        # Plotting
        ggplot() +
        geom_point(data = data_country, aes(x = gdppc, y = total_weighted_avg_benefit_wc), colour = "blue") +
        geom_line(data = predicted_data, aes(x = gdppc, y = total_weighted_avg_benefit_wc), colour = "red") +
        labs(title = paste("Comparison of Actual Data and Estimated Curve for", country_code),
            x = "GDP per capita (gdppc)",
            y = "Total Weighted Average Benefit WC") +
        theme_minimal()
    ## PLOT (END)
## Third APPROACH