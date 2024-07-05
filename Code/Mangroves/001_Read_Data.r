#001_Read_Data


#Summarising Benefits per Ha


    # Gridcell-level mangrove projections (start)
        scen_allforcings_allES <- read.csv(file="Data/input_modules/mangroves/scen_allforcings_allES_ssp270.csv")
        glimpse(scen_allforcings_allES)
    # Gridcell-level mangrove projections (end)


## Area Change

    ## Read country-level mangrove projections (start)
    
    
        diff_country_total <- read.csv(file="Data/input_modules/mangroves/diff_country_total_ssp370_brander_corrected_countries.csv")
        diff_country_total1 <- read.csv(file="Data/input_modules/mangroves/diff_country_total_ssp170_brander_corrected_countries.csv")
        diff_country_total2 <- read.csv(file="Data/input_modules/mangroves/diff_country_total_ssp270_brander_corrected_countries.csv")
        diff_country_total3 <- read.csv(file="Data/input_modules/mangroves/diff_country_total_ssp370_brander_corrected_countries.csv")
        diff_country_total4 <- read.csv(file="Data/input_modules/mangroves/diff_country_total_ssp470_brander_corrected_countries.csv")
        diff_country_total5 <- read.csv(file="Data/input_modules/mangroves/diff_country_total_ssp570_brander_corrected_countries.csv")

        glimpse(diff_country_total1)
        
        diff_country_total <- rbind(diff_country_total1 %>% dplyr::rename("POP_Country"="Pop_Country_ssp1","GDP_Country"="GDP_Country_ssp1","POP"="POP_SSP1","GDP"="GDP_SSP1") %>% mutate(scenario="SSP1"),
                                diff_country_total2 %>% dplyr::rename("POP_Country"="Pop_Country_ssp2","GDP_Country"="GDP_Country_ssp2","POP"="POP_SSP2","GDP"="GDP_SSP2") %>% mutate(scenario="SSP2"),
                                diff_country_total3 %>% dplyr::rename("POP_Country"="Pop_Country_ssp3","GDP_Country"="GDP_Country_ssp3","POP"="POP_SSP3","GDP"="GDP_SSP3") %>% mutate(scenario="SSP3"),
                                diff_country_total4 %>% dplyr::rename("POP_Country"="Pop_Country_ssp4","GDP_Country"="GDP_Country_ssp4","POP"="POP_SSP4","GDP"="GDP_SSP4") %>% mutate(scenario="SSP4"),
                                diff_country_total5 %>% dplyr::rename("POP_Country"="Pop_Country_ssp5","GDP_Country"="GDP_Country_ssp5","POP"="POP_SSP5","GDP"="GDP_SSP5") %>% mutate(scenario="SSP5"))

    ## Read country-level mangrove projections (end)

    
    ##  Merge with GDP (start)

        ssp_gdp <- read.csv(file=paste0(dir_box,'\\SSPs\\ssp_gdp.csv'))
        ssp_temp <- read.csv(file=paste0(dir_box,"\\SSPs\\CO2Pulse\\SSP585_magicc_202310021547.csv"))
        ssp_temp <- read.csv(file=paste0(dir_box,"\\SSPs\\CO2Pulse\\SSP370_magicc_202311031621.csv"))
        countries_in_ssps <- unique(ssp_gdp$ISO3)            
            
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
    
        benefit_ssp <- merge(ssp_gdp,diff_country_total,by=c("countrycode","year","scenario"),all=T)
        benefit_ssp <- merge(benefit_ssp ,ssp_temp_long,by=c("year"),all=F) %>% filter(year>2025)
        benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)
        glimpse(benefit_ssp)
    ##  Merge with GDP (start)




    ## Read Area loss (start)
    
    
        scen_arealoss_perc_both <- read.csv(file="Data/input_modules/mangroves/Proj_Area_Perc_onlyCC_SSP270.csv")
        grid_with_countries_ALL <- read.csv("Data/input_modules/mangroves/grid_with_specific_countries_FINAL.csv")
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

    ## Read Area loss (end)
