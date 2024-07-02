## Read Cheung et al Fig. 4 https://www.nature.com/articles/s41558-023-01822-1
## Change in nutrient availbaility

## Read Data (start)
    datadir <- "Data\\modules\\fish\\nutrition\\"
    nut_proj <- read.csv(paste0(datadir,"Figure4_Cheung_etal_2024.csv"))
    nut_proj_hi <- read.csv(paste0(datadir,"Cheung_etal_2024_Figure4_high_income.csv")) 
    nut_proj_li <- read.csv(paste0(datadir,"Cheung_etal_2024_Figure4_low_income.csv")) 
    glimpse(nut_proj_li)    
    nut_proj_long <- rbind(nut_proj_li %>% mutate(income="Low Income"), nut_proj_hi %>% mutate(income="High Income")) %>%
    pivot_longer(cols = c(Protein, Calcium, Omega3, Iron), 
                names_to = "nutrient", 
                values_to = "Value")

    # View the transformed dataframe
    glimpse(nut_proj_long)
    #low-income: below median GDP per capita
## Read Data (end)


## nutrient Damage functions coefficients (start)


    nut_tcoeff <- nut_proj_long%>%
        
        group_by(nutrient,income) %>%
        nest() %>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(Value ~ 0 + atmoT | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(Value ~ 0 + atmoT | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(Value ~ 0 + atmoT | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
             })
            #, 
            # living_coral_cover = map_dbl(data, ~{
            # mod <- lm(cover ~1, .x)
            # summary(mod)$coef[1]
            # })
        ) %>%
        unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(nutrient,income,tcoeff,se,pval) %>% as.data.frame()

        glimpse(nut_tcoeff)

        nut_proj_long_coeff <- nut_proj_long %>% left_join(nut_tcoeff,by=c("nutrient","income"))
        glimpse(nut_proj_long_coeff)
        #write.csv(nut_proj_long_coeff,"Data/Modules/fish/Nutrition/nut_proj_long_coeff.csv")
            

## nutrient Damage Fucntion Coefficients (end)


## Read Relative Risk increase dose-response function (start)

    nut_eff <- read.csv(paste0(datadir,"effect_nut.csv"))
    glimpse(nut_eff)

        # Function to extract the lower and upper bounds and calculate SE
            calculate_se <- function(ci) {
                bounds <- str_extract_all(ci, "\\d+\\.*\\d*")[[1]]
                lower <- as.numeric(bounds[1])
                upper <- as.numeric(bounds[2])
                se <- (upper - lower) / (2 * 1.96)
                return(se)
                }
    
    nut_eff <- nut_eff %>%
        mutate(se = sapply(X95_CI, calculate_se))
  
    nut_eff_sum <- nut_eff %>% filter(significant==1,measuring.RR==1,gbd==1,effect>0) %>% group_by(nutrient,condition_gbd) %>% 
    summarize(
        total_effect = sum(effect, na.rm = TRUE), 
        total_se = sqrt(sum(se^2, na.rm = TRUE))
    ) %>% ungroup()
    glimpse(nut_eff_sum)

    nut_eff_sum <- nut_eff_sum %>% left_join(nut_tcoeff,by="nutrient") %>% filter(!is.na(tcoeff))

    nut_eff_sum_avg <-  nut_eff_sum %>% 
        group_by(income) %>% 
        summarize(
            effectweighted_avg_tcoeff=sum(total_effect*tcoeff)/sum(total_effect),
            effectweighted_avg_tcoeff_se=sqrt(sum((total_effect*se)^2)/sum(total_effect)^2)
        ) #new_data 
    
    glimpse(nut_eff_sum_avg)
    ## Join with SSPs Temp, GDP and Pop Data (start)
        ## Join GDP, TEMP and POP (start)
            glimpse(ssp_gdp)
            glimpse(ssp_pop)
            ssp_gdp_pop <- ssp_pop %>% left_join(ssp_gdp,by=c("scenario","ISO3","year")) %>% rename(ssp=scenario)
            glimpse(ssp_gdp_pop)
            
            T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
            T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
            T_ssp126 <- read.csv("Data/scenarios/SSP126_magicc_202308040902.csv")
            T_ssp460 <- read.csv("Data/scenarios/SSP460_magicc_202402051249.csv")

            temp1 <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
            temp1$year <- as.integer(sub('X', '', temp1$year))
            names(temp1)[1] <- "temp"
            temp1$rcp <- "RCP45"
            temp1$ssp <- "SSP2"

            temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
            temp2$year <- as.integer(sub('X', '', temp2$year))
            names(temp2)[1] <- "temp"
            temp2$rcp <- "RCP85"
            temp2$ssp <- "SSP5"


            temp3 <- data.frame(temp = t(T_ssp126[7,c(13:length(T_ssp126))]), year = names(T_ssp126[17,c(13:length(T_ssp126))]))
            glimpse(temp3)
            temp3$year <- as.integer(sub('X', '', temp3$year))
            names(temp3)[1] <- "temp"
            temp3$rcp <- "RCP26"
            temp3$ssp <- "SSP1"


            temp4 <- data.frame(temp = t(T_ssp460[16,c(13:length(T_ssp460))]), year = names(T_ssp460[1,c(13:length(T_ssp460))]))
            glimpse(temp4)
            temp4$year <- as.integer(sub('X', '', temp4$year))
            names(temp4)[1] <- "temp"
            temp4$rcp <- "RCP60"
            temp4$ssp <- "SSP4"
            glimpse(temp4)

            temp <- rbind(temp1,temp2,temp3,temp4)
            glimpse(temp)
                
                temp <-temp %>% group_by(rcp) %>%
                    filter(year == 2021) %>%
                    mutate(t_12_21 = mean(temp,na.rm=T)) %>%
                    #filter(year==2012) %>%
                    select(t_12_21,rcp) %>%
                    inner_join(temp, by = c("rcp"))

                temp$tdif <- temp$temp - temp$t_12_21
            glimpse(temp)

            ssp_gdp_pop <- ssp_gdp_pop %>% left_join(temp,by=c("ssp","year"))
            glimpse(ssp_gdp_pop)



            ssp_gdp_pop_nut <- rbind(ssp_gdp_pop %>% mutate(nutrient="Calcium"),
                                ssp_gdp_pop %>% mutate(nutrient="Protein"),
                                ssp_gdp_pop %>% mutate(nutrient="Omega3"),
                                ssp_gdp_pop %>% mutate(nutrient="Iron"))
        
        
        ## Join GDP, TEMP and POP (end)

        #ssp_gdp_pop_nut2 <- ssp_gdp_pop_nut %>% left_join(nut_eff_sum, by=c("nutrient")) #HERE
        glimpse(ssp_gdp_pop_nut)

    ## Join with SSPs Temp, GDP and Pop Data (end)


    #ssp_gdp_pop_nut3 <- ssp_gdp_pop_nut2 %>% 
     #   mutate(future_risk = (1+(tcoeff/100)*tdif)*total_effect, 
      #  delta_risk = ((tcoeff/100)*tdif)*total_effect)
    
    
    #write.csv(ssp_gdp_pop_nut,"Data/Modules/fish/Nutrition/ssp_gdp_pop_nut.csv")

    glimpse(ssp_gdp_pop_nut)

## Read Relative Risk increase dose-response function (end)


## ALL CAUSES Read Global Burden Disease Data (start)
    gbd <- read.csv(paste0(datadir,"gbd_data\\IHME-GBD_2021_DATA-27f400d0-1.csv"))
    glimpse(gbd)

    gbd_deaths_number <- gbd %>% filter(metric_name=="Number",measure_name=="Deaths") %>% 
        mutate(ISO3 = countrycode(location_name, origin="country.name", destination="iso3c")) %>% 
        rename(deaths_base=val, condition_gbd=cause_name) %>% dplyr::select(deaths_base,ISO3,condition_gbd) 

    glimpse(gbd_deaths_number)
    
    medianGDPpc <- ssp_gdp_pop_nut %>% dplyr::filter(year==2021,ssp=="SSP2",nutrient=="Omega3") %>% 
            mutate(GDPpc = GDP.billion2005USDperYear/Pop.million) %>% 
            summarize(medianGDPpc = median(GDPpc,na.rm=TRUE))

    gbd_deaths_number_pop <- gbd_deaths_number %>% 
        left_join(ssp_gdp_pop_nut %>% dplyr::filter(year==2021,ssp=="SSP2",nutrient=="Omega3") %>% dplyr::select(-nutrient,-ssp,-year),by="ISO3") %>% 
        mutate(deaths_base_percapita = deaths_base / (Pop.million*10^6), 
            income = ifelse(medianGDPpc$medianGDPpc > (GDP.billion2005USDperYear/Pop.million),"Low Income","High Income"))  
    
        
    gbd_deaths_number_doseresponse_pop <- gbd_deaths_number_pop %>% 
        left_join(nut_eff_sum ,by=c("condition_gbd","income"))%>% 
        filter(!is.na(nutrient),!is.na(tcoeff)) %>% 
        rename(cause=condition_gbd)
    
    

    gbd_deaths_number_doseresponse_pop_subset <- gbd_deaths_number_doseresponse_pop %>% #%>% 
        dplyr::select(deaths_base_percapita,ISO3,cause,nutrient,total_effect,total_se,tcoeff,se) %>% mutate(iso_nut = paste0(ISO3,nutrient))

    glimpse(gbd_deaths_number_doseresponse_pop_subset)

    ## Here
    #    mortweigh_avg_effect_byNutrient <- gbd_deaths_number_doseresponse_pop_subset %>% 
    #        group_by(nutrient,ISO3) %>%
    #        summarize(mortalityweighted_avg_effect = sum(total_effect*deaths_base_percapita)/sum(deaths_base_percapita),
    #        mortalityweighted_avg_se = sqrt(sum((total_se*deaths_base_percapita)^2)/sum(deaths_base_percapita)^2), 
    #        sum_mortality = sum(deaths_base_percapita,na.rm=T)) %>% ungroup() 
    #    glimpse(mortweigh_avg_effect_byNutrient)#\

    #    medianGDPpc <- ssp_gdp_pop %>% dplyr::filter(year==2021,ssp=="SSP2") %>% 
     #       mutate(GDPpc = GDP.billion2005USDperYear/Pop.million) %>% 
      #      summarize(medianGDPpc = median(GDPpc,na.rm=TRUE))

        incomes <- ssp_gdp_pop %>% dplyr::filter(year==2021,ssp=="SSP2") %>% 
                    mutate(GDPpc = GDP.billion2005USDperYear/Pop.million, 
                    income = ifelse(GDPpc > medianGDPpc$medianGDPpc,"High Income","Low Income")) %>% dplyr::select(ISO3,income)

        mortweigh_avg_effect_byNutrient <- mortweigh_avg_effect_byNutrient %>% left_join(incomes,by="ISO3")
        glimpse(mortweigh_avg_effect_byNutrient)
        glimpse(nut_tcoeff)
        
        mortweigh_avg_effect_byNutrient <- mortweigh_avg_effect_byNutrient %>% left_join(nut_tcoeff, by = c("nutrient","income"))
        glimpse(mortweigh_avg_effect_byNutrient)

        mortweigh_avg_effect_byNutrient %>% filter(ISO3 %in% c("MEX")) %>% as.data.frame()

        mortweigh_avg_effect_nut_tcoeff <- mortweigh_avg_effect_byNutrient %>% 
            group_by(ISO3) %>% 
            summarize(mortweigh_avg_tcoeff = sum(tcoeff*mortalityweighted_avg_effect)/sum(mortalityweighted_avg_effect),   
                mortweigh_avg_tcoeff_se = sqrt(sum((se*mortalityweighted_avg_effect)^2)/sum(mortalityweighted_avg_effect)^2))
        glimpse(mortweigh_avg_effect_nut_tcoeff)
    ## Here


    mortweigh_avg_effect %>% filter(ISO3 %in% c("MEX","USA","SLB"))
    
    gbd_deaths_number_doseresponse_pop_temp <- 
        gbd_deaths_number_doseresponse_pop_subset %>%  
        left_join(ssp_gdp_pop_nut %>% dplyr::filter(!is.na(tdif)) %>% mutate(iso_nut = paste0(ISO3,nutrient)) %>% dplyr::select(-nutrient,-ISO3),by="iso_nut") 
        

    # gbd_deaths_number_doseresponse_pop_temp_future <-  gbd_deaths_number_doseresponse_pop_temp %>% 
    #     mutate(future_risk = (1+(tcoeff/100)*tdif)*(-total_effect), 
    #     future_risk_se = abs(future_risk) * sqrt((se / tcoeff)^2 + (total_se / total_effect)^2), 
    #     delta_risk = ((tcoeff/100)*tdif)*(-total_effect), 
    #     delta_risk_se = abs(delta_risk) * sqrt((se / tcoeff)^2 + (total_se / total_effect)^2)) %>% 
    #     mutate(deaths_percapita_future = delta_risk * deaths_base_percapita, 
    #     deaths_percapita_future_se = abs(deaths_base_percapita) * delta_risk_se, 
    #     lives_saved_percapita_future = -future_risk * deaths_base_percapita, 
    #     lives_saved_percapita_future_se = abs(deaths_base_percapita) * future_risk_se, 
    #     GDPpc_2020USD = def_mult$def_2005_to_2020 * (GDP.billion2005USDperYear*10^9)/(Pop.million*10^6))
    
    ## Read Dependence (start)
        nut_dep <- read.csv(paste0(datadir,"Cheung_etal_2024_World_Nut_Dependency.csv"))
        nut_dep <- nut_dep %>% mutate(ISO3=countrycode(Country.Territory,origin="country.name",destination="iso3c"))


        glimpse(gbd_deaths_number_doseresponse_pop_temp)
        
        avg_effect <- gbd_deaths_number_doseresponse_pop_subset %>% 
        group_by(nutrient,ISO3) %>%
        summarize( 
            mortalityweighted_avg_effect = sum(total_effect*deaths_base_percapita)/sum(deaths_base_percapita),
            mortalityweighted_avg_se = sqrt(sum((total_effect*deaths_base_percapita)^2)/sum(deaths_base_percapita)^2), 
            sum_mortality = sum(deaths_base_percapita,na.rm=T)
        ) %>% as.data.frame()

        glimpse(avg_effect)
        avg_effect %>% filter(ISO3=="MEX")


        mutate(future_risk = (1+(tcoeff/100)*tdif)*(-total_effect*Nutritional_D), 
        future_risk_se = sqrt(
         ((-total_effect * Nutritional_D) * (tdif / 100) * se)^2 + 
         ((1 + (tcoeff / 100) * tdif) * (Nutritional_D * total_se))^2), 
        delta_risk = ((tcoeff/100)*tdif)*(-total_effect), 
        delta_risk_se = abs(delta_risk) * sqrt((se / tcoeff)^2 + (total_se / total_effect)^2)) %>% 
        mutate(deaths_percapita_future = delta_risk * deaths_base_percapita, 
        deaths_percapita_future_se = abs(deaths_base_percapita) * delta_risk_se, 
        lives_saved_percapita_future = -future_risk * deaths_base_percapita, 
        lives_saved_percapita_future_se = abs(deaths_base_percapita) * future_risk_se, 
        GDPpc_2020USD = def_mult$def_2005_to_2020 * (GDP.billion2005USDperYear*10^9)/(Pop.million*10^6)) %>% 
        filter(!is.na(Nutritional_D))
        
        gbd_deaths_number_doseresponse_pop_temp_future_dep <- gbd_deaths_number_doseresponse_pop_temp %>% 
        left_join(nut_dep,by="ISO3") %>% 
        mutate(future_risk = (1+(tcoeff/100)*tdif)*(-total_effect*Nutritional_D), 
        future_risk_se = sqrt(
         ((-total_effect * Nutritional_D) * (tdif / 100) * se)^2 + 
         ((1 + (tcoeff / 100) * tdif) * (Nutritional_D * total_se))^2), 
        delta_risk = ((tcoeff/100)*tdif)*(-total_effect), 
        delta_risk_se = abs(delta_risk) * sqrt((se / tcoeff)^2 + (total_se / total_effect)^2)) %>% 
        mutate(deaths_percapita_future = delta_risk * deaths_base_percapita, 
        deaths_percapita_future_se = abs(deaths_base_percapita) * delta_risk_se, 
        lives_saved_percapita_future = -future_risk * deaths_base_percapita, 
        lives_saved_percapita_future_se = abs(deaths_base_percapita) * future_risk_se, 
        GDPpc_2020USD = def_mult$def_2005_to_2020 * (GDP.billion2005USDperYear*10^9)/(Pop.million*10^6)) %>% 
        filter(!is.na(Nutritional_D))
       

        glimpse(gbd_deaths_number_doseresponse_pop_temp_future_dep)

    
    ## Read Dependence (end)
    #write.csv(gbd_deaths_number_doseresponse_pop_temp_future_dep,"Data/Modules/fish/Nutrition/gbd_deaths_number_doseresponse_pop_temp_future.csv")


    

    gbd_deaths_number_doseresponse_pop_temp_future %>% filter(rcp=="RCP60") %>% select(ssp) %>% slice(1) 
      
      #ggsave("Figures/SM/fisheries/health_all_cause.png")

    deaths_by_nutrient <- gbd_deaths_number_doseresponse_pop_temp_future_dep %>% group_by(ISO3,nutrient,year,ssp) %>% 
        summarize(deaths_percapita_future= sum(deaths_percapita_future,na.rm=TRUE),    
        deaths_percapita_future_se = sqrt(sum(deaths_percapita_future_se^2, na.rm = TRUE)), 
        Pop.million = first(Pop.million), 
        tdif=first(tdif), 
        GDPpc_2020USD = first(GDPpc_2020USD)
        )
    glimpse(deaths_by_nutrient)
    #write.csv(deaths_by_nutrient,"Data/Modules/fish/Nutrition/deaths_by_nutrient.csv")

    
    deaths_by_country <- gbd_deaths_number_doseresponse_pop_temp_future_dep %>% group_by(ISO3,year,ssp) %>% 
        summarize(deaths_percapita_future= sum(deaths_percapita_future,na.rm=TRUE),    
        deaths_percapita_future_se = sqrt(sum(deaths_percapita_future_se^2, na.rm = TRUE)), 
        lives_saved_percapita_future= sum(lives_saved_percapita_future,na.rm=TRUE),    
        lives_saved_percapita_future_se = sqrt(sum(lives_saved_percapita_future_se^2, na.rm = TRUE)), 
        deaths_base_percapita = sum(deaths_base_percapita,na.rm=TRUE),
        Pop.million = first(Pop.million), 
        tdif=first(tdif), 
        rcp = first(rcp),
        GDPpc_2020USD = first(GDPpc_2020USD), 
        Nutritional_D = first(Nutritional_D), 
        GDP_2020USD = first(def_mult$def_2005_to_2020 * (GDP.billion2005USDperYear*10^9))
        ) %>% left_join(regions %>% rename(ISO3=countrycode),by=("ISO3"))


    

    #write.csv(deaths_by_country,"Data/Modules/fish/Nutrition/deaths_by_country.csv")
    deaths_by_country <- read.csv("Data/Modules/fish/Nutrition/deaths_by_country.csv")



    no_subs = 0.5

    vsl_base_us_million2020USD = 10.05 #based on https://www.nature.com/articles/s41586-022-05224-9
    gdp_base_us_million2020USD = deaths_by_country %>% ungroup() %>% 
                                filter(year==2020,ISO3=="USA",ssp=="SSP2") %>% 
                                dplyr::select(GDPpc_2020USD) 
    
    deaths_by_country_vsl <- deaths_by_country %>% 
        mutate(year_ssp = paste0(year,ssp)) %>% 
        
        left_join(deaths_by_country %>% 
            mutate(year_ssp = paste0(year,ssp)) %>% 
            group_by(year,ssp) %>% 
            summarize(
                totpop = sum(Pop.million,na.rm=TRUE),
                popweighted_average_GDPpc = sum(GDPpc_2020USD*Pop.million/totpop), 
                year_ssp = year_ssp[[1]]) %>% ungroup() %>% select(-ssp,-year), by="year_ssp") %>% 
    mutate(deaths_future=deaths_percapita_future*(no_subs*Pop.million*10^6), 
        deaths_future_se=abs(no_subs*Pop.million*10^6) * deaths_percapita_future_se, 
        lives_saved_future=lives_saved_percapita_future*(no_subs*Pop.million*10^6), 
        lives_saved_future_se=abs(no_subs*Pop.million*10^6) * lives_saved_percapita_future_se)%>% 
        mutate(vsl= vsl_base_us_million2020USD * ( GDPpc_2020USD / gdp_base_us_million2020USD[[1]]), 
            vsl_global = vsl_base_us_million2020USD * ( popweighted_average_GDPpc / gdp_base_us_million2020USD[[1]])) %>% 
        mutate(
                health_damage_countryVSL = vsl * deaths_future, 
                health_damage_percGDP_countryVSL = 100* vsl * 10^6 * deaths_future / (GDP_2020USD), 
                health_damage_se_countryVSL = abs(vsl) * deaths_future_se, 
                health_damage_percGDP_se_countryVSL = abs(100* vsl * 10^6/ (GDP_2020USD) * deaths_future_se ), 

                health_benefits_countryVSL = vsl * lives_saved_future, 
                health_benefits_percGDP_countryVSL = 100* vsl * 10^6 * lives_saved_future / (GDP_2020USD), 
                health_benefits_se_countryVSL = abs(vsl) * lives_saved_future_se, 
                health_benefits_percGDP_se_countryVSL = abs(100* vsl * 10^6/ (GDP_2020USD) * lives_saved_future_se ), 

                health_damage = vsl_global * deaths_future, 
                health_damage_percGDP = 100* vsl_global * 10^6 * deaths_future / (GDP_2020USD), 
                health_damage_se = abs(vsl_global) * deaths_future_se, 
                health_damage_percGDP_se = abs(100* vsl_global * 10^6/ (GDP_2020USD) * deaths_future_se ), 

                health_benefits = vsl_global * lives_saved_future, 
                health_benefits_percGDP = 100* vsl_global * 10^6 * lives_saved_future / (GDP_2020USD), 
                health_benefits_se = abs(vsl_global) * lives_saved_future_se, 
                health_benefits_percGDP_se = abs(100* vsl_global * 10^6/ (GDP_2020USD) * lives_saved_future_se ), 


                scenario=paste0(ssp,substr(rcp, nchar(rcp) - 1, nchar(rcp)))) %>% ungroup() %>% as.data.frame() %>% filter(!is.na(ssp))
    
    glimpse(deaths_by_country_vsl)

    # ggplot(deaths_by_country_vsl %>% filter(ssp=="SSP2",year==2100))+
    # geom_point(aes(y=health_benefits_percGDP,x=health_benefits_percGDP_countryVSL,color=GDPpc_2020USD/1000)) +
    # geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
    # scale_color_viridis() +
    # theme_minimal()+
    # labs(x="Monetized value using Country-level VSL (% of GDP)", y="Monetized value using Global VSL (%of GDP)",color="GDP per capita \n Thousand 2020 USD",title="Avoided premature deaths")
    # ggsave("Figures/SM/fisheries/comparisonGlobalVSL.jpg")
    # ggplot(deaths_by_country_vsl %>% filter(year %in% c(seq(1:100)*5+2000)))+
    # geom_point(aes(x=year,y=popweighted_average_GDPpc/1000,color=ssp,shape=ssp)) + 
    # geom_line(data=deaths_by_country_vsl,
    #      #%>% filter(ISO3=="USA"), 
    #      aes(x=year,y=GDPpc_2020USD/1000,color=ssp,group=interaction(ssp,ISO3)),alpha=0.1) +
    #      guides(color="none") +
    #      theme_minimal()+
    #      labs(x="year",y="GDP per capita (Thousand 2020 USD)",shape="Scenario")



   
    # ggplot(deaths_by_country_vsl) +
    #     geom_line(aes(x=GDPpc_2020USD,y=lives_saved_percapita_future,color=R5,linetype=scenario,group=interaction(ssp,ISO3)))+
    #     #geom_ribbon(aes(x=year,ymin=health_benefits - (1.96*health_benefits_se),ymax=health_benefits + (1.96*health_benefits_se),fill=R5,linetype=scenario,group=interaction(ssp,ISO3)),alpha=0.09)+
    #     #geom_text_repel(data=deaths_by_country_vsl %>% filter(year==2100),aes(x=year,y=health_benefits,color=R5,label=ISO3),max.overlaps=100,size=2.5)+
    #     scale_color_manual(values=hex_R5) +
    #     scale_fill_manual(values=hex_R5) +
    #     #labs(y = "Health Benefits \n(million 2020 USD)", linetype="Scenario", fill="Region", color="Region", x="Global Temperature Increase (°C)") +
    #     #xlim(c(0,4))+
    #     scale_x_continuous(trans="log10")
    #     theme_minimal()


    # ggplot(deaths_by_country_vsl) +
    #     geom_line(aes(x=year,y=health_benefits,color=R5,linetype=scenario,group=interaction(ssp,ISO3)))+
    #     geom_ribbon(aes(x=year,ymin=health_benefits - (1.96*health_benefits_se),ymax=health_benefits + (1.96*health_benefits_se),fill=R5,linetype=scenario,group=interaction(ssp,ISO3)),alpha=0.09)+
    #     #geom_text_repel(data=deaths_by_country_vsl %>% filter(year==2100),aes(x=year,y=health_benefits,color=R5,label=ISO3),max.overlaps=100,size=2.5)+
    #     scale_color_manual(values=hex_R5) +
    #     scale_fill_manual(values=hex_R5) +
    #     labs(y = "Health Benefits \n(million 2020 USD)", linetype="Scenario", fill="Region", color="Region", x="Global Temperature Increase (°C)") +
    #     #xlim(c(0,4))+
    #     #scale_y_continuous(trans="log10")
    #     theme_minimal()


    # ggplot(deaths_by_country_vsl) +
    #     geom_line(aes(x=year,y=health_benefits_percGDP,color=R5,linetype=scenario,group=interaction(ssp,ISO3)))+
    #     #geom_ribbon(aes(x=year,ymin=health_benefits_percGDP - (1.96*health_benefits_percGDP_se),ymax=health_benefits_percGDP + (1.96*health_benefits_percGDP_se),fill=R5,linetype=scenario,group=interaction(ssp,ISO3)),alpha=0.09)+
    #     #geom_text_repel(data=deaths_by_country_vsl %>% filter(year==2100),aes(x=tdif,y=health_benefits_percGDP,color=R5,label=ISO3),max.overlaps=100,size=2.5)+
    #     scale_color_manual(values=hex_R5) +
    #     scale_fill_manual(values=hex_R5) +
    #     labs(y = "Health Benefits \n(% of GDP)", linetype="Scenario", fill="Region", color="Region", x="Global Temperature Increase (°C)") +
    #     #xlim(c(0,4))+
    #     #scale_y_continuous(trans="log10")+
    #     theme_minimal()
    
    # glimpse(deaths_by_country_vsl)
    # ggplot(deaths_by_country_vsl) +
    #     geom_line(aes(x=tdif,y=health_benefits_percGDP_countryVSL,color=R5,linetype=scenario,group=interaction(ssp,ISO3)))+
    #     geom_ribbon(aes(x=tdif,ymin=health_benefits_percGDP_countryVSL - (1.96*health_benefits_percGDP_se_countryVSL),ymax=health_benefits_percGDP_countryVSL + (1.96*health_benefits_percGDP_se_countryVSL),fill=R5,linetype=scenario,group=interaction(ssp,ISO3)),alpha=0.09)+
    #     #geom_text_repel(data=deaths_by_country_vsl %>% filter(year==2100),aes(x=tdif,y=health_benefits_percGDP,color=R5,label=ISO3),max.overlaps=100,size=2.5)+
    #     scale_color_manual(values=hex_R5) +
    #     scale_fill_manual(values=hex_R5) +
    #     labs(y = "Health Benefits \n(% of GDP)", linetype="Scenario", fill="Region", color="Region", x="Global Temperature Increase (°C)") +
    #     #xlim(c(0,4))+
    #     #scale_y_continuous(trans="log10")+
    #     theme_minimal()

    # ggplot(deaths_by_country_vsl) +
    #     geom_line(aes(x=tdif,y=health_benefits/popweighted_average_GDPpc,color=R5,linetype=scenario,group=interaction(ssp,ISO3)))+
    #     #geom_ribbon(aes(x=tdif,ymin=health_benefits_percGDP - (1.96*health_benefits_percGDP_se),ymax=health_benefits_percGDP + (1.96*health_benefits_percGDP_se),fill=R5,linetype=scenario,group=interaction(ssp,ISO3)),alpha=0.09)+
    #     #geom_text_repel(data=deaths_by_country_vsl %>% filter(year==2100),aes(x=tdif,y=health_benefits_percGDP,color=R5,label=ISO3),max.overlaps=100,size=2.5)+
    #     scale_color_manual(values=hex_R5) +
    #     scale_fill_manual(values=hex_R5) +
    #     labs(y = "Health Benefits \n(% of GDP)", linetype="Scenario", fill="Region", color="Region", x="Global Temperature Increase (°C)") +
    #     #xlim(c(0,4))+
    #     #scale_y_continuous(trans="log10")+
    #     theme_minimal()
    
    deaths_by_country_vsl %>% filter(ISO3=="SLB") %>% 
    select(health_damage_percGDP_se) %>% 
    summarize(mean_se_2 = mean(health_damage_percGDP_se^2,na.rm=TRUE), 
            mean_se_2_root = sqrt(mean(health_damage_percGDP_se^2,na.rm=TRUE)), 
            mean_se = mean(health_damage_percGDP_se,na.rm=TRUE))
    #write.csv(deaths_by_country_vsl,"Data/Modules/fish/Nutrition/deaths_by_country_vsl.csv")
    #write.csv(deaths_by_country_vsl,"Data/Modules/fish/Nutrition/deaths_by_country_Globalvsl.csv")
    deaths_by_country_vsl <- read.csv("Data/Modules/fish/Nutrition/deaths_by_country_Globalvsl.csv")

     

    mod <- felm(lives_saved_percapita_future ~ tdif | 0 | 0 | 0, data=deaths_by_country_vsl %>% filter(ISO3=="SLB"))
            coef(mod)[1]
            summary(mod)
            
    health_benefits_tcoeff <- deaths_by_country_vsl %>% filter(!is.na(health_benefits_percGDP)) %>% 
        group_by(ISO3) %>%
        nest() %>%
        mutate(
            health_coeff_intercept = map_dbl(data, ~{
            mod <- felm(health_benefits_percGDP~ tdif | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            health_coeff_intercept_se = map_dbl(data, ~{
            mod <- felm(health_benefits_percGDP~  tdif  | 0 | 0 | 0, .x)
            model_se <- summary(mod)$coef[3]
            data_se <- sqrt(mean(.x$health_benefits_percGDP_se^2, na.rm = TRUE)) 
            combined_se <- sqrt(model_se^2 + data_se^2) 
            combined_se
            }),

            health_coeff_slopeT = map_dbl(data, ~{
            mod <- felm(health_benefits_percGDP~  tdif | 0 | 0 | 0, .x)
            coef(mod)[2]
             }), 
            
            health_coeff_slopeT_se = map_dbl(data, ~{
            mod <- felm(health_benefits_percGDP~  tdif | 0 | 0 | 0, .x)
            model_se <- summary(mod)$coef[2, "Std. Error"]
            model_se
            })
        ) %>%
        unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(ISO3,health_coeff_intercept,health_coeff_intercept_se,health_coeff_slopeT,health_coeff_slopeT_se) %>% 
        rename(HealthBenefit_PercentageGDP_perDegreeC = health_coeff_slopeT, 
                HealthBenefit_PercentageGDP_perDegreeC_se = health_coeff_slopeT_se,
                HealthBenefit_PercentageGDP_intercept = health_coeff_intercept, 
                HealthBenefit_PercentageGDP_intercept_se = health_coeff_intercept_se) %>%  as.data.frame()

        glimpse(health_benefits_tcoeff)
        ggplot(health_benefits_tcoeff,aes(x=HealthBenefit_PercentageGDP_intercept,y=HealthBenefit_PercentageGDP_intercept_se))+
        geom_point(aes(label=ISO3))

        ggplot(health_benefits_tcoeff,aes(x=HealthBenefit_PercentageGDP_intercept,y=HealthBenefit_PercentageGDP_perDegreeC))+
        geom_point(aes(label=ISO3)) +
        labs(x= "Health Benefits from fisheries in 2020\n (as %GDP)", y="Benefits change per Degree of Warming\n(as %GDP)")+
        theme_minimal()

        
        #write.csv(health_benefits_tcoeff,"Data/Modules/fish/Nutrition/health_benefits_tcoeff.csv")
        #write.csv(health_benefits_tcoeff,"Data/Modules/fish/Nutrition/health_benefits_tcoeff_GlobalVSL.csv")
        health_benefits_tcoeff <- read.csv("Data/Modules/fish/Nutrition/health_benefits_tcoeff.csv")
        health_benefits_tcoeff_global <- read.csv("Data/Modules/fish/Nutrition/health_benefits_tcoeff_GlobalVSL.csv")
    

    health_tcoeff_dam <- deaths_by_country_vsl %>% filter(!is.na(health_damage_percGDP)) %>% 
        group_by(ISO3) %>%
        nest() %>%
        mutate(
            health_coeff = map_dbl(data, ~{
            mod <- felm(health_damage_percGDP~ 0 + tdif | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            health_coeff_se = map_dbl(data, ~{
            mod <- felm(health_damage_percGDP~ 0 + tdif  | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            health_coeff_pval = map_dbl(data, ~{
            mod <- felm(health_damage_percGDP~ 0 + tdif | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
             })
            #, 
            # living_coral_cover = map_dbl(data, ~{
            # mod <- lm(cover ~1, .x)
            # summary(mod)$coef[1]
            # })
        ) %>%
        unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(ISO3,health_coeff,health_coeff_se,health_coeff_pval) %>% 
        rename(HealthDam_PercentageGDP_perC = health_coeff, HealthDam_PercentageGDP_perC_se = health_coeff_se,HealthDam_PercentageGDP_perC_pval = health_coeff_pval) %>% as.data.frame()

        glimpse(health_tcoeff_dam)
        
        #write.csv(health_tcoeff_dam,"Data/Modules/fish/Nutrition/health_damages_tcoeff.csv")
        write.csv(health_tcoeff_dam,"Data/Modules/fish/Nutrition/health_damages_tcoeff_GlobalVSL.csv")





## ALL CAUSES Read Global Burden Disease Data (end)
