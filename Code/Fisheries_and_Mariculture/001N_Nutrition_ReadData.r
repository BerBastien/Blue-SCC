## Read Cheung et al Fig. 4 https://www.nature.com/articles/s41558-023-01822-1
## Change in nutrient availbaility

## Read Data (start)
    datadir <- "Data\\input_modules\\fish\\nutrition\\"
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


    nut_tcoeff <- nut_proj_long %>%
        group_by(nutrient, income) %>%
        nest() %>%
        mutate(
            model = map(data, ~ felm(Value ~ 0 + atmoT | 0 | 0 | 0, .x)),
            tcoeff = map_dbl(model, ~ coef(.x)[1]),
            se = map_dbl(model, ~ summary(.x)$coef[2]),
            pval = map_dbl(model, ~ summary(.x)$coef[4])
        ) %>%
        select(-data, -model) %>%
        unnest(cols = c(tcoeff, se, pval)) %>%
        slice(1) %>%
        ungroup() %>%
        as.data.frame()

        nut_proj_long_coeff <- nut_proj_long %>% left_join(nut_tcoeff,by=c("nutrient","income"))
        glimpse(nut_proj_long_coeff)
        #write.csv(nut_proj_long_coeff,"Data/output_modules_input_rice50x/output_modules/fish/nut_proj_long_coeff.csv")
            

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
## RR

## GBD
    gbd <- read.csv(paste0(datadir,"gbd_data\\IHME-GBD_2021_DATA-27f400d0-1.csv"))
    glimpse(gbd)
    # Prepare GBD deaths data with ISO3 codes and relevant renaming
    gbd_deaths_number <- gbd %>%
        filter(metric_name == "Number", measure_name == "Deaths") %>%
        mutate(ISO3 = countrycode(location_name, origin = "country.name", destination = "iso3c")) %>%
        rename(deaths_base = val, condition_gbd = cause_name) %>%
        select(deaths_base, ISO3, condition_gbd)

    # Calculate median GDP per capita for a specific year and SSP
    medianGDPpc <- ssp_gdp_pop %>%
        filter(year == 2021, ssp == "SSP2") %>%
        transmute(GDPpc = GDP.billion2005USDperYear / Pop.million) %>%
        summarize(medianGDPpc = median(GDPpc, na.rm = TRUE)) %>%
        pull(medianGDPpc)  # Extract the median value directly

    glimpse(ssp_gdp_pop)
    glimpse(gbd_deaths_number)
    # Join GBD deaths data with SSP population data and classify income levels
    gbd_deaths_number_pop <- gbd_deaths_number %>%
        left_join(
            ssp_gdp_pop %>%
            filter(year == 2021, ssp == "SSP2"),
            by = "ISO3"
        ) %>%
        mutate(
            GDPpc = GDP.billion2005USDperYear / Pop.million, 
            deaths_base_percapita = deaths_base / (Pop.million * 10^6),
            income = ifelse(GDPpc > medianGDPpc, "High Income", "Low Income")
        )

    glimpse(gbd_deaths_number_pop)

    ### Input Equation RICE50x
        gbd_deaths_number_doseresponse_pop <- gbd_deaths_number_pop %>%
            left_join(nut_eff_sum, by = c("condition_gbd", "income")) %>%
            filter(!is.na(nutrient), !is.na(tcoeff)) %>%
            rename(cause = condition_gbd) %>%
            select(deaths_base_percapita, ISO3, cause, nutrient, total_effect, total_se, tcoeff, se) %>%
            mutate(
                iso_nut = paste0(ISO3, nutrient),
                magnitude_risk = deaths_base_percapita * total_effect,
                magnitude_risk_var = (se * magnitude_risk)^2
            ) %>%
            group_by(ISO3) %>%
            summarize(
                risk_w_avg_tcoeff = sum(magnitude_risk * tcoeff) / sum(magnitude_risk),
                risk_w_avg_tcoeff_se = sqrt(sum((se * magnitude_risk)^2) / sum(magnitude_risk)^2),
                sum_magnitude_risk = sum(magnitude_risk),
                sum_magnitude_risk_se = sqrt(sum(magnitude_risk_var))
            ) %>%
            ungroup() %>% 
            rename(countrycode=ISO3, 
                beta_nutrient_percChange_perDegreeC = risk_w_avg_tcoeff, 
                beta_nutrient_percChange_perDegreeC_se = risk_w_avg_tcoeff_se, 
                TAME_nutrients_MortalityEffect = sum_magnitude_risk, 
                TAME_nutrients_MortalityEffect_se = sum_magnitude_risk_se)


        glimpse(gbd_deaths_number_doseresponse_pop)
        write.csv(gbd_deaths_number_doseresponse_pop,"Data/output_modules_input_rice50x/input_rice50x/mortality_seafood_nutrition.csv")

    ### Input Equation RICE50x

    ## Join with SSPs Temp, GDP and Pop Data (start)
            
            ssp_gdp_pop_nut <- rbind(ssp_gdp_pop %>% mutate(nutrient="Calcium"),
                                ssp_gdp_pop %>% mutate(nutrient="Protein"),
                                ssp_gdp_pop %>% mutate(nutrient="Omega3"),
                                ssp_gdp_pop %>% mutate(nutrient="Iron"))
        
 

        
    ## Join with SSPs Temp, GDP and Pop Data (end)



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


    
    gbd_deaths_number_doseresponse_pop_temp <- 
        gbd_deaths_number_doseresponse_pop_subset %>%  
        left_join(ssp_gdp_pop_nut %>% dplyr::filter(!is.na(tdif)) %>% 
        mutate(iso_nut = paste0(ISO3,nutrient)) %>% dplyr::select(-nutrient,-ISO3),by="iso_nut") 
    
    ## Read Dependence (start)
        nut_dep <- read.csv(paste0(datadir,"Cheung_etal_2024_World_Nut_Dependency.csv"))
        write.csv(nut_dep,file="Data/output_modules_input_rice50x/input_rice50x/seafood_dependence.csv")
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
    #write.csv(gbd_deaths_number_doseresponse_pop_temp_future_dep,"Data/output_modules_input_rice50x/output_modules/fish/gbd_deaths_number_doseresponse_pop_temp_future.csv")


    deaths_by_nutrient <- gbd_deaths_number_doseresponse_pop_temp_future_dep %>% group_by(ISO3,nutrient,year,ssp) %>% 
        summarize(deaths_percapita_future= sum(deaths_percapita_future,na.rm=TRUE),    
        deaths_percapita_future_se = sqrt(sum(deaths_percapita_future_se^2, na.rm = TRUE)), 
        Pop.million = first(Pop.million), 
        tdif=first(tdif), 
        GDPpc_2020USD = first(GDPpc_2020USD)
        )
    glimpse(deaths_by_nutrient)
    #write.csv(deaths_by_nutrient,"Data/output_modules_input_rice50x/output_modules/fish/deaths_by_nutrient.csv")

    
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


    

    #write.csv(deaths_by_country,"Data/output_modules_input_rice50x/output_modules/fish/deaths_by_country.csv")
    deaths_by_country <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/deaths_by_country.csv")



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

    deaths_by_country_vsl %>% filter(ISO3=="SLB") %>% 
    select(health_damage_percGDP_se) %>% 
    summarize(mean_se_2 = mean(health_damage_percGDP_se^2,na.rm=TRUE), 
            mean_se_2_root = sqrt(mean(health_damage_percGDP_se^2,na.rm=TRUE)), 
            mean_se = mean(health_damage_percGDP_se,na.rm=TRUE))
    #write.csv(deaths_by_country_vsl,"Data/output_modules_input_rice50x/output_modules/fish/deaths_by_country_Globalvsl.csv")
    deaths_by_country_vsl <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/deaths_by_country_Globalvsl.csv")


####---- Analysis below is if we want to get specific "Benefit Functions" or "Damage Functions"
     

    # mod <- felm(lives_saved_percapita_future ~ tdif | 0 | 0 | 0, data=deaths_by_country_vsl %>% filter(ISO3=="SLB"))
    #         coef(mod)[1]
    #         summary(mod)
            
    # health_benefits_tcoeff <- deaths_by_country_vsl %>% filter(!is.na(health_benefits_percGDP)) %>% 
    #     group_by(ISO3) %>%
    #     nest() %>%
    #     mutate(
    #         health_coeff_intercept = map_dbl(data, ~{
    #         mod <- felm(health_benefits_percGDP~ tdif | 0 | 0 | 0, .x)
    #         coef(mod)[1]
    #         }),

    #         health_coeff_intercept_se = map_dbl(data, ~{
    #         mod <- felm(health_benefits_percGDP~  tdif  | 0 | 0 | 0, .x)
    #         model_se <- summary(mod)$coef[3]
    #         data_se <- sqrt(mean(.x$health_benefits_percGDP_se^2, na.rm = TRUE)) 
    #         combined_se <- sqrt(model_se^2 + data_se^2) 
    #         combined_se
    #         }),

    #         health_coeff_slopeT = map_dbl(data, ~{
    #         mod <- felm(health_benefits_percGDP~  tdif | 0 | 0 | 0, .x)
    #         coef(mod)[2]
    #          }), 
            
    #         health_coeff_slopeT_se = map_dbl(data, ~{
    #         mod <- felm(health_benefits_percGDP~  tdif | 0 | 0 | 0, .x)
    #         model_se <- summary(mod)$coef[2, "Std. Error"]
    #         model_se
    #         })
    #     ) %>%
    #     unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(ISO3,health_coeff_intercept,health_coeff_intercept_se,health_coeff_slopeT,health_coeff_slopeT_se) %>% 
    #     rename(HealthBenefit_PercentageGDP_perDegreeC = health_coeff_slopeT, 
    #             HealthBenefit_PercentageGDP_perDegreeC_se = health_coeff_slopeT_se,
    #             HealthBenefit_PercentageGDP_intercept = health_coeff_intercept, 
    #             HealthBenefit_PercentageGDP_intercept_se = health_coeff_intercept_se) %>%  as.data.frame()

    #     glimpse(health_benefits_tcoeff)
    #     ggplot(health_benefits_tcoeff,aes(x=HealthBenefit_PercentageGDP_intercept,y=HealthBenefit_PercentageGDP_intercept_se))+
    #     geom_point(aes(label=ISO3))

    #     ggplot(health_benefits_tcoeff,aes(x=HealthBenefit_PercentageGDP_intercept,y=HealthBenefit_PercentageGDP_perDegreeC))+
    #     geom_point(aes(label=ISO3)) +
    #     labs(x= "Health Benefits from fisheries in 2020\n (as %GDP)", y="Benefits change per Degree of Warming\n(as %GDP)")+
    #     theme_minimal()

        
    #     #write.csv(health_benefits_tcoeff,"Data/Modules/fish/Nutrition/health_benefits_tcoeff.csv")
    #     #write.csv(health_benefits_tcoeff,"Data/Modules/fish/Nutrition/health_benefits_tcoeff_GlobalVSL.csv")
    #     health_benefits_tcoeff <- read.csv("Data/Modules/fish/Nutrition/health_benefits_tcoeff.csv")
    #     health_benefits_tcoeff_global <- read.csv("Data/Modules/fish/Nutrition/health_benefits_tcoeff_GlobalVSL.csv")
    

    # health_tcoeff_dam <- deaths_by_country_vsl %>% filter(!is.na(health_damage_percGDP)) %>% 
    #     group_by(ISO3) %>%
    #     nest() %>%
    #     mutate(
    #         health_coeff = map_dbl(data, ~{
    #         mod <- felm(health_damage_percGDP~ 0 + tdif | 0 | 0 | 0, .x)
    #         coef(mod)[1]
    #         }),

    #         health_coeff_se = map_dbl(data, ~{
    #         mod <- felm(health_damage_percGDP~ 0 + tdif  | 0 | 0 | 0, .x)
    #         summary(mod)$coef[2]
    #         }),

    #         health_coeff_pval = map_dbl(data, ~{
    #         mod <- felm(health_damage_percGDP~ 0 + tdif | 0 | 0 | 0, .x)
    #         summary(mod)$coef[4]
    #          })
    #         #, 
    #         # living_coral_cover = map_dbl(data, ~{
    #         # mod <- lm(cover ~1, .x)
    #         # summary(mod)$coef[1]
    #         # })
    #     ) %>%
    #     unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(ISO3,health_coeff,health_coeff_se,health_coeff_pval) %>% 
    #     rename(HealthDam_PercentageGDP_perC = health_coeff, HealthDam_PercentageGDP_perC_se = health_coeff_se,HealthDam_PercentageGDP_perC_pval = health_coeff_pval) %>% as.data.frame()

    #     glimpse(health_tcoeff_dam)
        
    #     #write.csv(health_tcoeff_dam,"Data/Modules/fish/Nutrition/health_damages_tcoeff.csv")
    #     write.csv(health_tcoeff_dam,"Data/Modules/fish/Nutrition/health_damages_tcoeff_GlobalVSL.csv")





## ALL CAUSES Read Global Burden Disease Data (end)
