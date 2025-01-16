## Get benefits (use non-use) by country, by year, weighted by area (start)
        glimpse(scen_allforcings_allES)
        def_mult <- deflator_data %>% 
            summarize(def_2005_to_2020 =NY.GDP.DEFL.ZS[year==2020]/NY.GDP.DEFL.ZS[year==2005] )
        
        weighted_avg_benefits <- scen_allforcings_allES %>% filter(year>2019) %>%
            group_by(countrycode, year,type,forcing) %>%
            mutate(gdppc = (def_mult[[1]]*GDP_Country_ssp2*10^9)/(Pop_Country_ssp2*10^6)) %>% # GDP is in billion 2005 USD per year, Pop is in million #Cumulative Inflation Global from 2005 to 2020
            mutate(benefits_perha_percGDP = (100*benefits_perha)/(def_mult[[1]]*GDP_Country_ssp2*10^9)) %>%
            summarize(
                weighted_avg_benefit_perha = sum(benefits_perha * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
                weighted_avg_benefit_perha_percGDP = sum(benefits_perha_percGDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
                gdppc = first(gdppc),
                GDP_SSP2 = first(def_mult[[1]]*GDP_Country_ssp2*10^9),
                .groups = "drop"
            )
        glimpse(weighted_avg_benefits)
        
        #write.csv(weighted_avg_benefits,file="Data\\output_modules_input_rice50x\\output_modules\\weighted_avg_benefits.csv")
    ## Get benefits (use non-use) by country, by year, weighted by area (end)
    
    # Market damage Function (start)
        weighted_avg_benefits_prov <- weighted_avg_benefits %>% filter(type=="provision",forcing=="both",year>2025) %>%
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




        

        glimpse(weighted_avg_benefits_prov2)
        glimpse(weighted_avg_benefits_prov)

        #write.csv(weighted_avg_benefits_prov2,file="Data\\output_modules_input_rice50x\\output_modules\\weighted_avg_benefits_prov2.csv")
        #write.csv(weighted_avg_benefits_prov,file="Data\\output_modules_input_rice50x\\output_modules\\weighted_avg_benefits_prov.csv")
        
        market_coefficients_by_country <- weighted_avg_benefits_prov2 %>% filter(year==2100) %>% dplyr::select(countrycode,GDPDam_perC,GDPDam_perC_se_adj,GDPDam_perC_sq,GDPDam_perC_sq_se_adj,cov_t_t2_adj)
        glimpse(market_coefficients_by_country)
        #write.csv(market_coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\output_modules_input_rice50x\\output_modules\\mangrove_GDPdam_coefficients.csv")
        market_coefficients_by_country <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\output_modules_input_rice50x\\output_modules\\mangrove_GDPdam_coefficients.csv")
    
    # Market damage Function (end)

    ## Non-Market Projections (start)
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

            output_modules_input_rice50x\\output_modules <- combined_summary %>% 
            mutate(unit_benefit="Int2020$_perkm2_peryear",
            use_market_perkm2 = total_weighted_avg_benefit_use_market_perha*100,
            use_nonmarket_perkm2 = total_weighted_avg_benefit_use_nonmarket_perha*100,
            nonuse_perkm2 = total_weighted_avg_benefit_nonuse_perha*100) %>% 
            dplyr::select(-gdppc,-total_weighted_avg_benefit_nonuse_perha,-total_weighted_avg_benefit_use_nonmarket_perha,-total_weighted_avg_benefit_use_market_perha)

            glimpse(output_modules_input_rice50x\\output_modules)
            output_modules_input_rice50x\\output_modules %>% filter(countrycode=="AGO")
            #write.csv(output_modules_input_rice50x\\output_modules,"Data/output_modules_input_rice50x/input_rice50x/mangrove_benefits_per_km2.csv") ## This is what enters into 


    ## Non-market projections (end)

    ## Regression of benefits (start)
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
        
        #write.csv(model_coefficients_use_market,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\output_modules_input_rice50x\\output_modules\\model_coefficients_use_marketv3.csv")


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
            
        #write.csv(model_coefficients_nonuse ,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\output_modules_input_rice50x\\output_modules\\model_coefficients_nonusev3.csv")

        
        
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
        c_list <- unique(combined_summary$countrycode)

            # for(country_interest in c_list) {
            #     intercept_c_nu <- model_coefficients_nonuse%>% filter(countrycode==country_interest) %>% select(intercept) %>% as.double()
            #     elas_c_nu <- model_coefficients_nonuse%>% filter(countrycode==country_interest) %>% select(elasticity) %>% as.double()
            #     intercept_c_um <- model_coefficients_use_market%>% filter(countrycode==country_interest) %>% select(intercept) %>% as.double()
            #     elas_c_um <- model_coefficients_use_market%>% filter(countrycode==country_interest) %>% select(elasticity) %>% as.double()
            #     intercept_c_un <- model_coefficients_use_nonmarket%>% filter(countrycode==country_interest) %>% select(intercept) %>% as.double()
            #     elas_c_un <- model_coefficients_use_nonmarket%>% filter(countrycode==country_interest) %>% select(elasticity) %>% as.double()
            #     plot <- ggplot(long_df%>% filter(countrycode==country_interest)) +
            #         geom_point(aes(x=year,y=total_weighted_avg_benefit*100,color=category))+
            #         geom_line(aes(x=year,y=exp(intercept_c_nu[1]) * gdppc^elas_c_nu[1])) + 
            #         geom_line(aes(x=year,y=exp(intercept_c_um[1]) * gdppc^elas_c_um[1])) + 
            #         geom_line(aes(x=year,y=exp(intercept_c_un[1]) * gdppc^elas_c_un[1])) + 
            #         theme_minimal()+
            #         ggtitle(paste0("Projection of values for ",country_interest))+
            #         ylab("Mangrove Benefits") + 
            #         scale_color_npg()
            #     ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Value_Function//", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

            # }


        #write.csv(model_coefficients_use_nonmarket ,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\output_modules_input_rice50x\\output_modules\\model_coefficients_use_nonmarketv3.csv")
    ## Regression of benefits (end)

