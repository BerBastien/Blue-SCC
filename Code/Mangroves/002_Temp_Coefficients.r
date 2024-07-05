#002_Temp_Coefficients    ## Quadratic Function (Key: benefit_ssp2)
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
        
        #write.csv(coefficients_by_country_sq,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v5April2024.csv")
        #coefficients_by_country_sq <- read.csv(file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients_sq_v5April2024.csv")

        coefficients_by_country_sq$MangroveArea_2020_km2 <- 0.01*coefficients_by_country_sq$MangroveArea_2020_hectares
        coefficients_by_country_sq <- coefficients_by_country_sq %>% dplyr::select(-MangroveArea_2020_hectares) %>% 
            dplyr::rename(FractionChange_perC = coefficient_temp,
            FractionChange_perC_se = coefficient_temp_se,
            FractionChange_perC_sq = sq_coefficient_temp,
            FractionChange_perC_sq_se = sq_coefficient_temp_se)
        glimpse(coefficients_by_country_sq)

        #write.csv(coefficients_by_country_sq,file="Data\\output_modules_input_rice50x\\input_rice50x\\mangrove_area_coefficients_sq.csv")
        

        ## PLOTS
            # c_list <- unique(benefit_ssp2$countrycode)
            # library("ggsci")

            # for(country_interest in c_list) {
            #     coef_area_c <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC) %>% as.double()
            #     se_area_c <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_se) %>% as.double()
            #     coef_area_c_sq <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_sq) %>% as.double()
            #     se_area_c_sq <- coefficients_by_country_sq %>% filter(countrycode==country_interest) %>% dplyr::select(FractionChange_perC_sq_se) %>% as.double()
                
            #     plot <- ggplot(benefit_ssp2 %>% filter(countrycode==country_interest)) +
            #         geom_point(aes(x=year,y=frac_loss,color=scenario.x))+
            #         geom_line(aes(x=year,y=temp*coef_area_c+coef_area_c_sq*temp^2)) +
            #         geom_ribbon(aes(x=year, ymin = temp*(coef_area_c-se_area_c*1.654)+(coef_area_c_sq-se_area_c_sq*1.654)*temp^2, 
            #             ymax = temp*(coef_area_c+se_area_c*1.654)+(coef_area_c_sq+se_area_c_sq*1.654)*temp^2), fill = "grey80", alpha = 0.5) +
            #         theme_minimal()+
            #         ggtitle(paste0("Projection of values for ",country_interest))+
            #         ylab("Mangrove Area Loss") 
            #     plot
            #     ggsave(filename = paste0("Figures/all_figures/mangroves/Country_Area_Damage//AprilCoeff//AllSSPs//sq_", country_interest, "_mangrove_benefits.jpg"), plot = plot, width = 8, height = 4)

            # }


        ## PLOTS
        
    ## Quadratic Function
    