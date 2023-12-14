    #Summarising Benefits per Ha

    library(dplyr)
    library(dplyr)
    library(rlang)

    install.packages("dplyr")
    install.packages("purrr")

    library(dplyr)
    library(purrr)

    scen_allforcings_allES <- read.csv(file="Data/projections/scen_allforcings_allES_ssp370.csv")
    glimpse(scen_allforcings_allES)

## Area Change
    
    
    diff_country_total <- read.csv(file="Data/projections/diff_country_total.csv")
    diff_country_total1 <- read.csv(file="Data/projections/diff_country_total_ssp170.csv")
    diff_country_total2 <- read.csv(file="Data/projections/diff_country_total_ssp270.csv")
    diff_country_total3 <- read.csv(file="Data/projections/diff_country_total_ssp370.csv")
    diff_country_total4 <- read.csv(file="Data/projections/diff_country_total_ssp470.csv")
    diff_country_total5 <- read.csv(file="Data/projections/diff_country_total_ssp570.csv")

    glimpse(diff_country_total3)
    glimpse(diff_country_total2)

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
    pivot_longer(
        cols = starts_with("X"),
        names_to = "year",
        values_to = "value"
    ) %>%
    # Remove the "X" prefix from the year column and convert to numeric
    mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")

    ssp_temp_long$temp2025 <- ssp_temp_long %>% filter(year==2025) %>% select(value) %>% unlist()
    glimpse(ssp_temp_long)
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2025
    ssp_temp_long$countrycode <- ssp_temp_long$region - ssp_temp_long$temp2025

    glimpse(ssp_gdp)

    diff_country_total <- rbind(diff_country_total1 %>% rename("POP"="POP_SSP1","GDP"="GDP_SSP1") %>% mutate(scenario="SSP1"),
                                diff_country_total2 %>% rename("POP"="POP_SSP2","GDP"="GDP_SSP2") %>% mutate(scenario="SSP2"),
                                diff_country_total3 %>% rename("POP"="POP_SSP3","GDP"="GDP_SSP3") %>% mutate(scenario="SSP3"),
                                diff_country_total4 %>% rename("POP"="POP_SSP4","GDP"="GDP_SSP4") %>% mutate(scenario="SSP4"),
                                diff_country_total5 %>% rename("POP"="POP_SSP5","GDP"="GDP_SSP5") %>% mutate(scenario="SSP5"))
    glimpse(diff_country_total)
        glimpse(diff_country_total1)
    
    benefit_ssp <- merge(ssp_gdp,diff_country_total,by=c("countrycode","year","scenario"),all=T)
    benefit_ssp <- merge(benefit_ssp ,ssp_temp_long,by=c("year"),all=F) %>% filter(year>2025)
    benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)
    glimpse(benefit_ssp)

    
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP370.csv")
    glimpse(scen_arealoss_perc_both)
    glimpse(diff_country_total3)
    benefit_ssp3 <- benefit_ssp %>% filter(scenario.x=="SSP3")
    glimpse(benefit_ssp3)

    ggplot(benefit_ssp3 %>% filter(countrycode=="MEX"))+
    geom_line(aes(x=year,y=diff_mangrove_area_future_loss))

    country_area_mangrove_2020 <- scen_arealoss_perc_both %>% group_by(countrycode) %>% summarize(area_2020 = sum(mangrove_area2020,na.rm=T))
    benefit_ssp3 <- merge(benefit_ssp3 ,country_area_mangrove_2020,by=c("countrycode"),all=F) 
    glimpse(benefit_ssp3)

    benefit_ssp3$frac_loss <- benefit_ssp3$diff_mangrove_area_future_loss/benefit_ssp3$area_2020

    ggplot(benefit_ssp3 %>% filter(countrycode=="MEX"))+
    geom_line(aes(x=year,y=frac_loss))
  
  coefficients_by_country <- benefit_ssp3 %>% filter(!is.na(frac_loss)) %>%
        #mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
        group_by(countrycode) %>%
        do({
            model_area <- lm(frac_loss ~ 0 + temp, data = .)
            data.frame(coefficient_temp = coef(model_area)["temp"],
            coefficient_temp_se = summary(model_area)$coefficients[2]
            )
        }) %>% ungroup()

    glimpse(coefficients_by_country)

    coefficients_by_country <-  merge(coefficients_by_country,country_area_mangrove_2020,by=c("countrycode"),all=F)
    names(coefficients_by_country)[4] <- "MangroveArea_2020_hectares"
    coefficients_by_country$Area_2020_hectares <- coefficients_by_country$Area_2020_hectares*100
    
    write.csv(coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients.csv")

    world_ne <- ne_countries(scale = "medium", returnclass = "sf")

    world_ne_with_coeffs <-  merge(world_ne,coefficients_by_country,by.x="iso_a3",by.y="countrycode",all=T)
    glimpse(world_ne_with_coeffs)

    # 1. Transform the coefficient_temp column
    world_ne_with_coeffs <- world_ne_with_coeffs %>%
    mutate(log_coeff_temp = log10(-coefficient_temp))

    library("scales")
    gg <- ggplot(data = world_ne_with_coeffs) +
    geom_sf(aes(fill = coefficient_temp)) +
    scale_fill_scico(palette = "lajolla",na.value="transparent",
        begin=0.1,end=0.9,direction=-1) +
    labs(fill = "Area Damage \n(Fraction per Degree C)") +
    theme_bw(legend="bottom")+
            coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))

    gg
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients.png")


## Area Change

## First APPROACH GDP
    weighted_avg_benefits <- scen_allforcings_allES %>% filter(year>2025) %>%
    group_by(countrycode, year,type) %>%
    #mutate(gdppc = (GDP_Country_ssp1*10^9)/(Pop_Country_ssp1*10^6)) %>%
    #mutate(gdppc = Pop_Country_ssp1*10^6) %>%
    mutate(gdp = GDP_Country_ssp1*10^9) %>%
    mutate(benefits_perha_percGDP = (100*benefits_perha)/(GDP_Country_ssp1*10^9)) %>%
    summarize(
        weighted_avg_benefit = sum(benefits_perha * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        weighted_avg_benefit_percGDP = sum(benefits_perha_percGDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        gdp = first(gdp),
        .groups = "drop"
    )

    # Summarize for "water" + "coastal"
    water_coastal <- weighted_avg_benefits %>%
    filter(type %in% c("water", "coastal")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_wc = sum(weighted_avg_benefit_percGDP, na.rm = TRUE),
        gdp = first(gdp),
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
    filter(total_weighted_avg_benefit_wc > 0, gdp > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdp), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdp), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdp), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_wc) ~ log(gdp), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_wc <- model_coefficients_regulating_mangroves

    model_coefficients_provisioning_mangroves <- combined_summary %>%
    group_by(countrycode)%>%
    filter(total_weighted_avg_benefit_fw > 0, gdp > 0) %>%
    do(intercept = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdp), data = .))$coefficients[1],
            elasticity = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdp), data = .))$coefficients[2],
            intercept_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdp), data = .))$coefficients[3],
            elasticity_se = summary(lm(log(total_weighted_avg_benefit_fw) ~ log(gdp), data = .))$coefficients[4]) %>%
    unnest(c(intercept,elasticity,intercept_se,elasticity_se))
    model_coefficients_fw <- model_coefficients_provisioning_mangroves



    ## PLOT (START)
        # Select a specific country, e.g., Australia (AUS)
        country_code <- "MEX"

        # Filter the data for the selected country
        data_country <- combined_summary %>%
        filter(countrycode == country_code, total_weighted_avg_benefit_wc > 0, gdp > 0)

        # Get the model coefficients for the country
        coeffs <- model_coefficients_wc %>%
        filter(countrycode == country_code)

        # Create a sequence of gdppc values for plotting
        gdppc_range <- seq(min(data_country$gdp), max(data_country$gdp), length.out = 100)

        # Calculate predicted values
        predicted_values <- exp(coeffs$intercept + coeffs$elasticity * log(gdppc_range))

        # Create a data frame for the predicted curve
        predicted_data <- data.frame(gdp = gdppc_range, total_weighted_avg_benefit_wc = predicted_values)

        # Plotting
        ggplot() +
        geom_point(data = data_country, aes(x = gdp, y = total_weighted_avg_benefit_wc), colour = "blue") +
        geom_line(data = predicted_data, aes(x = gdp, y = total_weighted_avg_benefit_wc), colour = "red") +
        labs(title = paste("Comparison of Actual Data and Estimated Curve for", country_code),
            x = "GDP",
            y = "Total Weighted Average Benefit WC") +
        theme_minimal()
    ## PLOT (END)
## First APPROACH

## Second APPROACH GDP per capita
    weighted_avg_benefits <- scen_allforcings_allES %>% filter(year>2025) %>%
    group_by(countrycode, year,type) %>%
    mutate(gdppc = (GDP_Country_ssp3*10^9)/(Pop_Country_ssp3*10^6)) %>%
    #mutate(gdppc = Pop_Country_ssp1*10^6) %>%
    #mutate(gdppc = GDP_Country_ssp1*10^9) %>%
    mutate(benefits_perha_percGDP = (100*benefits_perha)/(GDP_Country_ssp1*10^9)) %>%
    summarize(
        weighted_avg_benefit = sum(benefits_perha * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        #weighted_avg_benefit_percGDP = sum(benefits_perha_percGDP * mangrove_area, na.rm = TRUE) / sum(mangrove_area, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    # Summarize for "water" + "coastal"
    water_coastal <- weighted_avg_benefits %>%
    filter(type %in% c("water", "coastal")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_wc = sum(weighted_avg_benefit, na.rm = TRUE),
        gdppc = first(gdppc),
        .groups = "drop"
    )

    # Summarize for "food" + "wood"
    food_wood <- weighted_avg_benefits %>%
    filter(type %in% c("food", "wood")) %>%
    group_by(countrycode, year) %>%
    summarize(
        total_weighted_avg_benefit_fw = sum(weighted_avg_benefit, na.rm = TRUE),
        .groups = "drop"
    )

    # Bind the two summaries together
    combined_summary <- full_join(water_coastal, food_wood, by = c("countrycode", "year"))
    glimpse(combined_summary)


    # Summarize for "food" + "wood"
    use_values <- weighted_avg_benefits %>%
    group_by(countrycode, year) %>%
    summarize(
        avg_benefit_perha = sum(weighted_avg_benefit, na.rm = TRUE),
        .groups = "drop"
    )
    glimpse(use_values)
    
    # Bind the two summaries together
    combined_summary <- full_join(water_coastal, food_wood , by = c("countrycode", "year"))
    combined_summary <- full_join(combined_summary , use_values , by = c("countrycode", "year"))
    glimpse(combined_summary)

    # Fit log-log model for total_weighted_avg_benefit_wc
    model_coefficients_use_values <- combined_summary %>%
    group_by(countrycode)%>%
    filter(avg_benefit_perha  > 0, gdppc > 0) %>%
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