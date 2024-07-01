#setup 


    dir1 <- 'Data\\modules\\mangroves\\'
    
#setup 

# Read future mangroves cover (start)
    mangroves <- read.csv(paste0(dir1,"scen_arealoss_perc_onlyCC_countrycode.csv"))
    glimpse(mangroves)
    
    T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
    T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
    temp <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
    temp$year <- as.integer(sub('X', '', temp$year))
    names(temp)[1] <- "temp"
    temp$scenario <- "RCP45"

    temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
    temp2$year <- as.integer(sub('X', '', temp2$year))
    names(temp2)[1] <- "temp"
    temp2$scenario <- "RCP85"

    temp <- rbind(temp,temp2)
    #temp <- temp2
    glimpse(temp)

    temp <-temp %>% group_by(scenario) %>%
    filter(year > 1996, year<2019) %>%
    mutate(t_96_18 = mean(temp)) %>%
    filter(year==1997) %>%
    dplyr::select(t_96_18,scenario) %>%
    inner_join(temp, by = c("scenario"))

    glimpse(temp)

    temp$tdif <- temp$temp - temp$t_96_18

    
    mangroves_temp <- merge(mangroves,temp,by=c("year"),all=FALSE)
    glimpse(mangroves_temp)
    

    ggplot(mangroves_temp)+
    geom_line(aes(x=tdif,y=area,color=scenario,group=interaction(countrycode,scenario)), alpha = 0.5)+
    theme_bw()



# Read future mangrove cover (end)



## Estimate Temperature Coefficient (start)
    # Calculate the percent change in mangrove area
    # Calculate the percent change in mangrove area from the area in 2021
    mangroves_temp <- mangroves_temp %>%
    group_by(countrycode) %>%
    mutate(area_2021 = first(area[year == 2021]),
            area_pct_change = (area - area_2021) / area_2021 * 100) %>% ungroup()
    
    glimpse(mangroves_temp)

    # Fit a linear model for each country and extract the coefficient for tdif
    results <- mangroves_temp %>% filter(countrycode != "BRB", countrycode != "MDV", countrycode != "SLV") %>%
    group_by(countrycode) %>%
    nest()%>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(area_pct_change~ 0 + I(tdif) | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(area_pct_change ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(area_pct_change ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
            })
        )

        glimpse(results)
    
        mangrove_tcoeff <- results %>%
        slice(1) %>% dplyr::select(countrycode, tcoeff, se, pval)  %>% ungroup()
        
        #save(mangrove_tcoeff,file="Data/Modules/Mangroves/mangroves_tcoeff.Rds")
        glimpse( mangrove_tcoeff )

    mangrove_temp_df <- merge(mangrove_tcoeff,mangroves_temp %>% filter(year==2021),) %>% dplyr::select(-pval,-year,-X,-t_96_18,-scenario,-temp,-tdif,-area,-area_pct_change)
    names(mangrove_temp_df)[c(1:4)] <- c("countrycode","DamCoef_changeperC","DamCoeff_se","area_2021_km2")
    glimpse(mangrove_temp_df)
    
    mangrove_temp_df$DamCoef_changeperC <- mangrove_temp_df$DamCoef_changeperC*0.01
    mangrove_temp_df$DamCoeff_se <- mangrove_temp_df$DamCoeff_se*0.01
    mangrove_temp_df$provisioning_value_perkm2year <- 2998
    mangrove_temp_df$regulating_value_perkm2year <- 171515
    mangrove_temp_df$habitat_value_perkm2year <- 17138
    mangrove_temp_df$cultural_value_perkm2year <- 2193

    
    write.csv(mangrove_temp_df,file="Data/intermediate_output/mangroves_area_damage_valuer.csv")

## Estimate Temperature Coefficient (end)
