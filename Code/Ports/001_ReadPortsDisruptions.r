#- Physical_risk_base is physical asset damages (in USD/yr) in base year (2015). 
#- Physical_risk is physical asset damages (in USD/yr) in 2050 under climate change. Different across RCPs but similar across SSP. 
#- Physical_risk_expansion is physical asset damages (in USD/yr) in 2050 under climate change and port expansions. Different across RCPs and SSP. 
#- Revenue_risk_base is revenue losses (in USD/yr) in base year (2015). 
#- Revenue_risk is revenue losses (in USD/yr) in 2050 under CC alone (no trade growth). Different across RCPs but similar across SSP. 
#- Revenue_risk_expansion is revenue losses (in USD/yr) in 2050 under CC and trade growth. Different across RCPs and SSP.
#- Trade at-risk. Separate dataset for imports and exports, could be added together for countries. 
#-trade_risk_mUSD_yr_base: trade at-risk (in million USD/yr) in base year (2015). 
#-trade_risk_mUSD_yr_2050: trade at-risk (in million USD/yr) in 2050 under both CC and trade growth. 


port_exp <- read.csv("Data\\input_modules\\ports\\future_country_exports_at_risk.csv")
port_imp <- read.csv("Data\\input_modules\\ports\\future_country_imports_at_risk.csv")
port_phy_rev <- read.csv("Data\\input_modules\\ports\\future_country_port_infra_revenue_risk.csv")
port_df_old <- read.csv("Data\\input_modules\\ports\\Old Data\\port_risk_midcentury.csv")

## Merge base risk and future risk (start)
  names(port_imp)[c(4:5)] <- paste0(names(port_imp)[c(4:5)],"_imp")## Merge import and exports
  names(port_exp)[c(4:5)] <- paste0(names(port_exp)[c(4:5)],"_exp")## Merge import and exports
  port_trade <- port_exp %>%## Merge import and exports
    inner_join(port_imp, by = c("iso3", "rcp", "SSP")) %>%
    mutate(trade_risk_USD_yr_base = 10^6*(trade_risk_mUSD_yr_base_exp + trade_risk_mUSD_yr_base_imp),
    trade_risk_USD_yr_2050 = 10^6*(trade_risk_mUSD_yr_2050_exp + trade_risk_mUSD_yr_2050_imp)
    )

  port_df <- port_trade %>%
    #inner_join(port_phy_rev, by=c("iso3","rcp","SSP"))%>%
    inner_join(port_phy_rev, by=c("iso3","rcp","SSP"))

  # Creating data frame with risk values
  risk_df <- port_df %>% 
  dplyr::select(iso3, rcp, SSP, physical_risk_expansion, revenue_risk_expansion, trade_risk_USD_yr_2050) %>% 
    gather(key = "type", value = "risk", -iso3, -rcp, -SSP) %>%
    mutate(type = case_when(
      type == "physical_risk_expansion" ~ "physical",
      type == "revenue_risk_expansion" ~ "revenue",
      type == "trade_risk_USD_yr_2050" ~ "trade"
    ))
  glimpse(risk_df)


  # Creating data frame with risk base values
  risk_base_df <- port_df %>% 
  dplyr::select(iso3, rcp, SSP, trade_risk_USD_yr_base, physical_risk_base, revenue_risk_base) %>% 
    gather(key = "type", value = "risk_base", -iso3, -rcp, -SSP) %>%
    mutate(type = case_when(
      type == "physical_risk_base" ~ "physical",
      type == "revenue_risk_base" ~ "revenue",
      type == "trade_risk_USD_yr_base" ~ "trade"
    ))

  # Creating data frame with risk base values
  risk_noexpansion_df <- port_df %>% 
  dplyr::select(iso3, rcp, SSP, physical_risk, revenue_risk) %>% 
    gather(key = "type", value = "risk_noexpansion", -iso3, -rcp, -SSP) %>%
    mutate(type = case_when(
      type == "physical_risk" ~ "physical",
      type == "revenue_risk" ~ "revenue"
    ))

  glimpse(risk_base_df)
  glimpse(risk_noexpansion_df)
  glimpse(risk_df)
  # Joining the two data frames
  final_df <- risk_df %>%
    left_join(risk_base_df, by = c("iso3", "rcp", "type","SSP"))
  final_df <- final_df %>%
    left_join(risk_noexpansion_df, by = c("iso3", "rcp", "type","SSP"))

  glimpse(final_df)

  gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 1990, end = 2022) #constant 2017 USD
  names(gdp_data)[c(3,5)] <-  c("iso3","GDP_ppp")
  port_df <- merge(port_df,gdp_data[which(gdp_data$year==2022),])



  names(gdp_data)[c(3,5)] <-  c("iso3","GDP_ppp_2022")
  final_df <- merge(final_df,gdp_data[which(gdp_data$year==2022),])

  final_df$risk_base_perc <- 100*final_df$risk_base/final_df$GDP_ppp_2022
## Merge base risk and future risk (end)

## Read RCPs Temps (start)

  T_ssp45 <- read.csv("Data/other/scenarios/SSP245_magicc_202303021423.csv")
  T_ssp85 <- read.csv("Data/other/scenarios/SSP585_magicc_202303221353.csv")
  T_ssp126 <- read.csv("Data/other/scenarios/SSP126_magicc_202308040902.csv")


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
  glimpse(temp3)

  temp <- rbind(temp,temp2,temp3)
    
      temp <-temp %>% group_by(scenario) %>%
          filter(year > 1996, year<2019) %>%
          mutate(t_96_18 = mean(temp)) %>%
          filter(year==1997) %>%
        dplyr::select(t_96_18,scenario) %>%
          inner_join(temp, by = c("scenario"))

      temp$tdif <- temp$temp - temp$t_96_18

  
    port_df <- final_df %>%
        mutate(across(where(is.character), toupper))
    glimpse(port_df)
    names(port_df)[2] <- "scenario"

    port_df_temp <- merge(port_df,temp %>% filter(year==2050) %>%dplyr::select(-year),by="scenario")
    glimpse(port_df_temp)
## Read RCPs Temps (end)  

## Read SSps GDP (start)
    ssps <- read.csv(paste0(dir_box,"/SSPs/SspDb_country_data_2013-06-12.csv"))
    glimpse(ssps)
    


    
    ssp <- melt(ssps, id = c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")) 
    
    ssp <- ssp[which(!is.na(ssp$value)),]
    ssp <- ssp[which(ssp$VARIABLE %in% c("GDP|PPP","Population")),]
    

    names(ssp) <- c("model","scenario","region","variable","unit","year","value")
    
    ssp$year <- as.double(sub('.', '', as.character(ssp$year)))

    ssp$ssp <- substr(ssp$scenario,1,4)

    ssp_gdp2050 <- ssp %>% filter(variable == "GDP|PPP" & year ==2050) %>%
    group_by(ssp , region) %>%
    summarise(GDP_ppp_2050 = mean(value)*10^9) 
    
    names(ssp_gdp2050)[c(1,2)] <- c("SSP","iso3")
## Read SSps GDP (end)
    
    port_ssp <-  port_df_temp %>%## Merge import and exports
                  inner_join(ssp_gdp2050, by = c("iso3", "SSP")) 
    port_ssp$risk_percGDP_2050 <- 100 * port_ssp$risk / port_ssp$GDP_ppp_2050
    port_ssp$risk_percGDP_2050_noexpansion <- 100 * port_ssp$risk_noexpansion / port_ssp$GDP_ppp_2050
    port_ssp$risk_percChange_2050 <- 100 * port_ssp$risk / port_ssp$risk_base
    port_ssp$risk_percChange_2050_noexpansion <- 100 * port_ssp$risk_noexpansion / port_ssp$risk_base
    port_ssp$risk_diffUSD_2050 <- port_ssp$risk - port_ssp$risk_base
    port_ssp$risk_diffUSD_2050 <- port_ssp$risk_noexpansion - port_ssp$risk_base 
    port_ssp$risk_percdiff_2050 <- 100 * port_ssp$risk / port_ssp$risk_base
    port_ssp$risk_percdiff_2050 <- 100 * port_ssp$risk_noexpansion / port_ssp$risk_base 
    
    port_ssp$risk_change_percGDP <- port_ssp$risk_percGDP_2050 - port_ssp$risk_base_perc 

    glimpse(port_ssp)
    names(port_ssp)[1] <- "RCP"
    glimpse(port_ssp)

    port_ssp %>% summarise(mean(risk_base_perc, na.rm=T))
    
    #write.csv(port_ssp,"Data/output_modules_input_rice50x/output_modules/ports/ports_ssps_rcps.csv")

    glimpse(port_ssp)
  
