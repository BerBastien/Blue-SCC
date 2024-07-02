## Load Data (start)
  datadir <- "Data\\modules\\fish\\Statistical\\"
  data <- readRDS(paste0(datadir, "total_catch_profit_timeseries.Rds"))
## Load Data (End)

## Get baseline (start)

  data_2scen <- data %>% filter(scenario %in% c("Full Adaptation", "No Adaptation"), !is.na(country_iso3))
  data_2scen_noduplicates <- data_2scen %>% group_by(country_iso3, scenario, year,rcp) %>%
    summarise(profits_usd = mean(profits_usd, na.rm = TRUE))

  rcp_26_baseline <- data_2scen_noduplicates %>%
    filter(rcp == "RCP 2.6",scenario %in% c("Full Adaptation", "No Adaptation")) %>%
    select(country_iso3, scenario, year, profits_usd_baseline = profits_usd)


  duplicated_rows <- rcp_26_baseline %>%
    group_by(country_iso3, scenario, year) %>%
    summarise(n = n()) %>%
    filter(n > 1, year==2020)

  if(nrow(duplicated_rows) > 0) {
    print("Duplicates found")
  } else {
    print("No duplicates")
  }

  # Step 2: Join this baseline back to the original dataset
  data_with_baseline <- data_2scen_noduplicates  %>%
    left_join(rcp_26_baseline, by = c("country_iso3", "scenario", "year"))

  glimpse(data_with_baseline)

  # Step 3: Calculate the difference between each RCP scenario's profits and the baseline
  data_with_diff <- data_with_baseline %>%
  mutate(profit_diff_from_rcp_26 = profits_usd - profits_usd_baseline,
          profit_percDiff_from_rcp_26 =100* profits_usd / profits_usd_baseline-100)

  glimpse(data_with_diff)

## Get baseline (end)




gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 2012, end = 2021) #constant 2017 USD

names(gdp_data)[c(3,5)] <-  c("sovereign_iso3","GDP_ppp")
gdp_data_sum <- gdp_data %>% group_by(sovereign_iso3) %>% summarise(gdp_ppp_2012_2021 = mean(GDP_ppp,na.rm=T)) %>% ungroup()
fisheries_df <- data_with_diff
glimpse(fisheries_df)

## Temp SSPS (start)
  T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
  T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
  T_ssp126 <- read.csv("Data/scenarios/SSP126_magicc_202308040902.csv")
  T_ssp460 <- read.csv("Data/scenarios/SSP460_magicc_202402051249.csv")

  temp1 <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
  temp1$year <- as.integer(sub('X', '', temp1$year))
  names(temp1)[1] <- "temp"
  temp1$scenario <- "RCP45"

  temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
  temp2$year <- as.integer(sub('X', '', temp2$year))
  names(temp2)[1] <- "temp"
  temp2$scenario <- "RCP85"


  temp3 <- data.frame(temp = t(T_ssp126[7,c(13:length(T_ssp126))]), year = names(T_ssp126[17,c(13:length(T_ssp126))]))
  glimpse(temp3)
  temp3$year <- as.integer(sub('X', '', temp3$year))
  names(temp3)[1] <- "temp"
  temp3$scenario <- "RCP26"


  temp4 <- data.frame(temp = t(T_ssp460[16,c(13:length(T_ssp460))]), year = names(T_ssp460[1,c(13:length(T_ssp460))]))
  glimpse(temp4)
  temp4$year <- as.integer(sub('X', '', temp4$year))
  names(temp4)[1] <- "temp"
  temp4$scenario <- "RCP60"
  glimpse(temp4)

  temp <- rbind(temp1,temp2,temp3,temp4)
  glimpse(temp)
    
      temp <-temp %>% group_by(scenario) %>%
          filter(year > 2011, year<2022) %>%
          mutate(t_12_21 = mean(temp,na.rm=T)) %>%
          filter(year==2012) %>%
          select(t_12_21,scenario) %>%
          inner_join(temp, by = c("scenario"))

      temp$tdif <- temp$temp - temp$t_12_21
  glimpse(temp)
  tdif_rcp26 <- temp %>%
    filter(scenario == "RCP26") %>%
    select(year, tdif_rcp26= tdif, -scenario)

  temp <- temp %>%
    left_join(tdif_rcp26 , by = "year")%>% select(-scenario.y) %>% dplyr::rename(scenario=scenario.x)
  glimpse(temp)

  temp <- temp %>%
    mutate(tdif_from_rcp26 = tdif - tdif_rcp26)

  ggplot(temp,aes(x=year,y=tdif))+geom_point(aes(color=scenario))    
      
      fisheries_df2 <- fisheries_df %>%
          mutate(across(where(is.character), toupper))
      names(temp)[2] <- "rcp"

      fisheries_df <- fisheries_df %>%
    mutate(rcp = gsub(" ", "", rcp), # Remove spaces
          rcp = gsub("\\.", "", rcp))

      glimpse(fisheries_df)
      glimpse(temp)
      
      fisheries_df_temp <- fisheries_df %>%
                          left_join(temp %>% group_by(rcp,year) %>% summarise(tdif_from_rcp26=mean(tdif_from_rcp26,na.rm=T)),by=c("rcp", "year"))
      glimpse(fisheries_df_temp)
      glimpse(temp)
## Temp SSPS (start)

## ADD GDP (start)
  ssps <- read.csv(paste0(dir_box,"/SSPs/SspDb_country_data_2013-06-12.csv"))
  glimpse(ssps)
    
    ssp <- melt(ssps, id = c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")) 
    ssp <- ssp[which(!is.na(ssp$value)),]
    ssp <- ssp[which(ssp$VARIABLE %in% c("GDP|PPP","Population")),]
    glimpse(ssp)    

    names(ssp) <- c("model","scenario","region","variable","unit","year","value")
    
    ssp$year <- as.double(sub('.', '', as.character(ssp$year)))

    ssp$ssp <- substr(ssp$scenario,1,4)

    ssp_gdp <- ssp %>% filter(variable == "GDP|PPP") %>%
    group_by(ssp , region, year) %>%
    summarise(gdp = mean(value)) %>%
    group_by(region, year) %>%
    summarise(GDP_ppp = mean(gdp)*10^9)

    names(ssp_gdp)[1] <- "country_iso3"
    
    fisheries_df_temp_gdp <- fisheries_df_temp %>% left_join(ssp_gdp, by = c("country_iso3","year"))

## ADD GDP (end)

## Add Continent and multipliers (start)
  glimpse(fisheries_df_temp_gdp)
  fisheries_df_temp_gdp$continent <- countrycode(fisheries_df_temp_gdp$country_iso3,origin="iso3c",destination="continent")
  fisheries_df_temp_gdp$region <- countrycode(fisheries_df_temp_gdp$country_iso3,origin="iso3c",destination="region")

  fisheries_df_temp_gdp <- fisheries_df_temp_gdp %>% 
    mutate(new_continent = ifelse(region %in% c("Latin America & Caribbean", "North America"), region, continent)) %>% 
    mutate(new_continent = ifelse(new_continent %in% c("Americas"), "North America",new_continent))
  levels(factor(fisheries_df_temp_gdp$new_continent))

  multiplier <- data.frame(new_continent = c("Africa","Asia","Europe","Latin America & Caribbean","North America","Oceania"), 
  multiplier_value = c(1+2.59+0.62, #Africa
                1+2.67+0.62, #Asia
                1+3.12+0.76, #Europe
                1+2.05+0.56, #LA
                1+3.52+1.22, #NA
                1+3.27 +0.73)) #Oceania)) #From: https://www.researchgate.net/publication/227347673_Economic_Impact_of_Ocean_Fish_Populations_in_the_Global_Fishery
            

  fisheries_df_temp_gdp <- fisheries_df_temp_gdp %>% 
    left_join(multiplier,by="new_continent") %>% 
    mutate(multiplier_value=ifelse(is.na(multiplier_value),4.55,multiplier_value)) #world average


  fisheries_df_temp_gdp <- fisheries_df_temp_gdp %>% 
      mutate(profits_usd_percGDP = 100*profits_usd * multiplier_value/ (1.14*GDP_ppp), # 95.5/83.6  https://databank.worldbank.org/source/world-development-indicators/Series/NY.GDP.DEFL.ZS
              profits_usd_percGDP_baseline = 100*profits_usd_baseline * multiplier_value / (1.14*GDP_ppp)) %>%
              mutate(profit_ppDiff_from_rcp_26 = profits_usd_percGDP - profits_usd_percGDP_baseline )
## Add Continent and multipliers (end)

  glimpse(fisheries_df_temp_gdp)

  write.csv(fisheries_df_temp_gdp,"Data/modules/fish/Statistical/fisheries_Free_EtAl.csv")



