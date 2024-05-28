# -Physical_risk_base: present-day physical asset risk to port infrastructure (in USD/year)
# -Physical_risk: mid-century physical asset risk to port infrastructure (in USD/year)
# -Revenue_risk_base: present-day revenue risk to port operators from downtime (in USD/year)
# -Revenue_risk_CC_expension: mid-century revenue risk to port operators from downtime, which includes future trade flows (in USD/year)
# -Trade_risk_value_base: present-day risk to disrupted trade flows from downtime (in USD/year)
# -Trader_risk_value: mid-century  risk to disrupted trade flows from downtime, which includes future trade flows (in USD/year)
x<-c("ggplot2", "dplyr","WDI","ggpubr","scico")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

port_df <- read.csv("Data\\modules\\ports\\port_risk_midcentury.csv")
glimpse(port_df)

library(tidyverse)

# Creating data frame with risk values
risk_df <- port_df %>% 
  select(iso3, rcp, physical_risk, revenue_risk_CC_expansion, trade_risk_value) %>% 
  gather(key = "type", value = "risk", -iso3, -rcp) %>%
  mutate(type = case_when(
    type == "physical_risk" ~ "physical",
    type == "revenue_risk_CC_expansion" ~ "revenue",
    type == "trade_risk_value" ~ "trade"
  ))

# Creating data frame with risk base values
risk_base_df <- port_df %>% 
  select(iso3, rcp, physical_risk_base, revenue_risk_base, trade_risk_value_base) %>% 
  gather(key = "type", value = "risk_base", -iso3, -rcp) %>%
  mutate(type = case_when(
    type == "physical_risk_base" ~ "physical",
    type == "revenue_risk_base" ~ "revenue",
    type == "trade_risk_value_base" ~ "trade"
  ))

# Joining the two data frames
final_df <- risk_df %>%
  left_join(risk_base_df, by = c("iso3", "rcp", "type"))

ggplot(final_df)+
geom_boxplot(aes(x=rcp,y=risk-risk_base,fill=type)) + 
scale_y_log10()+
theme_bw()

gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 1990, end = 2022) #constant 2017 USD
glimpse(gdp_data)
gdp_data %>% filter(iso3=="IND")
names(gdp_data)[c(3,5)] <-  c("iso3","GDP_ppp")
port_df <- merge(port_df,gdp_data[which(gdp_data$year==2022),])



names(gdp_data)[c(3,5)] <-  c("iso3","GDP_ppp_2022")
final_df <- merge(final_df,gdp_data[which(gdp_data$year==2022),])
glimpse(final_df)
final_df$risk_base_perc <- 100*final_df$risk_base/final_df$GDP_ppp_2022




T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
T_ssp126 <- read.csv("Data/scenarios/SSP126_magicc_202308040902.csv")

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
        select(t_96_18,scenario) %>%
        inner_join(temp, by = c("scenario"))

    temp$tdif <- temp$temp - temp$t_96_18
    
    
    port_df <- final_df %>%
        mutate(across(where(is.character), toupper))
    names(port_df)[2] <- "scenario"

    port_df <- port_df %>% #I suspect scenarios were flipped accidentaly in the original dataset
        mutate(
            scenario= recode(scenario, 
            "RCP45" = "RCP85", 
            "RCP85" = "RCP45")
        )

    glimpse(port_df)
    port_df_temp <- merge(port_df,temp %>% filter(year==2050) %>% select(-year),by="scenario")

    ssps <- read.csv("C://Users/basti/Box/Data/SSPs/SspDb_country_data_2013-06-12.csv")
    glimpse(ssps)
    library("reshape")



    
    ssp <- melt(ssps, id = c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")) 
    
    ssp <- ssp[which(!is.na(ssp$value)),]
    ssp <- ssp[which(ssp$VARIABLE %in% c("GDP|PPP","Population")),]
    
    #s <-reshape(s, idvar = c("MODEL","SCENARIO","REGION","variable","UNIT"), timevar = "VARIABLE", direction = "wide")
    glimpse(ssp)    

    names(ssp) <- c("model","scenario","region","variable","unit","year","value")
    
    ssp$year <- as.double(sub('.', '', as.character(ssp$year)))
    glimpse(ssp)

    ssp$ssp <- substr(ssp$scenario,1,4)

    ssp_gdp2050 <- ssp %>% filter(variable == "GDP|PPP" & year ==2050) %>%
    group_by(ssp , region) %>%
    summarise(gdp = mean(value)) %>%
    group_by(region) %>%
    summarise(GDP_ppp_2050 = mean(gdp)*10^9)

        glimpse(ssp_gdp2050 )
        glimpse(port_df_temp)

    port_ssp <- merge(port_df_temp,ssp_gdp2050,by.y="region",by.x="iso3")
    glimpse(port_ssp)

    port_ssp$risk_perc <- 100 * port_ssp$risk / port_ssp$GDP_ppp_2050

    
    port_ssp$risk_change <- port_ssp$risk_perc - port_ssp$risk_base_perc

    glimpse(port_ssp)

    
    write.csv(port_ssp,"Data/modules/ports/ports_ssps.csv")
