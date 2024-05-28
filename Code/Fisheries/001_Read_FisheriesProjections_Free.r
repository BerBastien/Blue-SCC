x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","lfe","rnaturalearth","scales")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")
datadir <- "C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\modules\\fish\\Statistical"

data <- readRDS(file.path(datadir, "total_catch_profit_timeseries.Rds"))
glimpse(data)

levels(factor(data$rcp))
levels(factor(data$scenario))
levels(factor(data$year))

ggplot(data %>% filter(country_iso3=="MEX", 
                scenario %in% c("Full Adaptation", "No Adaptation"),
                year > 2019)) + 
geom_line(aes(x=year, y=profits_usd, linetype = scenario, color=rcp, group=interaction(country,rcp,scenario)))+
#guides(fill="none")+
theme_minimal() + 
#scale_y_continuous(lim=c(-100,200)) + 
xlab("year")+
ylab("profits_usd") #+ scale_fill_scico_d(palette="roma",direction=-1)


data_2scen <- data %>% filter(scenario %in% c("Full Adaptation", "No Adaptation"), !is.na(country_iso3))
glimpse(data_2scen)

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



glimpse(data_2scen_noduplicates)
glimpse(rcp_26_baseline)

# Step 2: Join this baseline back to the original dataset
data_with_baseline <- data_2scen_noduplicates  %>%
  left_join(rcp_26_baseline, by = c("country_iso3", "scenario", "year"))

glimpse(data_with_baseline)

# Step 3: Calculate the difference between each RCP scenario's profits and the baseline
data_with_diff <- data_with_baseline %>%
mutate(profit_diff_from_rcp_26 = profits_usd - profits_usd_baseline,
        profit_percDiff_from_rcp_26 =100* profits_usd / profits_usd_baseline-100)

glimpse(data_with_diff)



ggplot(data_with_diff %>% filter(country_iso3=="MEX", 
                scenario %in% c("Full Adaptation", "No Adaptation"),
                year > 2019)) + 
geom_line(aes(x=year, y=profit_diff_from_rcp_26, linetype = scenario, color=rcp, group=interaction(country_iso3,rcp,scenario)))+
theme_minimal() +  
xlab("year")+
ylab("profits_usd") 


#ggsave("Figures/SM/fisheries/profits_dif.png",dpi=600)


# glimpse(data)

# ggplot(data %>% filter(scenario=="No Adaptation")) + 
# geom_line(aes(x=rcp,y=p_pdiff,group=interaction(country,scenario),color=country))+
# guides(color="none")+
# theme_minimal() + 
# scale_y_continuous(lim=c(-100,80)) + 
# xlab("")+
# ylab("Difference in Country-level\n Profits from Fisheries (%)") + 
# scale_color_scico_d(direction=-1)+
# ggtitle("No Adaptation")
# ggsave("Figures/SM/fisheries/profits_dif_noadaptation.png",dpi=600)



# glimpse(data)

gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 2012, end = 2021) #constant 2017 USD

names(gdp_data)[c(3,5)] <-  c("sovereign_iso3","GDP_ppp")
glimpse(gdp_data)
gdp_data_sum <- gdp_data %>% group_by(sovereign_iso3) %>% summarise(gdp_ppp_2012_2021 = mean(GDP_ppp,na.rm=T)) %>% ungroup()
glimpse(gdp_data_sum)

#fisheries_df <- merge(data_with_diff,gdp_data_sum)
fisheries_df <- data_with_diff
glimpse(fisheries_df)


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



    ggplot(fisheries_df_temp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX")) + 
    geom_line(aes(x=tdif_from_rcp26,y=profit_diff_from_rcp_26,group=interaction(country_iso3,rcp),color=rcp),alpha=0.5)

    ggplot(fisheries_df_temp %>% filter(scenario=="Full Adaptation", country_iso3=="MEX")) + 
    geom_line(aes(x=tdif_from_rcp26,y=profit_percDiff_from_rcp_26,group=interaction(country_iso3,rcp),color=rcp),alpha=0.5)






## ADD GDP
ssps <- read.csv("C://Users/basti/Box/Data/SSPs/SspDb_country_data_2013-06-12.csv")
    glimpse(ssps)
    install.packages("reshape")
    library("reshape")


    
    ssp <- melt(ssps, id = c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")) 
    glimpse(ssp)
    ssp <- ssp[which(!is.na(ssp$value)),]
    ssp <- ssp[which(ssp$VARIABLE %in% c("GDP|PPP","Population")),]
    
    #s <-reshape(s, idvar = c("MODEL","SCENARIO","REGION","variable","UNIT"), timevar = "VARIABLE", direction = "wide")
    glimpse(ssp)    

    names(ssp) <- c("model","scenario","region","variable","unit","year","value")
    
    ssp$year <- as.double(sub('.', '', as.character(ssp$year)))
    glimpse(ssp)

    ssp$ssp <- substr(ssp$scenario,1,4)

    ssp_gdp <- ssp %>% filter(variable == "GDP|PPP") %>%
    group_by(ssp , region, year) %>%
    summarise(gdp = mean(value)) %>%
    group_by(region, year) %>%
    summarise(GDP_ppp = mean(gdp)*10^9)

    names(ssp_gdp)[1] <- "country_iso3"

        glimpse(ssp_gdp)
        glimpse(fisheries_df_temp)
    
    fisheries_df_temp_gdp <- fisheries_df_temp %>% left_join(ssp_gdp, by = c("country_iso3","year"))
## ADD GDP

glimpse(fisheries_df_temp_gdp)

library(countrycode)
fisheries_df_temp_gdp$continent <- countrycode(fisheries_df_temp_gdp$country_iso3,origin="iso3c",destination="continent")
fisheries_df_temp_gdp$region <- countrycode(fisheries_df_temp_gdp$country_iso3,origin="iso3c",destination="region")

fisheries_df_temp_gdp <- fisheries_df_temp_gdp %>% 
  mutate(new_continent = ifelse(region %in% c("Latin America & Caribbean", "North America"), region, continent)) %>% 
  mutate(new_continent = ifelse(new_continent %in% c("Americas"), "North America",new_continent))
levels(factor(fisheries_df_temp_gdp$new_continent))

multiplier <- data.frame(new_continent = levels(factor(fisheries_df_temp_gdp$new_continent)), 
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
    mutate(profits_usd_percGDP = 100*profits_usd * multiplier_value/ (1.34*GDP_ppp), #GDP in 2005 Int Dollars, profits in 2012 USD, using cumulative inflation: 
            profits_usd_percGDP_baseline = 100*profits_usd_baseline * multiplier_value / (1.34*GDP_ppp)) %>%
            mutate(profit_ppDiff_from_rcp_26 = profits_usd_percGDP - profits_usd_percGDP_baseline )



    write.csv(fisheries_df_temp_gdp,"Data/modules/fish/Statistical/fisheries_Free_EtAl.csv")








largest_profit_country <- fisheries_df_temp_gdp %>%
  filter(scenario == "Full Adaptation", 
         year > 2013, 
         !is.na(profits_usd_percGDP)) %>%
  group_by(country_iso3) %>%
  summarise(max_profit_percGDP = max(profits_usd_percGDP)) %>%
  ungroup() %>%
  arrange(desc(max_profit_percGDP)) %>%
  top_n(1, max_profit_percGDP)

# View the result
print(largest_profit_country)

#######






    
    guides(color="none")+
    theme_minimal() + 
    scale_y_continuous(lim=c(-100,80)) + 
    xlab("Temperature Increase by the end of the Century")+
    ylab("Difference in Country-level\n Profits from Fisheries (%)") + 
    scale_color_scico_d(direction=-1)+
    ggtitle("No Adaptation")
    ggsave("Figures/SM/fisheries/profits_dif_noadaptation.png",dpi=600)

    glimpse(fisheries_df_temp )
    
    levels(factor(fisheries_df_temp$rcp))
    
    ggplot(fisheries_df_temp %>% filter(scenario=="No Adaptation")) + 
    geom_line(aes(x=tdif,y=(100*p1/gdp_ppp_2012_2021),group=interaction(country,scenario),color=country),alpha=0.5)+
    geom_text_repel(data=fisheries_df_temp %>% filter(scenario=="No Adaptation" & rcp=="RCP85" & (-(p2-p1)/gdp_ppp_2012_2021) >0.1),
        aes(x=tdif+0.3,y=(100*(p1)/gdp_ppp_2012_2021),group=interaction(country,scenario),color=country,label=sovereign),
        alpha=0.5, max.pverlaps=200)+
    guides(color="none")+
    theme_minimal() + 
    #scale_y_continuous(lim=c(-100,80)) + 
    #coord_y_continuous(trans=log)+
    xlab("Temperature Increase by the end of the Century\nw.r.t. 2012-2021 period")+
    ylab("Profit from Fisheries \n (% of present GDP)") + 
    scale_color_scico_d(direction=-1)+
    ggtitle("No Adaptation")

    library("ggrepel")  
    ggplot(fisheries_df_temp %>% filter(scenario=="No Adaptation")) + 
    geom_line(aes(x=tdif,y=((p2-p1)/gdp_ppp_2012_2021),group=interaction(country,scenario),color=country),alpha=0.5)+
    geom_text_repel(data=fisheries_df_temp %>% filter(scenario=="No Adaptation" & rcp=="RCP85" & (-(p2-p1)/gdp_ppp_2012_2021) >0.1),
        aes(x=tdif+0.3,y=((p2-p1)/gdp_ppp_2012_2021),group=interaction(country,scenario),color=country,label=sovereign),
        alpha=0.5, max.pverlaps=200)+
    guides(color="none")+
    theme_minimal() + 
    #scale_y_continuous(lim=c(-100,80)) + 
    #coord_y_continuous(trans=log)+
    xlab("Temperature Increase by the end of the Century\nw.r.t. 2012-2021 period")+
    ylab("Profit Loss from Fisheries Damages\n (Fraction of present GDP)") + 
    scale_color_scico_d(direction=-1)+
    ggtitle("No Adaptation")+
    xlim(0,4.5)
    ggsave("Figures/SM/fisheries/profits_dif_noadaptation_temp_percGDP.png",dpi=600)


    fisheries_df_temp <- fisheries_df_temp %>%
    mutate(loss=(p2-p1)/gdp_ppp_2012_2021)


        glimpse(  fisheries_df_temp)
    library('tidyverse')
    fisheries_tcoeff <- fisheries_df_temp%>%
        filter(!is.na(loss)) %>%
        group_by(sovereign_iso3) %>%
        nest() %>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(loss ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(loss~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(loss ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
             })
            
        )

        glimpse(fisheries_tcoeff)

        fisheries_tcoeff <- fisheries_tcoeff %>%
        unnest(data) %>% slice(1) %>% ungroup %>% select(sovereign_iso3,tcoeff,se,pval) %>% as.data.frame()
        

        save(fisheries_tcoeff,file="Data/Modules/fish/fisheries_tcoeff.Rds")
        load(file="Data/Modules/fish/fisheries_tcoeff.Rds")
        glimpse(fisheries_tcoeff)

    fisheries_tcoeff$tcoeff <- fisheries_tcoeff$tcoeff*100
    fisheries_tcoeff$se <- fisheries_tcoeff$se*100
    write.csv(fisheries_tcoeff,file="Data/intermediate_output/fisheries_tcoeff.csv")


    # Get the world map in sf format
    library("rnaturalearth")
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, fisheries_tcoeff, by = c("iso_a3" = "sovereign_iso3"))
    merged_data <- left_join(merged_data, fisheries_df_temp %>% group_by(sovereign_iso3) %>% slice(1) %>% ungroup, by = c("iso_a3" = "sovereign_iso3"))

    glimpse(merged_data)

    # Plot

    max(merged_data$tcoeff,na.rm=T)
    max(merged_data$tcoeff,na.rm=T)

    merged_data %>% filter(tcoeff>0)
    merged_data %>% filter(region_un== "Seven seas (open ocean)")
    
    ggplot(merged_data)+geom_histogram(aes(x=-tcoeff))    +
    scale_x_continuous(trans="log")

    merged_data <- merged_data %>% filter(!is.na(tcoeff) & region_un!= "Seven seas (open ocean)")
    merged_data$region_un <- factor(merged_data$region_un)
    merged_data$region_un <- droplevels(merged_data$region_un)
    levels(merged_data$region_un)

    library(ggrepel)
    Loss_perC <- ggplot(merged_data )+geom_point(aes(x=gdp_ppp_2012_2021,y=-tcoeff,color=region_un)) +
    geom_text_repel(data=merged_data %>% filter(abs(tcoeff)>1),aes(x=gdp_ppp_2012_2021,y=-tcoeff,label=iso_a3,color=region_un))+
    scale_x_continuous(trans="log")+
    scale_y_continuous(trans="log")+
    scale_color_scico_d(begin=0.1,end=0.8)+
    theme_bw() + 
    xlab("GDP")+ylab("%GDP Loss per Degree C") + 
    labs(color="")+theme(legend.position="bottom")
    

    
    Map_Loss <- ggplot(data = merged_data) +
    geom_sf(aes(fill = -tcoeff)) +
    scale_fill_scico(palette = "lajolla", limits = c(0, 0.1), oob=squish) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "%GDP Loss per Degree C")+theme(legend.position="bottom")
    
    ggarrange(Map_Loss,Loss_perC,widths=c(2,1.1))
    ggsave("Figures/SM/fisheries/MapFisheries_Coef_2.png",dpi=600)

        


    
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

    ssp_gdp <- ssp %>% filter(variable == "GDP|PPP") %>%
    group_by(ssp , region, year) %>%
    summarise(gdp = mean(value)) %>%
    group_by(region, year) %>%
    summarise(GDP_ppp = mean(gdp)*10^9)

        glimpse(ssp_gdp)
        glimpse(port_df_temp)

    port_ssp <- merge(port_df_temp,ssp_gdp2050,by.y="region",by.x="iso3")
    glimpse(port_ssp)

    port_ssp$risk_perc <- 100 * port_ssp$risk / port_ssp$GDP_ppp_2050

    
    port_ssp$risk_change <- port_ssp$risk_perc - port_ssp$risk_base_perc

    glimpse(port_ssp)

    




































gaines_ap1 <- read.csv(paste0(datadir,"gaines_country_level_results_approach1_2050.csv"))
glimpse(gaines_ap1)
levels(factor(gaines_ap1$scenario))

ggplot(gaines_ap1) + 
geom_boxplot(aes(x=rcp,y=p_pdiff,fill=scenario))+
#guides(fill="none")+
theme_minimal()

ggplot(gaines_ap1 %>% filter(scenario=="No Adaptation")) + 
geom_boxplot(aes(x=rcp,y=p_pdiff,fill=scenario))+
#guides(fill="none")+
theme_minimal()

ggplot(gaines_ap1 %>% filter(scenario=="No Adaptation" & sovereign_iso3 !="Disputed" & sovereign_iso3 !="Joint")) + 
geom_line(aes(x=rcp,y=p_pdiff,color=sovereign_iso3,group=sovereign_iso3))+
geom_text(aes(x=rcp,y=p_pdiff,label=sovereign_iso3))+
#guides(fill="none")+
theme_minimal()

glimpse(gaines_ap1)

ggplot(gaines_ap1 %>% filter(scenario=="No Adaptation" & sovereign_iso3 !="Disputed" & sovereign_iso3 !="Joint")) + 
geom_point(aes(x=log(p1),y=p_pdiff,color=rcp))+
#geom_text(aes(x=rcp,y=p_pdiff,label=sovereign_iso3))+
#guides(fill="none")+
theme_minimal()
