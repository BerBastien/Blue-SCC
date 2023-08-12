x<-c("ggplot2", "dplyr","WDI","scico")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")
datadir <- "C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\modules\\fish\\"

#load("Data\\modules\\fish\\gaines_data_for_eez_analysis.Rds")
data <- readRDS(file.path(datadir, "gaines_eez_level_results_approach1_2100.Rds"))
glimpse(data)

ggplot(data) + 
geom_boxplot(aes(x=rcp,y=p_pdiff,fill=scenario))+
#guides(fill="none")+
theme_minimal() + 
scale_y_continuous(lim=c(-100,200)) + 
xlab("")+
ylab("Difference in Country-level\n Profits from Fisheries (%)") + 
scale_fill_scico_d(palette="roma",direction=-1)
ggsave("Figures/SM/fisheries/profits_dif.png",dpi=600)


glimpse(data)

ggplot(data %>% filter(scenario=="No Adaptation")) + 
geom_line(aes(x=rcp,y=p_pdiff,group=interaction(country,scenario),color=country))+
guides(color="none")+
theme_minimal() + 
scale_y_continuous(lim=c(-100,80)) + 
xlab("")+
ylab("Difference in Country-level\n Profits from Fisheries (%)") + 
scale_color_scico_d(direction=-1)+
ggtitle("No Adaptation")
ggsave("Figures/SM/fisheries/profits_dif_noadaptation.png",dpi=600)



glimpse(data)

gdp_data <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 2012, end = 2021) #constant 2017 USD

names(gdp_data)[c(3,5)] <-  c("sovereign_iso3","GDP_ppp")
glimpse(gdp_data)
gdp_data_sum <- gdp_data %>% group_by(sovereign_iso3) %>% summarise(gdp_ppp_2012_2021 = mean(GDP_ppp,na.rm=T)) %>% ungroup()
glimpse(gdp_data_sum)
fisheries_df <- merge(data,gdp_data_sum)
glimpse(fisheries_df)


T_ssp45 <- read.csv("Data/scenarios/SSP245_magicc_202303021423.csv")
T_ssp85 <- read.csv("Data/scenarios/SSP585_magicc_202303221353.csv")
T_ssp126 <- read.csv("Data/scenarios/SSP126_magicc_202308040902.csv")

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
glimpse(temp3)

temp <- rbind(temp1,temp2,temp3)
glimpse(temp)
   
    temp <-temp %>% group_by(scenario) %>%
        filter(year > 2011, year<2022) %>%
        mutate(t_12_21 = mean(temp,na.rm=T)) %>%
        filter(year==2012) %>%
        select(t_12_21,scenario) %>%
        inner_join(temp, by = c("scenario"))

    temp$tdif <- temp$temp - temp$t_12_21
    
    
    fisheries_df2 <- fisheries_df %>%
        mutate(across(where(is.character), toupper))
    names(temp)[2] <- "rcp"

    fisheries_df_temp <- merge(fisheries_df,temp %>% filter(year>2090) %>% group_by(rcp) %>% summarise(tdif=mean(tdif,na.rm=T)),by="rcp")
    glimpse(fisheries_df_temp)
    glimpse(temp)



    ggplot(fisheries_df_temp %>% filter(scenario=="No Adaptation")) + 
    geom_line(aes(x=tdif,y=p_pdiff,group=interaction(country,scenario),color=country),alpha=0.5)+
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
    ylab("Profit Loss from Fisheries Damages\n (% of present GDP)") + 
    scale_color_scico_d(direction=-1)+
    ggtitle("No Adaptation")+
    xlim(0,4.5)
    ggsave("Figures/SM/fisheries/profits_dif_noadaptation_temp_percGDP.png",dpi=600)


    
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
