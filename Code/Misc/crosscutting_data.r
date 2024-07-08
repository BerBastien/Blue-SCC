## Loading Cross-cutting dataframes


    regions  <- read.csv('Data\\other\\r5regions.csv')
    names(regions) <- c("R5","countrycode")
    regions$R5 <- as.character(gsub("R5", "", regions$R5))
    ssp_gdp <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv')
    ssp_pop <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_pop.csv')
    ssp_gdp_pop <- ssp_pop %>% left_join(ssp_gdp,by=c("scenario","ISO3","year")) %>% rename(ssp=scenario)
            
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP245_magicc_202303021423.csv")
    countries_in_ssps <- unique(ssp_pop$ISO3)            

    ssp_temp %>% dplyr::select(scenario)
    ssp_gdp$countrycode <- ssp_gdp$ISO3
    ssp_pop$countrycode <- ssp_pop$ISO3
    ssp_temp_long <- ssp_temp %>%
        tidyr::pivot_longer(
            cols = starts_with("X"),
            names_to = "year",
            values_to = "value"
        ) %>%
        mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")



    ssp_temp_long$temp2020 <- ssp_temp_long %>% filter(year==2020) %>% dplyr::select(value) %>% unlist()
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2020




    #deflator
    deflator_data <- WDI(country = "USA", indicator = c("NY.GDP.DEFL.ZS"), start = 2000, end = 2021)
    def_mult <- deflator_data %>% 
        summarize(def_2005_to_2020 =NY.GDP.DEFL.ZS[year==2020]/NY.GDP.DEFL.ZS[year==2005] )

##



            T_ssp45 <- read.csv("Data/other/scenarios/SSP245_magicc_202303021423.csv")
            T_ssp85 <- read.csv("Data/other/scenarios/SSP585_magicc_202303221353.csv")
            T_ssp126 <- read.csv("Data/other/scenarios/SSP126_magicc_202308040902.csv")
            T_ssp460 <- read.csv("Data/other/scenarios/SSP460_magicc_202402051249.csv")

            temp1 <- data.frame(temp = t(T_ssp45[17,c(13:length(T_ssp45))]), year = names(T_ssp45[17,c(13:length(T_ssp45))]))
            temp1$year <- as.integer(sub('X', '', temp1$year))
            names(temp1)[1] <- "temp"
            temp1$rcp <- "RCP45"
            temp1$ssp <- "SSP2"

            temp2 <- data.frame(temp = t(T_ssp85[17,c(13:length(T_ssp85))]), year = names(T_ssp85[17,c(13:length(T_ssp85))]))
            temp2$year <- as.integer(sub('X', '', temp2$year))
            names(temp2)[1] <- "temp"
            temp2$rcp <- "RCP85"
            temp2$ssp <- "SSP5"


            temp3 <- data.frame(temp = t(T_ssp126[7,c(13:length(T_ssp126))]), year = names(T_ssp126[17,c(13:length(T_ssp126))]))
            glimpse(temp3)
            temp3$year <- as.integer(sub('X', '', temp3$year))
            names(temp3)[1] <- "temp"
            temp3$rcp <- "RCP26"
            temp3$ssp <- "SSP1"


            temp4 <- data.frame(temp = t(T_ssp460[16,c(13:length(T_ssp460))]), year = names(T_ssp460[1,c(13:length(T_ssp460))]))
            glimpse(temp4)
            temp4$year <- as.integer(sub('X', '', temp4$year))
            names(temp4)[1] <- "temp"
            temp4$rcp <- "RCP60"
            temp4$ssp <- "SSP4"
            glimpse(temp4)

            temp <- rbind(temp1,temp2,temp3,temp4)
            glimpse(temp)
                
                temp <-temp %>% group_by(rcp) %>%
                    filter(year == 2021) %>%
                    mutate(t_12_21 = mean(temp,na.rm=T)) %>%
                    #filter(year==2012) %>%
                    select(t_12_21,rcp) %>%
                    inner_join(temp, by = c("rcp"))

                temp$tdif <- temp$temp - temp$t_12_21
            glimpse(temp)

            ssp_gdp_pop <- ssp_gdp_pop %>% left_join(temp,by=c("ssp","year"))
            glimpse(ssp_gdp_pop)
