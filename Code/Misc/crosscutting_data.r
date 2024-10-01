## Loading Cross-cutting dataframes


    regions  <- read.csv('Data\\other\\r5regions.csv')
    names(regions) <- c("R5","countrycode")
    ed57  <- read.csv('Data/other/ed57regions.csv')
    regions$R5 <- as.character(gsub("R5", "", regions$R5))
    ssp_gdp <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv')
    ssp_pop <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_pop.csv')
    ssp_gdp_pop <- ssp_pop %>% left_join(ssp_gdp,by=c("scenario","ISO3","year")) %>% rename(ssp=scenario)
            
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP245_magicc_202303021423.csv")
    countries_in_ssps <- unique(ssp_pop$ISO3)            

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


    
    corals_df_iso_with_gdp <- read.csv(file="Data/output_modules_input_rice50x/input_rice50x/corals_areaDam_Value.csv")
    ssp_corals <- ssp_gdp %>% left_join(corals_df_iso_with_gdp,by="countrycode")
        ssp_corals <- ssp_corals %>% left_join(ssp_pop,by=c("countrycode","scenario","year"))
        ssp_corals <- ssp_corals %>% left_join(ssp_temp_long %>% dplyr::select(-scenario),by="year")



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


## Functions

# find_surpass_year <- function(df, country_col, year_col, c_dif_col, threshold = 1) {
#   df %>%
#     arrange(!!sym(country_col), !!sym(year_col)) %>%
#     group_by(!!sym(country_col)) %>%
#     summarize(
#       year_surpass = approx(
#         x = !!sym(c_dif_col), 
#         y = !!sym(year_col), 
#         xout = threshold, 
#         rule = 2 # if set to 2 Allows extrapolation if the threshold is not within the data range
#       )$y[1]
#     ) %>%
#     ungroup()
# }

find_surpass_year <- function(df, country_col, year_col, c_dif_col, threshold = 1) {
  df %>%
    arrange(!!sym(country_col), !!sym(year_col)) %>%
    group_by(!!sym(country_col)) %>%
    mutate(
      # Check if the sign of the value at year 2100 is different from the threshold
      is_opposite_sign_in_2100 = ifelse(
        any(!!sym(year_col) == 2100 & sign(!!sym(c_dif_col)) != sign(threshold)),
        TRUE, FALSE
      )
    ) %>%
    summarize(
      year_surpass = if_else(
        is_opposite_sign_in_2100, 
        99999,  # Assign 9999 if the sign is opposite in year 2100
        approx(
          x = !!sym(c_dif_col), 
          y = !!sym(year_col), 
          xout = threshold, 
          rule = 2  # Allows extrapolation if the threshold is not within the data range
        )$y[1]
      )
    ) %>%
    ungroup()
}


## Functions

    #var_names <- c("C","ocean_consump_damage_coef","OCEAN_NONUSE_VALUE","OCEAN_USENM_VALUE","ocean_consump_damage_coef_sq","ocean_health_tame","CPC","CPC_OCEAN_DAM")
    var_names <- c("C","ocean_consump_damage_coef","YNET")
    exp_names <- c("today","damage")

    process_var_table <- function(var_table, exp_name, var_name) {
        var_table <- var_table[3:nrow(var_table), ]
        var_table <- as.data.frame(var_table)
        print(var_name)
        
        if (ncol(var_table) < 4) {
            if (var_name %in%  c("scc","pop")) {
                names(var_table) <- c("year", "country", var_name)
                var_table$country <- as.factor(var_table$country)
                var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
                var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
                var_table$exp <- exp_name
                var_table$id <- paste(var_table$country, var_table$year, var_table$exp, sep = "")
            } else {
                names(var_table) <- c("capital", "country", var_name)
                var_table$country <- as.factor(var_table$country)
                var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
                var_table$exp <- exp_name
                var_table$capital <- as.factor(var_table$capital)
            }
        } else {
            if (ncol(var_table) < 7) {
                if(var_name=="VSL"){
                    names(var_table) <- c("year", paste0(var_name, "_low"), var_name, paste0(var_name, "_high"), paste0(var_name, "_marginal"))
                }else{
                names(var_table) <- c("year", "country", paste0(var_name, "_low"), var_name, paste0(var_name, "_high"), paste0(var_name, "_marginal"))
                    var_table$country <- as.factor(var_table$country)
                }
            } else {
                names(var_table) <- c("capital", "year", "country", paste0(var_name, "_low"), var_name, paste0(var_name, "_high"), paste0(var_name, "_marginal"))
                var_table$capital <- as.factor(var_table$capital)
                var_table$country <- as.factor(var_table$country)
            }
            var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
            var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
            var_table$exp <- exp_name
            #var_table$id <- paste(var_table$country, var_table$year, var_table$exp, sep = "")
        }
        
        return(var_table)
    }


    process_data <- function(exp_names, var_names, input_path = 'Data/output_rice50x/results_ocean_') {
    
    process_var_table <- function(var_table, exp_name, var_name) {
        var_table <- var_table[3:nrow(var_table), ]
        var_table <- as.data.frame(var_table)
        print(var_name)
        
        if (ncol(var_table) < 4) {
            if (var_name %in%  c("scc","pop")) {
                names(var_table) <- c("year", "country", var_name)
                var_table$country <- as.factor(var_table$country)
                var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
                var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
                var_table$exp <- exp_name
                var_table$id <- paste(var_table$country, var_table$year, var_table$exp, sep = "")
            } else {
                names(var_table) <- c("capital", "country", var_name)
                var_table$country <- as.factor(var_table$country)
                var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
                var_table$exp <- exp_name
                var_table$capital <- as.factor(var_table$capital)
            }
        } else {
            if (ncol(var_table) < 7) {
                if(var_name=="VSL"){
                    names(var_table) <- c("year", paste0(var_name, "_low"), var_name, paste0(var_name, "_high"), paste0(var_name, "_marginal"))
                }else{
                names(var_table) <- c("year", "country", paste0(var_name, "_low"), var_name, paste0(var_name, "_high"), paste0(var_name, "_marginal"))
                    var_table$country <- as.factor(var_table$country)
                }
            } else {
                names(var_table) <- c("capital", "year", "country", paste0(var_name, "_low"), var_name, paste0(var_name, "_high"), paste0(var_name, "_marginal"))
                var_table$capital <- as.factor(var_table$capital)
                var_table$country <- as.factor(var_table$country)
            }
            var_table$year <- 1980 + (as.integer(var_table$year) - 1) * 5
            var_table[, var_name] <- as.double(unlist(var_table[, var_name]))
            var_table$exp <- exp_name
            #var_table$id <- paste(var_table$country, var_table$year, var_table$exp, sep = "")
        }
        
        return(var_table)
    }
    
    for (i in 1:length(exp_names)) {
        for (j in 1:length(var_names)) {
            var_table <- read_excel(paste0(input_path, exp_names[i], '.xlsx'), sheet = var_names[j])
            var_table <- process_var_table(var_table, exp_names[i], var_names[j])
            
            if (j == 1) {
                assign(paste0("exp_data_", exp_names[i]), var_table, envir = .GlobalEnv)
            } else {
                existing_data <- get(paste0("exp_data_", exp_names[i]))
                common_cols <- intersect(names(existing_data), names(var_table))
                assign(paste0("exp_data_", exp_names[i]), merge(existing_data, var_table, by = common_cols, all = TRUE), envir = .GlobalEnv)
            }
        }
    }
}


