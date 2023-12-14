## Load Modules Data
    load(file="Data/intermediate_output/corals_df.Rds") #corals_df

## Load Temperature Trajectory
    ssp585 <- read.csv("Data/SSP585_magicc_202303141113.csv")
    glimpse(ssp585[17,c(13:118)])
    temp <- as.data.frame(t(ssp585[17,c(13:118)]))
    glimpse(temp)
    temp$year <- c(1995:2100)
    glimpse(temp)
    names(temp)[1] <- "temp"               
    glimpse(df_deepvalues)
    glimpse(val_temp)

## Choose SSP and Associated Temperature
    # with and without climate change
    # in decade 1
    # get GDP of country A
    # calculate area change of corals
    # calculate benefits of corals


