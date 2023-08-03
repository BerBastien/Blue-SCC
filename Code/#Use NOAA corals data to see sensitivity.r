#Use NOAA corals data to see sensitivity to SST
#setup#

    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales')
    lapply(x, require, character.only = TRUE)
    
    
    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\'

    #install.packages("wesanderson")
    library(wesanderson)
    library(hrbrthemes) # for plot themes

    ## Function
        sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(1:end1),c(1:end1)]
            beta.hat <- coef(model)[c(1:end1)]
            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(1, 2*x)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x))
        }
    ## Function
    
#setup#

## Data cleanup
  coral_data <- read.csv(paste0(dir1,"deep_sea_corals_d631_2e44_7e16.csv")) #deep_sea_corals_d631_2e44_7e16
  glimpse(coral_data)
  names(coral_data)[names(coral_data)=="Cover..percent."] <- "cover"
  names(coral_data)[names(coral_data)=="DepthInMeters..m."] <- "depth"
  names(coral_data)[names(coral_data)=="Temperature..degrees_Celsius."] <- "temperature"
  names(coral_data)[names(coral_data)=="ObservationYear"] <- "year"
  names(coral_data)[names(coral_data)=="latitude..degrees_north."] <- "lat"
  names(coral_data)[names(coral_data)=="Salinity..PSU."] <- "salinity"
  names(coral_data)[names(coral_data)=="Oxygen..mg.L."] <- "oxygen"
  names(coral_data)[names(coral_data)=="SampleAreaInSquareMeters..m.2."] <- "sample_area"
  
  coral_data$depth <- as.double(coral_data$depth)
  coral_data$temperature <- as.double(coral_data$temperature)
  coral_data$cover <- as.double(coral_data$cover)
  coral_data$Station <- factor(coral_data$Station)
  coral_data$Locality <- factor(coral_data$Locality)

  # m1 <- felm(cover~temperature+depth|0|0|0,data=coral_data)
  # summary(m1)
  # coral_data$month <- as.double(substr(coral_data$ObservationDate, 6,7))
  
  # m1 <- felm(cover~temperature+depth+factor(month)|year|0|0,data=coral_data)
  # summary(m1)
  # levels(factor(coral_data$month))
  # 1964-04-14

## Data cleanup

## Summary graphs

  dim(coral_data)
  dim(coral_data[which(coral_data$cover>0),])
  
  dim(coral_data[which(coral_data$cover>0 & coral_data$Class!=""),])
  dim(coral_data[which(coral_data$cover>0 & coral_data$Class!=""),])
  
  dim(coral_data[which(coral_data$cover>0 & coral_data$Class!=""),])
  
  class_plot <- ggplot(data = coral_data[which(coral_data$cover>0 & coral_data$Class!=""),], aes(x = lat, fill=Class)) +
    geom_histogram() +
  #geom_histogram(position = "identity", alpha = 0.4) +
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10", oob = scales::oob_censor_any)+
  theme_minimal() +
    coord_flip()+
    labs(x = "Latitude", y = "Number of observations",
        title = "Observations by latitude (n>11,000)")+ 
    scale_fill_brewer(palette="Set3")
  #ggsave("Figures/corals/noaa_database/obs_by_lat.png",dpi=300)

  dim(coral_data[which(coral_data$cover>0 & coral_data$Class=="Anthozoa" & coral_data$Order!=""),])
  
  #wes_palette("Darjeeling1",n=5)
  
  anth_plot <- ggplot(data = coral_data[which(coral_data$cover>0 & coral_data$Class=="Anthozoa" & coral_data$Order!=""),], aes(x = lat, fill=Order)) +
    geom_histogram() +
  #geom_histogram(position = "identity", alpha = 0.4) +
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10", oob = scales::oob_censor_any)+
  theme_minimal() +
    coord_flip()+
    labs(x = "Latitude", y = "Number of observations",
        title = "Anthozoa by latitude (n>5,000)")+ 
    scale_fill_brewer(palette="Spectral")

  ggarrange(class_plot,anth_plot)

  ggsave("Figures/corals/noaa_database/Class_Anthozoa_by_lat.png",dpi=300)


  country_plot <- ggplot(data = coral_data[which(coral_data$cover>0),], aes(x = lat, fill=Country)) +
    geom_histogram() +
  #geom_histogram(position = "identity", alpha = 0.4) +
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10", oob = scales::oob_censor_any)+
  theme_minimal() +
    coord_flip()+
    labs(x = "Latitude", y = "Number of observations",
        title = "Anthozoa by latitude (n>5,000)")+ 
    scale_fill_brewer(palette="Spectral")
    country_plot

  depth_plot <- ggplot(data = coral_data[which(coral_data$cover>0 & coral_data$Class!=""),], aes(x = temperature,y=-depth, color=Class)) +
    geom_point(alpha=0.5) + theme_minimal() +
    
    labs(x = "Temperature (C)", y = "Depth (m)",
        title = "Observations")+ 
    scale_color_brewer(palette="Set2")
  
  depth_plot
  ggsave("Figures/corals/noaa_database/depth.png",dpi=300)

  depth_plot_nodeepsea <- ggplot(data = coral_data[which(coral_data$depth<200 & coral_data$cover>0 & coral_data$Class!=""),], aes(x = temperature,y=-depth, color=Class)) +
    geom_point(alpha=0.5) + theme_minimal() +  
    labs(x = "Temperature (C)", y = "Depth (m)",
        title = "Observations above 200m (n=9680, min=42m)")+ 
    scale_color_brewer(palette="Set2")
  
  depth_plot_nodeepsea
  ggsave("Figures/corals/noaa_database/depth_nodeepsea.png",dpi=300)
  min(coral_data$depth[which(coral_data$depth<200 & coral_data$cover>0 & coral_data$Class!="")])
  
  length(coral_data$depth[which(coral_data$depth<200 & coral_data$cover>0 & coral_data$Class!="")])
    
  #geom_histogram(position = "identity", alpha = 0.4) +
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10", oob = scales::oob_censor_any)+

  coral_data$season <- coral_data$month
  coral_data$season[which(coral_data$season %in% c(12,1,2))] <-"DJF"
  coral_data$season[which(coral_data$season %in% c(3,4,5))] <-"MAM"
  coral_data$season[which(coral_data$season %in% c(6,7,8))] <-"JJA"
  coral_data$season[which(coral_data$season %in% c(9,10,11))] <-"SON"
  levels(factor(coral_data$season))
  coral_data$season <- factor(coral_data$season)
  coral_data$year <- as.integer(coral_data$year)
  
  min(coral_data$year[which(coral_data$month!="" & coral_data$cover>0)],na.rm=TRUE)
  max(coral_data$year[which(coral_data$month!="" & coral_data$cover>0)],na.rm=TRUE)
  month_plot <- ggplot(data = coral_data[which(coral_data$month!="" & coral_data$cover>0),], aes(x = year, fill=season)) +
    geom_histogram(bins=10) +
  #geom_histogram(position = "identity", alpha = 0.4) +
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10", oob = scales::oob_censor_any)+
  theme_minimal() +
    #coord_flip()+
    labs(x = "years", y = "Number of observations",
        title = "Seasons of observations")+ 
    scale_fill_brewer(palette="Set2")+
    scale_x_continuous("Year", labels = c(2010:2019) , breaks = c(2010:2019))
    month_plot
  ggsave("Figures/corals/noaa_database/year_month.png",dpi=300)


  cover_plot <- ggplot(data = coral_data[which(#coral_data$Condition!="" & 
  coral_data$month!="" & coral_data$cover>0),], aes(x = cover, fill=Condition)) +
    geom_histogram(bins=10) +
  #geom_histogram(position = "identity", alpha = 0.4) +
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(trans = "log10", oob = scales::oob_censor_any)+
  theme_bw() +
    #coord_flip()+
    labs(x = "Cover (%)", y = "Number of observations",
        title = "Cover and conditions")+ 
    scale_fill_brewer(palette="RdYlGn")+
    facet_wrap(~year)

    #scale_x_continuous("Year", labels = c(2010:2019) , breaks = c(2010:2019))
    cover_plot

    ggsave("Figures/corals/noaa_database/cover_condition_facet.png",dpi=300)


  


  #levels(factor(coral_data$Species)) 1919
  #levels(factor(coral_data$Genus)) #753
  #levels(factor(coral_data$Family)) #187
  levels(factor(coral_data$Suborder)) #25
  levels(factor(coral_data$Order)) #45
  levels(factor(coral_data$Class)) #8

  dim(coral_data)


## Summary graphs

## Averages
  coral_data$latitude_round1dec <- round(coral_data$lat,digits=1)
  coral_data$longitude_round1dec <- round(coral_data$longitude..degrees_east.,digits=1)

  coral_data$location <- paste0(coral_data$latitude_round1dec,coral_data$longitude_round1dec)

  glimpse(coral_data)
  length(table(coral_data$location[which(coral_data$Condition!="Death" & 
  coral_data$month!="" & coral_data$cover>0)]))

  cover_ag <- aggregate(Cover~location+Class+year,data=coral_data[which(coral_data$season=="JJA"& coral_data$temperature >0  & coral_data$Condition!="Death"),],FUN="mean")
  cover_ag  <- coral_data %>%
  filter(season == "JJA", Cover>0, temperature >0, Condition != "Death") %>%
  group_by(location, Class, year) %>%
  summarise(Cover = mean(Cover), .groups = "drop")
  
  temp_ag <- coral_data %>%
  filter(season == "JJA", Cover >0, Condition != "Death", temperature >0) %>%
  group_by(location, Class, year) %>%
  summarise(temperature = weighted.mean(temperature, Cover), .groups = "drop")

  depth_ag <- coral_data %>%
  filter(season == "JJA", Cover >0, Condition != "Death", temperature >0) %>%
  group_by(location, Class, year) %>%
  summarise(depth = weighted.mean(depth, Cover),
    Count = n(), .groups = "drop")
  
  coral_ag <- cbind(cover_ag,temp_ag[,4],depth_ag[,c(4,5)])  
  
  names(coral_ag)[5] <- "temperature"
  names(coral_ag)[6] <- "depth"
  glimpse(coral_ag)
  library("zoo")





  ggplot(coral_ag, aes(x=year,y=Cover,color=location,group=interaction(Class,location)))+
  geom_line()

  ggplot(coral_ag, aes(x=year,y=Count,color=location,group=interaction(Class,location)))+
  geom_line()

  model_coral_2 <- felm(Cover~temperature+I(temperature^2)+depth+Count #+ Condition #+year+I(year^2) 
      | year+Class|0|0,data=coral_ag)
    summary(model_coral_2)
    
    estimates_coral <- sqest(coral_data,model_coral_2,"temperature","Observed NOAA")
coral_effect <- ggplot(estimates_coral,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature",y="Effect of 1 Degree Warming \nin Coral Cover (pp)")
coral_effect
    ggsave("Figures/corals/noaa_database/warming_effect_cover.png",dpi=300)


coral_ag_interp <- coral_ag %>%
  group_by(Class, location) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  mutate(
    Cover = na.approx(Cover, rule = 2, maxgap = Inf),
    temperature = na.approx(temperature, rule = 2, maxgap = Inf),
    depth = na.approx(depth, rule = 2, maxgap = Inf)
  ) %>%
  ungroup()

ggplot(coral_ag_interp, aes(x=year,y=Cover,color=location,group=interaction(Class,location)))+
  geom_line()

  #   coral_ag_cover_change <- coral_ag_interp %>%
  #   group_by(Class, location) %>%
  #   mutate(
  #     Cover_change = Cover - lag(Cover, default = first(Cover))
  #   ) %>%
  #   ungroup()

  #   model_coral_3 <- felm(Cover_change~temperature+I(temperature^2)+depth+Count#+ Condition #+year+I(year^2) 
  #       |year+ Class|0|0,data=coral_ag_cover_change)
  #     summary(model_coral_3)

  #     estimates_coral <- sqest(coral_ag_cover_change,model_coral_3,"temperature","Observed NOAA")
  # coral_effect <- ggplot(estimates_coral,aes(x=temp,y=gestimated,color=exp))+
  #                     geom_line(lwd=1.25)+ theme_bw()+
  #                     geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
  #                     geom_hline(yintercept=0,lty=3)+
  #                     labs(x="Temperature change",y="Estimated Impact in Corals")
  # coral_effect


















  count_ag <- aggregate(Cover~location+Order+year,
  data=coral_data[which(coral_data$season=="JJA"& coral_data$temperature >0  & coral_data$Condition!="Death"),],
  FUN="count")
  

  cover_ag <- aggregate(Cover~location+Order+year,data=coral_data[which(coral_data$season=="JJA"& coral_data$temperature >0  & coral_data$Condition!="Death"),],FUN="mean")
  temp_ag <- coral_data %>%
  filter(season == "JJA",temperature>0,  Cover >0, Condition != "Death") %>%
  group_by(location, Order, year) %>%
  summarise(temperature = weighted.mean(temperature, Cover), .groups = "drop")

  depth_ag <- coral_data %>%
  filter(season == "JJA",temperature>0, Cover >0, Condition != "Death") %>%
  group_by(location, Order, year) %>%
  summarise(depth = weighted.mean(depth, Cover),
  Count=n(), .groups = "drop")
  
  #temp_ag <- aggregate(temperature~location+Order+year,data=coral_data[which(coral_data$season=="JJA" & coral_data$Cover >0 & coral_data$Condition!="Death"),],FUN="mean")
  #depth_ag <- aggregate(depth~location+Order+year,data=coral_data[which(coral_data$season=="JJA" & coral_data$temperature >0 & coral_data$Cover >0 & coral_data$Condition!="Death"),],FUN="mean")
  oxygen_ag <- aggregate(oxygen~location+Order+year,data=coral_data[which(coral_data$season=="JJA" & coral_data$temperature >0 & coral_data$Cover >0 & coral_data$Condition!="Death"),],FUN="mean")
  salinity_ag <- aggregate(salinity~location+Order+year,data=coral_data[which(coral_data$season=="JJA" & coral_data$Cover >0 & coral_data$Condition!="Death"),],FUN="mean")

  coral_ag <- cbind(cover_ag,temp_ag[,4],depth_ag[,c(4,5)])  
  names(coral_ag)[5] <- "temperature"
  names(coral_ag)[6] <- "depth"
  glimpse(coral_ag)
  library("zoo")





  ggplot(coral_ag, aes(x=year,y=Cover,color=location,group=interaction(Order,location)))+
  geom_line()

  model_coral_2 <- felm(Cover~temperature+I(temperature^2)+depth+Count #+ Condition #+year+I(year^2) 
      | year+Order|0|0,data=coral_ag)
    summary(model_coral_2)
    
    estimates_coral <- sqest(coral_data,model_coral_2,"temperature","Observed NOAA")
    coral_effect <- ggplot(estimates_coral,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature",y="Effect of 1 Degree Warming \nin Coral Cover (pp)")
coral_effect


coral_ag_interp <- coral_ag %>%
  group_by(Order, location) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  mutate(
    Cover = na.approx(Cover, rule = 2, maxgap = Inf),
    temperature = na.approx(temperature, rule = 2, maxgap = Inf),
    depth = na.approx(depth, rule = 2, maxgap = Inf)
  ) %>%
  ungroup()

ggplot(coral_ag_interp, aes(x=year,y=Cover,color=location,group=interaction(Order,location)))+
  geom_line()

  coral_ag_cover_change <- coral_ag_interp %>%
  group_by(Order, location) %>%
  mutate(
    Cover_change = Cover - lag(Cover)
  ) %>%
  ungroup()

  model_coral_3 <- felm(Cover_change~temperature#+I(temperature^2)
      +depth#+ Condition #+year+I(year^2) 
      | Order+year+location |0|0,data=coral_ag_cover_change)
    summary(model_coral_3)

    estimates_coral <- sqest(coral_ag_cover_change,model_coral_3,"temperature","Observed NOAA")
coral_effect <- ggplot(estimates_coral,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Corals")
coral_effect



## Averages

## Models

  glimpse(coral_data)
  (table(coral_data$lat[which(coral_data$Condition!="Death" & 
  coral_data$month!="" & coral_data$cover>0)]))
  
  model_coral_2 <- felm(Cover~temperature+I(temperature^2)+ depth + salinity + oxygen#+ Condition #+year+I(year^2) 
      | year + season + Species + Station |0|0,data=coral_data[which(coral_data$Condition!="Death"),])
    summary(model_coral_2)
    #library(stargazer)
    stargazer(model_coral_2,type="text")


    
estimates_coral <- sqest(coral_data,model_coral_2,"temperature","Observed NOAA")
coral_effect <- ggplot(estimates_coral,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Corals")
coral_effect
  ggsave("Figures/corals/noaa_database/coral_effect.png",dpi=300)
## Models





coral_data_noNA <- coral_data %>%
  filter(!is.na(Station))
unique_years_noNA <- unique(coral_data_noNA$ObservationYear)
unique_stations_noNA <- unique(coral_data_noNA$Station)

station_year_counts_noNA <- coral_data_noNA %>%
  group_by(Station, ObservationYear) %>%
  summarize(n = n(), .groups = 'drop')

max_years <- length(unique_years_noNA)
max_stations <- 0
selected_years <- NULL
stations_complete_years <- NULL

for (n_years in seq_len(max_years)) {
  combinations_n_years <- combn(unique_years_noNA, n_years)
  for (i in seq_along(combinations_n_years[1,])) {
    current_years <- combinations_n_years[, i]
    current_stations <- station_year_counts_noNA %>%
      filter(ObservationYear %in% current_years) %>%
      group_by(Station) %>%
      summarize(n_years = n_distinct(ObservationYear), .groups = 'drop') %>%
      filter(n_years == n_years) %>%
      pull(Station)

    if (length(current_stations) > max_stations) {
      max_stations <- length(current_stations)
      selected_years <- current_years
      stations_complete_years <- current_stations
    }
  }
}

max_stations
selected_years
stations_complete_years

coral_data_filtered <- coral_data_noNA %>%
  filter(Station %in% stations_complete_years, ObservationYear %in% selected_years)

model_coral_1 <- felm(Cover ~ ObservationYear + I(ObservationYear^2) + Temperature + I(Temperature^2) + LargeMarineEcosystem +latitude+I(latitude^2)|
                     VernacularNameCategory | 0 | 0, data = coral_data)
summary(model_coral_1)

ggplot(coral_data_filtered, aes(x=ObservationYear,y=Cover,group=interaction(latitude,VernacularNameCategory)))+
geom_line()

model_coral_2 <- felm(Cover~Temperature+I(Temperature^2)+ObservationYear+I(ObservationYear^2)+LargeMarineEcosystem
+abs(latitude)+DepthInMeters|VernacularNameCategory|0|0,data=coral_data)
summary(model_coral_2)

estimates_coral <- sqest(coral_data,model_coral_2,"Temperature","Observed NOAA")
coral_effect <- ggplot(estimates_coral,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Corals")
coral_effect
glimpse(coral_data)
DepthInMeters
ggplot(data = coral_data, aes(x = Temperature, y = -DepthInMeters,color= VernacularNameCategory)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Temperature", y = "Depth (m)",
       title = "Scatter plot: Temperature vs Depth")

ggplot(data = coral_data, aes(x = Temperature, y = Cover)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Temperature", y = "Cover",
       title = "Scatter plot: Temperature vs Cover")

ggplot(data = coral_data, aes(x = ObservationYear, y = Cover)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Observation Year", y = "Cover",
       title = "Scatter plot: Observation Year vs Cover")

ggplot(data = coral_data, aes(x = LargeMarineEcosystem, y = Cover)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Large Marine Ecosystem", y = "Cover",
       title = "Box plot: Cover by Large Marine Ecosystem")


ggplot(data = coral_data, aes(x = Cover)) +
  geom_histogram(binwidth = 5) +
  theme_minimal() +
  labs(x = "Cover", y = "Count",
       title = "Histogram: Cover distribution")

ggplot(data = coral_data, aes(x = (latitude), y = Cover)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Absolute Latitude", y = "Cover",
       title = "Scatter plot: Absolute Latitude vs Cover")

ggplot(data = coral_data, aes(x = Temperature, y = Cover, color = LargeMarineEcosystem, size = abs(latitude))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(x = "Temperature", y = "Cover",
       title = "Scatter plot: Temperature vs Cover, colored by Large Marine Ecosystem, sized by Absolute Latitude")

ggplot(data = coral_data, aes(x = ObservationYear, y = Cover, color = VernacularNameCategory)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ LargeMarineEcosystem) +
  theme_minimal() +
  labs(x = "Observation Year", y = "Cover",
       title = "Scatter plot: Observation Year vs Cover, colored by Vernacular Name Category, faceted by Large Marine Ecosystem")

ggplot(data = coral_data, aes(x = VernacularNameCategory, y = Cover, fill = LargeMarineEcosystem)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Vernacular Name Category", y = "Cover",
       title = "Box plot: Cover by Vernacular Name Category, colored by Large Marine Ecosystem")


summary(coral_data)                    
hist_cover <- hist(coral_data$Cover)
ggarrange(hist_cover,coral_effect)

model_coral_1 <- felm(Cover~Temperature+I(Temperature^2)+ObservationYear+I(ObservationYear^2)+Locality+abs(latitude)|VernacularNameCategory|0|0,data=coral_data)



