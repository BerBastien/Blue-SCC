 wealth <- read.csv('Data\\modules\\fish\\natural_capital_fish.csv')
        colnames(wealth) <- c("Country.Name" ,"Country.Code",   "Series.Name" , "Series.Code",
        1995:2018)
        glimpse(wealth)
        #table(wealth$countrycode)
        dim(table(wealth$countrycode))

        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- wealth[,which(colnames(wealth) %in% 
                c("Country.Name","Country.Code","Series.Name",as.character(yeari)))]
            w3 <- reshape(w2, idvar=c("Country.Name" ,"Country.Code"), 
                timevar="Series.Name", direction="wide")
            
            w3 <- w3[,c(1,2,3)]
            w3[,(dim(w3)[2]+1)] <- yeari
            colnames(w3) <- c("countryname","countrycode","Nfisheries","year")
            if (i==1){Wealth <- w3} else {
                Wealth <- rbind(Wealth,w3)
            }
        }
        glimpse(Wealth)
        
        
        #levels(factor(Wealth$countrycode))[levels(factor(Wealth$countrycode)) %in% isos_missin3]
        
        #ggplot(data=Wealth, aes(x = year, y=log(N), group = countrycode)) + 
        #geom_line(aes(color=countrycode))  +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 1)) +
        #geom_dl(aes(label = countrycode), method = list(dl.combine("first.points", "last.points")), cex = 0.8)

        gdp <- read.csv('Data\\all_data\\GDP_constant2015values.csv')
        colnames(gdp) <- c("countryname", "countrycode", "seriesname","seriescode",1960:2022)
        #glimpse(gdp)
        for (i in 1:length(years)){
            yeari <- years[i]
            gdp2 <- gdp[,which(colnames(gdp) %in% 
                c("countryname","countrycode","seriesname",yeari))]
            
            gdp3 <- reshape(gdp2, idvar=c("countryname","countrycode"), 
                timevar="seriesname", direction="wide")
            gdp3 <- gdp3[,c(1:3)]
            gdp3[,4] <- yeari
            colnames(gdp3) <- c("countryname","countrycode","GDP","year")
            if (i==1){Gdp <- gdp3} else {
                Gdp <- rbind(Gdp,gdp3)
            }
        }
        
        glimpse(Wealth)
        glimpse(Gdp)
        wealth <- merge(Wealth %>% select(-countryname),Gdp, by = c("countrycode","year"),all=T)
        wealth <- wealth[-c(which(wealth[,1]=="")),] #no data in these rows
        glimpse(wealth)
        wealth$GDP <- wealth$GDP * 1.06 #converting 2015 to 2018 usd

        ggplot(wealth %>% filter(year==2018),aes(x=GDP,y=Nfisheries))        +geom_point()
        
        wealth <-  wealth %>%
        mutate(fisheries_profits = Nfisheries*0.03) %>%
        mutate(fisheries_percGDP = 100*fisheries_profits/GDP)

        
        ggplot(wealth %>% filter(year==2018),aes(x=GDP,y=fisheries_percGDP))+
        #geom_point()
        geom_text(aes(label=countrycode))
