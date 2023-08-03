#setup#

    x <- c('raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
    lapply(x, require, character.only = TRUE)
    #install.packages('marginaleffects')
    setwd('C:\\Users\\basti\\Documents\\GitHub\\NatCap_PFTs')
    dir1 <- "C:/Users/basti/Box/VegetationData/"
    isos <- world$iso_a2

    library(RColorBrewer)
    my.palette <- brewer.pal(n = 10, name = "BrBG")
    `%notin%` <- Negate(`%in%`)

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
            xmat <- cbind(x, x^2)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x))
        }
    ## Function
#setup

#READ WORLD BANK DATA
        wealth <- read.csv('Data\\WorldBank_WealthAccounts_2018USD.csv')
        colnames(wealth) <- c("Country.Name" ,"Country.Code",   "Series.Name" , "Series.Code",
        1995:2018)
        #glimpse(wealth)
        #table(wealth$countrycode)
        dim(table(wealth$countrycode))

        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- wealth[,which(colnames(wealth) %in% 
                c("Country.Name","Country.Code","Series.Name",yeari))]
            w3 <- reshape(w2, idvar=c("Country.Name" ,"Country.Code"), 
                timevar="Series.Name", direction="wide")
            w3 <- w3[,c(1,2,3,4,34,37,38,39,41,47,49,51,53)]
            w3[,(dim(w3)[2]+1)] <- yeari
            colnames(w3) <- c("countryname","countrycode", "N","H","Nagg","Nfisheries","NforestES","NforestT","Nmangroves","Npa","Foreign", "K","TotalWealth","year")
            if (i==1){Wealth <- w3} else {
                Wealth <- rbind(Wealth,w3)
            }
        }
        #glimpse(Wealth)
        #levels(factor(Wealth$countrycode))[levels(factor(Wealth$countrycode)) %in% isos_missin3]
        
        #ggplot(data=Wealth, aes(x = year, y=log(N), group = countrycode)) + 
        #geom_line(aes(color=countrycode))  +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 1)) +
        #geom_dl(aes(label = countrycode), method = list(dl.combine("first.points", "last.points")), cex = 0.8)

        gdp <- read.csv('Data\\GDP_2015USD_19952018.csv')
        colnames(gdp) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
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
        
        wealth <- merge(Wealth,Gdp, by = c("countrycode","year"),all=TRUE)
        wealth <- wealth[-c(which(wealth[,1]=="")),] #no data in these rows
        glimpse(wealth)
        wealth$GDP <- as.numeric(as.character(wealth$GDP)) * 1.06 #converting 2015 to 2018 usd
        colnames(wealth)[3] <- "countryname"
        #table(wealth$countrycode)
        

        
        population  <- read.csv('Data\\Pop1995_2018.csv')
        colnames(population) <-  c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        #glimpse(population)
        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- population[,which(colnames(population) %in% 
                c("countryname", "countrycode", "seriesname",yeari))]
            w3 <- reshape(w2, idvar=c("countryname", "countrycode"), 
                timevar="seriesname", direction="wide")
            #glimpse(w3)
            #w3 <- w3[,c(1,2,5)]
            w3[,4] <- yeari
            colnames(w3) <- c("countryname","countrycode","Population","year")
            if (i==1){Population <- w3} else {
                Population  <- rbind(Population,w3)
            }
        }
        wealth_data <- merge(wealth,Population,all=TRUE,by=c("countrycode","year"))
        wdata_1995_2018 <- wealth_data
        glimpse(Population[Population$countrycode=="RUS",])
        glimpse(wealth_data[wealth_data$countrycode=="RUS",])
        #glimpse(wealth_data)
        #table(wdata_1995_2018$countrycode)

        #labor  <- read.csv('Data\\laborIncomeShare.csv') #Labour income share as a percent of GDP -- ILO modelled estimates, July 2019 (%). Downloaded from ILOSTAT. Last update on 10JAN21.
        #colnames(labor) <- c("countryname", "source", "year", "labor_share")
        #glimpse(labor)
        #b <- merge(wealth_data,labor, by = c("countrycode","year"),all.x=T)
        #wealth_data <- b

        wealth_data$H <- as.numeric(as.character(wealth_data$H))
        wealth_data$K <- as.numeric(as.character(wealth_data$K))
        wealth_data$N <- as.numeric(as.character(wealth_data$N))
        wealth_data$NforestES <- as.numeric(as.character(wealth_data$NforestES))
        wealth_data$NforestT <- as.numeric(as.character(wealth_data$NforestT))
        wealth_data$Nmangroves <- as.numeric(as.character(wealth_data$Nmangroves))
        wealth_data$Nfisheries <- as.numeric(as.character(wealth_data$Nfisheries))
        wealth_data$Nagg <- as.numeric(as.character(wealth_data$Nagg))
        wealth_data$Foreign <- as.numeric(as.character(wealth_data$Foreign))
        wealth_data$Npa <- as.numeric(as.character(wealth_data$Npa))
        wealth_data$TotalWealth <- as.numeric(as.character(wealth_data$TotalWealth))
        #wealth_data$labor_share <- as.numeric(as.character(wealth_data$labor_share))
        wealth_data$Population <- as.numeric(as.character(wealth_data$Population))
        
        #glimpse(wealth_data)
        
        wealth_data <- wealth_data[,-which(names(wealth_data) %in% c("countryname.y","NA","source"))]
        #glimpse(wealth_data)
        #table(wealth_data$labor_share)

        #ggplot(wealth_data[])+
        #geom_point(aes(x=log(GDP),y=log(Nfisheries/K),color=countrycode,group=countrycode))

        # Forest Rents (start)
        Natrents  <- read.csv('Data\\ForestRents_2018.csv') #World Bank Data
        
        colnames(Natrents) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- Natrents[,which(colnames(Natrents) %in% 
                c("countryname", "countrycode", "seriesname",yeari))]
            w3 <- reshape(w2, idvar=c("countryname", "countrycode"), 
                timevar="seriesname", direction="wide")
            #glimpse(w3)
            w3 <- w3[,c(1,2,3)]
            w3[,4] <- yeari
            colnames(w3) <- c("countryname","countrycode","NatRents","year")
            if (i==1){Natrents2 <- w3} else {
                Natrents2  <- rbind(Natrents2,w3)
            }
        }
        #glimpse(Natrents2)
        #glimpse(wealth_data)    
        
        wealth_data <- merge(wealth_data,Natrents2, by = c("countrycode","year"),all.x=TRUE)
        #glimpse(wealth_data)
        wealth_data$NatRents <- as.numeric(as.character(wealth_data$NatRents))
        save(file="Data/wealth_data.Rda",wealth_data)

        #table(wealth_data$countrycode)
    # Forest Rents (end)
#READ WORLD BANK DATA (end)

glimpse(wealth_data)


fish <- aggregate(Nfisheries~year,data=wealth_data,FUN="sum")
capital <- aggregate(K~year,data=wealth_data,FUN="sum")
Global_W <- merge(fish,capital,by="year")
mangroves <- aggregate(Nmangroves~year,data=wealth_data,FUN="sum")
Global_W <- merge(Global_W,mangroves,by="year")
GDP <- aggregate(GDP~year,data=wealth_data,FUN="sum")
Global_W <- merge(Global_W,GDP,by="year")
N <- aggregate(N~year,data=wealth_data,FUN="sum")
Global_W <- merge(Global_W,N,by="year")
NES <- aggregate(NforestES~year,data=wealth_data,FUN="sum")
Global_W <- merge(Global_W,NES,by="year")

ggplot(Global_W, aes(x=year,y=K/(Nmangroves)))+
geom_line()

ggplot(Global_W, aes(x=year,y=Nfisheries/GDP))+
geom_line()

Global_W$K/(Global_W$Nfisheries+Global_W$Nmangroves)

Global_W$K/(Global_W$N)

(Global_W$Nfisheries+Global_W$Nmangroves)/Global_W$GDP

ggplot(Global_W)+
geom_line(aes(x=year,y=100*N/K),color="seagreen3")+
geom_line(aes(x=year,y=100*(Nfisheries+Nmangroves)/K),color="darkblue")+
scale_y_continuous(trans="log2") +
theme_bw()+
ylab("Natural Capital (as % of Manufactured)")
ggsave("nat_capitals.png",dpi=300)

glimpse(wealth_data)

ggplot(wealth_data)+
geom_point(aes(x=log()))

wealth_data2 <- merge(wealth_data,Global_W,by="year",suffixes=c("","global"))
wealth_data2$iso_a2 <- countrycode(wealth_data2$countrycode,origin="iso3c",destination="iso2c")
wealth_data2 <- merge(wealth_data2,world,by="iso_a2")
glimpse(wealth_data2)

w_blue <- wealth_data2[which((wealth_data2$Nfisheries)>0 & wealth_data2$NforestT>0),]

model_blue <- felm(log(GDP)~log(K)+log(H)+log(Nfisheries)+log(NforestT)+log(NforestESglobal)+log(Nmangrovesglobal)+
    year+I(year^2)+Foreign+Population|countrycode|0|0,data=w_blue)
summary(model_blue)

model_blue <- felm(log(GDP)~log(K)+log(H)+log(Nfisheries)+log(Nmangroves/GDP)+log(NforestT)+Population+Foreign|countrycode|0|0,data=w_blue)
summary(model_blue)


model_blue <- felm(log(GDP)~log(K)+log(H)+log(Nfisheries)+log(Nmangroves),data=w_blue)
summary(model_blue)

wblue2018 <- w_blue[which(w_blue$year==2018),]
sum(wblue2018$K)/sum(wblue2018$Nfisheries+wblue2018$Nmangrove)
