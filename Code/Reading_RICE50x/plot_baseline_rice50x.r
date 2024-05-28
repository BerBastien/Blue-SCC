#setup#
    x <- c('arrow','raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth","ddply")
    lapply(x, require, character.only = TRUE)
    '%notin%' <- Negate('%in%')

    setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE')
    
    
    BR_Results <- read_parquet("Data/rice50x/baseline_results.parquet")
    ## Extra variables needed
    ####    R5, pop, YGROSS_DAM
    #### NONUSE/USE_VALUE_DAM and NONUSE/USE_VALUE_Baseline

    ## Questions
    ####    Is NONUSE/USE_VALUE, CPC, etc in trill USD? 
    ####    is UTARG "meaningless" until we get SCC?
    glimpse(BR_Results)    

    Values_2020 <- BR_Results[which(BR_Results$t==2),] %>% 
        mutate(YGROSS2020 = YGROSS, UTARG2020 = UTARG, CPC2020 = CPC, CPC_OCEAN_DAM2020 = CPC_OCEAN_DAM) %>% 
        dplyr::select(n,YGROSS2020, UTARG2020, CPC2020, CPC_OCEAN_DAM2020)
    
    BR_Results <- BR_Results %>% left_join(Values_2020, by="n")
    BR_Results$t <- as.double(BR_Results$t)
    BR_Results$year <- 2010 + BR_Results$t * (5)
    BR_Results <- BR_Results %>% filter(year<2101)

    ## CPC
        ## Adjust following plot. Multiply by population to obtain total market losses
        ## Can I obtain fisheries damages only or ports damages only?
        ## Q: CPC_DAM also includes damages for market use values from corals and mangroves?
        ggplot(BR_Results) + geom_line(aes(x=year,y=CPC_OCEAN_DAM-CPC,color=n))+ 
            geom_text(data=BR_Results %>% filter(year==2100),aes(x=year,y=CPC_OCEAN_DAM-CPC,color=n,label=n)) 

        ggplot(BR_Results) + geom_line(aes(x=year,y=CPC_OCEAN_DAM/CPC,color=n))+
            geom_text(data=BR_Results %>% filter(year==2100),aes(x=year,y=CPC_OCEAN_DAM/CPC,label=n,color=n))
        
        ggplot(BR_Results) + geom_line(aes(x=year,y=CPC_OCEAN_DAM/CPC_OCEAN_DAM2020,color=n),linetype=2)+ 
            geom_line(aes(x=year,y=CPC/CPC2020,color=n))

    ## Utility
        
        ggplot(BR_Results ) + geom_line(aes(x=CPC_OCEAN_DAM/CPC_OCEAN_DAM2020,y=UTARG/UTARG2020,color=n)) + 
            geom_text(data=BR_Results %>% filter(year==2100),aes(x=CPC_OCEAN_DAM/CPC_OCEAN_DAM2020,y=UTARG/UTARG2020,color=n,label=n)) 
        
    ## Corals
        ggplot(BR_Results ) + geom_line(aes(x=CPC/CPC2020,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n)) +
                geom_point(aes(x=CPC/CPC2020,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n,size=OCEAN_AREA_coral)) +
                geom_text(data=BR_Results %>% filter(year==2100),aes(x=(CPC/CPC2020+1),y=(OCEAN_USE_VALUE_coral/YGROSS),color=n,label=n))+
                scale_x_continuous(trans="log10")+
                scale_y_continuous(trans="log10")


        # ## Q: Why chn recovers coral value even if coral is declining?
        # ggplot(BR_Results ) + geom_line(aes(x=OCEAN_AREA_coral,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n)) +
        #         #geom_point(aes(x=CPC,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n,size=OCEAN_AREA_coral)) +
        #         geom_text(data=BR_Results %>% filter(year==2100),aes(x=OCEAN_AREA_coral,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n,label=n))+
        #         scale_x_continuous(trans="log10")+
        #         scale_y_continuous(trans="log10")

        ggplot(BR_Results ) + geom_line(aes(x=OCEAN_AREA_coral,y=(OCEAN_USE_VALUE_coral/CPC),color=n)) +
                #geom_point(aes(x=CPC,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n,size=OCEAN_AREA_coral)) +
                geom_text(data=BR_Results %>% filter(year==2100),aes(x=OCEAN_AREA_coral,y=(OCEAN_USE_VALUE_coral/CPC),color=n,label=n))+
                #scale_x_continuous(trans="log10")+
                #scale_y_continuous(trans="log10")
                theme_minimal() 

        ggplot(BR_Results ) + geom_line(aes(x=OCEAN_AREA_coral,y=(OCEAN_NONUSE_VALUE_coral/CPC),color=n)) +
                #geom_point(aes(x=CPC,y=(OCEAN_USE_VALUE_coral/YGROSS),color=n,size=OCEAN_AREA_coral)) +
                geom_text(data=BR_Results %>% filter(year==2100),aes(x=OCEAN_AREA_coral,y=(OCEAN_NONUSE_VALUE_coral/CPC),color=n,label=n))+
                #scale_x_continuous(trans="log10")+
                #scale_y_continuous(trans="log10")
                theme_minimal() 


############ END





    ggplot(BR_Results ) + geom_line(aes(x=log(YGROSS),y=(OCEAN_NONUSE_VALUE_coral/YGROSS),color=n))
    ggplot(BR_Results ) + geom_line(aes(x=log(CPC),y=log(OCEAN_USE_VALUE_coral/YGROSS),color=n))
    ggplot(BR_Results ) + geom_line(aes(x=year,y=OCEAN_USE_VALUE_port/YGROSS,color=n))
    ggplot(BR_Results ) + geom_line(aes(x=year,y=OCEAN_USE_VALUE_fish/YGROSS,color=n))
    ggplot(BR_Results ) + geom_line(aes(x=year,y=OCEAN_USE_VALUE_fish/YGROSS,color=n))
    
    BR_Results$GDP_change_perc <- (BR_Results$YGROSS_damages-BR_Results$YGROSS_baseline)*100/BR_Results$YGROSS_baseline
    BR_Results$GDP_change_perc_ub <- (BR_Results$YGROSS_ub_mkt_damages-BR_Results$YGROSS_baseline)*100/BR_Results$YGROSS_baseline
    BR_Results$GDP_change_perc_lb <- (BR_Results$YGROSS_lb_mkt_damages-BR_Results$YGROSS_baseline)*100/BR_Results$YGROSS_baseline
    
    
    BR_Results$GDP_change <- (BR_Results$YGROSS_damages-BR_Results$YGROSS_baseline)
    
    

    BR_Results$dr <- 1/(1+0.03)^(BR_Results$year  - 2020)
    BR_Results$dr[which(BR_Results$year==2020)]=1
    BR_Results$disc_damage <- BR_Results$dr * BR_Results$GDP_change

    isos <- levels(factor(BR_Results$n))
    for(i in 1:length(isos)){
      BR_d <- data.frame(approx(BR_Results$year[which(BR_Results$n==isos[i])],BR_Results$disc_damage[which(BR_Results$n==isos[i])],n=57*5+1),n=isos[i])
      n_npv<-data.frame(n=isos[i],NPV=sum(BR_d$y[which(BR_d$x>2019)]))
      if(i==1){
        n_NPV <- n_npv
      }else{
        n_NPV <- rbind(n_NPV,n_npv)
      }

    }
    
    
 
    BR_Results<-merge(BR_Results,n_NPV,by="n",all=TRUE)

    BR_Results$disc_GDP <- BR_Results$dr * BR_Results$YGROSS_baseline

    isos <- levels(factor(BR_Results$n))
    for(i in 1:length(isos)){
      BR_d <- data.frame(approx(BR_Results$year[which(BR_Results$n==isos[i])],BR_Results$disc_GDP[which(BR_Results$n==isos[i])],n=57*5+1),n=isos[i])
      n_npv<-data.frame(n=isos[i],NPV=sum(BR_d$y[which(BR_d$x>2019)]))
      if(i==1){
        n_NPV <- n_npv
      }else{
        n_NPV <- rbind(n_NPV,n_npv)
      }

    }
    names(n_NPV)[2] <- "NPV_GDP"
    
    
 
    BR_Results<-merge(BR_Results,n_NPV,by="n",all=TRUE)
    
    BR_Results$r5 <- factor(BR_Results$r5)
    levels(BR_Results$r5) <- c("ASIA","LAM","MAF","OECD","REF")

    100*sum(BR_Results$NPV[which(BR_Results$t==18)])/sum(BR_Results$YGROSS2020[which(BR_Results$t==18)])

    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="MAF")] )/sum(BR_Results$YGROSS2020[which(BR_Results$t==18  & BR_Results$r5=="MAF")])
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="OECD")] )/sum(BR_Results$YGROSS2020[which(BR_Results$t==18  & BR_Results$r5=="OECD")])
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="LAM")] )/sum(BR_Results$YGROSS2020[which(BR_Results$t==18  & BR_Results$r5=="LAM")])
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="REF")] )/sum(BR_Results$YGROSS2020[which(BR_Results$t==18  & BR_Results$r5=="REF")])
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="ASIA")] )/sum(BR_Results$YGROSS2020[which(BR_Results$t==18  & BR_Results$r5=="ASIA")])

    glimpse(BR_Results)
    

    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="MAF")] )/sum(BR_Results$NPV_GDP[which(BR_Results$t==18 & BR_Results$r5=="MAF")] )
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="OECD")] )/sum(BR_Results$NPV_GDP[which(BR_Results$t==18 & BR_Results$r5=="OECD")] )
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="LAM")] )/sum(BR_Results$NPV_GDP[which(BR_Results$t==18 & BR_Results$r5=="LAM")] )
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="REF")] )/sum(BR_Results$NPV_GDP[which(BR_Results$t==18 & BR_Results$r5=="REF")] )
    100*sum(BR_Results$NPV[which(BR_Results$t==18 & BR_Results$r5=="ASIA")] )/sum(BR_Results$NPV_GDP[which(BR_Results$t==18 & BR_Results$r5=="ASIA")] )

   

    BR_Results$NPV_perc <- 100*BR_Results$NPV/BR_Results$YGROSS2020
    
    
    BR_Results$disc_gdp <- BR_Results$dr * BR_Results$YGROSS_baseline

    isos <- levels(factor(BR_Results$n))
     
    
    BR_Results$ES_change <- (BR_Results$nat_omega_damages_nN-1)*100

    

    #ddply(BR_Results[which(BR_Results$t==18),], .(r5),   # so by asset class invoke following function
    #  function(x) data.frame(npvr5=weighted.mean(x$NPV_perc, x$pop)))

    BR_Results$ES_change[which(BR_Results$t==18)]>0



    
library(ggallin)
GDPNPV <- ggplot(BR_Results[which(BR_Results$t==18),])+
        geom_point(aes(x=log(YGROSS2020/pop2020),y=NPV_perc,color=r5))+theme_bw() +
        geom_smooth(data=BR_Results[which(BR_Results$t==18),],aes(x=log(YGROSS2020/pop2020),y=NPV_perc,formula="y~x",method="lm"))+
        ylab("NPV Damages (% GDP in 2020)")+
        geom_hline(aes(yintercept=0),linetype="dashed")+
        scale_y_continuous(trans=pseudolog10_trans)
    GDPNPV       



    BR_Results$ES_change <- (BR_Results$nat_omega_damages_nN-1)*100
    BR_Results$ES_change_ub <- (BR_Results$nat_omega_ub_nonmkt_damages_nN-1)*100
    BR_Results$ES_change_lb <- (BR_Results$nat_omega_lb_nonmkt_damages_nN-1)*100
    BR_Results$ES_change <- (BR_Results$nat_omega_damages_nN-1)*100


 
    BR_Results$ES_dam <- (BR_Results$nat_omega_damages_nN*0.03)*(1+(0.593*(100*((BR_Results$YGROSS_damages-BR_Results$YGROSS2020)/BR_Results$YGROSS2020)))/100)
    BR_Results$ES_nodam <- (BR_Results$nat_omega_baseline_nN*0.03)*(1+(0.593*(100*((BR_Results$YGROSS_baseline-BR_Results$YGROSS2020)/BR_Results$YGROSS2020)))/100)
    BR_Results$ES_change <- 100*(BR_Results$ES_dam-BR_Results$ES_nodam)/BR_Results$ES_nodam
    BR_Results$ES_change_val <- BR_Results$ES_dam-BR_Results$ES_nodam
    BR_Results$ES_change_val_percGDP <- BR_Results$ES_change_val / BR_Results$YGROSS_damages
    ggplot(BR_Results) + geom_line(aes(x=year,y=ES_change_val,group=n))
    glimpse(BR_Results)
    BR_Results %>% filter(t==18) %>% group_by(r5) %>% summarise(ES_change_val=sum(ES_change_val,na.rm=T),YGROSS_damages=sum(YGROSS_damages,na.rm=T),ES_change_val_percGDP=median(ES_change_val_percGDP,na.rm=T))

    BR_Results2100 <- BR_Results[which(BR_Results$t==18),] 
    BR_Results2100$ES_change[which(BR_Results2100$ES_change<0)]
    BR_Results$t[BR_Results$ES_nodam==0]

    ggplot(BR_Results[which(BR_Results$t!=3),])+
    geom_line(aes(x=t,y=ES_dam,group=n,color=r5))
    
    ggplot(BR_Results)+
    geom_line(aes(x=t,y=ES_nodam,group=n,color=r5))
    
    ggplot(BR_Results[which(BR_Results$t!=3),])+
    geom_line(aes(x=t,y=ES_change,group=n,color=r5))

    
    BR_Results$ES_dam_ub <- (BR_Results$nat_omega_ub_nonmkt_damages_nN*0.03)*(1+(0.593*(100*((BR_Results$YGROSS_damages-BR_Results$YGROSS2020)/BR_Results$YGROSS2020)))/100)
    BR_Results$ES_change_ub <- 100*(BR_Results$ES_dam_ub-BR_Results$ES_nodam)/BR_Results$ES_nodam

    BR_Results$ES_dam_lb <- (BR_Results$nat_omega_lb_nonmkt_damages_nN*0.03)*(1+(0.593*(100*((BR_Results$YGROSS_damages-BR_Results$YGROSS2020)/BR_Results$YGROSS2020)))/100)
    BR_Results$ES_change_lb <- 100*(BR_Results$ES_dam_lb-BR_Results$ES_nodam)/BR_Results$ES_nodam



    
    GDP2100 <- ggplot(BR_Results[which(BR_Results$t==18),])+
        geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline,color=r5))+theme_bw() +
        geom_smooth(data=BR_Results[which(BR_Results$t==18),],aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline),formula="y~x",method="lm")+
        ylab("GDP change in 2100 (%)")+
        geom_hline(aes(yintercept=0),linetype="dashed")+
     xlab("Log GDP per capita in 2020")+ 
     guides(color=guide_legend(title="Region"))
    GDP2100        

    ES2100 <- ggplot(BR_Results[which(BR_Results$t==18),])+
       geom_point(aes(x=log(YGROSS2020/pop2020),y=ES_change,color=r5))+theme_bw() + 
       geom_smooth(data=BR_Results[which(BR_Results$t==18),],
     aes(x=log(YGROSS2020/pop2020),y=ES_change),formula="y~x",method="lm")+
     ylab("Non-market benefits  \nchange in 2100 (%)")+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     xlab("Log GDP per capita in 2020")+ 
     guides(color=guide_legend(title="Region"))

    ES2100    
    
    BR_Results_lpj_noS <- BR_Results
    save(BR_Results_lpj_noS,file="Data/DataForFigures/BR_Results_lpj_noS.Rds")
     
     ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")
     results2100lpj <- ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")

     #ggsave("Figures/Final figures/Submission 3/Change_GDP_ES_in2100_submission3.png",dpi=600)


    BR_Results2100 <-  BR_Results[which(BR_Results$t==18),]
    BR_Results2100$ES_change[which(BR_Results2100$ES_change<0)]
    BR_Results2100$n[which(BR_Results2100$ES_change<0)]
     BR_Results2100$n[which(BR_Results2100$ES_change>0)]

     glimpse(world)

    BR_Results$country <-(BR_Results$n)
    BR_Results$Continent <-(BR_Results$r5)
    
    
    BR_Results <- BR_Results[which(BR_Results$year <2101),]
    BR_Results$r5 <- (factor(BR_Results$r5))
    levels(BR_Results$r5) <- c("ASIA","LAM","MAF","OECD","REF")
    
    
    x_gdp <- BR_Results$GDP_change_perc[which(BR_Results$year %in% c(2100))]
    x_es <- BR_Results$ES_change[which(BR_Results$year %in% c(2100))]
    w <- BR_Results$pop[which(BR_Results$year %in% c(2100))]
    pop_mean_gdp_lpj <- weighted.mean(x=x_gdp,w=w)
    pop_mean_es_lpj <- weighted.mean(x=x_es,w=w)
    weighted_var_es <- sum(w * (x_es - pop_mean_es_lpj)^2) / sum(w)
    pop_se_es_lpj <- sqrt(weighted_var_es)
    weighted_var_gdp <- sum(w * (x_gdp - pop_mean_gdp_lpj)^2) / sum(w)
    pop_se_gdp_lpj <- sqrt(weighted_var_gdp)

    aggdp <-aggregate(GDP_change_perc~n+r5,data=BR_Results,FUN="sum")
    aggregate(GDP_change_perc~r5,data=aggdp,FUN="mean")

    #install.packages("ggbreak")
    library(ggbreak) 
    
    glimpse(BR_Results)
    
    #BR_Results_lpj_traj_noS <- BR_Results
    #save(BR_Results_lpj_traj_noS,file="Data/DataForFigures/BR_Results_lpj_traj_noS.Rds")
    plot_trajectory <- ggplot(BR_Results,
     aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
     geom_hline(aes(yintercept=0))+
     geom_vline(aes(xintercept=0))+
      geom_line()+
       #geom_text(data = BR_Results[which(BR_Results$year %in% c(2100) ),], aes(x=ES_change,y=GDP_change_perc,color=r5,label=n),alpha=0.4)+ 

       geom_point(data = BR_Results[which(BR_Results$year %in% c(2050,2080,2100) ),], 
        aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_minimal()+
       geom_hline(aes(yintercept=pop_mean_gdp_lpj),linetype=2,alpha=0.9)+
       geom_vline(aes(xintercept=pop_mean_es_lpj),linetype=2,alpha=0.9)+
      geom_errorbar(data=BR_Results[which(BR_Results$year %in% c(2100)),],
        aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
      geom_errorbar(data=BR_Results[which(BR_Results$year %in% c(2100)),],
        aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
        #geom_errorbarh(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(xmin=(es_change_minus_se)*100, xmax=(es_change_plus_se)*100),alpha=0.3)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       ylab("Annual GDP change (%)\n") + ggtitle("")  +
       #scale_y_break(c(-0.6, 0.1))+
       #coord_cartesian(xlim=c(-45,10),ylim=c(-0.6, 0.1)) +
       xlab("Annual non-market benefits change (%)")+
       scale_color_scico_d()

       plot_trajectory
  
       
       
       leg_trj <-  get_legend(plot_trajectory)

        maxyear <- aggregate(year~country, data=BR_Results,FUN="max")
        maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
        BR_Results$countrymax <- paste0(BR_Results$country,BR_Results$year)

        all_simmax <- BR_Results[BR_Results$countrymax %in% maxyear$countrymax,]
    
    # plot_count_es <- ggplot(BR_Results[which(BR_Results$year==2100),], # & abs(all_sim_mean$es_change)>0.01
    #     aes(x=es_change*100,fill=Continent))+
    #     xlim(c(-45,10))+
    #     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") #+xlim(-12,5)#+ xlim(-100,60)
    # plot_count_es
        
    ordered_GR <- BR_Results[which(BR_Results$year==2100),]
    ordered_GR <- ordered_GR[order(ordered_GR$ES_change),]

    totpop <- sum(BR_Results$pop[which(BR_Results$year==2100)])

    ordered_BR_lpj <- ordered_GR
    #save(ordered_BR_lpj,file="Data/DataForFigures/ordered_BR_lpj.Rds")
    plot_count_es <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=(ES_change),y=cumsum(100*pop/totpop)))+
        #geom_bar(stat="identity")+
        geom_line(color="gray")+
        geom_point(aes(color=r5),shape=15)+
        xlim(c(-45,10))+
        theme_minimal()+ ylab("Cumulative \npopulation in 2100 (%)")+xlab('')+
       scale_color_scico_d()
    plot_count_es


    ordered_GR <- BR_Results[which(BR_Results$year==2100),]
    #ordered_GR$GDPpc <- ordered_GR$YGROSS_baseline/ordered_GR$pop
    ordered_GR$GDPpc <- ordered_GR$YGROSS2020*10^6/ordered_GR$pop2020
    ordered_GR <- ordered_GR[order(ordered_GR$GDPpc),]

    totGDP_change <- sum(BR_Results$GDP_change[which(BR_Results$year==2100)])
    ordered_GR$cumdam <- cumsum(100*ordered_GR$GDP_change/totGDP_change)

    plot_distribution_damages <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=(GDPpc),y=cumdam))+
        #geom_bar(stat="identity")+
        geom_line(color="gray")+
        geom_point(aes(color=r5),shape=15)+
        #geom_vline(aes(xintercept=0.0815))+
        #geom_hline(aes(yintercept=90),linetype="dashed")+
        geom_vline(aes(xintercept=18668.173))+
        geom_hline(aes(yintercept=88.74806),linetype="dashed")+
        #xlim(c(-45,10))+
        labs(color="Region")+
        theme_minimal()+ ylab("Cumulative global GDP \ndamages in 2100 (% of total)")+xlab('GDP per capita in 2020 ($ per person)')+
       scale_color_scico_d()
    plot_distribution_damages
    ordered_BR_dam_lpj <- ordered_GR
    #save(ordered_BR_dam_lpj,file="Data/DataForFigures/ordered_BR_dam_lpj.Rds")
    #ggsave("Figures/Final figures/Submission 3/Distribution_damages.png",dpi=600)


    plot_count_gdp <- ggplot(BR_Results[which(BR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=gdp_change*100,fill=Continent))+xlab('')+#xlim(-15,6)+
        ylab("count in 2100")+
        geom_histogram()+theme_minimal()+coord_flip() +
       scale_color_scico_d()

    glimpse(BR_Results[which(BR_Results$year==2100),])


    plot_count_gdp <- ggplot(BR_Results[which(BR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=(GDP_change),y=(100*pop/totpop)))+
        #geom_bar(stat="identity")+
        geom_line(color="gray")+
        geom_point(aes(color=r5),shape=15)+
        theme_bw() + coord_flip() + ylab("Population in 2100 (%)")+ 
          #scale_y_break(c(-2.8, -2))+
      scale_x_continuous(trans = scales::pseudo_log_trans())+theme_minimal()+xlab('')+
       scale_color_scico_d()
    plot_count_gdp

    ordered_GR <- BR_Results[which(BR_Results$year==2100),]
    ordered_GR <- ordered_GR[order(-ordered_GR$GDP_change_perc),]
    plot_count_gdp_cum <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(GDP_change_perc),y=cumsum(100*pop/totpop)))+
    #geom_bar(stat="identity")+
    geom_line(color="gray")+
    geom_point(aes(color=r5),shape=15)+
    #xlim(c(-0.65,0.1))+ 
    #xlim(c(-2.8,0))+ 
    theme_bw() + coord_flip() + ylab("Cumulative\npopulation \nin 2100 (%)")+
    theme_minimal()+xlab('')+
       scale_color_scico_d()

    ordered_BR_lpj_gdp <- ordered_GR
    #save(ordered_BR_lpj_gdp,file="Data/DataForFigures/ordered_BR_lpj_gdp.Rds")

    plot_count_gdp_cum
    #install.packages("ggridges")
    #library("ggridges")



    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
  

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")

        #ggsave("Figures/Final figures/Submission 3/Trajectory_percent_noS.png",dpi=600)
        #ggsave("Figures/Final figures/Submission 3/Fig4_pre.png",dpi=600)
    #ggarrange(plot_trajectory, results2100lpj_noS,ncol=1,heights=c(3,1))
    #ggsave("Figures/Final figures/Submission 3/Trajectory_percent_noS.png",dpi=600)
    
    trajlpj <- plot_trajectory+geom_text(data = BR_Results[which(BR_Results$year %in% c(2100) ),],   aes(x=ES_change,y=GDP_change_perc,color=r5,label=n))
        trajlpj


# set iso3 'Country definition in ISO_3166-1_alpha-3' /
# ABW
# AFG
# AGO
# AIA
# ALA
# ALB
# AND
# ARE
# ARG
# ARM
# ASM
# ATA
# ATF
# ATG
# AUS
# AUT
# AZE
# BDI
# BEL
# BEN
# BES
# BFA
# BGD
# BGR
# BHR
# BHS
# BIH
# BLM
# BLR
# BLZ
# BMU
# BOL
# BRA
# BRB
# BRN
# BTN
# BVT
# BWA
# CAF
# CAN
# CCK
# CHE
# CHL
# CHN
# CIV
# CMR
# COD
# COG
# COK
# COL
# COM
# CPV
# CRI
# CUB
# CUW
# CXR
# CYM
# CYP
# CZE
# DEU
# DJI
# DMA
# DNK
# DOM
# DZA
# ECU
# EGY
# ERI
# ESH
# ESP
# EST
# ETH
# FIN
# FJI
# FLK
# FRA
# FRO
# FSM
# GAB
# GBR
# GEO
# GGY
# GHA
# GIB
# GIN
# GLP
# GMB
# GNB
# GNQ
# GRC
# GRD
# GRL
# GTM
# GUF
# GUM
# GUY
# HKG
# HMD
# HND
# HRV
# HTI
# HUN
# IDN
# IMN
# IND
# IOT
# IRL
# IRN
# IRQ
# ISL
# ISR
# ITA
# JAM
# JEY
# JOR
# JPN
# KAZ
# KEN
# KGZ
# KHM
# KIR
# KNA
# KOR
# KSV
# KWT
# LAO
# LBN
# LBR
# LBY
# LCA
# LIE
# LKA
# LSO
# LTU
# LUX
# LVA
# MAC
# MAF
# MAR
# MCO
# MDA
# MDG
# MDV
# MEX
# MHL
# MKD
# MLI
# MLT
# MMR
# MNE
# MNG
# MNP
# MOZ
# MRT
# MSR
# MTQ
# MUS
# MWI
# MYS
# MYT
# NAM
# NCL
# NER
# NFK
# NGA
# NIC
# NIU
# NLD
# NOR
# NPL
# NRU
# NZL
# OMN
# PAK
# PAN
# PCN
# PER
# PHL
# PLW
# PNG
# POL
# PRI
# PRK
# PRT
# PRY
# PSE
# PYF
# QAT
# REU
# ROU
# RUS
# RWA
# SAU
# SDN
# SEN
# SGP
# SGS
# SHN
# SJM
# SLB
# SLE
# SLV
# SMR
# SOM
# SPM
# SRB
# SSD
# STP
# SUR
# SVK
# SVN
# SWE
# SWZ
# SXM
# SYC
# SYR
# TCA
# TCD
# TGO
# THA
# TJK
# TKL
# TKM
# TLS
# TON
# TTO
# TUN
# TUR
# TUV
# TWN
# TZA
# UGA
# UKR
# UMI
# URY
# USA
# UZB
# VAT
# VCT
# VEN
# VGB
# VIR
# VNM
# VUT
# WLF
# WSM
# YEM
# ZAF
# ZMB
# ZWE
# /;
# set map_n_iso3(n,iso3) 'Mapping between WITCH regions and iso3'/
# rcam.BES
# rcam.CUW
# rcam.SXM #Sint Maarten from Netherlands
# noan.ESH
# aut.AUT
# bel.BEL
# rfa.DEU
# dnk.DNK
# esp.ESP
# fin.FIN
# fra.FRA
# gbr.GBR
# grc.GRC
# irl.IRL
# ita.ITA
# nld.NLD
# prt.PRT
# swe.SWE
# bgr.BGR
# rcz.CZE
# hun.HUN
# pol.POL
# rom.ROU
# rsl.SVK
# slo.SVN
# cro.HRV
# nor.NOR
# sui.CHE
# tur.TUR
# blt.EST
# blt.LTU
# blt.LVA
# oeu.CYP
# oeu.LUX
# oeu.MLT
# oeu.LIE
# oeu.GRL
# oeu.ISL
# oeu.FRO
# oeu.ALA
# oeu.AND
# oeu.GGY
# oeu.GIB
# oeu.IMN
# oeu.JEY
# oeu.MCO
# oeu.SJM
# oeu.SMR
# oeu.VAT
# oeu.SPM
# oeu.BIH
# oeu.ALB
# oeu.MKD
# oeu.MNE
# oeu.SRB
# oeu.KSV
# noap.LBY
# noap.DZA
# noan.TUN
# noan.MAR
# egy.EGY
# zaf.ZAF
# rsaf.AGO
# rsaf.BEN
# rsaf.BWA
# rsaf.BFA
# rsaf.BDI
# rsaf.CMR
# rsaf.CPV
# rsaf.CAF
# rsaf.TCD
# rsaf.COM
# rsaf.COG
# rsaf.COD
# rsaf.CIV
# rsaf.GNQ
# rsaf.ERI
# rsaf.ETH
# rsaf.GAB
# rsaf.GMB
# rsaf.GHA
# rsaf.GIN
# rsaf.GNB
# rsaf.KEN
# rsaf.LSO
# rsaf.LBR
# rsaf.MDG
# rsaf.MWI
# rsaf.MLI
# rsaf.MRT
# rsaf.MUS
# rsaf.MYT
# rsaf.MOZ
# rsaf.NAM
# rsaf.NER
# rsaf.NGA
# rsaf.REU
# rsaf.RWA
# rsaf.STP
# rsaf.SEN
# rsaf.SYC
# rsaf.SHN
# rsaf.SLE
# rsaf.SOM
# rsaf.SSD
# rsaf.SDN
# rsaf.SWZ
# rsaf.TZA
# rsaf.TGO
# rsaf.UGA
# rsaf.ZMB
# rsaf.ZWE
# rsaf.DJI
# rsaf.IOT
# rsaf.BVT
# rsaf.ATF
# meme.ISR
# meme.JOR
# meme.SYR
# meme.LBN
# meme.PSE
# golf57.ARE
# golf57.BHR
# golf57.IRN
# golf57.IRQ
# golf57.KWT
# golf57.OMN
# golf57.QAT
# golf57.SAU
# golf57.YEM
# aus.AUS
# rjan57.CXR
# rjan57.COK
# rjan57.HMD
# rjan57.NFK
# rjan57.NIU
# rjan57.NRU
# rjan57.PCN
# rjan57.TKL
# rjan57.TUV
# rjan57.UMI
# rjan57.WLF
# rjan57.FJI
# rjan57.PNG
# rjan57.FSM
# rjan57.GUM
# rjan57.ASM
# rjan57.TLS
# rjan57.PYF
# rjan57.KIR
# rjan57.MNP
# rjan57.MHL
# rjan57.NCL
# rjan57.PLW
# rjan57.WSM
# rjan57.SLB
# rjan57.TON
# rjan57.VUT
# rjan57.NZL
# chn.CHN
# nde.IND
# mys.MYS
# tha.THA
# vnm.VNM
# jpn.JPN
# cor.KOR
# idn.IDN
# osea.BRN
# osea.CCK
# osea.KHM
# osea.LAO
# osea.MMR
# osea.PHL
# osea.SGP
# osea.PRK
# osea.HKG
# osea.MAC
# osea.TWN
# osea.MNG
# rsas.AFG
# rsas.BGD
# rsas.BTN
# rsas.LKA
# rsas.MDV
# rsas.NPL
# rsas.PAK
# arg.ARG
# bra.BRA
# chl.CHL
# rsam.BOL
# rsam.COL
# rsam.ECU
# rsam.FLK
# rsam.GUF French Guiana
# rsam.GUY
# rsam.PER
# rsam.PRY
# rsam.SUR
# rsam.URY
# rsam.VEN
# mex.MEX
# rcam.ABW
# rcam.BHS
# rcam.BLZ
# rcam.BRB
# rcam.CRI
# rcam.CUB
# rcam.DMA
# rcam.DOM
# rcam.GRD
# rcam.GTM
# rcam.HND
# rcam.HTI
# rcam.JAM
# rcam.LCA
# rcam.NIC
# rcam.PAN
# rcam.SLV
# rcam.TTO
# rcam.VCT
# rcam.ATA
# rcam.BMU
# rcam.SGS
# rcam.TCA
# rcam.VGB
# rcam.VIR
# rcam.AIA
# rcam.ATG
# rcam.BLM
# rcam.CYM
# rcam.GLP
# rcam.KNA
# rcam.MAF
# rcam.MSR
# rcam.MTQ
# rcam.PRI
# can.CAN
# usa.USA
# rus.RUS
# ukr.UKR
# ris.ARM
# ris.AZE
# ris.BLR
# ris.GEO
# ris.KAZ
# ris.KGZ
# ris.MDA
# ris.TJK
# ris.TKM
# ris.UZB
# /;
# set oecd(n) 'OECD regions' /
# aus
# aut
# bel
# can
# chl
# cor
# dnk
# esp
# fin
# fra
# gbr
# grc
# hun
# irl
# ita
# jpn
# meme
# mex
# nld
# nor
# pol
# prt
# rcz
# rfa
# rjan57
# rsl
# slo
# sui
# swe
# tur
# usa
# /;
# set eu(n) 'EU regions' /
# aut
# bel
# bgr
# blt
# cro
# dnk
# esp
# fin
# fra
# gbr
# grc
# hun
# irl
# ita
# nld
# nor
# oeu
# pol
# prt
# rcz
# rfa
# rom
# rsl
# slo
# swe
# /;
# set eu27(n) 'EU27 regions' /
# aut
# bel
# bgr
# blt
# cro
# dnk
# esp
# fin
# fra
# grc
# hun
# irl
# ita
# nld
# pol
# prt
# rcz
# rfa
# rom
# rsl
# slo
# swe
# /;
# set eu28(n) 'EU28 regions' /
# aut
# bel
# bgr
# blt
# cro
# dnk
# esp
# fin
# fra
# gbr
# grc
# hun
# irl
# ita
# nld
# pol
# prt
# rcz
# rfa
# rom
# rsl
# slo
# swe
# /;
