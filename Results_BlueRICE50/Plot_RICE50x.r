dir <- "C:/Users/basti/Documents/GitHub/BlueDICE"
setwd(dir)
x<-c("readxl","tidyverse","ggplot2",
        "ggpubr","reshape2","dplyr",
        "RColorBrewer","directlabels")
    lapply(x, require, character.only = TRUE)

#exp_names <- c("GreenRICE", "GreenRICE_nodam")
#exp_names <- c("baseline","GR_nodam","GR")
exp_names <- c("results_ocean_impact_off","results_ocean_off_impact_off")
    #exp_names <- c("R","GR_NWB_g005","GR_NWB_gWB","GR_NWB_gWB_dam10","GR_NWB_gWB_dam10cum","GR_NWB_gWB_damNolan",
   # "R_sav","GR_NWB_gWB_damNDVI")
var_names <- c("CPC_OCEAN_DAM","CPC","OCEAN_NONUSE_VALUE")
#To convert gdx to xls: in a comand window type: gdx2xls results_R_sim.gdx

for (i in 1:length(exp_names)){
    for (j in 1:length(var_names)){
        #var_table <- read_excel(paste('RICE50x/results_',exp_names[i],'.xlsx',sep=''), sheet = var_names[j])
        var_table <- read_excel(paste('Results_BlueRICE50/',exp_names[i],'.xlsx',sep=''), sheet = var_names[j])
        glimpse(var_table)
        var_table <- var_table[3:(dim(var_table)[1]),]
        var_table <- as.data.frame(var_table)
        if (dim(var_table)[2]<4){
            if(var_names[j]=="scc"){
                names(var_table) <- c("year","country",var_names[j])
                var_table$country <- as.factor(var_table$country)
                var_table[,which(names(var_table)==var_names[j])] <- as.double(unlist(var_table[,which(names(var_table)==var_names[j])]))
                var_table$year <- 1980 + (as.integer(var_table$year)-1)*5
                var_table$country <- as.factor(var_table$country)
                var_table[,which(names(var_table)==var_names[j])] <- as.double(unlist(var_table[,which(names(var_table)==var_names[j])]))
                var_table$exp <- exp_names[i]
                var_table$id <- paste(var_table$country,var_table$year,var_table$exp, sep="")
                var_table$exp <- exp_names[i] 
                #var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))] <- as.factor(var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))])
            }else{
            names(var_table) <- c(paste("type",var_names[j],sep='_'),"country",var_names[j])
            var_table$country <- as.factor(var_table$country)
            var_table[,which(names(var_table)==var_names[j])] <- as.double(unlist(var_table[,which(names(var_table)==var_names[j])]))
            var_table$exp <- exp_names[i] 
            var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))] <- as.factor(var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))])
            }
        } else {
            if (dim(var_table)[2]<7){
                names(var_table) <- c("year", "country",paste(var_names[j],"low",sep='_'),var_names[j],paste(var_names[j],"high",sep='_'),paste(var_names[j],"marginal",sep='_'))
            } else {
                names(var_table) <- c(paste("type",var_names[j],sep='_'),"year", "country",paste(var_names[j],"low",sep='_'),var_names[j],paste(var_names[j],"high",sep='_'),paste(var_names[j],"marginal",sep='_'))
                var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))] <- as.factor(var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))])
            }
            var_table$year <- 1980 + (as.integer(var_table$year)-1)*5
            var_table$country <- as.factor(var_table$country)
            var_table[,which(names(var_table)==var_names[j])] <- as.double(unlist(var_table[,which(names(var_table)==var_names[j])]))
            var_table$exp <- exp_names[i]
            var_table$id <- paste(var_table$country,var_table$year,var_table$exp, sep="")
        }
            
        if (j==1) {
            assign(paste("exp_data",exp_names[i],sep="_"), var_table)
        } else {
            
            assign(paste("exp_data",exp_names[i],sep="_"), merge(eval(parse(text = paste("exp_data",exp_names[i],sep="_"))),var_table,all=FALSE)) 
            
            }
    }
}

    
glimpse(exp_data_results_ocean_off_impact_off)
    exp_data_BR <- exp_data_results_ocean_impact_off
    exp_data_baseline <- exp_data_results_ocean_off_impact_off

BR <- exp_data_BR %>% 
                            left_join(exp_data_baseline %>% 
                            dplyr::select("year","country","type_OCEAN_NONUSE_VALUE","OCEAN_NONUSE_VALUE"), 
                            by=c("year","country","type_OCEAN_NONUSE_VALUE"),) %>% 
                            mutate(CPC_percentage_change = 100*(CPC_OCEAN_DAM-CPC)/CPC,
                                    OCEAN_NONUSE_VALUE_percentage_change = 100*(OCEAN_NONUSE_VALUE.y-OCEAN_NONUSE_VALUE.x)/OCEAN_NONUSE_VALUE.x)

glimpse(BR)     
BR %>% filter(year==2025,country=="arg")

ggplot(BR)+
geom_point(aes(x=CPC_percentage_change,
    y=OCEAN_NONUSE_VALUE.y,
    shape=type_OCEAN_NONUSE_VALUE,
    color=country))

exp_data_BR_merged 
        merge(exp_data_BR_nodam,exp_data_baseline[names(exp_data_baseline) %in% c("countryyear","OCEAN_NONUSE_VALUE")],by="countryyear",suffixes=c("","baseline"),all=FALSE)
    glimpse(exp_data_BR_nodam)

    exp_data_BR_nodam$CPCchange <- exp_data_BR_nodam$CPC - exp_data_BR_nodam$CPCbaseline

    exp_data_BR <- merge(exp_data_BR,exp_data_BR_nodam[names(exp_data_BR_nodam) %in% c("countryyear","OCEAN_NONUSE_VALUE")],by="countryyear",suffixes=c("","baseline"),all=FALSE)
    exp_data_BR$CPCchange <- exp_data_BR$CPC - exp_data_BR$CPCbaseline
    exp_data_BR$CPCchangeperc <- 100*(exp_data_BR$CPC_OCEAN_DAM - exp_data_BR$CPC)/exp_data_BR$CPC
    exp_data_BR$OCEAN_NONUSE_VALUE_changeperc <- 100*(exp_data_BR$OCEAN_NONUSE_VALUE - exp_data_BR$OCEAN_NONUSE_VALUEbaseline)/exp_data_BR$OCEAN_NONUSE_VALUEbaseline
glimpse(exp_data_BR)    
    #exp_data_BR$NKdamperc <- 100*(exp_data_BR$NAT_KAP_DAM - exp_data_BR$NAT_KAP_DAMbaseline)/exp_data_BR$NAT_KAP_DAMbaseline
    #glimpse(exp_data_BR)

    ggplot(exp_data_BR[exp_data_BR$year<2090,], aes(x=year,y=OCEAN_NONUSE_VALUE-OCEAN_NONUSE_VALUEbaseline,color=country))+
    geom_line(aes(linetype=type_OCEAN_NONUSE_VALUE))+
    theme_bw()+
    #ylim(c(0,100))+
    xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(data = exp_data_BR[exp_data_BR$year==2090,],aes(label=country))


     ggplot(exp_data_BR[exp_data_BR$year<2090,], aes(x=year,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(data = exp_data_BR[exp_data_BR$year==2090,],aes(label=country))


    ggplot(exp_data_BR[exp_data_BR$year==2100,], aes(x=(CPC),y=CPCchangeperc))+
    geom_point()+
    theme_bw()+
    geom_smooth(data=exp_data_BR[exp_data_BR$year==2100,], aes(x=(CPC),y=CPCchangeperc))
    
    ggplot(exp_data_BR[exp_data_BR$year==2100,], aes(x=(CPC),y=CPCchangeperc))+
    geom_point()+
    theme_bw()+
    geom_smooth(data=exp_data_BR[exp_data_BR$year==2100,], aes(x=(CPC),y=CPCchangeperc),method="loess",span=0.9)+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")
    #geom_text(data = exp_data_BR[exp_data_BR$year==2100,],aes(label=country)) +



    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$year==2090,], aes(x=NAT_KAP,y=CPCchange,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))


    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$type_prodshare=="nature" & exp_data_BR_nodam$year==2090,], aes(x=prodshare,y=CPCchange,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))

    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$type_prodshare=="nature" & exp_data_BR_nodam$year==2090,], aes(x=log(prodshare),y=log(NAT_KAP),color=CPCchange))+
    #geom_point()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    scale_colour_BRadient2(low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    #limits=c(quantile(exp_data_BR_nodam$CPCchange,0.01), quantile(exp_data_BR_nodam$CPCchange,0.99)), oob=squish
    limits=c(-10, 10), oob=squish
    )+
    #guides(color="none")+
    #ylab("Nat")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))

    exp_data_BR_nodam$CPCchangeperc <- 100*(exp_data_BR_nodam$CPC - exp_data_BR_nodam$CPCbaseline)/exp_data_BR_nodam$CPCbaseline

    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$year<2090,], aes(x=year,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (% of baseline)")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(data = exp_data_BR_nodam[exp_data_BR_nodam$year==2090,],aes(label=country))


    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$year==2090,], aes(x=NAT_KAP,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (%)")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))


    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$type_prodshare=="nature" & exp_data_BR_nodam$year==2090,], aes(x=prodshare,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))

    ggplot(exp_data_BR_nodam[exp_data_BR_nodam$type_prodshare=="nature" & exp_data_BR_nodam$year==2090,], aes(x=log(prodshare),y=log(NAT_KAP),color=CPCchangeperc))+
    #geom_point()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    scale_colour_BRadient2(low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    #limits=c(quantile(exp_data_BR_nodam$CPCchangeperc,0.01), quantile(exp_data_BR_nodam$CPCchangeperc,0.99)), oob=squish
    limits=c(-10, 10), oob=squish
    )+
    #guides(color="none")+
    #ylab("Nat")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))

    ggplot(exp_data_BR[exp_data_BR$year<2090,], aes(x=year,y=CPCchange,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(data = exp_data_BR[exp_data_BR$year==2090,],aes(label=country))


    ggplot(exp_data_BR[exp_data_BR$year==2090,], aes(x=NAT_KAP,y=CPCchange,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))


    ggplot(exp_data_BR[exp_data_BR$type_prodshare=="nature" & exp_data_BR$year==2090,], aes(x=prodshare,y=CPCchange,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))

    ggplot(exp_data_BR[exp_data_BR$type_prodshare=="nature" & exp_data_BR$year==2090,], aes(x=log(prodshare),y=log(NAT_KAP),color=CPCchange))+
    #geom_point()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    scale_colour_BRadient2(low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    #limits=c(quantile(exp_data_BR$CPCchange,0.01), quantile(exp_data_BR$CPCchange,0.99)), oob=squish
    limits=c(-10, 10), oob=squish
    )+
    #guides(color="none")+
    #ylab("Nat")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))


    ggplot(exp_data_BR[exp_data_BR$year<2090,], aes(x=year,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (% of baseline)")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(data = exp_data_BR[exp_data_BR$year==2090,],aes(label=country))

    ggplot(exp_data_BR[exp_data_BR$year<2190,], aes(x=year,y=NKdamperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    xlim(c(2000,2190))+
    guides(color="none")+
    ylab("Change in Damaged Natural Capital (% of baseline)")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt GreenRICE (no clim dam)")+
    geom_text(data = exp_data_BR[exp_data_BR$year==2190,],aes(label=country))


    ggplot(exp_data_BR[exp_data_BR$year==2090,], aes(x=NAT_KAP,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (%)")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))


    ggplot(exp_data_BR[exp_data_BR$type_prodshare=="nature" & exp_data_BR$year==2090,], aes(x=prodshare,y=CPCchangeperc,color=country))+
    geom_line()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    guides(color="none")+
    ylab("Change in Consumption per capita (Thousands 2005USD")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))

    ggplot(exp_data_BR[exp_data_BR$type_prodshare=="nature" & exp_data_BR$year==2090,], aes(x=log(prodshare),y=log(NAT_KAP),color=CPCchangeperc))+
    #geom_point()+
    theme_bw()+
    #ylim(c(0,100))+
    #xlim(c(2000,2090))+
    scale_colour_BRadient2(low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    #limits=c(quantile(exp_data_BR$CPCchangeperc,0.01), quantile(exp_data_BR$CPCchangeperc,0.99)), oob=squish
    limits=c(-10, 10), oob=squish
    )+
    #guides(color="none")+
    #ylab("Nat")+
    ggtitle("Comparing GreenRICE (no clim dam) wrt Baseline (no natural capital)")+
    geom_text(aes(label=country))








































    dim(exp_data_BR_nodam[exp_data_BR_nodam$type_prodshare=="nature",])
    dim(exp_data_baseline[exp_data_baseline$type_prodshare=="nature",])

    min(exp_data_baseline$year)
    min(exp_data_BR$year)



    exp_data_BR_nodam$CPC_change <- exp_data_BR_nodam$CPC[exp_data_BR_nodam$type_prodshare=="capital"] - exp_data_baseline$CPC[exp_data_baseline$type_prodshare=="capital"]


CPC_baseline <- ggplot(exp_data_baseline[exp_data_baseline$year<2051,], aes(x=year,y=CPC,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,100))+
    xlim(c(2000,2060))+
    guides(color="none")+
    ylab("Consumption per capita (Thousands 2005USD")+
    ggtitle("Baseline")+
    geom_text(data = exp_data_baseline[exp_data_baseline$year==2055,],aes(label=country))

CPC_noNK <- ggplot(exp_data_BReenRICE_noNK[exp_data_BReenRICE_noNK$year<2051,], aes(x=year,y=CPC,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,100))+
    xlim(c(2000,2060))+
    guides(color="none")+
    ggtitle("Standard RICE")+
    ylab("Consumption per capita (Thousands 2005USD)")+
    geom_text(data = exp_data_BReenRICE_noNK[exp_data_BReenRICE_noNK$year==2055,],aes(label=country))


CPC_nodam <- ggplot(exp_data_BReenRICE_nodam[exp_data_BReenRICE_nodam$year<2051,], aes(x=year,y=CPC,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,100))+
    xlim(c(2000,2060))+
    guides(color="none")+
    ggtitle("Standard RICE")+
    ylab("Consumption per capita (Thousands 2005USD)")+
    geom_text(data = exp_data_BReenRICE_nodam[exp_data_BReenRICE_nodam$year==2055,],aes(label=country))




SCC <- ggplot(exp_data_BReenRICE, aes(x=year,y=scc,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,2000))+
    xlim(c(2000,2050))+
    guides(color="none")+
    ggtitle("GreenRICE: market goods depend on natural capital")+
    geom_text(data = exp_data_BReenRICE[exp_data_BReenRICE$year==2045,],aes(label=country))

SCC_noNK <- ggplot(exp_data_BReenRICE_nodam, aes(x=year,y=scc,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,2000))+
    xlim(c(2000,2050))+
    guides(color="none")+
    ggtitle("Standard RICE")+
    geom_text(data = exp_data_BReenRICE_nodam[exp_data_BReenRICE_nodam$year==2050,],aes(label=country))

ggarrange(SCC_noNK,SCC)
ggsave("SCC_NK_and_noNK.png",dpi=300)

CPC <- ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year<2051,], aes(x=year,y=CPC,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,100))+
    xlim(c(2000,2060))+
    guides(color="none")+
    ylab("Consumption per capita (Thousands 2005USD")+
    ggtitle("GreenRICE: market goods depend on natural capital")+
    geom_text(data = exp_data_BReenRICE[exp_data_BReenRICE$year==2055,],aes(label=country))

CPC_noNK <- ggplot(exp_data_BReenRICE_noNK[exp_data_BReenRICE_noNK$year<2051,], aes(x=year,y=CPC,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,100))+
    xlim(c(2000,2060))+
    guides(color="none")+
    ggtitle("Standard RICE")+
    ylab("Consumption per capita (Thousands 2005USD)")+
    geom_text(data = exp_data_BReenRICE_noNK[exp_data_BReenRICE_noNK$year==2055,],aes(label=country))


CPC_nodam <- ggplot(exp_data_BReenRICE_nodam[exp_data_BReenRICE_nodam$year<2051,], aes(x=year,y=CPC,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(0,100))+
    xlim(c(2000,2060))+
    guides(color="none")+
    ggtitle("Standard RICE")+
    ylab("Consumption per capita (Thousands 2005USD)")+
    geom_text(data = exp_data_BReenRICE_nodam[exp_data_BReenRICE_nodam$year==2055,],aes(label=country))


ggarrange(CPC_nodam,CPC)
ggsave("CPC_NK_and_noNK.png",dpi=300)
    


    glimpse(exp_data_BReenRICE)
    glimpse(exp_data_BReenRICE_noNK)
    glimpse(exp_data_BReenRICE_nodam)


    exp_data_BReenRICE$scc_change <- exp_data_BReenRICE$scc - exp_data_BReenRICE_nodam$scc
    exp_data_BReenRICE$CPC_change <- exp_data_BReenRICE$CPC - exp_data_BReenRICE_nodam$CPC
    exp_data_BReenRICE$NAT_KAP_DAM_change <- exp_data_BReenRICE$NAT_KAP_DAM - exp_data_BReenRICE_nodam$NAT_KAP_DAM
    windows()

    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year==2100,], aes(x=NAT_KAP_DAM_change,y=CPC_change,label=country))+
    geom_text()
    
    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year==2100,], aes(x=CPC_change,y=NAT_OMEGA,label=country))+
    geom_text()+
    xlim(0,0.01)

    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year<2100,], aes(x=year,y=scc_change,color=country))+
    geom_line()

    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year<2100,], aes(x=year,y=CPC_change,color=country))+
    geom_line()


    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year==2020 & exp_data_BReenRICE$type_prodshare=="nature",], aes(x=(NAT_KAP/K),y=NAT_OMEGA,color=scc_change,label=country,size=CPC))+
    geom_text()+
    theme_bw() +
    scale_color_BRadient2(high="red", low="green", midpoint=0,limits = c(-10, 10), oob = scales::squish)


    exp_data_BReenRICE <- exp_data_BReenRICE[exp_data_BReenRICE$type_prodshare=="nature",]
    exp_data_BReenRICE$yearcountry <- paste0(exp_data_BReenRICE$year,exp_data_BReenRICE$country)
    
    
    exp_data_BReenRICE_nodam <- exp_data_BReenRICE_nodam[exp_data_BReenRICE_nodam$type_prodshare=="nature",]
    exp_data_BReenRICE_nodam$yearcountry <- paste0(exp_data_BReenRICE_nodam$year,exp_data_BReenRICE_nodam$country)
    SCC_nodam <- data.frame(yearcountry=exp_data_BReenRICE_nodam$yearcountry,SCC_nodam=exp_data_BReenRICE_nodam$scc)
    exp_data_BReenRICE <- merge(exp_data_BReenRICE,SCC_noNK,by="yearcountry",all=TRUE)
    exp_data_BReenRICE$scc_change <- exp_data_BReenRICE$scc-exp_data_BReenRICE$SCC_noNK
    
    exp_data_BReenRICE_noNK$yearcountry <- paste0(exp_data_BReenRICE_noNK$year,exp_data_BReenRICE_noNK$country)
    SCC_noNK <- data.frame(yearcountry=exp_data_BReenRICE_noNK$yearcountry,SCC_noNK=exp_data_BReenRICE_noNK$scc)
    exp_data_BReenRICE <- merge(exp_data_BReenRICE,SCC_noNK,by="yearcountry",all=TRUE)
    exp_data_BReenRICE$scc_change <- exp_data_BReenRICE$scc-exp_data_BReenRICE$SCC_noNK
    
    CPC_noNK <- data.frame(yearcountry=exp_data_BReenRICE_noNK$yearcountry,CPC_noNK=exp_data_BReenRICE_noNK$CPC)
    exp_data_BReenRICE <- merge(exp_data_BReenRICE,CPC_noNK,by="yearcountry",all=TRUE)
    exp_data_BReenRICE$CPC_change <- exp_data_BReenRICE$CPC-exp_data_BReenRICE$CPC_noNK
    
    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year==2100,], aes(x=NAT_OMEGA,y=CPC_change,color=country))+
    geom_line()+
    theme_bw()+
    ylim(c(-5,10))+
    xlim(c(0.9,1.5))+
    geom_text(data = exp_data_BReenRICE[exp_data_BReenRICE$year==2050,],aes(label=country))+
    guides(color="none")+
    xlab("Natural capital change")+
    ylab("Consumption per capita change (Thousands 2005USD)")
    ggsave("CPCchange_noNK.png",dpi=300)


    ggplot(exp_data_BReenRICE[exp_data_BReenRICE$year==2020 & exp_data_BReenRICE$type_prodshare=="nature",], aes(x=(NAT_KAP/K),y=NAT_OMEGA,color=scc_change,label=country,size=CPC))+
    geom_text()+
    theme_bw() +
    scale_color_BRadient2(high="red", low="green", midpoint=0,limits = c(-10, 10), oob = scales::squish)
    #ylim(c(-2500,1000))
    #xlim(c(2020,2050))
    #geom_text(data = exp_data_BReenRICE[exp_data_BReenRICE$year==2045,],aes(label=country))



    #geom_label(exp_data_BReenRICE[exp_data_BReenRICE$year==2050,],
    #aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)
        


#arrange dataset (start)
    glimpse(exp_data_BR_NWB_g005)
  
    #exp_data_BR_NWB_g005 <- arrange(exp_data_BR_NWB_g005, country, year)

    


    gr_data <- rbind(exp_data_R,exp_data_BR_NWB_g005,
        exp_data_BR_NWB_gWB,exp_data_BR_NWB_gWB_dam10,
        exp_data_BR_NWB_gWB_dam10cum,exp_data_BR_NWB_gWB_damNolan,exp_data_R_sav,exp_data_BR_NWB_gWB_damNDVI)

    gr_data <- gr_data[gr_data$type_CAPITAL %in% c("nature") & gr_data$type_prodshare %in% c("nature") & gr_data$year < 2105 & gr_data$year > 2015,]
    gr_data$exp <- as.factor(gr_data$exp)
    gr_data <- arrange(gr_data, exp,country, year)
#arrange dataset (end)

#compute new variables (start)
    #Yearly cpc growth
    g <- gr_data %>% group_by(exp,country) %>% mutate(Growth = 100*(CPC - lag(CPC))/lag(CPC)) 
    gr_data$CPC_BRowth <- g$Growth
    #Mean yearly cpc growth 
    g <- gr_data %>% group_by(exp,country) %>% mutate(Growth_mean = mean(CPC_BRowth, na.rm=TRUE) )
    gr_data$CPC_BRowth_mean <- g$Growth_mean
    
    #Yearly CAPITAL Capital growth
    g <- gr_data %>% group_by(exp,country) %>% mutate(CAPITALGrowth = 100*(CAPITAL - lag(CAPITAL))/lag(CAPITAL)) 
    gr_data$CAPITAL_BRowth <- g$CAPITALGrowth
    #Mean yearly CAPITAL Capital growth 
    g <- gr_data %>% group_by(exp,country) %>% mutate(CAPITALGrowth_mean = mean(CAPITAL_BRowth, na.rm=TRUE) )
    gr_data$CAPITAL_BRowth_mean <- g$CAPITALGrowth_mean

    #Comparing to optimized RICE
    R_g <- gr_data[gr_data$exp == "R",which(names(gr_data)=="CPC_BRowth_mean")]
    gr_data$CPC_BRowth_mean_wrt_std <- gr_data$CPC_BRowth_mean - R_g
    R_g <- gr_data[gr_data$exp == "R",which(names(gr_data)=="CPC_BRowth")]
    gr_data$CPC_BRowth_wrt_std <- gr_data$CPC_BRowth - R_g
    R_g <- gr_data[gr_data$exp == "R",which(names(gr_data)=="CPC")]
    gr_data$CPC_wrt_std <- gr_data$CPC - R_g
    R_g <- gr_data[gr_data$exp == "R",which(names(gr_data)=="CPC")]
    gr_data$CPC_pc_wrt_std <- 100*(gr_data$CPC - R_g)/gr_data$CPC

    #Comparing to GR nwbgWB
    gr_nwbgWB_g <- gr_data[gr_data$exp == "GR_NWB_gWB",which(names(gr_data)=="CPC_BRowth_mean")]
    gr_data$CPC_BRowth_mean_wrt_nwbgWB <- gr_data$CPC_BRowth_mean - gr_nwbgWB_g
    gr_nwbgWB_g <- gr_data[gr_data$exp == "GR_NWB_gWB",which(names(gr_data)=="CPC_BRowth")]
    gr_data$CPC_BRowth_wrt_nwbgWB <- gr_data$CPC_BRowth - gr_nwbgWB_g
    gr_nwbgWB_g <- gr_data[gr_data$exp == "GR_NWB_gWB",which(names(gr_data)=="CAPITAL")]
    gr_data$NCchange <- gr_data$CAPITAL - gr_nwbgWB_g


    #Comparing to RICE NO POLICY (simulation)
    R_g <- gr_data[gr_data$exp == "R_sav",which(names(gr_data)=="CPC_BRowth_mean")]
    gr_data$CPC_BRowth_mean_wrt_std_sim <- gr_data$CPC_BRowth_mean - R_g
    R_g <- gr_data[gr_data$exp == "R_sav",which(names(gr_data)=="CPC_BRowth")]
    gr_data$CPC_BRowth_wrt_std_sim <- gr_data$CPC_BRowth - R_g


    gr_data$NKratio <- gr_data$CAPITAL / gr_data$K
#compute new variables (end)

#Non Climate Damages (start)

    #Flexible savings
        ggplot(data = gr_data[gr_data$exp %in% c("R","R_sav"),], 
        aes(y = CPC, x = year, col = country, lty = exp))+
        geom_line()+
        scale_colour_discrete(guide = 'none') +
        #geom_dl(data=gr_data[gr_data$exp=="R_sim",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
        theme_bw() +
        xlab("year") + 
        ylab("CPC (thousands 2005 USD per year)")+
        ggtitle("Per capita consumption (RICE; optimal vs no policy simulation)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020, 2120))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_CPC_optsim.png")
    #Flexible savings

    #NWB g005
        #CPC
        ggplot(data = gr_data[gr_data$exp %in% c("R","GR_NWB_g005"),], 
        aes(y = CPC, x = year, col = country, linetype = exp))+
        geom_line()+
        scale_linetype_manual(values=c("dotted","solid"))+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC (thousands 2005 USD per year)")+
        ggtitle("Per capita consumption. GreenRICE vs RICE \
        (params: N = World Bank; g3 = 0.05; no damages)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPC.png",dpi=600)

        #CPC growth change w.r.t. R
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
        aes(y = CPC_BRowth_wrt_std, x = year, col = country))+
        geom_line()+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC yearly growth change (pp)")+
        ggtitle("Per capita consumption. GreenRICE vs RICE \
        (params: N = World Bank; g3 = 0.05; no damages)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCchange.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
        aes(x = NKratio, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
        geom_point()+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("NK ratio") + 
        ylab("CPC growth w.r.t. std (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
        (params: N = World Bank; g3 = 0.005; no damages; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKCPCchangev1.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
        aes(x = prodshare, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
        geom_point()+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("Production elasticity") + 
        ylab("CPC growth w.r.t. std (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
        (params: N = World Bank; g3 = 0.005; no damages; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_prodshareCPCchangev1.png",dpi=600)

    #NWB g005

    #NWB gWB
        #CPC
        ggplot(data = gr_data[gr_data$exp %in% c("R","GR_NWB_gWB"),], 
        aes(y = CPC, x = year, col = country, linetype = exp))+
        geom_line()+
        scale_linetype_manual(values=c("dotted","solid"))+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC (thousands 2005 USD per year)")+
        ggtitle("Per capita consumption. GreenRICE vs RICE \
        (params: N = World Bank; g3 = WB; no damages)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPC.png",dpi=600)

        #CPC growth change w.r.t. R
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
        aes(y = CPC_BRowth_wrt_std, x = year, col = country))+
        geom_line()+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC yearly growth change (pp)")+
        ggtitle("Per capita consumption. GreenRICE vs RICE \
        (params: N = World Bank; g3 = WB; no damages)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCchange.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
        aes(x = NKratio, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
        geom_point()+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("NK ratio") + 
        ylab("CPC growth w.r.t. std (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
        (params: N = World Bank; g3 = WB; no damages; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKCPCchangev1.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
        aes(x = prodshare, y = CPC_BRowth_wrt_std, col = country))+
        geom_point(aes(alpha=year))+
        geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("Production elasticity") + 
        ylab("CPC growth w.r.t. std (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
        (params: N = World Bank; g3 = WB; no damages; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_prodshareCPCchangev1.png",dpi=600)

    #NWB gWB

    #NWB gWB dam10
        #CPC
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB","GR_NWB_gWB_dam10"),], 
        aes(y = CPC, x = year, col = country, linetype = exp))+
        geom_line()+
        scale_linetype_manual(values=c("dotted","solid"))+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC (thousands 2005 USD per year)")+
        ggtitle("Per capita consumption. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10_CPC.png",dpi=600)

        #CPC growth change w.r.t. R
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
        aes(y = CPC_BRowth_wrt_nwbgWB, x = year, col = country))+
        geom_line()+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC yearly growth change (pp)")+
        ggtitle("Per capita consumption. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10_CPCchange.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
        aes(x = NKratio, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
        geom_point()+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("NK ratio") + 
        ylab("CPC growth w.r.t. std GR (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10_NKCPCchangev1.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
        aes(x = prodshare, y = CPC_BRowth_wrt_nwbgWB, col = country))+
        geom_point(aes(alpha=year))+
        geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB_dam10",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("Production elasticity") + 
        ylab("CPC growth w.r.t. std GR (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10_prodshareCPCchangev1.png",dpi=600)

    #NWB gWB dam10

    #NWB gWB dam10 cumulative (start)
        #CPC
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB","GR_NWB_gWB_dam10cum"),], 
        aes(y = CPC, x = year, col = country, linetype = exp))+
        geom_line()+
        scale_linetype_manual(values=c("dotted","solid"))+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC (thousands 2005 USD per year)")+
        ggtitle("Per capita consumption. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC cumulative)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10cum_CPC.png",dpi=600)

        #CPC growth change w.r.t. R
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10cum"),], 
        aes(y = CPC_BRowth_wrt_nwbgWB, x = year, col = country))+
        geom_line()+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("year") + 
        ylab("CPC yearly growth change (pp)")+
        ggtitle("Per capita consumption. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC cumulative)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())  +
        xlim(c(2020,2100))
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10cum_CPCchange.png",dpi=600)

        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10cum"),], 
        aes(x = NKratio, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
        geom_point()+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("NK ratio") + 
        ylab("CPC growth w.r.t. std GR (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC cumulative; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10cum_NKCPCchangev1.png",dpi=600)

        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10cum"),], 
        aes(x = NCchange, y = CAPITAL, col = exp, alpha = year))+
        geom_point()+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("NC change") + 
        ylab("Natural Capital")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC cumulative; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)


        #CPC change w.r.t. R; function of NK ratio
        ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10cum"),], 
        aes(x = prodshare, y = CPC_BRowth_wrt_nwbgWB, col = country))+
        geom_point(aes(alpha=year))+
        geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB_dam10cum",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
        #scale_x_continuous(trans = 'log10')+
        scale_colour_discrete(guide = 'none') +
        theme_bw() +
        xlab("Production elasticity") + 
        ylab("CPC growth w.r.t. std GR (pp)")+
        ggtitle("CPC growth change as NK ratio changes. GreenRICE \
        (params: N = World Bank; g3 = WB; 10% dam NC cumulative; no policy)")+
        theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
            geom_hline(yintercept=0)
        ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_dam10cum_prodshareCPCchangev1.png",dpi=600)

    #NWB gWB dam10 cumulative (end)
    
    #NWB gWB damNolan
        ##First analysis 
            #Compared to NWB gWB
            #CPC
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB","GR_NWB_gWB_damNolan"),], 
            aes(y = CPC, x = year, col = country, linetype = exp))+
            geom_line()+
            scale_linetype_manual(values=c("dotted","solid"))+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("year") + 
            ylab("CPC (thousands 2005 USD per year)")+
            ggtitle("Per capita consumption. GreenRICE \
            (params: N = World Bank; g3 = WB; Nolan dam in NC)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())  +
            xlim(c(2020,2100))
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNolan_CPC.png",dpi=600)

            #CPC growth change w.r.t. R
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
            aes(y = CPC_BRowth_wrt_nwbgWB, x = year, col = country))+
            geom_line()+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("year") + 
            ylab("CPC yearly growth change (pp)")+
            ggtitle("Per capita consumption. GreenRICE \
            (params: N = World Bank; g3 = WB; Nolan dam in NC)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())  +
            xlim(c(2020,2100))
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNolan_CPCchange.png",dpi=600)

            #CPC change w.r.t. R; function of NK ratio
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
            aes(x = NKratio, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
            geom_point()+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("NK ratio") + 
            ylab("CPC growth w.r.t. std GR (pp)")+
            ggtitle("CPC growth change as NK ratio changes. GreenRICE \
            (params: N = World Bank; g3 = WB; Nolan dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()) +
                geom_hline(yintercept=0)
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNolan_NKCPCchangev1.png",dpi=600)

            #CPC change w.r.t. R; function of NK ratio
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
            aes(x = prodshare, y = CPC_BRowth_wrt_nwbgWB, col = country))+
            geom_point(aes(alpha=year))+
            geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB_damNolan",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("Production elasticity") + 
            ylab("CPC growth w.r.t. std GR (pp)")+
            ggtitle("CPC growth change as NK ratio changes. GreenRICE \
            (params: N = World Bank; g3 = WB; Nolan dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()) +
                geom_hline(yintercept=0)
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNolan_prodshareCPCchangev1.png",dpi=600)
        ##First analysis 
        #NEW GRAPH HEATMATP


            levelplot(CPC_BRowth_wrt_std ~ NKratio * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
                panel = panel.levelplot.points, cex = 1.2,
                main = "CPC growth change wrt std (pp)",
                ) + 
                layer_(panel.2dsmoother(..., n = 1000))

            

            levelplot(CPC_BRowth_wrt_nwbgWB ~ NKratio * prodshare, data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
                panel = panel.levelplot.points, cex = 1.2
                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


             
            #NEW GRAPH HEATMATP
        #NEW GRAPH HEATMATP
    #NWB gWB damNolan

    #NWB gWB damNDVI
        ##First analysis 
            #Compared to NWB gWB
            #CPC
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB","GR_NWB_gWB_damNDVI"),], 
            aes(y = CPC, x = year, col = country, linetype = exp))+
            geom_line()+
            scale_linetype_manual(values=c("dotted","solid"))+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("year") + 
            ylab("CPC (thousands 2005 USD per year)")+
            ggtitle("Per capita consumption. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())  +
            xlim(c(2020,2100))
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNDVI_CPC.png",dpi=600)

            #CPC growth change w.r.t. R
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
            aes(y = CPC_BRowth_wrt_nwbgWB, x = year, col = country))+
            geom_line()+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("year") + 
            ylab("CPC yearly growth change (pp)")+
            ggtitle("Per capita consumption. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())  +
            xlim(c(2020,2100))
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNDVI_CPCchange.png",dpi=600)

            #CPC change w.r.t. R; function of NK ratio
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
            aes(x = NKratio, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
            geom_point()+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("NK ratio") + 
            ylab("CPC growth w.r.t. std GR (pp)")+
            ggtitle("CPC growth change as NK ratio changes. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()) +
                geom_hline(yintercept=0)
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNDVI_NKCPCchangev1.png",dpi=600)

            #CPC change w.r.t. R; function of NK ratio
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
            aes(x = NKratio, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
            geom_point()+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("NK ratio") + 
            ylab("CPC growth w.r.t. std GR (pp)")+
            ggtitle("CPC growth change as NK ratio changes. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()) +
                geom_hline(yintercept=0)
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNDVI_NKCPCchangev1.png",dpi=600)

            #CPC change w.r.t. R; function of NK ratio
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
            aes(x = prodshare, y = CPC_BRowth_wrt_nwbgWB, col = country))+
            geom_point(aes(alpha=year))+
            geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB_damNDVI",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("Production elasticity") + 
            ylab("CPC growth w.r.t. std GR (pp)")+
            ggtitle("CPC growth change as NK ratio changes. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()) +
                geom_hline(yintercept=0)
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNDVI_prodshareCPCchangev1.png",dpi=600)
        
            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
            aes(y =CAPITAL_BRowth, x = year, col = country))+
            geom_line()+
            geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB_damNDVI",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("Year") + 
            ylab("Natural Capital growth w.r.t. std GR (pp)")+
            ggtitle("Natural Capital growth change. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())
            ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_damNDVI_prodshareCPCchangev1.png",dpi=600)


            ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
            aes(x =CAPITAL_BRowth_mean, y = CPC_BRowth_mean_wrt_nwbgWB, col = country))+
            #geom_point()+
            geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB_damNDVI",],aes(label = country), method = list(dl.combine("first.points")), cex = 0.9)+
            #scale_x_continuous(trans = 'log10')+
            scale_colour_discrete(guide = 'none') +
            theme_bw() +
            xlab("Natural Capital growth rate w.r.t. GR") + 
            ylab("CPC growth rate w.r.t. GR")+
            ggtitle("Natural Capital growth change. GreenRICE \
            (params: N = World Bank; g3 = WB; NDVI dam in NC; no policy)")+
            theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())
            
        ##First analysis 
        #NEW GRAPH HEATMATP

            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB"),]$CPC_BRowth_wrt_std), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_std ~ NKratio * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth change w.r.t. RICE (pp)",
                xlab = "Natural Capital / Manufacture Capital",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))



            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),]$CPC_BRowth_wrt_std), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_std ~ NKratio * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt R (pp)",
                xlab = "Natural Capital / Manufacture Capital",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))
           
            
            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),]$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),]$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))
            

            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),]$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Average Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * NKratio, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Natural Capital yearly growth rate (pp) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


             
            #NEW GRAPH HEATMATP
        #NEW GRAPH HEATMATP

        #NEW GRAPH HEATMATP

            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB"),]$CPC_BRowth_wrt_std), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            install.packages('deldir')
            library('deldir')
            data= gr_data[gr_data$exp %in% c("GR_NWB_gWB"),]
            panel.voronoi(data$NKratio, data$prodshare, data$CPC_BRowth_wrt_std, 
            subscripts = TRUE, at = pretty(data$CPC_BRowth_wrt_std),
            points = TRUE, border = "transparent",
            na.rm = FALSE, win.expand = 0.07, use.tripack = FALSE,
            col.regions = regions$col, alpha.regions = regions$alpha)
            
            
            data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI", "GR_NWB_gWB_damNolan"),]
            max_abs <- max(abs(data$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= data, 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth (change w.r.t. GreenRICE no damages, in pp) \n damages: PALEO (triangle) and NDVI (diamond)",
                xlab = "Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital",
                pch = data$exp

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))



            
            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),]$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),]$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))
            

            max_abs <- max(abs(gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),]$CPC_BRowth_wrt_nwbgWB), na.rm = T)
            brk <- do.breaks(c(-max_abs, max_abs), 100)
            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * prodshare, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNolan"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Average Natural Capital yearly growth (%) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


            levelplot(CPC_BRowth_wrt_nwbgWB ~ CAPITAL_BRowth_mean * NKratio, 
                data= gr_data[gr_data$exp %in% c("GR_NWB_gWB_damNDVI"),], 
                panel = panel.levelplot.points, cex = 1.2,
                at = brk,
                main = "CPC yearly growth rate change wrt GR no dam (pp) \n NDVI damage function",
                xlab = "Natural Capital yearly growth rate (pp) \n due to climate damages",
                ylab = "Production elasticity to natural capital"

                ) + 
                layer_(panel.2dsmoother(..., n = 1000))


             
            #NEW GRAPH HEATMATP
        #NEW GRAPH HEATMATP
    #NWB gWB damNDVI
#No Climate Damages (end)



# Optimized


    #Standard RICE Optimal

    ggplot(data = gr_data[gr_data$exp=="R",], aes(y = CPC, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    geom_dl(data=gr_data[gr_data$exp=="R",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption (RICE) \
        No policy, No climate damages")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020, 2120))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_CPC.png")




    ggplot(data = gr_data[gr_data$exp %in% c("R","R_sav"),], 
    aes(y = CPC, x = year, col = country, lty = exp))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    #geom_dl(data=gr_data[gr_data$exp=="R_sim",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption (RICE; optimal vs no policy simulation)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020, 2120))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_CPC_optsim.png")



    # NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("R","GR_NWB_g005"),], 
    aes(y = CPC, x = year, col = country, linetype = exp))+
    geom_line()+
    scale_linetype_manual(values=c("dotted","solid"))+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPC.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("R","GR_NWB_gWB"),], 
    aes(y = CPC, x = year, col = country, linetype = exp))+
    geom_line()+
    scale_linetype_manual(values=c("dotted","solid"))+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPC.png",dpi=600)

   
    plot1 <- ggplot(data = gr_data[gr_data$exp %in% c("R"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("RICE50x")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    plot2 <- ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("GreenRICE  \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    ggarrange(plot1, plot2)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth.png",dpi=600)

   

    plot1 <- ggplot(data = gr_data[gr_data$exp %in% c("R"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("RICE50x")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    plot2 <- ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    ggarrange(plot1, plot2)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCgrowth.png",dpi=600)

    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
    aes(y = CPC_BRowth_wrt_std, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth change (pp)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCchange.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = CPC_BRowth_wrt_std, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth change (pp)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCchange.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = CPC_BRowth_mean_wrt_std, x = NKratio, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC mean growth change")+
    ggtitle("GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  
    #xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCmean.png",dpi=600)

     ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
    aes(y = CPC_BRowth_wrt_std, x = NKratio, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC mean growth change")+
    ggtitle("GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  
    #xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCmean.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%) change")+
    ggtitle("Per capita consumption. GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_CPCgrowth.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = NKratio, x = year, col = country))+
    geom_line()+
    #scale_y_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    geom_dl(data=gr_data[gr_data$exp=="R",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    theme_bw() +
    xlab("year") + 
    ylab("N / K ratio")+
    ggtitle("Natural capital growth. GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKchange.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
    aes(x = NKratio, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC growth w.r.t. std (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.005; no damages; no policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKCPCchangev1.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(x = NKratio, y = CPC, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC")+
    ggtitle("CPC  GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_NKCPC.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("Prod elasticity") + 
    ylab("CPC growth change w.r.t. std (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages; No policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_prodshareCPCchange.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("Prod elasticity") + 
    ylab("CPC growth change w.r.t. std (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_prodshareCPCchange.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_std, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    geom_dl(data=gr_data[gr_data$exp=="GR_NWB_gWB",],
        aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("Prod elasticity") + 
    ylab("CPC growth change w.r.t. std (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)+
        xlim(c(0,0.005))+ylim(c(-0.1,0.05))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_prodshareCPCchangev2.png",dpi=600)


    
# Plots Rice vs GreenRICE (params: N = World Bank; g3 = WB; no damages; BAU)

# Plots Rice vs GreenRICE (params: N = World Bank; g3 = WB; 10% DAM; BAU)
    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10","GR_NWB_gWB"),], 
    aes(y = CPC, x = year, col = country, linetype = exp))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption. GreenRICE \
    (params: N = World Bank; g3 = WB; no damages & 10% dam)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_CPC.png",dpi=600)

   
   
    plot1 <- ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("GreenRICE \
    (params: N = World Bank; g3 = WB; 10% dam)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    plot2 <- ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("GreenRICE  \
    (params: N = World Bank; g3 = WB; 10% dam)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    ggarrange(plot1, plot2)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_CPCgrowth.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(y = CPC_BRowth_wrt_nwbgWB, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth change (pp)")+
    ggtitle("Per capita consumption GreenRICE. Effect of 10% dam  \
    (params: N = World Bank; g3 = WB)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_CPCchange.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB"),], 
    aes(y = CPC_BRowth_mean_wrt_std, x = NKratio, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC mean growth change")+
    ggtitle("GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  
    #xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCmean.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth")+
    ggtitle("Per capita consumption. GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_CPCgrowth.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in WB for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(y = NKratio, x = year, col = country))+
    geom_line()+
    #scale_y_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    geom_dl(data=gr_data[gr_data$exp=="R",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    theme_bw() +
    xlab("year") + 
    ylab("N / K ratio")+
    ggtitle("Natural capital growth. GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKchange.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(x = NKratio, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC_BRowth_wrt_std")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_NKCPCchangev1.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(x = NKratio, y = CPC, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC")+
    ggtitle("CPC  GreenRICE  \
    (params: N = World Bank; g3 = WB; no damages; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_NKCPC.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("Prod elasticity") + 
    ylab("CPC groeth change (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE 10%DAM \
    (params: N = World Bank; g3 = WB; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_prodshareCPCchange.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_dam10"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_nwbgWB, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("Prod elasticity") + 
    ylab("CPC_BRowth_wrt_std")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages; BAU)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)+
        xlim(c(0,0.003))+ylim(c(-0.0005,0.0001))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbgWB_dam10_prodshareCPCchangev2.png",dpi=600)


    
# Plots Rice vs GreenRICE (params: N = World Bank; g3 = WB; no damages; BAU)

# Plots Rice vs GreenRICE (params: N = World Bank; g3 = 0.05; no damages; no policy)
    # SIM NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("R_sim","GR_NWB_g005_sim"),], 
    aes(y = CPC, x = year, col = country, linetype = exp))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPC_sim.png",dpi=600)

   
   
    plot1 <- ggplot(data = gr_data[gr_data$exp %in% c("R_sim"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("RICE50x")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    plot2 <- ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%)")+
    ggtitle("GreenRICE  \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))

    ggarrange(plot1, plot2)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_sim.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(y = CPC_BRowth_wrt_std_sim, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth change (pp)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCchange_sim.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(y = CPC_BRowth_mean_wrt_std_sim, x = NKratio, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC mean growth change")+
    ggtitle("GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  
    #xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCmean_sim.png",dpi=600)


     ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(y = CPC_BRowth, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%) change")+
    ggtitle("Per capita consumption. GreenRICE  \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbg005_CPCgrowth_sim.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(y = NKratio, x = year, col = country))+
    geom_line()+
    #scale_y_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    geom_dl(data=gr_data[gr_data$exp=="R_sim",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    theme_bw() +
    xlab("year") + 
    ylab("N / K ratio")+
    ggtitle("Natural capital growth. GreenRICE  \
    (params: N = World Bank; g3 = 0.05; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_NKchange_sim.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(x = NKratio, y = CPC_BRowth_wrt_std_sim, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC growth change (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages; no policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_NKCPCchange_simv1.png",dpi=600)

    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(x = NKratio, y = CPC, col = country, alpha = year))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC")+
    ggtitle("CPC  GreenRICE  \
    (params: N = World Bank; g3 = 0.05; no damages; no policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbg005_NKCPC_sim.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_std_sim, col = country))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("Prod elasticity") + 
    ylab("CPC_BRowth_wrt_std_sim")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages; no policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_prodshareCPCchange_sim.png",dpi=600)
# Plots Rice vs GreenRICE (params: N = World Bank; g3 = 0.05; no damages; no policy)


# Plots Rice vs GreenRICE (params: N = World Bank; g3 = WB; no damages; no policy)
    # SIM NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("R_sim","GR_NWB_gWB_sim"),], 
    aes(y = CPC, x = year, col = country, linetype = exp))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC (thousands 2005 USD per year)")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_CPC_sim.png",dpi=600)

    # SIM NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_sim"),], 
    aes(y = CPC_BRowth_wrt_std_sim, x = year, col = country))+
    geom_line()+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("year") + 
    ylab("CPC yearly growth (%) change")+
    ggtitle("Per capita consumption. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_CPCchange_sim.png",dpi=600)


    # SIM NatCap as given by WB and elasticity fixed in 0.05 for all countries
    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_sim"),], 
    aes(y = NKratio, x = year, col = country))+
    geom_line()+
    scale_y_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    geom_dl(data=gr_data[gr_data$exp=="R_sim",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
    theme_bw() +
    xlab("year") + 
    ylab("N / K ratio")+
    ggtitle("Natural capital growth. GreenRICE vs RICE \
    (params: N = World Bank; g3 = WB; no damages)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
    xlim(c(2020,2100))
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKchange_sim.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_sim"),], 
    aes(x = NKratio, y = CPC_BRowth_wrt_std_sim, alpha = year,col = country))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("NK ratio") + 
    ylab("CPC growth change w.r.t. RICE (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages; no policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgWB_NKCPCchange_sim.png",dpi=600)


    ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_gWB_sim"),], 
    aes(x = prodshare, y = CPC_BRowth_wrt_std_sim, alpha = year, col = country))+
    geom_point()+
    #scale_x_continuous(trans = 'log10')+
    scale_colour_discrete(guide = 'none') +
    theme_bw() +
    xlab("production elasticity") + 
    ylab("CPC growth change w.r.t. RICE (pp)")+
    ggtitle("CPC growth change as NK ratio changes. GreenRICE vs RICE \
    (params: N = World Bank; g3 = 0.05; no damages; no policy)")+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
        geom_hline(yintercept=0)
    ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_prodshareCPCchange_sim.png",dpi=600)
# Plots Rice vs GreenRICE (params: N = World Bank; g3 = WB; no damages; no policy)



ggplot(data = gr_data[gr_data$exp %in% c("R_sim","GR_NWB_gWB_sim"),], 
aes(y = CPC, x = year, col = country, linetype = exp))+
geom_line()+
scale_colour_discrete(guide = 'none') +
theme_bw() +
xlab("year") + 
ylab("CPC (thousands 2005 USD per year)")+
ggtitle("Per capita consumption. GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")+
 theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())  +
xlim(c(2020,2100))


ggplot(data = gr_data[gr_data$exp=="R" & gr_data$year==2015,], aes(y = CPC_BRowth_mean*100, x = CAPITAL, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate  RICE (%)")+
ggtitle("CPC growth rate RICE ")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_CPCgrowth.png",dpi=600)

ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005",], aes(y = CPC_BRowth_mean_wrt_std*100, x = CAPITAL, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth.png",dpi=600)

gr_data$NKratio <- gr_data$CAPITAL / gr_data$K
ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005" & gr_data$year==2015,], aes(y = CPC_BRowth_mean_wrt_std*100, x = NKratio, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital in 2015 (fraction of K)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_NKratio.png",dpi=600)


 ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005",], 
 aes(y = CPC_BRowth_mean_wrt_std*100, x = prodshare, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Production elasticity to natural capital") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_prodshare.png",dpi=600)



ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005_dam10",], 
aes(y = CPC_BRowth_mean_wrt_nwbg005*100, x = CAPITAL, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate change w.r.t. GR NWB_g005 (pp)")+
ggtitle("CPC growth rate loss GreenRICE dam vs nodam \
 (params: N = World Bank; g3 = 0.05; no damages & 10% of damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_damnodam.png",dpi=600)


ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005_dam10"& gr_data$year==2015,], 
aes(y = CPC_BRowth_mean_wrt_nwbg005*100, x = NKratio, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (fraction of K)") + 
ylab("CPC growth rate change w.r.t. GR NWB_g005 (pp)")+
ggtitle("CPC growth rate loss GreenRICE dam vs nodam \
 (params: N = World Bank; g3 = 0.05; no damages & 10% of damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbg005_CPCgrowth_NKratio_damnodam.png",dpi=600)


ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005","GR_NWB_gWB"),], 
aes(y = CPC_BRowth_mean_wrt_std*100, x = CAPITAL, label = country, col = exp))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05 & WB; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth.png",dpi=600)


 ggplot(data = gr_data[gr_data$exp%in% c("GR_NWB_g005","GR_NWB_gWB"),], 
 aes(y = CPC_BRowth_mean_wrt_std*100, x = prodshare, label = country,col = exp))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Production elasticity to natural capital") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05 & WB; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_prodshare.png",dpi=600)



ggplot(data = gr_data[gr_data$exp=="GR_NWB_gWB" & gr_data$year==2015,], 
aes(y = prodshare, x = NKratio, label = country, col =CPC_BRowth_mean_wrt_std*100 ))+
geom_point()+
#geom_text()+
#scale_colour_BRadient2(low = "red", mid = "green", high = "blue", midpoint = -0.05, breaks = c(-0.05,-0.01,0)) +
scale_colour_BRadient(name = 'CPC growth rate change w.r.t. RICE (pp)')+
theme_bw() +
xlab("Natural capital in 2015 (fraction of K)") + 
ylab("Production Elasticity to NC")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = World Bank; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_NKratio_prodshare.png",dpi=600)


ggplot(data = gr_data[gr_data$exp%in% c("GR_NWB_gWB"),], 
 aes(y = CPC_BRowth_mean_wrt_std*100, x = prodshare, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Production elasticity to natural capital") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 =  WB; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_prodshare.png",dpi=600)









########################## SIMULATION (NO POLICY)
exp_names <- c("R_sim","GR_NWB_g005_sim","GR_NWB_gWB_sim","GR_NWB_gWB_dam10_sim")
var_names <- c("OMEGA_NATURE","CAPITAL","CPC","prodshare","K")
#To convert gdx to xls: in a comand window type: gdx2xls results_R_sim.gdx

for (i in 1:4){
    for (j in 1:5){
        var_table <- read_excel(paste('RICE50x-clean/nonclimdam/results_',exp_names[i],'.xlsx',sep=''), sheet = var_names[j])
        glimpse(var_table)
        var_table <- var_table[3:(dim(var_table)[1]),]
        var_table <- as.data.frame(var_table)
        if (dim(var_table)[2]<4){
            names(var_table) <- c(paste("type",var_names[j],sep='_'),"country",var_names[j])
            var_table$country <- as.factor(var_table$country)
            var_table[,which(names(var_table)==var_names[j])] <- as.double(unlist(var_table[,which(names(var_table)==var_names[j])]))
            var_table$exp <- exp_names[i] 
            var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))] <- as.factor(var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))])

        } else {
            if (dim(var_table)[2]<7){
                names(var_table) <- c("year", "country",paste(var_names[j],"low",sep='_'),var_names[j],paste(var_names[j],"high",sep='_'),paste(var_names[j],"marginal",sep='_'))
            } else {
                names(var_table) <- c(paste("type",var_names[j],sep='_'),"year", "country",paste(var_names[j],"low",sep='_'),var_names[j],paste(var_names[j],"high",sep='_'),paste(var_names[j],"marginal",sep='_'))
                var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))] <- as.factor(var_table[,which(names(var_table)==paste("type",var_names[j],sep='_'))])
            }
            var_table$year <- 1980 + (as.integer(var_table$year)-1)*5
            var_table$country <- as.factor(var_table$country)
            var_table[,which(names(var_table)==var_names[j])] <- as.double(unlist(var_table[,which(names(var_table)==var_names[j])]))
            var_table$exp <- exp_names[i]
            var_table$id <- paste(var_table$country,var_table$year,var_table$exp, sep="")
        }
            
        if (j==1) {
            assign(paste("exp_data",exp_names[i],sep="_"), var_table)
        } else {
            
            assign(paste("exp_data",exp_names[i],sep="_"), merge(eval(parse(text = paste("exp_data",exp_names[i],sep="_"))),var_table,all=FALSE)) 
            
            }
    }
    
}
glimpse(exp_data_BR_NWB_g005_sim)
#exp_data_BR_NWB_g005 <- arrange(exp_data_BR_NWB_g005, country, year)

gr_data <- rbind(exp_data_R_sim,exp_data_BR_NWB_g005_sim,
    exp_data_BR_NWB_gWB_sim,exp_data_BR_NWB_gWB_dam10_sim)

gr_data <- gr_data[gr_data$type_CAPITAL %in% c("nature") & gr_data$type_prodshare %in% c("nature") & gr_data$year < 2105,]
gr_data$exp <- as.factor(gr_data$exp)
gr_data <- arrange(gr_data, exp,country, year)
glimpse(gr_data)



g <- gr_data %>% group_by(exp,country) %>% mutate(Growth = (CPC - lag(CPC))/lag(CPC)) 
glimpse(g)
gr_data$CPC_BRowth <- g$Growth

g <- gr_data %>% group_by(exp,country) %>% mutate(Growth_mean = mean(CPC_BRowth, na.rm=TRUE) )
gr_data$CPC_BRowth_mean <- g$Growth_mean

#Comparing to RICE
R_g <- gr_data[gr_data$exp == "R_sim",which(names(gr_data)=="CPC_BRowth_mean")]
R_g <- c(R_g)
gr_data$CPC_BRowth_mean_wrt_std <- gr_data$CPC_BRowth_mean - R_g

R_g <- gr_data[gr_data$exp == "R_sim",which(names(gr_data)=="CPC_BRowth")]
R_g <- c(R_g)
gr_data$CPC_BRowth_wrt_std <- gr_data$CPC_BRowth_mean - R_g


#Comparing to GR nwbg005
gr_nwbgwb_g <- gr_data[gr_data$exp == "GR_NWB_gWB_sim",which(names(gr_data)=="CPC_BRowth_mean")]
gr_data$CPC_BRowth_mean_wrt_nwbgwb <- gr_data$CPC_BRowth_mean - gr_nwbgwb_g





# Standard RICE

ggplot(data = gr_data[gr_data$exp=="R_sim",], aes(y = CPC, x = year, col = country))+
geom_line()+
scale_colour_discrete(guide = 'none') +
geom_dl(data=gr_data[gr_data$exp=="R_sim",],aes(label = country), method = list(dl.combine("last.points")), cex = 0.9)+
theme_bw() +
xlab("year") + 
ylab("CPC (thousands 2005 USD per year)")+
ggtitle("Per capita consumption (RICE)")+
 theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())  +
xlim(c(1980, 2120))
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_CPC_sim.png")


# NatCap as given by WB and elasticity fixed in 0.05 for all countries


ggplot(data = gr_data[gr_data$exp %in% c("R_sim","GR_NWB_g005_sim"),], 
aes(y = CPC, x = year, col = country, linetype = exp))+
geom_line()+
scale_colour_discrete(guide = 'none') +
theme_bw() +
xlab("year") + 
ylab("CPC (thousands 2005 USD per year)")+
ggtitle("Per capita consumption. GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")+
 theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())  +
xlim(c(2020,2100))
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPC_sim.png",dpi=600)


ggplot(data = gr_data[gr_data$exp=="R_sim" & gr_data$year==2015,], aes(y = CPC_BRowth_mean*100, x = CAPITAL, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate  RICE (%)")+
ggtitle("CPC growth rate RICE ")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_CPCgrowth_sim.png",dpi=600)

ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005_sim",], aes(y = CPC_BRowth_mean_wrt_std*100, x = CAPITAL, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_sim.png",dpi=600)

gr_data$NKratio <- gr_data$CAPITAL / gr_data$K
ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005_sim" & gr_data$year==2015,], aes(y = CPC_BRowth_mean_wrt_std*100, x = NKratio, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital in 2015 (fraction of K)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_NKratio_sim.png",dpi=600)


ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005_sim" & gr_data$year==2100,], 
aes(y = CPC_BRowth_wrt_std*100, x = NKratio, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital in 2015 (fraction of K)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_NKratio_sim.png",dpi=600)


 ggplot(data = gr_data[gr_data$exp=="GR_NWB_g005_sim",], 
 aes(y = CPC_BRowth_mean_wrt_std*100, x = prodshare, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Production elasticity to natural capital") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_CPCgrowth_prodshare_sim.png",dpi=600)



ggplot(data = gr_data[gr_data$exp=="GR_NWB_gWB_dam10_sim",], 
aes(y = CPC_BRowth_mean_wrt_nwbgwb*100, x = CAPITAL, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate change w.r.t. GR NWB_gWB (pp)")+
ggtitle("CPC growth rate loss GreenRICE dam vs nodam \
 (params: N = World Bank; g3 = WB; no damages & 10% of damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbgwb_CPCgrowth_damnodam_sim.png",dpi=600)


ggplot(data = gr_data[gr_data$exp=="GR_NWB_gWB_dam10_sim"& gr_data$year==2015,], 
aes(y = CPC_BRowth_mean_wrt_nwbgwb*100, x = NKratio, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (fraction of K)") + 
ylab("CPC growth rate change w.r.t. GR NWB_gWB (pp)")+
ggtitle("CPC growth rate loss GreenRICE dam vs nodam \
 (params: N = World Bank; g3 = WB; no damages & 10% of damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/GR_nwbg005_CPCgrowth_NKratio_damnodam.png",dpi=600)


ggplot(data = gr_data[gr_data$exp %in% c("GR_NWB_g005_sim","GR_NWB_gWB_sim"),], 
aes(y = CPC_BRowth_mean_wrt_std*100, x = CAPITAL, label = country, col = exp))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Natural capital (trill USD)") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05 & WB; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_sim.png",dpi=600)


 ggplot(data = gr_data[gr_data$exp%in% c("GR_NWB_g005_sim","GR_NWB_gWB_sim"),], 
 aes(y = CPC_BRowth_mean_wrt_std*100, x = prodshare, label = country,col = exp))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Production elasticity to natural capital") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = 0.05 & WB; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_prodshare_sim.png",dpi=600)



ggplot(data = gr_data[gr_data$exp=="GR_NWB_gWB_sim" & gr_data$year==2015,], 
aes(y = prodshare, x = NKratio, label = country, col =CPC_BRowth_mean_wrt_std*100 ))+
geom_point()+
#geom_text()+
#scale_colour_BRadient2(low = "red", mid = "green", high = "blue", midpoint = -0.05, breaks = c(-0.05,-0.01,0)) +
scale_colour_BRadient(name = 'CPC growth rate change w.r.t. RICE (pp)')+
theme_bw() +
xlab("Natural capital in 2015 (fraction of K)") + 
ylab("Production Elasticity to NC")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 = World Bank; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_NKratio_prodshare_sim.png",dpi=600)


ggplot(data = gr_data[gr_data$exp%in% c("GR_NWB_gWB_sim"),], 
 aes(y = CPC_BRowth_mean_wrt_std*100, x = prodshare, label = country))+
#geom_point()+
geom_text()+
theme_bw() +
xlab("Production elasticity to natural capital") + 
ylab("CPC growth rate change w.r.t. RICE (pp)")+
ggtitle("CPC growth rate loss GreenRICE vs RICE \
 (params: N = World Bank; g3 =  WB; no damages)")
ggsave("C:/Users/bastien/Documents/GitHub/RICE50x-clean/nonclimdam/Figures/R_nwbg005_nwbgwb_CPCgrowth_prodshare_sim.png",dpi=600)




