x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","rnaturalearth","scales")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

port_ssp<- read.csv("Data/modules/ports/ports_ssps.csv")
glimpse(port_ssp)

## Figure 1

pr_type <- ggplot(port_ssp %>% filter(scenario=="RCP26"))+
#geom_text(aes(x=GDP_ppp_2022,y=risk_base_perc,col=type,label=iso3)) + 
geom_point(aes(x=GDP_ppp_2022,y=risk_base_perc,col=type,shape=type)) + 
scale_x_log10()+
scale_y_log10()+
theme_bw()+
xlab("GDP in 2022")+
ylab("Economic Value at Risk \n(% of GDP) - Log Scale")+
ggtitle("Present Risk by Type") +
theme(legend.position="bottom") +
scale_color_scico_d(end=0.8)

pr_type

pr_total <- ggplot(port_ssp %>% group_by(iso3) %>% summarise(GDP_ppp_2022=GDP_ppp_2022,rb=sum(risk_base_perc)))+
geom_text(aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
scale_x_log10()+
theme_bw()+
xlab("GDP in 2022")+
ylim(c(0,30))+
ylab("Economic Value at Risk (% of GDP)")+
ggtitle("Total Present Risk")

ggarrange(pr_type,pr_total,ncol=2,align="h",widths=c(3,4))
#ggsave("Figures/SM/ports/PresentRisk.png",dpi=600)

## Figure 2
    

    ggplot(port_ssp %>% filter(iso3 != "GUY")) +
    geom_boxplot(aes(x= scenario , y=risk_perc, color=type)) +
    scale_y_log10(breaks = c(0.01,0.1, 1, 10)) +
    ylab("Value Loss in 2050\n(as % of country's GDP in 2050)") +
    xlab("") +
    scale_color_scico_d(end=0.8)+
    theme_bw()

    ggsave("Figures/SM/ports/Future_Risk.png",dpi=600)


# Figure 3


    load(file="Data/Modules/Ports/ports_tcoeff.Rds")
    write.csv(ports_tcoeff,file="Data/intermediate_output/ports_tcoeff.csv")


    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, ports_tcoeff, by = c("iso_a3" = "iso3"))

    # Plot
    ggplot(data = merged_data) +
    geom_sf(aes(fill = tcoeff)) +
    scale_fill_scico(palette = "lajolla", limits = c(0, 1), oob=squish) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Ports Damage\n(%GDP/Degree C)")
    ggsave("Figures/SM/ports/Map_Ports_Coef.png",dpi=600)
