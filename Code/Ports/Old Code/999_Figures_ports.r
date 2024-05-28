x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","rnaturalearth","scales")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

port_ssp<- read.csv("Data/modules/ports/ports_ssps.csv")
glimpse(port_ssp)

## Figure 1

pr_type <- ggplot(port_ssp %>% filter(scenario=="RCP85"))+
#geom_text(aes(x=GDP_ppp_2022,y=risk_base_perc,col=type,label=iso3)) + 
geom_point(aes(x=GDP_ppp_2022,y=risk_base_perc,col=type,shape=type)) + 
scale_x_log10()+
#scale_y_log10()+
theme_bw()+
xlab("GDP in 2022")+
ylab("Economic Value at Risk \n(% of GDP) - Log Scale")+
ggtitle("Present Risk by Type") +
theme(legend.position="bottom") +
scale_color_scico_d(end=0.8)

pr_type
library(ggrepel)

pr_total <- ggplot(port_ssp %>% group_by(iso3) %>% summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_base_perc)) %>% slice(1))+
geom_point(aes(x=GDP_ppp_2022,y=rb)) + 
geom_text_repel(aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
scale_x_log10()+
theme_bw()+
xlab("GDP in 2022 (2015 Bill USD)")+
#ylim(c(0,30))+
ylab("Economic Value at Risk (% of GDP)")+
ggtitle("Total Present Risk")
pr_total

ggarrange(pr_type,pr_total,ncol=2,align="h",widths=c(3,4))

# Summarize data to get total risk_perc by type
summarized_data <- port_ssp %>%
                   filter(scenario == "RCP85") %>%
                   group_by(type) %>%
                   summarize(total_risk_perc = sum(risk_base_perc,na.rm=TRUE))

# Create a pie chart
pr_type <- ggplot(summarized_data, aes(x = "", y = total_risk_perc, fill = type)) +
           geom_bar(stat = "identity", width = 1) +
           coord_polar(theta = "y") +
           scale_fill_scico_d(end = 0.8) +
           theme_void() + labs(fill=" ")

library(cowplot)
# Assuming pr_type and pr_total are your ggplot objects
final_plot <- ggdraw() +
              draw_plot(pr_total) +
              draw_plot(pr_type, x = 0.56, y = 0.55, width = 0.4, height = 0.4) # Adjust x, y, width, height as needed
print(final_plot)
# or to save the plot
ggsave("Figures/SM/ports/present_risk.png", final_plot, width = 10, height = 8) # adjust size as needed



#ggsave("Figures/SM/ports/present_risk.png",dpi=600)


    glimpse(port_ssp)
    ggplot(port_ssp %>% filter(iso3 != "GUY")) +
    #ggplot(port_ssp ) +
    geom_boxplot(aes(x= scenario , y=risk_perc-risk_base_perc, color=type)) +
    scale_y_log10(breaks = c(0.01,0.1, 1, 10)) +
    ylab("Value Loss in 2050\n(as % of country's GDP in 2050)") +
    xlab("") +
    scale_color_scico_d(end=0.8)+
    theme_bw()

    
    glimpse(port_ssp)
    pr_total2 <- ggplot(port_ssp %>% 
        filter(iso3 != "GUY") %>% 
        group_by(iso3,scenario) %>% 
        summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_perc-risk_base_perc)))+
    geom_point(aes(x=GDP_ppp_2022,y=rb,color=scenario)) + 
    #geom_text_repel(aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
    scale_x_log10()+
    theme_bw()+
    xlab("GDP in 2022 (2015 Bill USD)")+
    #ylim(c(0,40))+
    geom_abline(aes(intercept=0,slope=0),linetype="dashed")+
    ylab("Change in Economic Value \nat Risk in 2050 (pp)")+
    labs(col=" ") + 
    scale_color_scico_d(palette="berlin",begin=0.2,end=0.8)+
    theme(legend.position="bottom") 
    pr_total2


## Figure 2
    

    boxplot1 <- ggplot(port_ssp %>% filter(iso3 != "GUY")) +
    geom_boxplot(aes(x= scenario , y=risk_perc, color=type)) +
    scale_y_log10(breaks = c(0.01,0.1, 1, 10)) +
    ylab("Value Loss in 2050\n(as % of country's GDP in 2050)") +
    xlab("") +
    scale_color_scico_d(end=0.8)+
    theme_bw()+
    theme(legend.position="bottom") 

    ggarrange(boxplot1,pr_total2)
    ggsave("Figures/SM/ports/Future_Risk2.png",dpi=600)


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
