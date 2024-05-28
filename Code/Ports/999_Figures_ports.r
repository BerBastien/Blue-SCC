x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","rnaturalearth","scales")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

port_ssp<- read.csv("Data/modules/ports/ports_ssps_rcps.csv")
glimpse(port_ssp)
port_ssp %>% filter(iso3=="VEN")

  density_RCP_ssp <- ggplot(data=port_ssp %>%
    group_by(iso3, RCP, SSP) %>% 
    summarise(risk_change_percGDP = sum(risk_change_percGDP,na.rm=TRUE),GDP_ppp_2050=first(GDP_ppp_2050)),
    aes(x = log(GDP_ppp_2050), y = risk_change_percGDP)) +
  #geom_point(alpha=0.1) +
  #stat_density_2d(aes(alpha = ..level..,fill=SSP), geom = "polygon", bins = 4) +
  #scale_alpha(range = c(0.1,0.3), guide = FALSE) +  # Adjust alpha range as needed
  geom_hline(aes(yintercept=0),linetype="dashed")+
  geom_density_2d_filled(contour_var = "ndensity",aes(alpha=..level..,fill=RCP),bins=4,size=1)+
  geom_point(data=port_ssp %>%
    group_by(iso3, RCP, SSP) %>% 
    summarise(risk_change_percGDP = sum(risk_change_percGDP,na.rm=TRUE),GDP_ppp_2050=first(GDP_ppp_2050))%>%
                ungroup() %>%
                group_by(RCP, SSP) %>% 
    summarise(risk_change_percGDP = median(risk_change_percGDP,na.rm=TRUE),GDP_ppp_2050=mean(GDP_ppp_2050,na.rm=T)),
                aes(x = log(GDP_ppp_2050), y = risk_change_percGDP,shape=SSP) )+
  #geom_density_2d_filled(contour_var = "ndensity",aes(fill=SSP,alpha=..level..),bins=4,size=1)+
  scale_alpha_discrete(range = c(0,0.6,0.9,1),guide = guide_none()) +
  #geom_tile(aes(alpha = ..density..)) +
  facet_wrap(~ RCP) +
  scale_fill_brewer(palette = "Set1",direction=-1) +
  #scale_alpha_manual(values = c(0.1,0.1,0.1, 0.8), guide = FALSE) +  # Adjust alpha range as needed
  
  labs(
    title = "C. Increase in Risk",
    x = "Log of GDP per Capita in 2050",
    y = "Change in the Port-related Risk \n(percentage points of GDP)"
  ) +
  coord_cartesian(ylim = c(-0.1, 0.4)) +
  theme_minimal()


density_RCP_ssp

# library(ggrepel)
#         ggplot(port_ssp) + 
#             geom_point(aes(x=log(GDP_ppp_2022),y=risk_base,color=type)) + 
#             geom_text(aes(x=log(GDP_ppp_2022),y=risk_base,color=type,label=iso3))

# ## Figure 1

# pr_type <- ggplot(port_ssp %>% filter(RCP=="RCP85"))+
# #geom_text(aes(x=GDP_ppp_2022,y=risk_base_perc,col=type,label=iso3)) + 
# geom_point(aes(x=GDP_ppp_2022,y=risk_base_perc,col=type,shape=type)) + 
# scale_x_log10()+
# #scale_y_log10()+
# theme_bw()+
# xlab("GDP in 2022")+
# ylab("Economic Value at Risk \n(% of GDP)")+
# ggtitle("Present Risk by Type") +
# theme(legend.position="bottom") +
# scale_color_scico_d(end=0.8)

# pr_type
library(ggrepel)

glimpse(port_ssp)
pr_total_present <- ggplot( )+
geom_point(data=port_ssp %>% group_by(iso3,RCP,SSP) %>% 
    summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_base_perc))%>% slice(1),
    aes(x=GDP_ppp_2022,y=rb)) + 
geom_text_repel(data= port_ssp %>% group_by(iso3) %>% filter(RCP=="RCP45", SSP=="SSP2") %>%
    summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_base_perc) )%>% slice(1),aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
theme_bw()+
xlab("GDP in 2022 (2015 Bill USD)")+
scale_color_scico_d(palette="batlow", begin=0.1, end=0.8)+
scale_y_continuous(trans="log2",lim=c(0.01,16)) +
scale_x_continuous(trans="log10",lim=c(0.5,10^5)) +
#ylim(c(0,30))+
ylab("Value at Risk\n(% of GDP)")+
ggtitle("A. Present Risk")
pr_total_present


# ggarrange(pr_type,pr_total_present,ncol=2,align="h",widths=c(3,4))


# pr_total_future <- ggplot( )+
# geom_point(data=port_ssp %>% group_by(iso3,RCP,SSP) %>% 
#     summarise(GDP_ppp_2022=GDP_ppp_2050/10^9,rb=sum(risk_percGDP_2050) ),
#     aes(x=GDP_ppp_2022,y=rb,color=RCP,shape=SSP)) + 
# geom_text_repel(data= port_ssp %>% group_by(iso3) %>% filter(RCP=="RCP45", SSP=="SSP2") %>%
#     summarise(GDP_ppp_2022=GDP_ppp_2050/10^9,rb=sum(risk_percGDP_2050) )%>% slice(1),aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
# theme_bw()+
# xlab("GDP in 2022 (2015 Bill USD)")+
# #scale_color_scico_d(palette="batlow", begin=0.1, end=0.8)+
# scale_color_brewer(palette = "Set1",direction=-1) +
# ylab("Port-related Value at Risk in 2050 \n(% of GDP)")+
# scale_y_continuous(trans="log2",lim=c(0.01,16)) +
# scale_x_continuous(trans="log10") +
# ggtitle("Total Future Risk")
# pr_total_future


pr_total_future_1scen <- ggplot( )+
geom_point(data=port_ssp %>% filter(RCP=="RCP45",SSP=="SSP2") %>% group_by(iso3) %>% 
    summarise(GDP_ppp_2022=GDP_ppp_2050/10^9,rb=sum(risk_percGDP_2050) ),
    aes(x=GDP_ppp_2022,y=rb)) + 
geom_text_repel(data= port_ssp %>% group_by(iso3) %>% filter(RCP=="RCP45", SSP=="SSP2") %>%
    summarise(GDP_ppp_2022=GDP_ppp_2050/10^9,rb=sum(risk_percGDP_2050) )%>% slice(1),aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
theme_bw()+
xlab("GDP in 2050 (2015 Bill USD)")+
#scale_color_scico_d(palette="batlow", begin=0.1, end=0.8)+
scale_color_brewer(palette = "Set1",direction=-1) +
ylab("Present Value at Risk in 2050 \n(% of GDP)")+
scale_y_continuous(trans="log2",lim=c(0.01,16)) +
scale_x_continuous(trans="log10",lim=c(0.5,10^5)) +
ggtitle("B. Future Risk")+
theme(axis.title.y = element_blank(),  # Removes y-axis label
        axis.text.y = element_blank()) 
pr_total_future_1scen
#ggarrange(pr_type,pr_total_future,ncol=2,align="h",widths=c(3,4))
ggarrange(pr_total_present,pr_total_future_1scen,ncol=2,align="h",widths=c(4,3))



# ggarrange(ggarrange(pr_total_present,pr_total_future_1scen,ncol=2,align="h",widths=c(4,3)),
#     density_RCP_ssp,ncol=1)

# summarized_data <- port_ssp %>%
#                    filter(RCP == "RCP85" & SSP=="SSP2") %>%
#                    group_by(type) %>%
#                    summarize(total_risk_perc = sum(risk_base_perc,na.rm=TRUE),
#                    total_risk_perc_f = sum(risk_percGDP_2050,na.rm=TRUE))

# pr_type <- ggplot(summarized_data, aes(x = "", y = total_risk_perc, fill = type)) +
#            geom_bar(stat = "identity", width = 1) +
#            coord_polar(theta = "y") +
#            scale_fill_scico_d(end = 0.8) +
#            theme_void() + labs(fill=" ")

                   
# pr_type_future <- ggplot(summarized_data, aes(x = "", y = total_risk_perc_f, fill = type)) +
#            geom_bar(stat = "identity", width = 1) +
#            coord_polar(theta = "y") +
#            scale_fill_scico_d(end = 0.8) +
#            theme_void() + labs(fill=" ")


# present_pie <- ggdraw() +
#               draw_plot(pr_total_present,x=0,y=-0.25, width = 1, height = 1.42) +
#               draw_plot(pr_type, x = 0.56, y = 0.55, width = 0.4, height = 0.6) 
# future_pie <- ggdraw() +
#               draw_plot(pr_total_future_1scen,x=0,y=-0.25, width = 1, height = 1.42) +
#               draw_plot(pr_type_future, x = 0.56, y = 0.55, width = 0.4, height = 0.6) 


# ggarrange(ggarrange(present_pie,pr_total_future_1scen,ncol=2,align="h",widths=c(4,3)),
#     density_RCP_ssp,ncol=1)

#ggsave("Figures/SM/ports/present_future_risk.png")

ggarrange(ggarrange(pr_total_present,pr_total_future_1scen,ncol=2,align="h",widths=c(4,3)),
    density_RCP_ssp,ncol=1)

#ggsave("Figures/SM/ports/present_future_risk_nopie.png")

ggarrange(ggarrange(present_pie,future_pie,ncol=2,align="h",widths=c(4,3)),
    density_RCP_ssp,ncol=1)


glimpse(port_ssp)
pr_dif_present <- ggplot( )+
#geom_point(data=port_ssp %>% group_by(iso3,RCP,SSP) %>% 
#    summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_change_percGDP)),
#    aes(x=GDP_ppp_2022,y=rb,shape=SSP,color=RCP),alpha=0.5) + 
#geom_text_repel(data= port_ssp %>% group_by(iso3) %>% filter(RCP=="RCP45", SSP=="SSP2") %>%
#    summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_change_percGDP) )%>% slice(1),aes(x=GDP_ppp_2022,y=rb,label=iso3)) + 
geom_smooth(data=port_ssp%>% group_by(iso3,RCP)  %>% filter(SSP=="SSP2") %>%
    summarise(GDP_ppp_2022=GDP_ppp_2022/10^9,rb=sum(risk_change_percGDP,na.rm=TRUE)),
    aes(x=GDP_ppp_2022,y=rb,color=RCP)) + 
scale_x_log10()+
theme_bw()+
xlab("GDP in 2022 (2015 Bill USD)")+
scale_color_scico_d(palette="batlow", begin=0.1, end=0.8)+
#ylim(c(-0.5,1))+
geom_hline(aes(yintercept=0),linetype="dashed")+
ylab("Change in Economic Value at Risk in 2050\n (pp of GDP)")+
ggtitle("Total Future Risk") 


pr_dif_present


ggarrange(pr_total,pr_dif_present,ncol=2,align="h",widths=c(3,4))

GDP_ppp_2050_mex <- port_ssp %>%
  filter(iso3 == "MEX", SSP == "SSP2") %>%
  summarise(GDP_ppp_2050_mex = first(GDP_ppp_2050) / 10^9) %>%
  pull(GDP_ppp_2050_mex)

pr_dif_future <- ggplot( )+ 
geom_smooth(data=port_ssp%>% group_by(iso3,RCP)  %>% filter(SSP=="SSP2") %>%
    summarise(GDP_ppp_2050=GDP_ppp_2050/10^9,rb=sum(risk_change_percGDP,na.rm=TRUE)),
    aes(x=GDP_ppp_2050,y=rb,color=RCP,fill=RCP)) + 
scale_x_log10()+
theme_bw()+
xlab("GDP in 2050 (2015 Bill USD)")+
scale_color_scico_d(palette="batlow", begin=0.1, end=0.8)+
scale_fill_scico_d(palette="batlow", begin=0.1, end=0.8)+
geom_vline(xintercept = GDP_ppp_2050_mex, linetype = "dotted", color = "blue") + # Add the vertical line for Mexico's GDP  
geom_text(aes(x = GDP_ppp_2050_mex+2500, y = 3, label = "MEX"), vjust = -0.5, color = "blue") + # Label for the line
#ylim(c(-0.5,1))+
geom_hline(aes(yintercept=0),linetype="dashed")+
ylab("Change of Impacts\n (pp of GDP)")+
ggtitle("A. Distribution of Impacts") 


pr_dif_future



linear_damage_country <- ggplot()+
geom_point(data = port_ssp %>% filter(iso3=="MEX")%>% group_by(RCP,SSP)   %>%
    summarise(GDP_ppp_2050=GDP_ppp_2050/10^9,risk_change=sum(risk_change_percGDP),tdif=mean(tdif)),
    aes(x=tdif,y=risk_change,color=RCP,shape=SSP) ) +
    geom_smooth(data = port_ssp %>%
                filter(iso3 == "MEX") %>%
                group_by(RCP, SSP) %>%
                summarise(GDP_ppp_2050 = GDP_ppp_2050 / 10^9,
                          risk_change = sum(risk_change_percGDP),
                          tdif = mean(tdif), .groups = 'drop'), # Data for geom_smooth
              aes(x = tdif, y = risk_change),
              method = "lm",
              formula = y ~ x - 1, # This forces the line through the origin
              #se = FALSE, # Set to FALSE if you don't want the confidence interval shaded
              color = "black")+
geom_hline(aes(yintercept=0),linetype="dashed")+
scale_color_scico_d(palette="batlow", begin=0.1, end=0.8) + theme_bw() + ylab("Change of Impacts\n (pp of GDP)") + 
labs(title="B. Impacts in Mexico in 2050") + xlab("Change in GMST w.r.t. Baseline")+
  guides(color = FALSE) # This removes the color legend


plot_effect_ports <- ggarrange(pr_dif_future,linear_damage_country,ncol=1,legend="right")
plot_effect_ports
 
 load(file="Data/Modules/Ports/ports_tcoeff.Rds")
    glimpse(ports_tcoeff)

    ports_tcoeff  <- ports_tcoeff %>% dplyr::rename(GDP_FractionChange_perC = tcoeff, 
    GDP_FractionChange_perC_se = se)
    write.csv(ports_tcoeff,file="Data/intermediate_output/ports_tcoeff_v3.csv")


    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, ports_tcoeff, by = c("iso_a3" = "iso3"))

    # Plot
    map_port <- ggplot(data = merged_data) +
    geom_sf(aes(fill = tcoeff)) +
    scale_fill_scico(palette = "lajolla", limits = c(0, 0.5), oob=squish) + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
  theme(legend.position = "bottom",
        #legend.title = element_blank(), # Ensure no title is displayed
        legend.text = element_text(angle = 0)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "Ports Damage\n(%GDP/Degree C)")) +
  ggtitle("C. Damage Function Coefficients")

ggarrange(plot_effect_ports,map_port,ncol=2)




ggsave("Figures/SM/ports/coefficients.png")

summarized_data <- summarized_data %>%
  # Calculate the sum of total_risk_perc across all types
  mutate(total_sum = sum(total_risk_perc)) %>%
  # Calculate the percentage of each type relative to the total sum
  mutate(percentage_of_total = (total_risk_perc / total_sum) * 100)

# View the updated summarized_data
print(summarized_data)




glimpse(port_ssp)
port_ssp  %>% dplyr::filter(is.na(risk_change_percGDP)) %>%  dplyr::select(iso3)


ports_tcoeff  %>% dplyr::filter(is.na(tcoeff)) 

countries_nocoeff <- merged_data  %>% dplyr::filter(is.na(tcoeff)) %>% dplyr::select(iso_a3)%>% st_drop_geometry()
countries_coeff <- merged_data  %>% dplyr::filter(!is.na(tcoeff)) %>% dplyr::select(iso_a3)%>% st_drop_geometry()

port_ssp %>% filter(iso3 %in% countries_nocoeff$iso_a3)
levels(factor(port_ssp$iso3))

# Calculate the 25th, 50th, and 75th percentiles of GDP in 2050
GDP_percentiles <- port_ssp %>%
  filter(SSP == "SSP2", RCP=="RCP45",type=="TRADE") %>%
  summarise(
    P25 = quantile(GDP_ppp_2050, probs = 0.25, na.rm = TRUE),
    P50 = quantile(GDP_ppp_2050, probs = 0.5, na.rm = TRUE),
    P75 = quantile(GDP_ppp_2050, probs = 0.75, na.rm = TRUE)
  )

# Find the countries closest to these percentiles
closest_countries <- port_ssp %>%
  filter(SSP == "SSP2", RCP=="RCP45",type=="TRADE") %>%
  summarise(
    iso3,
    GDP_ppp_2050,
    Difference = abs(GDP_ppp_2050 - GDP_percentiles$P25)
  ) %>%
  arrange(Difference) %>%
  slice(1) %>%
  bind_rows(
    port_ssp %>%
      filter(SSP == "SSP2") %>%
      summarise(
        iso3,
        GDP_ppp_2050,
        Difference = abs(GDP_ppp_2050 - GDP_percentiles$P50)
      ) %>%
      arrange(Difference) %>%
      slice(1),
    port_ssp %>%
      filter(SSP == "SSP2") %>%
      summarise(
        iso3,
        GDP_ppp_2050,
        Difference = abs(GDP_ppp_2050 - GDP_percentiles$P75)
      ) %>%
      arrange(Difference) %>%
      slice(1)
  ) %>%
  select(-Difference)




pr_dif_future <- ggplot() + 
  geom_smooth(data = port_ssp %>% 
                group_by(iso3, RCP) %>% 
                filter(SSP == "SSP2") %>%
                summarise(GDP_ppp_2050 = GDP_ppp_2050 / 10^9, rb = sum(risk_change_percGDP, na.rm = TRUE), .groups = 'drop'),
              aes(x = GDP_ppp_2050, y = rb, color = RCP, fill = RCP), 
              method = "loess") + 
  scale_x_log10() +
  theme_bw() +
  xlab("GDP in 2022 (2015 Bill USD)") +
  scale_color_scico_d(palette = "batlow", begin = 0.1, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end = 0.8) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

# Add vertical lines and labels for the closest countries
for(i in 1:nrow(closest_countries)) {
  pr_dif_future <- pr_dif_future +
    geom_vline(xintercept = closest_countries$GDP_ppp_2050[i] / 10^9, linetype = "dotted", color = "blue") +
    geom_text(aes(x = closest_countries$GDP_ppp_2050[i] / 10^9, y = Inf, label = closest_countries$iso3[i]), vjust = -0.5, color = "blue", hjust = -0.5)
}

pr_dif_future <- pr_dif_future +
  ylab("Change in Value at Risk\n (pp of GDP)") +
  ggtitle("A. Smoothed Global Relationship Between GDP and Risk Change in 2050")

print(pr_dif_future)

glimpse(port_ssp)
# Summarize data to get total risk_perc by type
summarized_data <- port_ssp %>%
                   filter(RCP == "RCP85") %>%
                   group_by(type) %>%
                   summarize(total_risk_perc = sum(risk_base_perc,na.rm=TRUE))

# Create a pie chart
glimpse(summarized_data)
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
ggsave("Figures/SM/ports/present_risk.png", final_plot, width = 10, height = 8) 



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
