## Supplementary Figures Corals Module

## Load Data (start)
    
    
    load(file="Data/Modules/Corals/corals_tcoeff.Rds") #corals_tcoeff
    load(file="Data/Modules/Corals/corals_temp_unique.Rds") #corals_temp_unique
        

## Load Data (end)

## Fig. S1 (start)
    individual_coral_change <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)), alpha = 0.05)+
    theme_bw()+
    guides(color="none")+
    scale_color_manual(values=c("#0072B2", "#D55E00"))+
    geom_text(aes(x=1,y=1.5,label="RCP4.5 - 2050"),color="#0072B2",size=3) +
    geom_text(aes(x=1.5,y=-2,label="RCP8.5 - 2050"),color="#D55E00",size=3)+
    geom_text(aes(x=2.1,y=-5,label="RCP4.5 - 2100"),color="#0072B2",size=3)+
    geom_text(aes(x=3.5,y=-12,label="RCP8.5 - 2100"),color="#D55E00",size=3) +
    xlab("Temperature increase from 1997-2018 baseline")+
    xlim(c(0,4.5))+
    ylab("Coral cover change from baseline (%)")

    legend_plot <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)))+
    theme_bw()+
    guides(color=guide_legend("Scenario"))+
    scale_color_manual(values=c("#0072B2", "#D55E00"))

    leg <- get_legend(legend_plot)

    density_coral_coeff_cover <- ggplot(corals_tcoeff, aes(x = tcoeff, y = 100*living_coral_cover)) +
        geom_hex() +
        labs(x = "GMST Coefficient (% cover change/degree C)", y = "Present Living Coral Cover (%)") +
        scale_fill_scico(palette="berlin")+theme_bw()
        density_coral_coeff_cover
    
    ggarrange(ggarrange(individual_coral_change,leg,ncol=2,widths=c(4,1)),density_coral_coeff_cover)


## Fig. S1 (end)

## Fig. S1 (start)
    individual_coral_change <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)), alpha = 0.05)+
    theme_bw()+
    guides(color="none")+
    scale_color_manual(values=c("#0072B2", "#D55E00"))+
    geom_text(aes(x=1,y=1.5,label="RCP4.5 - 2050"),color="#0072B2",size=3) +
    geom_text(aes(x=1.5,y=-2,label="RCP8.5 - 2050"),color="#D55E00",size=3)+
    geom_text(aes(x=2.1,y=-5,label="RCP4.5 - 2100"),color="#0072B2",size=3)+
    geom_text(aes(x=3.5,y=-12,label="RCP8.5 - 2100"),color="#D55E00",size=3) +
    xlab("Temperature increase from 1997-2018 baseline")+
    xlim(c(0,4.5))+
    ylab("Coral cover change from baseline (%)")

    legend_plot <- ggplot(corals_temp_unique)+
    geom_line(aes(x=tdif,y=cover_change_perc,color=scenario,group=interaction(scenario,uniqueplace)))+
    theme_bw()+
    guides(color=guide_legend("Scenario"))+
    scale_color_manual(values=c("#0072B2", "#D55E00"))

    leg <- get_legend(legend_plot)

    density_coral_coeff_cover <- ggplot(corals_tcoeff, aes(x = tcoeff, y = 100*living_coral_cover)) +
        geom_hex() +
        labs(x = "GMST Coefficient (% cover change/degree C)", y = "Present Living Coral Cover (%)") +
        scale_fill_scico(palette="berlin")+theme_bw()
        density_coral_coeff_cover
    
    ggarrange(ggarrange(individual_coral_change,leg,ncol=2,widths=c(4,1)),density_coral_coeff_cover)


## Fig. S1 (end)

