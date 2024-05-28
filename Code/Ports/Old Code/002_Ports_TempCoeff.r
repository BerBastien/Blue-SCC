x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","lfe")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

port_ssp<- read.csv("Data/modules/ports/ports_ssps.csv")
glimpse(port_ssp)

port_ssp_sum <- port_ssp %>% group_by(iso3,scenario) %>% summarise(risk_change=sum(risk_change),tdif=mean(tdif))
glimpse(port_ssp_sum)

ports_tcoeff <- port_ssp_sum %>%
        filter(!is.na(risk_change)) %>%
        group_by(iso3) %>%
        nest() %>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(risk_change ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(risk_change ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(risk_change ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
             })
            #, 
            # living_coral_cover = map_dbl(data, ~{
            # mod <- lm(cover ~1, .x)
            # summary(mod)$coef[1]
            # })
        )

        ports_tcoeff <- ports_tcoeff %>%
        unnest(data) %>% slice(1) %>% ungroup %>% select(iso3,tcoeff,se,pval) %>% as.data.frame()
        

        save(ports_tcoeff,file="Data/Modules/Ports/ports_tcoeff.Rds")
        glimpse(ports_tcoeff)
        
