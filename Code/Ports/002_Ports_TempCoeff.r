

port_ssp<- read.csv("Data/modules/ports/ports_ssps_rcps.csv")

port_ssp_sum <- port_ssp %>% 
                group_by(iso3,RCP,SSP) %>% 
                summarise(risk_change=sum(risk_change_percGDP,na.rm=TRUE),tdif=mean(tdif)) %>%
                mutate(FractionGDP = -risk_change/100) %>%
                ungroup()

ports_tcoeff <- port_ssp_sum %>%
        filter(!is.na(risk_change)) %>%
        group_by(iso3) %>%
        nest() %>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(FractionGDP ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(FractionGDP ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(FractionGDP ~ 0 + I(tdif) | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
             })
        )

        glimpse(ports_tcoeff)
        
        ports_tcoeff <- ports_tcoeff %>%
        unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(iso3,tcoeff,se,pval) %>% as.data.frame()
        

        #save(ports_tcoeff,file="Data/Modules/Ports/ports_tcoeff.Rds")

        glimpse(ports_tcoeff)
        
