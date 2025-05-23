## Damage Funcion Fisheries


fisheries_df_temp_gdp <- read.csv("Data/output_modules_input_rice50x/output_modules/fish/fisheries_Free_EtAl.csv")

fish_df <- fisheries_df_temp_gdp %>% 
         filter(scenario == "Full Adaptation", 
                #country_iso3 == "MEX", 
                year > 2013, 
                !is.na(profits_usd_percGDP))
glimpse(fish_df)


fish_tcoeff <- fish_df %>%
        
        group_by(country_iso3) %>%
        nest() %>%
        mutate(
            tcoeff = map_dbl(data, ~{
            mod <- felm(I(profit_ppDiff_from_rcp_26/100) ~ 0 + I(tdif_from_rcp26) | 0 | 0 | 0, .x)
            coef(mod)[1]
            }),

            se = map_dbl(data, ~{
            mod <- felm(I(profit_ppDiff_from_rcp_26/100)~ 0 + I(tdif_from_rcp26) | 0 | 0 | 0, .x)
            summary(mod)$coef[2]
            }),

            pval = map_dbl(data, ~{
            mod <- felm(I(profit_ppDiff_from_rcp_26/100)~ 0 + I(tdif_from_rcp26) | 0 | 0 | 0, .x)
            summary(mod)$coef[4]
             })
            #, 
            # living_coral_cover = map_dbl(data, ~{
            # mod <- lm(cover ~1, .x)
            # summary(mod)$coef[1]
            # })
        )

        glimpse(fish_tcoeff)
        
        fish_tcoeff <- fish_tcoeff %>%
        unnest(data) %>% slice(1) %>% ungroup %>% dplyr::select(country_iso3,tcoeff,se,pval) %>% as.data.frame()


        
        save(fish_tcoeff,file="Data/output_modules_input_rice50x/output_modules/fish/fish_tcoeff_FreeEtAl.Rds")

        glimpse(fish_tcoeff)
    
    fish_tcoeff  <- fish_tcoeff %>% dplyr::rename(GDP_FractionChange_perC=tcoeff,GDP_FractionChange_perC_se=se)
    write.csv(fish_tcoeff,file="Data/output_modules_input_rice50x/input_rice50x/fish_tcoeff.csv")
    
        
