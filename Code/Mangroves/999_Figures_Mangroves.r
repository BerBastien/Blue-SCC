x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","rnaturalearth","scales")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")

# Figure 3


    load(file="Data/Modules/Mangroves/mangroves_tcoeff.Rds")

    # Get the world map in sf format
    world <- ne_countries(scale = "medium", returnclass = "sf")

    # Merge your data with the world map data
    merged_data <- left_join(world, mangrove_tcoeff, by = c("iso_a3" = "countrycode"))

    # Plot
    ggplot(data = merged_data) +
    geom_sf(aes(fill = tcoeff)) +
    scale_fill_scico(palette = "vik", oob=squish,midpoint=0,limits=c(-2,2), 
                     na.value="transparent") + # Use the desired scico palette
    coord_sf(crs = "+proj=robin") + # Robinson projection
    theme_minimal() +
    labs(fill = "Mangroves Damage\n(%Cover/Degree C)")
    ggsave("Figures/SM/mangroves/Map_Mangroves_Coef.png",dpi=600)
