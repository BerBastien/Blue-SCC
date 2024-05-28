## Set-up File

## Libraries (start)
    install.packages('rgeos')
    x <- c('raster','ggOceanMapsData','ggOceanMaps', 'ggpubr',
    'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf',
    'lfe','marginaleffects','rgdal',"rnaturalearth",'rgeos','geosphere','sf','ggthemes','scales',"exactextractr")
    lapply(x, require, character.only = TRUE)
## Libraries (end)

## Dir (start)
    script_path <- here::here()
    setwd(script_path)
    #setwd('C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Code\\Corals')
    #dir1 <- 'C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\corals\\'
    #dir_ssps <- "C:\\Users\\basti\\Box\\Data\\SSPs\\Gridded\\Pop\\"
## Dir (end)