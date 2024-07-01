x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","lfe","rnaturalearth","scales","ncdf4")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")
datadir <- "G:\\My Drive\\Fishmip_BOATS\\Profits\\"

ssp <- "ssp126"
nc_data <- nc_open(paste0(datadir,ssp,"\\Fishmip_Futur_trev.nc"))
print(nc_data)
