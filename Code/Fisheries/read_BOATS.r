install.packages("raveio")


x<-c("ggplot2", "dplyr","WDI","ggpubr","scico","lfe","rnaturalearth","scales","raveio")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\BlueDICE")
datadir <- "G:\\My Drive\\Fishmip_BOATS"
pathname <- file.path(datadir, "Fishmip_Hist_hd_ind_6290.mat")
data <- read_mat("G:\\My Drive\\Fishmip_BOATS\\GFDL\\Fishmip_Hist_hd_ind_6290.mat")
print(data)

names(data)
class(data)


fish_g_out <- data[[89]]
glimpse(fish_g_out)
