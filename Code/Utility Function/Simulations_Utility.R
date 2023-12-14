library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scico)
## Functions
    ces_utility <- function(c, e, i, s1 = 0.5, s2 = 0.1, theta1 = 0.55, theta2 = 0.58, alpha = 1.45) {
        utility_ces <- (
            (
                (
                    (1 - s2) * (((1 - s1) * c)^theta1 + (s1 * e)^theta1)
                )^((theta2) / (theta1)) +
                (s2 * i)^theta2
            )^((1 - alpha) / (theta2))
            - 1
        ) / (1 - alpha)

        return(utility_ces)
    }


    ces_utility <- function(c, e, i, theta1 = 0.55, theta2 = 0.58, alpha = 1.45) {
        utility_ces <- (
            (
                (((c)^theta1 + ( e)^theta1)
                )^((theta2) / (theta1)) +
                (i)^theta2
            )^((1 - alpha) / (theta2))
            - 1
        ) / (1 - alpha)

        return(utility_ces)
    }

## Functions


s1 <- 0.1
s2 <- 0.1
s1 <- 0.1
s2 <- 0.1
alpha <- 1.45
theta1 <- 0.55
theta2 <- 0.58


ct <- data.frame(year=seq(1:100),value=100+seq(1:100),component="market good", type = "Baseline")
et <- data.frame(year=seq(1:100),value=50+seq(1:100)* 0.5,component="ecosystem service", type = "Baseline")
it <- data.frame(year=seq(1:100),value=10+seq(1:100)* 0.2,component="existence value", type = "Baseline")
et_dam <- data.frame(year=seq(1:100),value=50+seq(1:100)* 0.5*0.8,component="ecosystem service", type = "Damaged")
it_dam <- data.frame(year=seq(1:100),value=10+seq(1:100)* 0.2*1,component="existence value", type = "Damaged")
df_comp <- rbind(ct,et,it,et_dam,it_dam)
glimpse(df_comp)

plot_components <- ggplot(df_comp)+
geom_line(aes(x=year,y=value,col=component,linetype=type)) + theme_bw() + 
labs(x="year",y="Value ($)",title="A. Utility Components") + scale_color_scico_d(begin=0.3,end=0.8)

#u_leontieff <- data.frame(year = ct$year, utility = pmin(ct$value,et$value,it$value), fun  = "Leontief", type = "Baseline")
#u_perfect_subs <- data.frame(year = ct$year, utility = rowSums(data.frame(ct$value,et$value,it$value)), fun  = "Perfect Substitutes", type = "Baseline")

u_leontieff <- data.frame(year = ct$year, utility = ((pmin(ct$value,et$value,it$value))^(1-alpha)-1)/(1-alpha), fun  = "Leontief", type = "Baseline")
u_perfect_subs <- data.frame(year = ct$year, utility = ((rowSums(data.frame(ct$value,et$value,it$value))^(1-alpha)-1)/(1-alpha)), fun  = "Perfect Substitutes", type = "Baseline")

#utility_ces <- ces_utility(c=ct$value,e=et$value,i=it$value)
#utility_ces <- ces_utility(c=ct$value,e=et$value,i=it$value)

# utility_ces <- ((
#     (
#         (
#             (1-s2)*(((1-s1)*ct$value)^theta1+(s1*et$value)^theta2)
#         )^((theta2)/(theta1)) +
#         (s2*it$value)^(theta2)
#     )^((1-alpha)/(theta2))
# -1) / (1-alpha)) 


u_ces_gd <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value), fun  = "GreenDICE", type = "Baseline")
u_ces_gd_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value), fun  = "GreenDICE", type = "Damaged")
u_ces <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=1,theta2=1), fun  = "thetas=1", type = "Baseline")
u_ces_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=1,theta2=1), fun  = "thetas=1", type = "Damaged")
u_ces2 <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=-0.9,theta2=-0.8), fun  = "thetas=-1", type = "Baseline")
u_ces2_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=-0.9,theta2=-0.8), fun  = "thetas=-1", type = "Damaged")
u_ces3 <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=0.8,theta2=0.7), fun  = "thetas=0.7 and 0.8", type = "Baseline")
u_ces3_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=0.8,theta2=0.7), fun  = "thetas=0.7 and 0.8", type = "Damaged")
u_ces4 <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=0.3,theta2=0.4), fun  = "thetas=0.3 and 0.4", type = "Baseline")
u_ces4_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=0.3,theta2=0.4), fun  = "thetas=0.3 and 0.4", type = "Damaged")
#u_ces <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=0.1,theta2=0.1), fun  = "s=0.5, thetas=0.1")
df_u <- rbind(u_leontieff,u_perfect_subs,u_ces_gd,u_ces_gd_damaged,u_ces,u_ces_damaged,u_ces2,u_ces2_damaged)
df_u <- rbind(u_ces_gd,u_ces_gd_damaged,u_ces,u_ces_damaged,u_ces2,u_ces2_damaged,u_ces3,u_ces3_damaged,u_ces4,u_ces4_damaged)
glimpse(df_u)



u_ces_gd <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value), fun  = "GreenDICE", type = "Baseline")
u_ces_gd_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value), fun  = "GreenDICE", type = "Damaged")
u_ces <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=1,), fun  = "theta1=1", type = "Baseline")
u_ces_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=1,), fun  = "theta1=1", type = "Damaged")
u_ces2 <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=-1,), fun  = "theta1=-1", type = "Baseline")
u_ces2_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=-1,), fun  = "theta1=-1", type = "Damaged")
u_ces3 <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=0.8), fun  = "theta1=0.8", type = "Baseline")
u_ces3_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=0.8), fun  = "theta1=0.8", type = "Damaged")
u_ces4 <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=0.3), fun  = "theta1=0.3", type = "Baseline")
u_ces4_damaged <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et_dam$value,i=it_dam$value,theta1=0.3), fun  = "theta1=0.3", type = "Damaged")
#u_ces <- data.frame(year = ct$year, utility = ces_utility(c=ct$value,e=et$value,i=it$value,theta1=0.1,theta2=0.1), fun  = "s=0.5, thetas=0.1")
df_u <- rbind(u_leontieff,u_perfect_subs,u_ces_gd,u_ces_gd_damaged,u_ces,u_ces_damaged,u_ces2,u_ces2_damaged)
df_u <- rbind(u_ces_gd,u_ces_gd_damaged,u_ces,u_ces_damaged,u_ces2,u_ces2_damaged,u_ces3,u_ces3_damaged,u_ces4,u_ces4_damaged)
df_u <- rbind(u_ces_gd,u_ces_gd_damaged,u_ces,u_ces_damaged,u_ces2,u_ces2_damaged)
glimpse(df_u)

ggplot(df_u)+
#ggplot(df_u %>% filter(fun=="thetas=1"))+
geom_line(aes(x=year,y=(utility),col=fun,linetype=type))+
theme_bw() 


plot_util <- ggplot(df_u)+
#ggplot(df_u %>% filter(fun=="thetas=-1"))+
geom_line(aes(x=year,y=(utility),col=fun,linetype=type))+
theme_bw() +
labs(x="year",y="Utility (utils)",title="B. Utility under different\n substitutability parameters")



damaged_utility_gd <- data.frame(year = ct$year, dif_utility=u_ces_gd$utility-u_ces_gd_damaged$utility,fun  = "GreenDICE")
damaged_u_ces <- data.frame(year = ct$year, dif_utility=(u_ces$utility-u_ces_damaged$utility),fun  = "theta1=1")
damaged_u_ces2 <- data.frame(year = ct$year, dif_utility=(u_ces2$utility-u_ces2_damaged$utility),fun  = "theta1=-1")
damaged_u_ces3 <- data.frame(year = ct$year, dif_utility=(u_ces3$utility-u_ces3_damaged$utility),fun  = "theta1=0.8")
damaged_u_ces4 <- data.frame(year = ct$year, dif_utility=(u_ces4$utility-u_ces4_damaged$utility),fun  = "theta1=0.3")
df_u_dam <- rbind(damaged_utility_gd,damaged_u_ces,damaged_u_ces2,damaged_u_ces3,damaged_u_ces4)
df_u_dam <- rbind(damaged_utility_gd,damaged_u_ces,damaged_u_ces2)

plot_dam <- 
#ggplot(df_u_dam %>% filter(fun!="thetas=-1"))+
ggplot(df_u_dam)+
geom_line(aes(x=year,y=(dif_utility),col=fun))+theme_bw()+
labs(x="year",y="Damaged utility (utils)",title="C. Utility Damages decreasing\n ecosystem services by 20%")

ggarrange(plot_components,ggarrange(plot_util,plot_dam,ncol=2,common.legend=T,legend="right"),ncol=1)


damaged_utility_gd <- data.frame(year = ct$year, dif_utility=100*(u_ces_gd$utility-u_ces_gd_damaged$utility)/(u_ces_gd_damaged$utility),fun  = "GreenDICE")
damaged_u_ces <- data.frame(year = ct$year, dif_utility=100*(u_ces$utility-u_ces_damaged$utility)/(u_ces$utility),fun  = "theta1=1")
damaged_u_ces2 <- data.frame(year = ct$year, dif_utility=100*(u_ces2$utility-u_ces2_damaged$utility)/(u_ces2$utility),fun  = "theta1=-1")
damaged_u_ces3 <- data.frame(year = ct$year, dif_utility=100*(u_ces3$utility-u_ces3_damaged$utility)/(u_ces3$utility),fun  = "theta1=0.7 and 0.8")
damaged_u_ces4 <- data.frame(year = ct$year, dif_utility=100*(u_ces4$utility-u_ces4_damaged$utility)/(u_ces4$utility),fun  = "theta1=0.3 and 0.4")
df_u_dam_perc <- rbind(damaged_utility_gd,damaged_u_ces,damaged_u_ces2)

glimpse(df_u_dam_perc)
plot_dam_perc <- 
#ggplot(df_u_dam_perc %>% filter(fun!="thetas=-1"))+
ggplot(df_u_dam_perc)+
geom_line(aes(x=year,y=(dif_utility),col=fun))+theme_bw()+
labs(x="year",y="Damaged utility \n(% of Baseline Utility)",title="D. Utility Damages decreasing\n ecosystem services by 20%")

plot_dam_perc

ggarrange(plot_components,ggarrange(plot_util,plot_dam,ncol=2,common.legend=T,legend="right"),ncol=1)


ggarrange(plot_components,ggarrange(plot_util,plot_dam,ncol=2,common.legend=T,legend="right"),plot_dam_perc,ncol=1)
ggsave("Figures/Utility/Util_sim.png",dpi=600)

getwd()
