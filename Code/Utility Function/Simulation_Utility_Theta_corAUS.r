# Load necessary libraries
library(ggplot2)

#Run Damages.R nd get cor_dam
glimpse(cor_dam)

# Define parameters
#market_value <- 10  # Initial value of the market component
market_value <- cor_dam %>% filter(year==2025,country=="aus") %>% select(YNET)*def_mult[[1]]*10^12 #in million 2020 USD
nonmarket_value <- cor_dam %>% filter(year==2025,country=="aus") %>% select(usenm_nodam)*def_mult[[1]]*10^12 #million 202o USD
market_value/nonmarket_value
dam <- 1 #damage is in million
eta <- 0.5  # Set a value for eta
#eta <- 1.45  # Set a value for eta


# Define theta values to evaluate (excluding theta = 0)
theta_values <- seq(-0.01, 1, by = 0.01)
theta_values <- theta_values[theta_values != 0]

# Function to calculate utility based on theta, market, and nonmarket values
calculate_utility <- function(theta, market, nonmarket) {
  #(1 / (1 - eta)) *(alpha*market^theta + (1-alpha)*nonmarket^theta)^((1-eta)/theta)
  (1 / (1 - eta)) *(market^theta + nonmarket^theta)^((1-eta)/theta)
}

# Calculate DeltaUtility for each theta
results <- data.frame(theta = theta_values, market_dollar_equivalent_loss = NA)

for (i in seq_along(theta_values)) {
  theta <- theta_values[i]
  
  # Calculate base and damaged utility
  utility_base <- calculate_utility(theta, market_value, nonmarket_value)
  utility_dam <- calculate_utility(theta, market_value, nonmarket_value - dam)
  #utility_base <- (market_value^theta + nonmarket_value^theta)^((1-eta)/theta)
  #utility_dam <- (market_value^theta + (nonmarket_value-dam)^theta)^((1-eta)/theta)
  
  # Delta Utility
  delta_utility <- utility_dam - utility_base
  
  # partial derivative of utility with respect to market
  partial_deriv_market <- (market_value^theta + (nonmarket_value-dam/2)^theta)^(((1-eta)/theta) - 1) * market_value^(theta - 1)
  
  # Calculate market dollar equivalent loss
  market_dollar_equivalent <- abs(delta_utility / partial_deriv_market)
  results$market_dollar_equivalent_loss[i] <- unlist(market_dollar_equivalent)
}

glimpse(results)
results
# Plot the results
market_eq <- ggplot(results, aes(x = theta, y = market_dollar_equivalent_loss)) +
  geom_line() +
  labs(
    title = "B. $1 Non-market Loss",
    x = expression("Substitutability Parameter" ~ theta),
    y = "Equivalent Loss in \nMarket Consumption ($)"
  ) +
  # Vertical dashed line at theta = 0 with label "Cobb-Douglas specification"
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 0., y = 15, 
           label = "Cobb-Douglas", vjust = -0.5, angle = 90,size=2.7) +
  # Blue line at (1, 1) with label "Perfect substitutes"
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  annotate("text", x = 1, y = 19, label = "Perfect substitutes", color = "steelblue", angle = 90,
           vjust = -0.5, size=2.8) +
  annotate("text", x = 1.08, y = 19, label = "Panel A", color = "steelblue", angle = 90,
           vjust = -0.5,size=2.5, 
         fontface = 'italic') +
  # Line at y = 0.21 with label "Drupp et al"
  geom_vline(xintercept = 0.21, linetype = "dashed", color = "indianred") +
  # annotate("text", x = 0.21, y = 3.5, label = "Drupp et al (chosen value)", angle = 90, 
  #          color = "red", vjust = -0.5,size=3.5) +
    annotate("text", x = 0.21, y = 19, label = "Imperfect Substitutes", angle = 90, 
           color = "indianred", vjust = -0.5,size=2.8) +
  annotate("text", x = 0.28, y = 19, label = "Panel C", angle = 90, 
           color = "indianred", vjust = -0.5,size=2.5, 
         fontface = 'italic') +
  # Bottom-left text with mathematical expression
  #annotate("text", x = min(results$theta), y = min(results$market_dollar_equivalent_loss) + 0.32, 
  #         label = "Perfect complements", hjust = 0, vjust = 1, size = 2.5) +
  #annotate("text", x = min(results$theta), y = min(results$market_dollar_equivalent_loss)+0.1, 
  #         label = expression(theta %->% -infinity), hjust = 0, vjust = 1, size = 2.5) + 
  #scale_y_continuous(trans="log10")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Left-align the title
  )+ xlim(c(-0.1,1.12))
market_eq
#ggsave("Figures/Utility/Simulation_Utility_MarketLoss.png")
getwd()


market_eq_gg <- ggarrange(ggplot() + theme_void() ,market_eq,ggplot()+ theme_void(), ncol=3,widths=c(1,2,1))
#ggarrange(time_damages_plot, market_eq_gg, plot_sectoral_damages + labs(title="C. Substitutability-adjusted Damages in 2050"),ncol=1, heights=c(3,2,3))

#ggsave("Figures/Main/Fig2_Damages2050_v4.jpg",dpi=300)
market_eq_gg2 <- market_eq +
  theme(
    plot.margin = margin(t = 50, r = 10, b = 50, l = 10)  # Top, Right, Bottom, Left in 'pt'
  )


  market_eq_gg2 <- market_eq_gg2 + theme(plot.margin = margin(50, 10, 50, 10))
plot_sectoral_damages <- plot_sectoral_damages + theme(plot.margin = margin(0, 0, 0, 0))


ggarrange(time_damages_plot, ggarrange(market_eq_gg2, plot_sectoral_damages_legbottom + labs(title="C. Substitutability-adjusted Damages in 2050"),align="h",
ncol=2,widths=c(2,3)),ncol=1,heights=c(9,10))
ggsave("Figures/Main/Fig2_Damages2050_v5.png",dpi=300)

market_eq_gg_small <- market_eq_gg +
  theme(
    text = element_text(size = 6),  # Reduces size for all text elements
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 6),
    plot.title = element_text(size = 6)
  )

ggarrange(
  time_damages_plot,
  ggarrange(
    market_eq_gg_small,
    plot_sectoral_damages + labs(title = "C. Substitutability-adjusted Damages in 2050"),
    ncol = 2,
    widths = c(2, 5)
  ),
  ncol = 1
)
