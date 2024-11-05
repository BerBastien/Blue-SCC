# Load necessary libraries
library(ggplot2)

# Define parameters
market_value <- 10  # Initial value of the market component
nonmarket_value <- 5  # Initial value of the nonmarket component
dam <- 1
eta <- 0.5  # Set a value for eta


# Define theta values to evaluate (excluding theta = 0)
theta_values <- seq(-1, 1, by = 0.1)
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
  
  # Delta Utility
  #delta_utility <- (1 / (1 - eta)) * (utility_dam^(1 - eta) - utility_base^(1 - eta))
  delta_utility <- utility_dam - utility_base
  
  # Corrected partial derivative of utility with respect to market
  partial_deriv_market <- (market_value^theta + (nonmarket_value-dam/2)^theta)^(((1-eta)/theta) - 1) * market_value^(theta - 1)
  #partial_deriv_market <- (alpha*market_value^theta + (1-alpha)*nonmarket_value^theta)^(((1-eta)/theta) - 1) * alpha*market_value^(theta - 1)
  
  # Calculate market dollar equivalent loss
  market_dollar_equivalent <- abs(delta_utility / partial_deriv_market)
  results$market_dollar_equivalent_loss[i] <- market_dollar_equivalent
}

# Plot the results
ggplot(results, aes(x = theta, y = market_dollar_equivalent_loss)) +
  geom_line() +
  labs(
    title = "Market-Dollar Equivalent of Losing One Dollar of Nonmarket Value",
    x = "Substitutability Parameter (theta)",
    y = "Market Dollar Equivalent Loss"
  ) +
  # Vertical dashed line at theta = 0 with label "Cobb-Douglas specification"
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 0, y = 3.5, 
           label = "Cobb-Douglas specification", vjust = -0.5, angle = 90) +
  # Blue line at (1, 1) with label "Perfect substitutes"
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  annotate("text", x = 1, y = 3.5, label = "Perfect substitutes", color = "blue", angle = 90,
           vjust = -0.5) +
  # Line at y = 0.21 with label "Drupp et al"
  geom_vline(xintercept = 0.21, linetype = "dashed", color = "red") +
  annotate("text", x = 0.21, y = 3.5, label = "Drupp et al (chosen value)", angle = 90, 
           color = "red", vjust = -0.5) +
  # Bottom-left text with mathematical expression
  annotate("text", x = min(results$theta), y = min(results$market_dollar_equivalent_loss) + 0.1, 
           label = "Perfect complements", hjust = 0, vjust = 1, size = 3.5) +
  annotate("text", x = min(results$theta), y = min(results$market_dollar_equivalent_loss), 
           label = expression(theta %->% -infinity), hjust = 0, vjust = 1, size = 3.5) +
  theme_minimal()

ggsave("Simulation_Utility_MarketLoss.png")
getwd()
