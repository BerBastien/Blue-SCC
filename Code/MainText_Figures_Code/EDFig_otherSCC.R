library(ggplot2)
library(patchwork)
library(dplyr)

# Data for Panel 1: Percentage comparison
panel1_data <- data.frame(
    Study = c("Burke, Hsiang,\nand Miguel", "Kalkuhl\nand Wenz", "Nordhaus"),
    Percentage = c(18, 91, 218),
    x = 1:3
)

# Data for Panel 2: Blue SCC estimates
panel2_data <- data.frame(
    Label = c("1.5% \n prtp=0.01%\n emuc=1.02", "2% \n prtp=0.2%\n emuc=1.24", "2.5% \n prtp=0.5%\n emuc=1.42", "3% \n prtp=0.8%\n emuc=1.57"),
    SCC = c(305, 168, 96, 64),
    x = 1:4
)

# Panel 1: Percentage increase
p1 <- ggplot(panel1_data, aes(x = x, y = Percentage)) +
    geom_col(fill = "#2E86AB", width = 0.6) +
    geom_text(aes(label = paste0(Study, "\n(", Percentage, "%)")), 
                        vjust = -0.5, hjust = 0, angle = 45, size = 3.5, lineheight = 0.9) +
    scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50),
                                         labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = 1:3, labels = NULL) +
    labs(y = "Percentage Increase", 
             title = "SCC Increase Due to Ocean Impacts") +
    theme_minimal() +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.margin = margin(t = 40, r = 10, b = 10, l = 10)
    )

# Panel 2: Blue SCC estimates
p2 <- ggplot(panel2_data, aes(x = x, y = SCC)) +
    geom_col(fill = "#A23B72", width = 0.6) +
    geom_text(aes(label = paste0("$", SCC)), vjust = -0.5, size = 3.5, fontface = "bold") +
    geom_text(aes(label = Label), vjust = 2, size = 3, color = "white", lineheight = 0.8) +
    scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50),
                                         labels = function(x) paste0("$", x)) +
    scale_x_continuous(breaks = 1:4, labels = NULL) +
    labs(y = "2020 Blue SCC (USD2020/tCO₂)", 
             title = "Blue SCC by Near-Term Consumption Discount Rate") +
    theme_minimal() +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
    )

combined_plot <- p1 + p2 + plot_layout(ncol = 2)

print(combined_plot)

ggsave("EDFig_otherSCC.png", combined_plot, width = 12, height = 6, dpi = 300)
ggsave("EDFig_otherSCC.pdf", combined_plot, width = 12, height = 6)
