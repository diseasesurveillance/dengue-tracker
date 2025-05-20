library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(viridis)
# Run models_series_refined first
data_test_norm_homosce <- generate_Prediction(brazil_ufs, K = 15, compare_length = 1, save = F,
                                    if_test = T)

# GT normality
p_long <- as.data.frame(as.table(data_test_norm_homosce[[2]]$GT_p))
colnames(p_long) <- c("state", "week", "p_value")


ggplot(p_long, aes(x = week, y = fct_rev(factor(state)), fill = p_value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", p_value)), size = 2.5) +
  scale_fill_viridis(
    option    = "plasma",
    direction = -1,
    begin     = 0.1,
    end       = 0.9,
    name      = "p-value",
    labels    = percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Shapiro–Wilk Normality Test p-Values Across Brazilian States",
    subtitle = "Each tile shows the p-value for a given epidemiological week",
    x        = "Epidemiological Week",
    y        = "State"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8),
    panel.grid         = element_blank(),
    legend.title       = element_text(face = "bold"),
    legend.key.width   = unit(0.5, "cm"),
    legend.key.height  = unit(2, "cm"),
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle      = element_text(size = 10, hjust = 0.5)
  )

ggplot(p_long, aes(x = week, y = fct_rev(factor(state)), fill = significant)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("TRUE" = "#D73027", "FALSE" = "#FCFCFC"),
    name   = "Significant (p < 0.05)",
    labels = c("No", "Yes")
  ) +
  labs(
    title    = "Shapiro–Wilk Normality Test Significance Across States",
    subtitle = "Tile is colored if p-value < 0.05",
    x        = "Epidemiological Week",
    y        = "State"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.text.y       = element_text(size = 8),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5)
  )
