library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(viridis)
# Run models_series_refined first
data_test_norm_homosce <- generate_Prediction(brazil_ufs, K = 15, compare_length = 1, save = F,
                                    if_test = T)
###### normality ######
# GT normality
p_long_norm_GT <- as.data.frame(as.table(data_test_norm_homosce[[2]]$GT_p))
colnames(p_long_norm_GT) <- c("state", "week", "p_value")

# DC normality
p_long_norm_DC <- as.data.frame(as.table(data_test_norm_homosce[[2]]$DC_p))
colnames(p_long_norm_DC) <- c("state", "week", "p_value")

# DCGT normality
p_long_norm_DCGT <- as.data.frame(as.table(data_test_norm_homosce[[2]]$DCGT_p))
colnames(p_long_norm_DCGT) <- c("state", "week", "p_value")

###### homoscedasticity ######
# GT homoscedasticity
p_long_homosce_GT <- as.data.frame(as.table(data_test_norm_homosce[[3]]$GT_p))
colnames(p_long_homosce_GT) <- c("state", "week", "p_value")

# DC homoscedasticity
p_long_homosce_DC <- as.data.frame(as.table(data_test_norm_homosce[[3]]$DC_p))
colnames(p_long_homosce_DC) <- c("state", "week", "p_value")

# DCGT homoscedasticity
p_long_homosce_DCGT <- as.data.frame(as.table(data_test_norm_homosce[[3]]$DCGT_p))
colnames(p_long_homosce_DCGT) <- c("state", "week", "p_value")

norm_GT <- ggplot(p_long, aes(x = week, y = fct_rev(factor(state)), fill = significant)) +
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
