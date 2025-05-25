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
p_long_norm_GT <- as.data.frame(as.table(data_test_norm_homosce[[2]]$GT_p)) %>%
  mutate(significant = ifelse(Freq < 0.05,T,F))
colnames(p_long_norm_GT) <- c("state", "week", "p_value", "significant")

# DC normality
p_long_norm_DC <- as.data.frame(as.table(data_test_norm_homosce[[2]]$DC_p)) %>%
  mutate(significant = ifelse(Freq < 0.05,T,F))
colnames(p_long_norm_DC) <-  c("state", "week", "p_value", "significant")

# DCGT normality
p_long_norm_DCGT <- as.data.frame(as.table(data_test_norm_homosce[[2]]$DCGT_p)) %>%
  mutate(significant = ifelse(Freq < 0.05,T,F))
colnames(p_long_norm_DCGT) <-  c("state", "week", "p_value", "significant")

###### homoscedasticity ######
# GT homoscedasticity
p_long_homosce_GT <- as.data.frame(as.table(data_test_norm_homosce[[3]]$GT_p)) %>%
  mutate(significant = ifelse(Freq < 0.05,T,F))
colnames(p_long_homosce_GT) <-  c("state", "week", "p_value", "significant")

# DC homoscedasticity
p_long_homosce_DC <- as.data.frame(as.table(data_test_norm_homosce[[3]]$DC_p)) %>%
  mutate(significant = ifelse(Freq < 0.05,T,F))
colnames(p_long_homosce_DC) <-  c("state", "week", "p_value", "significant")

# DCGT homoscedasticity
p_long_homosce_DCGT <- as.data.frame(as.table(data_test_norm_homosce[[3]]$DCGT_p)) %>%
  mutate(significant = ifelse(Freq < 0.05,T,F))
colnames(p_long_homosce_DCGT) <-  c("state", "week", "p_value", "significant")


make_heatmap <- function(df, main_title) {
  ggplot(df, aes(x = factor(week, levels = unique(week)),
                 y = fct_rev(factor(state)),
                 fill = significant)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c("TRUE" = "#D73027", "FALSE" = "#FCFCFC"),
      name   = "p < 0.05",
      labels = c("No", "Yes")
    ) +
    labs(title = main_title,
         x     = "Epidemiological Week",
         y     = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y       = element_text(size = 6),
      panel.grid        = element_blank(),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold"),
      plot.title        = element_text(face = "bold", size = 11, hjust = 0.5)
    )
}

make_heatmap <- function(df, main_title) {
  ggplot(df, aes(
    x    = factor(week, levels = unique(week)),
    y    = fct_rev(factor(state)),
    fill = significant
  )) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c("TRUE" = "#D73027", "FALSE" = "#FCFCFC"),
      name   = "p < 0.05",
      labels = c("No", "Yes")
    ) +
    labs(
      title = main_title,
      x     = "Epidemiological Week",
      y     = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y       = element_text(size = 6),
      panel.grid        = element_blank(),
      plot.title        = element_text(face = "bold", size = 11, hjust = 0.5),
      
      # Legend styling: add box around keys and background
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold"),
      legend.key        = element_rect(fill = "white", color = "black"),
      legend.background = element_rect(fill = "gray95", color = "black")
    )
}


# 3. Create the six plots
norm_GT   <- make_heatmap(p_long_norm_GT,   "Normality (Shapiro–Wilk) — GT")
norm_DC   <- make_heatmap(p_long_norm_DC,   "Normality (Shapiro–Wilk) — DC")
norm_DCGT <- make_heatmap(p_long_norm_DCGT, "Normality (Shapiro–Wilk) — DCGT")

homo_GT   <- make_heatmap(p_long_homosce_GT,   "Homoscedasticity (Auxiliary Regression) — GT")
homo_DC   <- make_heatmap(p_long_homosce_DC,   "Homoscedasticity (ArchTest) — DC")
homo_DCGT <- make_heatmap(p_long_homosce_DCGT, "Homoscedasticity (ArchTest) — DCGT")

# 4. Combine into a 2×3 grid
combined_plot <- (
  norm_GT   + norm_DC   + norm_DCGT  # top row
) / (
  homo_GT   + homo_DC   + homo_DCGT  # bottom row
) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

print(combined_plot)
