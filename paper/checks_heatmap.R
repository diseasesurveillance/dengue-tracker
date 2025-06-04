generate_Prediction_for_test <- function(ufs, K = 15, compare_length = 1, year_window = 3, save = TRUE, 
                                gamma = c(0.95,0.5), if_test = FALSE) {
  final_df <- data.frame()
  epi_weeks <- c(seq(202410, 202452, by = 1), seq(202501, 202518, by = 1))
  #epi_weeks <- seq(202410, 202427, by = 1)
  
  if(if_test){
    pvalue_mat <- matrix(data = NA, nrow = length(brazil_ufs) , ncol = length(epi_weeks) - K)
    rownames(pvalue_mat) <- ufs; colnames(pvalue_mat) <- as.character(head(epi_weeks, length(epi_weeks) - K))
    homoscedasticity_p <- normality_p <- list(GT_p = pvalue_mat, DC_p = pvalue_mat, DCGT_p = pvalue_mat)
  }
  
  for (epi_week in epi_weeks) {
    if (epi_week + K > last(epi_weeks)) break
    if (epi_week == 202424) next
    if((epi_week+K) == 202424){K = K+1}
    
    # Determine look-ahead, skip missing week
    K_use <- K
    if (epi_week + K_use == 202424) {
      K_use <- K_use + 1
    }
    
    # Convert epi-week code to date
    ew_start_ <- get_date(
      week = as.numeric(substr(epi_week, 5, 6)),
      year = as.numeric(substr(epi_week, 1, 4))
    )
    ew_start_compare <- ew_start_ %m+% weeks(K_use)
    epi_week_compare <- epi_week + K_use
    print(epi_week)
    
    for (uf in ufs) {
      if (uf == "ES") next
      
      # Prepare true-case comparison data
      out_compare <- process_data(uf, ew_start_ %m+% weeks(K + 1), 
                                  ew = as.numeric(normalize_ew(ew_start_ %m+% weeks(K))))
      data_compare <- tail(
        out_compare[[1]] %>%
          filter(
            ew_start <= ew_start_,
            ew_start > (ew_start_ %m-% weeks(20))
          ),
        20
      )
      
      # Generate nowcasting inputs from GT
      data  <- generate_data(
        uf, last_ew_start = ew_start_ %m+% weeks(1), index_of_queries = c(1),
        ew = epi_week, save = FALSE, year_window = year_window, gamma = gamma[1]
      ) %>% filter(ew_start <= ew_start_)
      
      GT_temp_50 <- generate_data(
        uf, last_ew_start = ew_start_ %m+% weeks(1), index_of_queries = c(1),
        ew = epi_week, save = FALSE, year_window = year_window, gamma = gamma[2]
      ) %>% filter(ew_start <= ew_start_)
      
      data <- data %>% mutate(
        GT_prediction = prediction,
        GT_lwr_95 = lwr, GT_upr_95 = upr,
        GT_lwr_50 = GT_temp_50$lwr, GT_upr_50 = GT_temp_50$upr)
      
      # Deduplicate for BR
      if (uf == "BR") {
        data  <- unique(data)
      }
      
      # Run delay-correction models from SARIMAX
      data_DCGT <- run_model_DCGT(
        data, topics = out_compare[[2]][1], last_date = ew_start_, K = 20, 
        year_window = year_window, gamma = gamma
      )
      data_DC   <- run_model_DC(
        data, topics = out_compare[[2]][1], last_date = ew_start_, K = 20, 
        year_window = year_window, gamma = gamma
      )
      
      print(paste0("The norm and homoske test of DCGT are ", data_DCGT[[2]], " ", data_DCGT[[3]]))
      print(paste0("The norm and homoske test of DCGT are ", data_DC[[2]], " ", data_DC[[3]]))
      # Merge model outputs and true values
      merged_data <- merge(
        data_DCGT[[1]], data_DC[[1]],
        by = intersect(names(data_DCGT[[1]]), names(data_DC[[1]]))
      )
      
      out <- tibble(
        ew_start = data_compare$ew_start,
        ew_pred  = epi_week + 1,
        True     = data_compare$sum_of_cases
      )
      merged_data <- merge(merged_data, out, by = "ew_start")
      
      
      if(if_test){
        merged_data <- merged_data %>% mutate(residual_GT = True - GT_prediction)
        # pvalues from each test
        normality_p$GT_p[uf, as.character(epi_week)] <- round(shapiro.test(merged_data$residual_GT)$p.value,3)
        normality_p$DC_p[uf, as.character(epi_week)] <- round(data_DC[[2]],3)
        normality_p$DCGT_p[uf, as.character(epi_week)] <- round(data_DCGT[[2]],3)
        
        homoscedasticity_p$GT_p[uf, as.character(epi_week)] <- round(check_heteroscedasticity(merged_data$residual_GT,merged_data$GT_prediction),3)
        homoscedasticity_p$DC_p[uf, as.character(epi_week)] <- round(data_DC[[3]],3)
        homoscedasticity_p$DCGT_p[uf, as.character(epi_week)] <- round(data_DCGT[[3]],3)
      }
      
      merged_data <- tail(merged_data, compare_length)
      merged_data$uf <- uf
      
      # Select and compute performance metrics
      merged_data <- merged_data %>%
        select(
          ew_start, ew, sum_of_cases, cases_est_id, cases_est_id_min,
          cases_est_id_max, dengue, sintomas.dengue, uf,
          GT_prediction, GT_lwr_95, GT_upr_95, GT_lwr_50, GT_upr_50,
          DCGT_pred, DCGT_lwr_95, DCGT_upr_95, DCGT_lwr_50, DCGT_upr_50,
          DC_pred, DC_lwr_95, DC_upr_95, DC_lwr_50, DC_upr_50,
          ew_pred, True
        ) %>%
        mutate(
          DCGT_CoverageRate_95 = True > DCGT_lwr_95   & True < DCGT_upr_95,
          DC_CoverageRate_95   = True > DC_lwr_95     & True < DC_upr_95,
          GT_CoverageRate_95   = True > GT_lwr_95       & True < GT_upr_95,
          DCGT_CoverageRate_50 = True > DCGT_lwr_50   & True < DCGT_upr_50,
          DC_CoverageRate_50   = True > DC_lwr_50     & True < DC_upr_50,
          GT_CoverageRate_50   = True > GT_lwr_50       & True < GT_upr_50,
          # ID_CoverageRate   = True > cases_est_id_min & True < cases_est_id_max,
          DCGT_CI_WD_95 = DCGT_upr_95 - DCGT_lwr_95,
          DC_CI_WD_95   = DC_upr_95   - DC_lwr_95,
          GT_CI_WD_95   = GT_upr_95     - GT_lwr_95,
          DCGT_CI_WD_50 = DCGT_upr_50 - DCGT_lwr_50,
          DC_CI_WD_50   = DC_upr_50   - DC_lwr_50,
          GT_CI_WD_50   = GT_upr_50  - GT_lwr_50
          # ID_CI_WD   = cases_est_id_max - cases_est_id_min
        )
      
      # Naive (last-week) forecast
      ew_start_naive_start <- min(merged_data$ew_start) %m-% weeks(1)
      ew_start_naive_end   <- max(merged_data$ew_start) %m-% weeks(1)
      Naive <- data %>%
        filter(
          ew_start <= ew_start_naive_end,
          ew_start >= ew_start_naive_start
        ) %>% pull(sum_of_cases)
      merged_data$Naive <- Naive
      
      final_df <- rbind(final_df, merged_data)
    }
  }
  
  if (save) {
    write.csv(
      final_df,
      sprintf("data/model_results/model_%s_general.csv", last_ew_start),
      row.names = FALSE
    )
  }
  
  if(if_test){
    return(list(final_df, normality_p, homoscedasticity_p))
  }else{
    return(final_df)
  }
}




library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(viridis)
# Run models_series_refined first
data_test_norm_homosce <- generate_Prediction_for_test(brazil_ufs, K = 15, compare_length = 1, save = F,
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
