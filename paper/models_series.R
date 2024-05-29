library(glmnet)
library(hdrm)
library(aweek)
library(lubridate)
library(forecast)
library(tidyverse)
library(xtable)

setwd('/Users/xiaoy0a/Desktop/GitHub/Dengue/dengue-tracker/')

source("data_functions.R")
aweek::set_week_start("Sunday")

brazil_ufs <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)


run_model_ARGO <- function(merged_data, topics, 
                           last_date = NULL,
                           offset_lag = 1, offset_GT = 0.001,
                           K = 5, critical_level = 0.05,
                           if_pred_){
  # set date
  if(is.null(last_date)){
    end <- (Sys.Date() - wday(Sys.Date()) + 1) %m-% weeks(K)
  }else{end <- (last_date - wday(last_date) + 1) %m-% weeks(K)}
  start <- end %m-% years(3)
  # log-trans
  merged_data_temp <- merged_data %>% select(ew_start, sum_of_cases, topics) %>%
    mutate(sum_of_cases = log(sum_of_cases + offset_lag),
           dengue  = log(dengue + offset_GT),
           sintomas.dengue = log(sintomas.dengue + offset_GT))
  # get train and pred data
  merged_data_temp_train <- merged_data_temp %>% filter(ew_start <= end & ew_start >= start) 
  end_pred <- end %m+% weeks(K); start_pred <- end %m+% weeks(1)
  merged_data_temp_pred <- merged_data_temp %>% filter(ew_start <= end_pred & ew_start >= start_pred)
  # fit the model
  fit <- auto.arima(merged_data_temp_train$sum_of_cases, 
                    xreg = as.matrix(merged_data_temp_train[,c(3:(length(topics) +2))]))
  forec <- forecast(fit, xreg = as.matrix(merged_data_temp_pred[, c(3:(length(topics) +2))]), level = 1 - critical_level)
  
  # rearrange
  forec_out <- tibble(ARGO_pred = as.numeric(exp(forec$mean) - offset_lag), 
                      ARGO_lb = as.numeric(exp(forec$lower) - offset_lag),
                      ARGO_ub = as.numeric(exp(forec$upper) - offset_lag))
  forec_out <- cbind(tail(merged_data, 20), forec_out)
  
  forec_out
}

run_model_SAR <- function(merged_data, topics, 
                          last_date = NULL,
                          offset_lag = 1,
                          K = 4, critical_level = 0.05){
  # set date
  if(is.null(last_date)){
    end <- (Sys.Date() - wday(Sys.Date()) + 1) %m-% weeks(K)
  }else{end <- (last_date - wday(last_date) + 1) %m-% weeks(K)}
  start <- end %m-% years(3)

  # log-trans
  merged_data_temp <- merged_data %>% select(ew_start, sum_of_cases) %>%
    mutate(sum_of_cases = log(sum_of_cases + offset_lag))
  # get train and pred data
  merged_data_temp_train <- merged_data_temp %>% filter(ew_start <= end & ew_start >= start) 
  # fit the model
  fit <- auto.arima(merged_data_temp_train$sum_of_cases)
  forec <- forecast(fit, h = K,level = 1 - critical_level)
  
  # rearrange
  forec_out <- tibble(SAR_pred = as.numeric(exp(forec$mean) - offset_lag), 
                      SAR_lb = as.numeric(exp(forec$lower) - offset_lag),
                      SAR_ub = as.numeric(exp(forec$upper) - offset_lag))
  forec_out <- cbind(tail(merged_data, 20), forec_out)
  
  forec_out
}


generate_Prediction <- function(ufs, K = 4, compare_length = 1, save = T,  gamma = 0.05){
  
  # if(compare_length > K){
  #   stop("Error! The length of prediction to compare should be equal to /smaller than the prediction length(K)!")}

  final_df <- data.frame()
  ## Weeks to be considered
  epi_weeks <- seq(202410, 202421, by = 1)

  for(epi_week in epi_weeks){
    if(epi_week > last(epi_weeks)- K) { break }
    ## Dates for training model
    ew_start_ <- get_date(week = as.numeric(substr(epi_week, 5, 6)), year = as.numeric(substr(epi_week, 1, 4)))
    
    ## Dates for filtering data to compare
    ew_start_compare <- ew_start_ %m+% weeks(K)
    epi_week_compare <- epi_week + K
    print(epi_week)
    for (uf in ufs) {
      #print(uf)
      
      out_compare <- process_data(uf, ew_start_ %m+% weeks(K + 1), ew = epi_week_compare)
      data_compare <- tail(out_compare[[1]] %>% filter(ew_start <=(ew_start_) &
                                                         ew_start > (ew_start_ %m-% weeks(20))), 20)
      #print(uf)
      if(uf == "ES") { next }
      if(uf == "RR"){ next }
      
      data <- generate_data(uf, last_ew_start = ew_start_ %m+% weeks(1), ew = epi_week, save=F) |> 
        filter(ew_start <= ew_start_)
      #data <- tail(data, 20)
      data_argo <- run_model_ARGO(data, topics = out_compare[[2]], last_date = ew_start_, K = K, critical_level = gamma)
      data_sar <- run_model_SAR(data, topics = out_compare[[2]], last_date = ew_start_, K = K, critical_level = gamma)
      
      merged_data <- merge(data_argo, data_sar, by=names(data_argo)[1:(ncol(data_argo) - 3)])
      #merged_data[nrow(merged_data), "ew"] <- max(merged_data$ew, na.rm=T) + 1
      ## Naive is using the last week case as prediction
      # Naive <- tail(data %>% filter(ew_start <= ew_start_ %m-% weeks(K)
      #                               & ew_start >= (ew_start_ %m-% weeks(2*K))), K)
      
      # ew_pred is the week that we do this prediction
      out <- tibble(ew_start = data_compare$ew_start, ew_pred = epi_week+1,
                    True = data_compare$sum_of_cases)
      
      merged_data <- tail(merge(merged_data, out, by = "ew_start"), compare_length) #|>
      #filter(ew == max(merged_data$ew))
      merged_data$uf <- uf
      
      merged_data <- merged_data |> select(c("ew_start", "ew", "sum_of_cases", "cases_est_id", "cases_est_id_min",
                                             "cases_est_id_max", "dengue", "sintomas.dengue", "uf", "lwr",       
                                             "upr", "prediction",  "ARGO_pred", "ARGO_lb", "ARGO_ub",   
                                             "SAR_pred", "SAR_lb","SAR_ub" ,"ew_pred","True"))
      # Naive
      ew_start_naive_start <- min(merged_data$ew_start) %m-% weeks(1)
      ew_start_naive_end <- max(merged_data$ew_start) %m-% weeks(1)
      Naive <- data |> filter(ew_start <= ew_start_naive_end & ew_start >= ew_start_naive_start) %>%
        select(sum_of_cases)
      merged_data$Naive <- Naive$sum_of_cases

      final_df <- rbind(final_df, merged_data)
    }
  }
  
  if (save) {
    write.csv(final_df,
              sprintf("data/model_results/model_%s_general.csv", last_ew_start),
              row.names = F)
  }
  final_df
}

compare_Measurement <- function(data,
                                relative_to_naive = TRUE){
  # Input data with first column to be the real value
  # and other columns are predicted values
  # And model names
  
  # Save the col names to use it in the end
  Models <- colnames(data)[-1]
  
  
  # Initialize Measurements data frame
  Measurements <- data.frame()
  
  # Get prediction measurements for each model
  for(j in 1:(ncol(data)-1)){
    if(j == 1){
      Measurements <- get_Metrics(data[, 1], data[, j+1], F)
    }else{
      Measurements <- rbind(Measurements, get_Metrics(data[, 1], data[, j+1], F))
    }
  }

  Measurements <- cbind(Models, Measurements)
  
  # If take values that are relative to naive
  if(relative_to_naive){
    # Get the measurement of Naive model
    naive_values <- unlist(Measurements[nrow(Measurements), -1])
    
    
    # Divided by Naive
    for(i in 1:(nrow(Measurements) - 1)){
      Measurements[i, c(-1,-6)] <- Measurements[i, c(-1,-6)] / naive_values
    }
    
    # Move Naive till the end 
    Measurements <- rbind(Measurements[-nrow(Measurements), ], Measurements[nrow(Measurements), ])
  }
  
  
  Measurements[,-1] <- round(Measurements[,-1],3)
  
  return(Measurements)
}


get_Metrics <- function(y_fitted, y , IF_log = F){
  # This function is to get the measurement by giving
  # the fitted(predicted) value and the real value
  # Unscale the value first
  y_fitted <- na.omit(y_fitted)
  y <- na.omit(y)
  
  if(IF_log == T){
    case <- exp(y) - 1; case_fitted <- exp(y_fitted) - 1
  }else{
    case <- y ; case_fitted <- y_fitted
  }
  
  n <- length(case)
  M <- as.data.frame(matrix(0, nrow = 1, ncol = 4))
  # names(M) <- c("RMSE", "MAE", "RMSPE", "MAPE", "CORR")
  names(M) <- c("RMSE", "MAE", "RMSPE", "MAPE")
  
  M[1,1] <- sqrt(mean((case_fitted - case) ^ 2))
  M[1,2] <- mean(abs(case_fitted - case))
  M[1,3] <- sqrt(mean(((case_fitted - case) / case) ^ 2))
  M[1,4] <- mean(abs((case_fitted - case) / case))
  # M[1,5] <- cor(case_fitted, case, method="kendall")
  
  return(M)
}

get_Boxplot <- function(data, 
                        type = "Diff",
                        order_by = "Model",
                        plot_title = "Difference between Predictions and Real Value",
                        x_lab = "Prediction Method",
                        y_lab = "Difference",
                        prediction_cols = NULL,
                        no_legend = FALSE) {
  
  # Determine the real value column name dynamically
  real_value_col <- colnames(data)[1]
  
  # Get the names of all the prediction columns
  # prediction_cols <- colnames(data)[-1] # now we use a customize order
  if(is.null(prediction_cols)){prediction_cols <- colnames(data)[-1]}
  
  # Add a prefix to distinguish between absolute and percentage differences
  prefix <- ifelse(type == "Diff", "Diff", "Perc")
  
  # Calculate the differences based on the type
  df_long <- data %>%
    mutate(across(all_of(prediction_cols),
                  list(Diff = ~ if(type == "Diff") {
                    . - .data[[real_value_col]]
                  } else {
                    100 * abs(. - .data[[real_value_col]]) / .data[[real_value_col]]
                  }),
                  .names = "{prefix}_{.col}")) %>%
    pivot_longer(cols = starts_with(prefix), 
                 names_to = "Prediction", 
                 values_to = "Difference") %>%
    mutate(Prediction = sub(paste0("^", prefix, "_"), "", Prediction),
           Prediction = factor(Prediction, levels = prediction_cols)) %>%
    select(Prediction, Difference)
  
  # Order by median or model names
  if (order_by == "Median") {
    median_order <- df_long %>%
      group_by(Prediction) %>%
      summarize(median_val = median(Difference)) %>%
      arrange(median_val) %>%
      pull(Prediction)
    
    df_long$Prediction <- factor(df_long$Prediction, levels = median_order)
  } else {
    df_long$Prediction <- factor(df_long$Prediction, levels = prediction_cols)
  }
  
  # Set relation between color and model
  colors <- c("green", "red", "purple", "orange", "blue")
  model_names <- prediction_cols
  
  # Plot the differences using ggplot2, with different colors for each prediction method
  p_out <- ggplot(df_long, aes(x = Prediction, y = Difference, fill = Prediction)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "red", shape = 18) +
    labs(title = plot_title,
         x = x_lab,
         y = y_lab) +
    theme_minimal() +
    scale_fill_manual(values = colors, breaks = model_names) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),  
      panel.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  if(!no_legend){
    p_out <- p_out +  
      theme(
        legend.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          vjust = 1
        )
      )
  } else {
    p_out <- p_out +  
      theme(
        legend.position = "none",
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          vjust = 1
        )
      )
  }
  
  return(p_out)
}


df <- generate_Prediction(brazil_ufs, K = 4, compare_length = 20, save = F)

generate_Prediction("AC", K = 4, compare_length = 20, save = F)

brazil_states_full <- c(
  "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", 
  "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", 
  "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", 
  "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", 
  "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"
)

## ERROR QUANTIFICATION
temp <- df |>
  group_by(ew_pred, uf) |>
  filter(ew == max(ew)) |>
  ungroup()
########### get metrics table ###########
model_preds_metrics <- temp %>% select(True, ARGO_pred, SAR_pred, prediction, cases_est_id, Naive) %>%
  rename(Real_value = True, SARIMAX = ARGO_pred, SAR = SAR_pred, GT = prediction, InfoDengue = cases_est_id) %>%
  as.data.frame()

real_time_list <- list()

count <- 1
for (state in brazil_ufs) {
  real_time_list[[brazil_states_full[count]]] <- compare_Measurement(model_preds_metrics[which(temp$uf == state),], 
                                                 relative_to_naive = F)
  count <- count + 1
}

########### get latex table for metrics ###########
metrics <- c("RMSE", "MAE", "RMSPE", "MAPE")
models <- unique(real_time_list[[1]]$Models)

# new dataframe to store
rmse_df <- data.frame(matrix(ncol = length(models), nrow = length(brazil_states_full)))
colnames(rmse_df) <- models
rownames(rmse_df) <- brazil_states_full

mae_df <- rmse_df
rmspe_df <- rmse_df
mape_df <- rmse_df

# fill
for (state in brazil_states_full) {
  data <- real_time_list[[state]]
  rmse_df[state, ] <- data$RMSE
  mae_df[state, ] <- data$MAE
  rmspe_df[state, ] <- data$RMSPE
  mape_df[state, ] <- data$MAPE
}

highlight_min <- function(df) {
  apply(df, 1, function(x) {
    min_val <- min(x, na.rm = TRUE)
    second_min_val <- min(x[x > min_val], na.rm = TRUE)
    sapply(x, function(y) {
      if (!is.na(y) && y == min_val) {
        paste0("\\textcolor{red}{", y, "}")
      } else if (!is.na(y) && y == second_min_val) {
        paste0("\\textcolor{blue}{", y, "}")
      } else {
        y
      }
    })
  }) %>% t() %>% as.data.frame()
}

metrics_list <- list(rmse = rmse_df, mae = mae_df, rmspe = rmspe_df, mape = mape_df)
for (name in names(metrics_list)) {
  df <- metrics_list[[name]]
  df_highlighted <- highlight_min(df)
  latex_table <- xtable(df_highlighted, caption = paste(toupper(name), "Comparison"))
  print(latex_table, sanitize.text.function = identity, 
        comment = FALSE, include.rownames = TRUE)
}

###################### boxplot ######################
model_preds_p <- temp %>% select(True, ARGO_pred, SAR_pred, prediction, cases_est_id, Naive, uf) %>%
  rename(Real_value = True, SARIMAX = ARGO_pred, SAR = SAR_pred, GT = prediction, InfoDengue = cases_est_id) %>%
  as.data.frame()

real_time_plot_1 <- list(); real_time_plot_2 <- list()
count_p = 0
# plots
for (state in brazil_ufs) {
  count_p <- count_p + 1
  if(state == "ES" | state == "RR"){next}
  if(count_p <= 13){
    real_time_plot_1[[state]] <- get_Boxplot(model_preds_p[which(model_preds_p$uf == state),-7], 
                                             x_lab = NULL, y_lab = expression(hat(c)[t] - c[t]),
                                             plot_title = brazil_states_full[count_p], no_legend = T)
  }else{
    real_time_plot_2[[state]] <- get_Boxplot(model_preds_p[which(model_preds_p$uf == state),-7], 
                                             x_lab = NULL, y_lab = expression(hat(c)[t] - c[t]),
                                             plot_title = brazil_states_full[count_p], no_legend = T)
  }
}

library(gridExtra)

grid.arrange(grobs = real_time_plot_1, ncol = 4)
grid.arrange(grobs = real_time_plot_2, ncol = 4)


# temp$err_pred <- abs(temp$prediction - temp$True)
# temp$err_infodengue <- abs(temp$cases_est_id - temp$True)
# temp$err_argo <- abs(temp$ARGO_pred - temp$True)
# temp$err_sar <- abs(temp$SAR_pred - temp$True)
# 
# temp$ape_pred <- temp$err_pred/temp$True
# temp$ape_infodengue <- temp$err_infodengue/temp$True
# temp$ape_argo <- temp$err_argo/temp$True
# temp$ape_sar <- temp$err_sar/temp$True
# 
# errors <- temp |>
#   group_by(uf) |>
#   summarise(mae_pred = mean(err_pred),
#             mae_infodengue = mean(err_infodengue),
#             mae_argo = mean(err_argo),
#             mae_sar = mean(err_sar),
#             rmse_pred = sqrt(mean(err_pred^2)),
#             rmse_infodengue = sqrt(mean(err_infodengue^2)),
#             rmse_argo = sqrt(mean(err_argo^2)),
#             rmse_sar = sqrt(mean(err_sar^2)),
#             mse_pred = mean(err_pred^2),
#             mse_infodengue = mean(err_infodengue^2),
#             mse_argo = mean(err_argo^2),
#             mse_sar = mean(err_sar^2),
#             rmsle_pred = sqrt(mean((log1p(err_pred))^2)),
#             rmsle_infodengue = sqrt(mean((log1p(err_infodengue))^2)),
#             rmsle_argo = sqrt(mean((log1p(err_argo))^2)),
#             rmsle_sar = sqrt(mean((log1p(err_sar))^2)),
#             mape_pred = mean(ape_pred),
#             mape_infodengue = mean(ape_infodengue),
#             mape_argo = mean(ape_argo),
#             mape_sar = mean(ape_sar))
# 
# 
# bold_max_in_row <- function(row) {
#   numeric_row <- as.numeric(row)
#   max_value <- min(numeric_row, na.rm = TRUE)
#   row_as_char <- format(numeric_row, digits = 3, nsmall = 3)
#   row_as_char[numeric_row == max_value] <- paste0("\\textbf{", format(max_value, digits = 3, nsmall = 3), "}")
#   return(row_as_char)
# }
# 
# data_with_bold <- errors |> select(uf, mape_pred, mape_infodengue, mape_argo, mape_sar)
# numeric_columns <- sapply(data_with_bold, is.numeric)
# data_with_bold[numeric_columns] <- t(apply(data_with_bold[numeric_columns], 1, bold_max_in_row))
# 
# latex_table <- xtable(data_with_bold, caption = "Data with Bold Maximum Values")
# print(latex_table, sanitize.text.function = identity)
# 
# 
# ### PLOTS
# 
# df <- df |>
#   filter(ew_pred == max(df$ew_pred))
# 
# plot_geofacet_series(df)
# 
# 
# plot_trends_data(df, "RJ")
