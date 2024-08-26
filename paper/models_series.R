library(glmnet)
library(hdrm)
library(aweek)
library(lubridate)
library(forecast)
library(tidyverse)
library(xtable)

#setwd('/Users/xiaoy0a/Desktop/GitHub/Dengue/dengue-tracker/')

source("data_functions.R")
aweek::set_week_start("Sunday")

brazil_ufs <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO", "BR"
)

merged_data <- data
run_model_DCGT <- function(merged_data, topics, 
                           last_date = NULL,
                           K = 4, gamma = 0.95){
  # set date
  if(is.null(last_date)){
    end <- (Sys.Date() - wday(Sys.Date()) + 1) %m-% weeks(K)
  }else{end <- (last_date - wday(last_date) + 1) %m-% weeks(K)}
  start <- end %m-% years(3)
  merged_data_temp <- merged_data %>% select(ew_start, sum_of_cases, topics) 
  # get train and pred data
  merged_data_temp_train <- merged_data_temp %>% filter(ew_start <= end & ew_start >= start) 
  end_pred <- end %m+% weeks(K); start_pred <- end %m+% weeks(1)
  merged_data_temp_pred <- merged_data_temp %>% filter(ew_start <= end_pred & ew_start >= start_pred)
  # fit the model
  # fit <- auto.arima(merged_data_temp_train$sum_of_cases, 
  #                   xreg = as.matrix(merged_data_temp_train[,c(3:(length(topics) +2))]))
  # forec <- forecast(fit, xreg = as.matrix(merged_data_temp_pred[, c(3:(length(topics) +2))]), level = gamma)
  # fit the model and output NA if error
  fit <- tryCatch(
    {
      auto.arima(merged_data_temp_train$sum_of_cases, 
                     xreg = as.matrix(merged_data_temp_train[,c(3:(length(topics) +2))]))
    },
    error = function(e) {
      return(NULL)
    }
  )
  
  forec <- tryCatch(
    {
      forecast(fit, xreg = as.matrix(merged_data_temp_pred[, c(3:(length(topics) +2))]), level = gamma)
    },
    error = function(e) {
      return(NULL)
    }
  )

  
  if (is.null(fit) | is.null(forec)) {
    forec_out <- tibble(
      DCGT_pred = rep(NA, K),
      DCGT_lb = rep(NA, K),
      DCGT_ub = rep(NA, K)
    )
  }else{
    # before change on (July 9th)
    # Conformative QR
    # error <-
    #   apply(cbind(
    #     forec$lower - forec$mean,
    #     forec$mean - forec$upper
    #   ), 1, max)
    # 
    # quantile_error <- quantile(error, probs = gamma, na.rm = T)
    # 
    # adjusted_forecast_lower <- pmax(forec$lower - quantile_error, 0)
    # adjusted_forecast_upper <- pmax(forec$upper + quantile_error, 0)
    # 
    # # rearrange
    # forec_out <- tibble(DCGT_pred = as.numeric(forec$mean), 
    #                     DCGT_lb = as.numeric(adjusted_forecast_lower),
    #                     DCGT_ub = as.numeric(adjusted_forecast_upper))
    # forec_out <- cbind(tail(merged_data, 20), forec_out)
    
    forec_out <- tibble(DCGT_pred = as.numeric(forec$mean),
                        DCGT_lb = as.numeric(forec$lower),
                        DCGT_ub = as.numeric(forec$upper))
  }
  forec_out <- cbind(tail(merged_data, 20), forec_out)
  
  forec_out
}

run_model_DC <- function(merged_data, topics, 
                          last_date = NULL,
                          K = 4, gamma = 0.95){
  # set date
  if(is.null(last_date)){
    end <- (Sys.Date() - wday(Sys.Date()) + 1) %m-% weeks(K)
  }else{end <- (last_date - wday(last_date) + 1) %m-% weeks(K)}
  start <- end %m-% years(3)
  
  # Convert to the start of ew weeks
  end <- (end - wday(end) + 1)
  merged_data_temp <- merged_data %>% select(ew_start, sum_of_cases)
  
  # get train and pred data
  merged_data_temp_train <- merged_data_temp %>% filter(ew_start <= end & ew_start >= start) 
  # fit the model
  # fit <- auto.arima(merged_data_temp_train$sum_of_cases)
  # forec <- forecast(fit, h = K, level = gamma)
  fit <- tryCatch(
    {
      auto.arima(merged_data_temp_train$sum_of_cases)
    },
    error = function(e) {
      return(NULL)
    }
  )
  
  forec <- tryCatch(
    {
      forecast(fit, h = K, level = gamma)
    },
    error = function(e) {
      return(NULL)
    }
  )
  
  if (is.null(fit) | is.null(forec)) {
    forec_out <- tibble(
      DC_pred = rep(NA, K),
      DC_lb = rep(NA, K),
      DC_ub = rep(NA, K)
    )
  }else{
    # before change on (July 9th)
    # Conformative QR
    # error <-
    #   apply(cbind(
    #     forec$lower - forec$mean,
    #     forec$mean - forec$upper
    #   ), 1, max)
    # 
    # quantile_error <- quantile(error, probs = gamma, na.rm = T)
    # 
    # adjusted_forecast_lower <- pmax(forec$lower - quantile_error, 0)
    # adjusted_forecast_upper <- pmax(forec$upper + quantile_error, 0)
    # 
    # # rearrange
    # forec_out <- tibble(DC_pred = as.numeric(forec$mean), 
    #                     DC_lb = as.numeric(adjusted_forecast_lower),
    #                     DC_ub = as.numeric(adjusted_forecast_upper))
    # forec_out <- cbind(tail(merged_data, 20), forec_out)
    
    forec_out <- tibble(DC_pred = as.numeric(forec$mean),
                        DC_lb = as.numeric(forec$lower),
                        DC_ub = as.numeric(forec$upper))
  }
  forec_out <- cbind(tail(merged_data, 20), forec_out)

  forec_out
}

generate_Prediction <- function(ufs, K = 10, K_true = 10, compare_length = 1, save = T,  gamma = 0.95){
  
  # if(compare_length > K){
  #   stop("Error! The length of prediction to compare should be equal to /smaller than the prediction length(K)!")}

  final_df <- data.frame()
  ## Weeks to be considered
  epi_weeks <- seq(202410, 202433, by = 1)
<<<<<<< HEAD
=======
  #epi_weeks <- seq(202426, 202433, by = 1)
>>>>>>> 9525765d8b67fbdddf3d609c95775aa061e5c845

  for(epi_week in epi_weeks){
    if(epi_week + K > last(epi_weeks)){break}

<<<<<<< HEAD
=======
    ## Since we didn't get data from 202424, we will compare the results for
    ## 202420 with 202425
    if(epi_week == 202420) {
      K <- 5
    } else K <- K_true
    if(epi_week == 202424) {
      epi_week <- 202423
    }
    # K_true should be larger than K
    if(epi_week > last(epi_weeks) - K_true) { break }
    
>>>>>>> 9525765d8b67fbdddf3d609c95775aa061e5c845
    # 202424 is missing
    if(epi_week == 202424){next}
    if((epi_week+K) == 202424){K = K+1}else{
      K = K_true
    }
    ## Dates for training model
    ew_start_ <- get_date(week = as.numeric(substr(epi_week, 5, 6)), year = as.numeric(substr(epi_week, 1, 4)))

    ## Dates for filtering data to compare
    ew_start_compare <- ew_start_ %m+% weeks(K)
    epi_week_compare <- epi_week + K
    
    print(epi_week)
    for (uf in ufs) {
      if(uf == "ES") { next }
      # out_compare <- process_data(uf, ew_start_ %m+% weeks(K + 1), ew = epi_week_compare)
      # Get K_ture is to control the delay of weeks for the "true data". The default value is 4.
      out_compare <- process_data(uf, ew_start_ %m+% weeks(K + 1), ew = (epi_week_compare + K - K))
      data_compare <- tail(out_compare[[1]] %>% filter(ew_start <=(ew_start_) &
                                                         ew_start > (ew_start_ %m-% weeks(20))), 20)
      # if(uf == "RR"){ next }
      data <- generate_data(uf, last_ew_start = ew_start_ %m+% weeks(1), ew = epi_week, save=F) |> 
        filter(ew_start <= ew_start_)
      data2 <- generate_data(uf, last_ew_start = ew_start_ %m+% weeks(1), index_of_queries = c(2),
                             ew = epi_week, save=F) |> 
        filter(ew_start <= ew_start_)
      if (uf == "BR") {
        data <- data |> unique()
        data2 <- data2 |> unique()
      }
      data <- data %>% mutate(lwr2 = data2$lwr, upr2 = data2$upr, GT2 = data2$prediction,
                              IDGT = (prediction + cases_est_id) / 2, IDGT2 = (GT2 + cases_est_id) / 2)
      
      # special case, need to be explained in paper. (202417 week cannot be fitted in RR)
      if (uf == "RR" & epi_week) {
        
      }
      # K here is the delay to train, not the K in this function for validation
      data_DCGT <- run_model_DCGT(data, topics = out_compare[[2]], last_date = ew_start_, K = 4, gamma = gamma)
      data_DC <- run_model_DC(data, topics = out_compare[[2]], last_date = ew_start_, K = 4, gamma = gamma)

      merged_data <- merge(data_DCGT, data_DC, by=names(data_DCGT)[1:(ncol(data_DCGT) - 3)])
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
                                             "cases_est_id_max", "dengue", "sintomas.dengue", "uf", 
                                             "lwr", "upr", "prediction", "lwr2", "upr2", "GT2", "IDGT", "IDGT2",  
                                             "DCGT_pred", "DCGT_lb", "DCGT_ub",   
                                             "DC_pred", "DC_lb","DC_ub" ,"ew_pred","True")) |>
        mutate(DCGT_CoverageRate = ifelse(True > DCGT_lb & True < DCGT_ub, TRUE, FALSE),
               DC_CoverageRate = ifelse(True > DC_lb & True < DC_ub, TRUE, FALSE),
               GT_CoverageRate = ifelse(True > lwr & True < upr, TRUE, FALSE),
               GT2_CoverageRate = ifelse(True > lwr2 & True < upr2, TRUE, FALSE),
               ID_CoverageRate = ifelse(True > cases_est_id_min & True < cases_est_id_max, TRUE, FALSE),
               DCGT_CI_WD = DCGT_ub - DCGT_lb,
               DC_CI_WD = DC_ub - DC_lb,
               GT_CI_WD = upr - lwr,
               GT2_CI_WD = upr2 - lwr2,
               ID_CI_WD = cases_est_id_max - cases_est_id_min)
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
                                num_of_models = 5, num_of_CI = 4,
                                relative_to_naive = TRUE){
  # Input data with first column to be the real value
  # and other columns are predicted values
  # And model names
  num_of_non_CI <- num_of_models - num_of_CI
  
  print(data$uf[1])
  
  # Coverage rate
  CR <- data[, c((num_of_models + 2) : (num_of_models + 1 + num_of_CI))]
  
  CR_out <- as.numeric(apply(CR, 2, function(col) {
    sum(na.omit(col)) / length(na.omit(col))
  }))
  
  CR_out <- c(CR_out, rep(NaN,num_of_non_CI))
  
  # CI width
  CI <- data[, c((num_of_models + 2 + num_of_CI) : ncol(data))]
  CI_out <- CI %>% 
    mutate(across(.cols = -uf, .fns = ~ sum(na.omit(.)) / length(na.omit(.)))) %>%
    distinct(uf, .keep_all = TRUE) %>% select(-uf)%>%as.numeric()
  
  CI_out <- c(CI_out, rep(NaN,num_of_non_CI))
  
  
  
  data <- data[, c(1:(num_of_models + 1))]
  
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
  
  # Combine the CR
  Measurements$CR <- CR_out
  Measurements$WD <- CI_out
  
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
    select(Prediction, Difference) %>%
    filter(is.finite(Difference))
  
  # Order by Mean or model names
  if (order_by == "Mean") {
    mean_order <- df_long %>%
      group_by(Prediction) %>%
      summarize(mean_val = mean(abs(Difference))) %>%
      arrange(mean_val) %>%
      pull(Prediction)
    
    df_long$Prediction <- factor(df_long$Prediction, levels = mean_order)
  } else {
    df_long$Prediction <- factor(df_long$Prediction, levels = prediction_cols)
  }
  
  # Set relation between color and model
  colors <- c("chartreuse2", "deepskyblue2", "brown3", "darkorange", "cornsilk2")
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

brazil_states_full <- c(
  "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", 
  "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", 
  "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", 
  "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", 
  "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins", "Brazil"
)

df <- generate_Prediction(brazil_ufs, K = 10, compare_length = 20, save = F)

## ERROR QUANTIFICATION
temp <- df |>
  group_by(ew_pred, uf) |>
  filter(ew == max(ew)) |>
  ungroup()
########### get metrics table ###########
model_preds_metrics <- temp %>% select(True, DCGT_pred, DC_pred, prediction, cases_est_id, Naive,
                                       DCGT_CoverageRate, DC_CoverageRate, GT_CoverageRate, ID_CoverageRate,
                                       DCGT_CI_WD, DC_CI_WD, GT_CI_WD, ID_CI_WD,uf) %>%
  rename(Real_value = True, DCGT = DCGT_pred, DC = DC_pred, GT = prediction, InfoDengue = cases_est_id) %>%
  as.data.frame()

model_preds_metrics_2nd <- temp %>% select(True, prediction, GT2, cases_est_id, IDGT, IDGT2,
                                       GT_CoverageRate, GT2_CoverageRate, ID_CoverageRate,
                                       GT_CI_WD, GT2_CI_WD, ID_CI_WD, uf) %>%
  rename(Real_value = True, GT = prediction, InfoDengue = cases_est_id) %>%
  as.data.frame()

real_time_list <- list(); real_time_list_2nd <- list()

count <- 1
for (state in brazil_ufs) {
  real_time_list[[brazil_states_full[count]]] <- compare_Measurement(model_preds_metrics[which(model_preds_metrics$uf == state),], 
                                                 relative_to_naive = F)
  real_time_list_2nd[[brazil_states_full[count]]] <- compare_Measurement(model_preds_metrics_2nd[which(model_preds_metrics_2nd$uf == state),],
                                                                        num_of_models = 5, num_of_CI = 3,
                                                                        relative_to_naive = F)
  count <- count + 1
}

########### get latex table for metrics ###########

highlight_values <- function(df, type = c("min", "max"), n = 1) {
  type <- match.arg(type)
  if (!(n %in% c(1, 2))) stop("n must be either 1 or 2")
  
  apply(df, 1, function(x) {
    if (type == "min") {
      values <- sort(x, na.last = NA)
    } else if (type == "max") {
      values <- sort(x, decreasing = TRUE, na.last = NA)
    }
    
    target_vals <- values[1:n]
    
    sapply(x, function(y) {
      if (!is.na(y) && y == target_vals[1]) {
        paste0("\\textcolor{red}{", y, "}")
      } else if (n == 2 && !is.na(y) && y == target_vals[2]) {
        paste0("\\textcolor{blue}{", y, "}")
      } else {
        y
      }
    })
  }) %>% t() %>% as.data.frame()
}

create_latex_tables <- function(real_time_list, brazil_states_full,
                                num_of_models = 5, num_of_CI = 4,
                                latex_code = TRUE) {
  metrics <- c("RMSE", "MAE", "RMSPE", "MAPE", "CR", "WD")
  models <- unique(real_time_list[[1]]$Models)
  
  # Initialize data frames for each metric
  metrics_df <- list()
  for (metric in metrics) {
    metrics_df[[metric]] <- data.frame(matrix(ncol = length(models), nrow = length(brazil_states_full)))
    colnames(metrics_df[[metric]]) <- models
    rownames(metrics_df[[metric]]) <- brazil_states_full
  }
  
  # Fill data frames with the appropriate metrics
  for (state in brazil_states_full) {
    if(state == "Brazil") {next}
    data <- real_time_list[[state]]
    for (metric in metrics) {
      sprintf("%s, %s", metric, state)
      metrics_df[[metric]][state, ] <- data[[metric]]
    }
  }
  
  # Round data
  metrics_df <- lapply(metrics_df, function(df){ df %>% mutate(across(where(is.numeric), ~ round(., 2)))})
  
  # Remove specific columns from CR and WD (All NAs)
  # Here last two columns InfoDengue and Naive is removed
  metrics_df$CR <- metrics_df$CR[, -c((num_of_CI ) : num_of_models)]
  metrics_df$WD <- metrics_df$WD[, -c((num_of_CI ) : num_of_models)]

  if(latex_code){
    # Generate LaTeX tables
    latex_tables <- list()
    for (name in names(metrics_df)) {
      df <- metrics_df[[name]]
      if (name == "CR") {
        df_highlighted <- highlight_values(df, type = "max", n = 1)
      } else {
        df_highlighted <- highlight_values(df, type = "min", n = 2)
      }
      latex_table <- xtable(df_highlighted, caption = paste(toupper(name), "Comparison"))
      latex_tables[[name]] <- print(latex_table, sanitize.text.function = identity,
                                    comment = FALSE, include.rownames = TRUE)
    }
  }else{
    return(metrics_df)
  }
}
create_latex_tables(real_time_list, brazil_states_full)
create_latex_tables(real_time_list_2nd, brazil_states_full, num_of_CI = 3)


###################### boxplot ######################
model_preds_p <- temp %>% select(True, DCGT_pred, DC_pred, prediction, cases_est_id, Naive, uf) %>%
  rename(Real_value = True, DCGT = DCGT_pred, DC = DC_pred, GT = prediction, InfoDengue = cases_est_id) %>%
  as.data.frame()
head(model_preds_p)

real_time_plot_1 <- list(); real_time_plot_2 <- list()
count_p = 0
# plots
for (state in brazil_ufs) {
  count_p <- count_p + 1
  if(state == "ES"){next}
  if(count_p <= 27){
    real_time_plot_1[[state]] <- get_Boxplot(model_preds_p[which(model_preds_p$uf == state),-7], 
                                             x_lab = NULL, y_lab = expression(hat(c)[t] - c[t]),
                                             order_by = "Mean",
                                             plot_title = brazil_states_full[count_p], no_legend = T)
  }else{
    real_time_plot_2[[state]] <- get_Boxplot(model_preds_p[which(model_preds_p$uf == state),-7], 
                                             x_lab = NULL, y_lab = expression(hat(c)[t] - c[t]),
                                             order_by = "Mean",
                                             plot_title = brazil_states_full[count_p], no_legend = T)
  }
}

library(gridExtra)

#States
boxp <- grid.arrange(grobs = real_time_plot_1, ncol = 5)
#BR
grid.arrange(grobs = real_time_plot_2, ncol = 4)

ggsave(
  filename = "box_plot.png",           # file name
  plot = boxp,                 # object to save
  path = "/Users/xiaoy0a/Desktop/Task/Nowcasting/7. Slides/",  # save dir
  width = 14,                        # width
  height = 18,                        # height
  units = "in",                      # size, "in", "cm", "mm"
  dpi = 300                          # DPI
)

###################### BR states MAP ######################
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)

# Get Brazil state boundaries data
brazil_states <- ne_states(country = "Brazil", returnclass = "sf")

# Ensure geometry is valid
brazil_states <- st_make_valid(brazil_states)
state_centers <- st_centroid(brazil_states)

# Set the central coordinates
state_coords <- st_coordinates(state_centers)

brazil_states <- brazil_states %>%
  mutate(
    centroid_long = state_coords[,1],
    centroid_lat = state_coords[,2],
    postal = as.character(postal),  # Ensure state abbreviations are characters
    name = as.character(name)  # Ensure state names are characters
  )

# Define states with different arrow operations
states_with_arrows_1 <- c("PB", "PE", "AL", "SE")
states_with_arrows_2 <- c("RN")
states_with_arrows_3 <- c("MS", "SC", "RS")
states_with_arrows_4 <- c("DF", "ES", "RJ")

# Create the plot
ggplot(data = brazil_states) +
  geom_sf(fill = "white", color = "black") +  # Draw state boundaries
  geom_text(data = filter(brazil_states, !postal %in% c(states_with_arrows_1, states_with_arrows_2, 
                                                        states_with_arrows_3, states_with_arrows_4)), 
            aes(x = centroid_long, y = centroid_lat, label = name), 
            color = "black", size = 4.5) +  # Add state names directly at centroid for states not in any arrow lists
  geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_1), 
               aes(x = -34, y = centroid_lat, xend = centroid_long, yend = centroid_lat), 
               color = "grey", arrow = arrow(length = unit(0.2, "cm"))) +  # Add arrows for states_with_arrows_1
  geom_text(data = filter(brazil_states, postal %in% states_with_arrows_1), 
            aes(x = -34, y = centroid_lat, label = name), 
            color = "black", size = 4.5, hjust = -0.1) +  # Add state names near arrows for states_with_arrows_1
  geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_2), 
               aes(x = -36, y = -2, xend = centroid_long, yend = centroid_lat), 
               color = "grey", arrow = arrow(length = unit(0.2, "cm"))) +  
  geom_text(data = filter(brazil_states, postal %in% states_with_arrows_2), 
            aes(x = -40, y = -1.3, label = name), 
            color = "black", size = 4.5, hjust = -0.1) +  
  geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_3), 
               aes(x = -60, y = centroid_lat, xend = centroid_long, yend = centroid_lat), 
               color = "grey", arrow = arrow(length = unit(0.2, "cm"))) + 
  geom_text(data = filter(brazil_states, postal %in% states_with_arrows_3), 
            aes(x = -73, y = centroid_lat, label = name), 
            color = "black", size = 4.5, hjust = -0.1) +  
  geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_4), 
               aes(x = -38, y = centroid_lat, xend = centroid_long, yend = centroid_lat), 
               color = "grey", arrow = arrow(length = unit(0.2, "cm"))) + 
  geom_text(data = filter(brazil_states, postal %in% states_with_arrows_4), 
            aes(x = -38, y = centroid_lat, label = name), 
            color = "black", size = 4.5, hjust = -0.1) +  
  theme_minimal() +
  theme(
    panel.background = element_blank(),      # Remove background of the plot panel
    panel.grid.major = element_blank(),       # Remove major grid lines
    panel.grid.minor = element_blank(),       # Remove minor grid lines
    plot.background = element_blank(),        # Remove background of the entire plot
    axis.text = element_blank(),              # Remove axis text
    axis.title = element_blank(),             # Remove axis titles
    axis.ticks = element_blank(),             # Remove axis ticks
    panel.border = element_blank()
  )+
  labs(title = NULL, x = "Longitude", y = "Latitude")

#################################################################
###################### BR states for model ######################
#################################################################
metrcis_plot <- function(metric_table){
  # Get Brazil state boundaries data
  brazil_states <- ne_states(country = "Brazil", returnclass = "sf")
  
  # Set specific rows to avoid the output being a list
  metric_table[c(8),] <- c(2, 2, 2, 1, 2)
  # metric_table[c(23),] <- c(2, 2, 2, 1, 2)
  
  # Find the column names with the minimum value for each row
  best_models <- apply(metric_table, 1, function(row) {
    colnames(metric_table)[which.min(row)]
  })
  best_models[8] <- "Non-comparable"
  # best_models[23] <- "Non-comparable"
  
  # Create a data frame with state names and corresponding models
  best_models$Brazil <- "Non-comparable"
  states_models <- data.frame(
    name = rownames(metric_table),
    best_models = unlist(best_models, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  # Rearrange the order
  states_models <- states_models[match(brazil_states$name, states_models$name), ]
  rownames(states_models) <- NULL
  
  # Assign models to Brazil states
  brazil_states$model <- states_models$best_models
  
  # Ensure geometry is valid
  brazil_states <- st_make_valid(brazil_states)
  state_centers <- st_centroid(brazil_states)
  
  # Set central coordinates
  state_coords <- st_coordinates(state_centers)
  
  brazil_states <- brazil_states %>%
    mutate(
      centroid_long = state_coords[, 1],
      centroid_lat = state_coords[, 2],
      postal = as.character(postal),  # Ensure state abbreviations are characters
      name = as.character(name)  # Ensure state names are characters
    )
  
  # Define states with different arrow operations
  states_with_arrows_1 <- c("PB", "PE", "AL", "SE")
  states_with_arrows_2 <- c("RN")
  states_with_arrows_3 <- c("MS", "SC", "RS")
  states_with_arrows_4 <- c("DF", "ES", "RJ")
  
  # Create the plot
  p <- ggplot(data = brazil_states) +
    geom_sf(aes(fill = model), color = "black") +  # Draw state boundaries
    geom_text(data = filter(brazil_states, !postal %in% c(states_with_arrows_1, states_with_arrows_2,
                                                          states_with_arrows_3, states_with_arrows_4)),
              aes(x = centroid_long, y = centroid_lat, label = name),
              color = "black", size = 5) +  # Add state names at centroids
    geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_1),
                 aes(x = -34, y = centroid_lat, xend = centroid_long, yend = centroid_lat),
                 color = "grey", arrow = arrow(length = unit(0.2, "cm"))) +  # Add arrows for specific states
    geom_text(data = filter(brazil_states, postal %in% states_with_arrows_1),
              aes(x = -34, y = centroid_lat, label = name),
              color = "black", size = 5, hjust = -0.1) +  # Add state names near arrows
    geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_2),
                 aes(x = -36, y = -2, xend = centroid_long, yend = centroid_lat),
                 color = "grey", arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = filter(brazil_states, postal %in% states_with_arrows_2),
              aes(x = -40, y = -1.3, label = name),
              color = "black", size = 5, hjust = -0.1) +
    geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_3),
                 aes(x = -60, y = centroid_lat, xend = centroid_long, yend = centroid_lat),
                 color = "grey", arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = filter(brazil_states, postal %in% states_with_arrows_3),
              aes(x = -73, y = centroid_lat, label = name),
              color = "black", size = 5, hjust = -0.1) +
    geom_segment(data = filter(brazil_states, postal %in% states_with_arrows_4),
                 aes(x = -38, y = centroid_lat, xend = centroid_long, yend = centroid_lat),
                 color = "grey", arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = filter(brazil_states, postal %in% states_with_arrows_4),
              aes(x = -38, y = centroid_lat, label = name),
              color = "black", size = 5, hjust = -0.1) +
    scale_fill_manual(values = c("DCGT" = "chartreuse2", "DC" = "deepskyblue2", "GT" = "brown3",
                                 "InfoDengue" = "darkorange", "Naive" = "cornsilk2", "Non-comparable" = "white"),
                      name = "Best Model", 
                      labels = c("DCGT" = "DCGT")) +
    theme_minimal() +
    labs(title = NULL, x = "Longitude", y = "Latitude") +
    theme(
      panel.background = element_blank(),      # Remove background of the plot panel
      panel.grid.major = element_blank(),       # Remove major grid lines
      panel.grid.minor = element_blank(),       # Remove minor grid lines
      plot.background = element_blank(),        # Remove background of the entire plot
      axis.text = element_blank(),              # Remove axis text
      axis.title = element_blank(),             # Remove axis titles
      axis.ticks = element_blank(),             # Remove axis ticks
      panel.border = element_blank()
    ) 
}

# Use MAE data frame
mae_df <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)$MAE
mae <- metrcis_plot(mae_df)

mape_df <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)$MAPE
mape <- metrcis_plot(mape_df)

rmse_df <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)$RMSE
rmse <- metrcis_plot(rmse_df)

rmspe_df <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)$RMSPE
rmspe <- metrcis_plot(rmspe_df)

# Save in a higher resolution
plots <- list(mae, mape, rmse, rmspe)
filenames <- c("map_br_model_mae.png", "map_br_model_mape.png", "map_br_model_rmse.png", "map_br_model_rmspe.png")

# Loop to save the plots
for (i in 1:length(plots)) {
  ggsave(
    filename = filenames[i],           # file name
    plot = plots[[i]],                 # object to save
    path = "/Users/xiaoy0a/Desktop/Task/Nowcasting/7. Slides/",  # save dir
    width = 12,                        # width
    height = 8,                        # height
    units = "in",                      # size, "in", "cm", "mm"
    dpi = 300                          # DPI
  )
}

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
# plot for dengue cases over time
BR_trends <- read.csv("data/weekly_data/Infodengue/202426/BR_2024-06-30_infodengue.csv")
BR_trends$ew_start <- as.Date(BR_trends$ew_start)

ggplot(BR_trends, aes(x = ew_start, y = sum_of_cases, color = sum_of_cases)) +
  geom_line(size = 1) +  
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Date",
       y = "Dengue cases",
       color = "Dengue cases") +
  theme_minimal()  


# ### PLOTS
# 
df <- df |>
  filter(ew_pred == max(df$ew_pred))

plot_geofacet_series(temp |> filter(uf != "BR"))

plot_map_best_metric(real_time_list, states_map, "MAE")
plot_map_best_metric(real_time_list, states_map, "MAPE")
# 
# plot_trends_data(df, "RJ")