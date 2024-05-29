library(glmnet)
library(hdrm)
library(aweek)
library(lubridate)
library(forecast)
library(tidyverse)
library(xtable)
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
  
  if(compare_length > K){
    stop("Error! The length of prediction to compare should be equal to /smaller than the prediction length(K)!")}

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
      Naive <- tail(data %>% filter(ew_start <= ew_start_ %m-% weeks(K)
                                    & ew_start >= (ew_start_ %m-% weeks(2*K))), K)
      
      
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


df <- generate_Prediction(brazil_ufs, K = 4, compare_length = 20, save = F)


## ERROR QUANTIFICATION
temp <- df |>
  group_by(ew_pred, uf) |>
  filter(ew == max(ew)) |>
  ungroup()

temp$err_pred <- abs(temp$prediction - temp$True)
temp$err_infodengue <- abs(temp$cases_est_id - temp$True)
temp$err_argo <- abs(temp$ARGO_pred - temp$True)
temp$err_sar <- abs(temp$SAR_pred - temp$True)

temp$ape_pred <- temp$err_pred/temp$True
temp$ape_infodengue <- temp$err_infodengue/temp$True
temp$ape_argo <- temp$err_argo/temp$True
temp$ape_sar <- temp$err_sar/temp$True

errors <- temp |>
  group_by(uf) |>
  summarise(mae_pred = mean(err_pred),
            mae_infodengue = mean(err_infodengue),
            mae_argo = mean(err_argo),
            mae_sar = mean(err_sar),
            rmse_pred = sqrt(mean(err_pred^2)),
            rmse_infodengue = sqrt(mean(err_infodengue^2)),
            rmse_argo = sqrt(mean(err_argo^2)),
            rmse_sar = sqrt(mean(err_sar^2)),
            mse_pred = mean(err_pred^2),
            mse_infodengue = mean(err_infodengue^2),
            mse_argo = mean(err_argo^2),
            mse_sar = mean(err_sar^2),
            rmsle_pred = sqrt(mean((log1p(err_pred))^2)),
            rmsle_infodengue = sqrt(mean((log1p(err_infodengue))^2)),
            rmsle_argo = sqrt(mean((log1p(err_argo))^2)),
            rmsle_sar = sqrt(mean((log1p(err_sar))^2)),
            mape_pred = mean(ape_pred),
            mape_infodengue = mean(ape_infodengue),
            mape_argo = mean(ape_argo),
            mape_sar = mean(ape_sar))


bold_max_in_row <- function(row) {
  numeric_row <- as.numeric(row)
  max_value <- min(numeric_row, na.rm = TRUE)
  row_as_char <- format(numeric_row, digits = 3, nsmall = 3)
  row_as_char[numeric_row == max_value] <- paste0("\\textbf{", format(max_value, digits = 3, nsmall = 3), "}")
  return(row_as_char)
}

data_with_bold <- errors |> select(uf, mape_pred, mape_infodengue, mape_argo, mape_sar)
numeric_columns <- sapply(data_with_bold, is.numeric)
data_with_bold[numeric_columns] <- t(apply(data_with_bold[numeric_columns], 1, bold_max_in_row))

latex_table <- xtable(data_with_bold, caption = "Data with Bold Maximum Values")
print(latex_table, sanitize.text.function = identity)


### PLOTS

df <- df |>
  filter(ew_pred == max(df$ew_pred))

plot_geofacet_series(df)


plot_trends_data(df, "RJ")
