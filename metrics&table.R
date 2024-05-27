######## New code ########
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




temp_m <- temp %>% select(True, ARGO_pred, SAR_pred, prediction, cases_est_id, Naive) %>%
  rename(Real_value = True, ARGO = ARGO_pred, SAR = SAR_pred, GT = prediction, InfoDengue = cases_est_id)

real_time_list <- list()

for (state in brazil_ufs) {
  real_time_list[[state]] <- compare_Measurement(temp_m[which(temp$uf == state),], relative_to_naive = F)
}
real_time_list[["AC"]]


# LATEX TABLE
metrics <- c("RMSE", "MAE", "RMSPE", "MAPE")
models <- unique(real_time_list[[1]]$Models)

# new dataframe to store
rmse_df <- data.frame(matrix(ncol = length(models), nrow = length(brazil_ufs)))
colnames(rmse_df) <- models
rownames(rmse_df) <- brazil_ufs

mae_df <- rmse_df
rmspe_df <- rmse_df
mape_df <- rmse_df

for (state in brazil_ufs) {
  data <- real_time_list[[state]]
  rmse_df[state, ] <- data$RMSE
  mae_df[state, ] <- data$MAE
  rmspe_df[state, ] <- data$RMSPE
  mape_df[state, ] <- data$MAPE
}

#### red and blue ###
# highlight_min <- function(df) {
#   apply(df, 1, function(x) {
#     min_val <- min(x, na.rm = TRUE)
#     second_min_val <- min(x[x > min_val], na.rm = TRUE)
#     sapply(x, function(y) {
#       if (!is.na(y) && y == min_val) {
#         paste0("\\textcolor{red}{", y, "}")
#       } else if (!is.na(y) && y == second_min_val) {
#         paste0("\\textcolor{blue}{", y, "}")
#       } else {
#         y
#       }
#     })
#   }) %>% t() %>% as.data.frame()
# }

#### red ####
highlight_min <- function(df) {
  apply(df, 1, function(x) {
    min_val <- min(x, na.rm = TRUE)
    sapply(x, function(y) {
      if (!is.na(y) && y == min_val) {
        paste0("\\textcolor{red}{", y, "}")
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
