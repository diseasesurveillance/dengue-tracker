library(glmnet)
library(hdrm)

get_InputData <- function(data, 
                          topics,
                    last_date = NULL, 
                    delay_weeks = 4 ,
                    lags = c(1:3),#c(1:3, 49:52),
                    if_lags = TRUE,
                    if_GT = TRUE,
                    offset_case = 1,
                    offset_GT = 0.0001) {
  if(!if_lags & !if_GT){
    stop("Error! This function returns at least one of the GT/Autoregressive part!")
  }
  # set date
  if(is.null(last_date)){
    end <- (Sys.Date() - wday(Sys.Date()) + 1) %m-% weeks(delay_weeks)
  }else{end <- last_date - wday(last_date) + 1}
  start <- end %m-% years(3)
  
  data_lag<- data %>% select(ew_start, sum_of_cases)
  loop_length <- length(lags)
  
  # Y
  Y <- as.data.frame(data_lag %>% filter(ew_start >= start & ew_start <= end))
  names(Y)[2] <- "Y"
  Y$Y <- log(Y$Y + offset_case)
  
  # lags part
  if(if_lags){
    for(i in 1:loop_length){
      start_lag <- start %m-% weeks(lags[i])
      end_lag <- end %m-% weeks(lags[i])
      
      temp <- data_lag %>% filter(ew_start >= start_lag & ew_start <= end_lag)
      if(i == 1){
        X <- temp
        names(X)[2] <- "lag_1"
      }else{
        X[paste0("lag_",lags[i])] <- temp[,2]
      }
    }
    
    X[,-1] <- apply(X[,-1], 2, function(x) log(x + offset_case))
  }
  
  # GT part
  if(if_GT){
    GT <- data %>% filter(ew_start >= start & ew_start <= end) %>%
      select(topics)
    
    GT <- apply(GT, 2, function(x) log(x + offset_GT))
    
    if(if_lags){
      X <- cbind(X[,-1],GT)
    }else{
      X <- GT
    }
  }else{
    X <- as.data.frame(X[,-1])
  }
  
  return(list(Y, X))
}

run_model_ARGO <- function(merged_data, topics, 
                           alpha_value = 0, offset_lag = 1,
                           K = 4, critical_level = 0.05) {
  # random seed
  set.seed(1)
  # lag designed to be 3
  n_lag = 3; n_GT <- 2
  # get input data for train
  data_temp <- get_InputData(merged_data, topics, delay_weeks = K)

  # Input
  Y_train <- data_temp[[1]][,2]
  X_train <- as.matrix((data_temp[[2]]))
  
  # Find optimal lambda by CV
  cv_fit <- cv.glmnet(X_train, Y_train, grouped = F)
  
  # Best lambda
  best_lambda <- cv_fit$lambda.min
  # Final model
  final_model <- glmnet(X_train, Y_train, 
                        alpha = alpha_value,
                        lambda = best_lambda,
                        standardize = FALSE)
  # Get the date for prediction
  end_pred <- data_temp[[1]][length(Y_train),1] + weeks(K) # this need to be check
  start_pred <- end_pred %m-% years(3) 
  # get input data for pred
  data_temp <- get_InputData(merged_data, topics, delay_weeks = K, last_date = end_pred)
  X_pred <- as.matrix(data_temp[[2]])
  # Get predition
  Y_pred <- predict(final_model, newx = X_pred) 

  # Confidence interval
  var_parameter <- boot.glmnet(X_train, Y_train, lambda = best_lambda, bar = FALSE)
  X_pred_square <- as.data.frame(apply(X_pred, 2, function(x){ x^2 })) # question here
  # Calculate variance
  variance <-sweep(var_parameter, MARGIN = 2, STATS = X_pred_square, FUN = "*") 
  variance$Y_sd <- sqrt(rowSums(variance))
  variance$Y_pred <- as.numeric(Y_pred)
  # Calculate CI
  merged_data_out <- merged_data %>% filter(ew_start >= start_pred & ew_start <= end_pred) %>%
    mutate(ARGO_pred = as.numeric(Y_pred) - offset_lag,
           ARGO_lb = exp(ARGO_pred - qnorm(1-critical_level/2) * variance$Y_sd) - offset_lag,
           ARGO_ub = exp(ARGO_pred + qnorm(1-critical_level/2) * variance$Y_sd) - offset_lag,
           ARGO_pred = exp(ARGO_pred) + 1)
  
  return(merged_data_out)
}


boot.glmnet <- function(X, y, B=500, lambda, seed, alpha=0.05, bar=TRUE) {
  p <- ncol(X)
  n <- nrow(X)
  if (missing(lambda)) {
    cvfit <- cv.glmnet(X, y)
    lambda <- cvfit$lambda.min
    lmax <- max(cvfit$lambda)
  } else {
    fit <- glmnet(X, y, nlambda=5, lambda.min.ratio=0.9)
    lmax <- max(fit$lambda)
  }
  lam_seq <- exp(seq(log(lmax), log(lambda), len=10))
  if (bar) pb <- txtProgressBar(0, B, style=3)
  beta_hat <- matrix(NA, B, p, dimnames=list(1:B, colnames(X)))
  if (!missing(seed)) set.seed(seed)
  for (i in 1:B) {
    ind <- sample(1:n, replace=TRUE)
    fit.i <- glmnet(X[ind,], y[ind], lambda=lam_seq)
    b <- coef(fit.i, s=lambda)
    beta_hat[i,] <- b[-1]
    if (bar) setTxtProgressBar(pb, i)
  }
  if (bar) close(pb)
  out <- as.data.frame(t(apply(beta_hat, 2, function(x){ sd(x)^2})))

  out
}

merged_data <- (process_data(uf, last_ew_start)[[1]])

data <- merged_data[, c(1,3)]

get_InputData(merged_data,topics, if_GT = T, if_lags = F)


### try
a <- run_model_ARGO(merged_data,topics)
b <- run_model(merged_data,topics, gamma = 0.95)
c <- run_model_ARGO(AC,topics)

plot(a[-157,]$sum_of_cases/a[-157,]$ARGO_pred)
abline(h = c(2:4))

plot(a[-157,]$sum_of_cases, a[-157,]$ARGO_pred)

red_points <- x > 80
points(x[red_points], y[red_points], col = "red", pch = 19)
# relationship between real and pred
lm1 <- lm(data = a, a$ARGO_pred ~ a$sum_of_cases)
summary(lm1)
1/(4.058e-01)
8.644e-03

##
plot(c[-157,]$sum_of_cases/c[-157,]$ARGO_pred)
abline(h = c(2:4))

lm2 <- lm(c$ARGO_pred ~ c$sum_of_cases)
summary(lm2)



############################################
# auto.arima

library(forecast)
delay_weeks <- 8
offset_lag <- 1; offset_GT <- 0.001

end <- (Sys.Date() - wday(Sys.Date()) + 1) %m-% weeks(delay_weeks)

start <- end %m-% years(3)


merged_data_temp <- merged_data %>% select(ew_start, sum_of_cases, topics) %>%
  mutate(sum_of_cases = log(sum_of_cases + offset_lag),
         dengue  = log(dengue + offset_GT),
         sintomas.dengue = log(sintomas.dengue + offset_GT))

###
merged_data_temp_ts <- ts(merged_data_temp$sum_of_cases, 
                          start = first(merged_data_temp$ew_start), end = last(merged_data_temp$ew_start))
plot(merged_data_temp_ts)
###
merged_data_temp_train <- merged_data_temp %>% filter(ew_start <= end & ew_start >= start) 
end_pred <- end %m+% weeks(delay_weeks); start_pred <- end_pred %m-% weeks(delay_weeks)
merged_data_temp_pred <- merged_data_temp %>% filter(ew_start <= end_pred & ew_start >= start_pred) 
###
fit <- auto.arima(merged_data_temp_train$sum_of_cases, xreg = as.matrix(merged_data_temp_train[,c(3,4)]))


forec <- forecast(fit, xreg = as.matrix(merged_data_temp_pred[,c(3,4)]), level = 0.95)
exp(forec$mean) - offset_lag

run_model_ARGO <- function(merged_data, topics, 
                          last_date = NULL,
                          offset_lag = 1, offset_GT = 0.001,
                          K = 4, critical_level = 0.05,
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
  forec_out <- cbind(tail(merged_data, K), forec_out)
  
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
  forec_out <- cbind(tail(merged_data, K), forec_out)
  
  forec_out
}

run_model_SAR(merged_data, topics)

run_model_GT <- function(merged_data, topics, 
                           last_date = NULL,
                           offset_lag = 1, offset_GT = 0.001,
                           K = 4, critical_level = 0.05){
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
  formula <- paste("sum_of_cases", "~", paste(topics, collapse = " + "))
  fit <- lm(formula, data = merged_data_temp_train)
  forec <- as.data.frame(predict(fit, newdata = merged_data_temp_pred, interval = "prediction"))
  
  # rearrange
  forec_out <- tibble(GT_pred = as.numeric(exp(forec$fit) - offset_lag), 
                      GT_lb = as.numeric(exp(forec$lwr) - offset_lag),
                      GT_ub = as.numeric(exp(forec$upr) - offset_lag))
  forec_out <- cbind(tail(merged_data, K), forec_out)
  
  forec_out
}

run_model_GT(merged_data, topics)
as.data.frame(forec)$fit

out <- generate_prediction(brazil_ufs)

generate_prediction <- function(ufs, gamma = 0.95, save = T) {
  
  K <- 5
  final_df <- data.frame()
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1
  for (uf in ufs) {
    # print(uf)
    out <- process_data(uf, last_ew_start)
    data <- out[[1]]
    topics <- out[[2]]
    
    if(uf == "ES") K <- 15
    if(uf == "RR"){ next }
    
    d1 <- run_model_ARGO(data, topics, last_date = last_ew_start, K = K, critical_level = gamma)
    d2 <- run_model_GT(data, topics, last_date = last_ew_start, K = K, critical_level = gamma)
    d3 <- run_model_SAR(data, topics, last_date = last_ew_start, K = K, critical_level = gamma)
    merged_data <- cbind(d1,d2[, (ncol(d2)-2):ncol(d2)], d3[, (ncol(d3)-2):ncol(d3)])
    merged_data[nrow(merged_data), "ew"] <- max(merged_data$ew, na.rm=T) + 1
    
    final_df <- rbind(final_df, merged_data)
  }
  
  
  if (save) {
    write.csv(final_df,
              sprintf("data/model_results/model_%s.csv", last_ew_start),
              row.names = F)
  }
  final_df
}


get_Measurement <- function(y_fitted, y , IF_log = F){
  # This function is to get the measurement by giving
  # the fitted(predicted) value and the real value
  # Unscale the value first
  
  if(IF_log == T){
    case <- exp(y) - 1; case_fitted <- exp(y_fitted) - 1
  }else{
    case <- y ; case_fitted <- y_fitted
  }
  
  n <- length(case)
  M <- as.data.frame(matrix(0, nrow = 1, ncol = 5))
  names(M) <- c("RMSE", "MAE", "RMSPE", "MAPE", "CORR")
  
  M[1,1] <- sqrt(mean((case_fitted - case) ^ 2))
  M[1,2] <- mean(abs(case_fitted - case))
  M[1,3] <- sqrt(mean(((case_fitted - case) / case) ^ 2))
  M[1,4] <- mean(abs((case_fitted - case) / case))
  M[1,5] <- cor(case_fitted, case, method="kendall")
  
  return(M)
}

d1$ARGO_pred
data_compare$sum_of_cases

generate_Measurement(brazil_ufs)
generate_Measurement <- function(ufs, K = 4, save = T, compare_length = 1){
  
  if(compare_length >= K){
    stop("Error! The length of prediction to compare should be equal to /smaller than the prediction length(K)!")}
  
  final_df <- data.frame()
  epi_weeks <- seq(202410, 202417, by = 1)
  for(epi_week in epi_weeks){
    if(epi_week >= last(epi_weeks)- K+1){ break }
    # Dates for training model
    last_ew_start <- get_date(week = as.numeric(substr(epi_week,5,6)), year = as.numeric(substr(epi_week,1,4)))
    first_ew_start <- last_ew_start %m-% weeks(K)
    file_last_ew_start <- last_ew_start %m+% weeks(1)
    # Dates for filtering data to compare
    file_last_ew_start_compare <- file_last_ew_start %m+% weeks(K)
    for (uf in ufs) {
      print(uf)
      out_train <- process_data(uf, file_last_ew_start, ew = epi_week)
      data_train <- out_train[[1]]; topics <- out_train[[2]]
      
      out_compare <- process_data(uf, file_last_ew_start_compare, ew = epi_week + K)
      data_compare <- out_compare[[1]] %>% filter(ew_start <=last_ew_start & ew_start > first_ew_start)
      
      if(uf == "ES") { next }
      if(uf == "RR"){ next }
      
      d1 <- run_model_ARGO(data_train, topics, last_date = last_ew_start, K = K, critical_level = gamma)
      d2 <- run_model_GT(data_train, topics, last_date = last_ew_start, K = K, critical_level = gamma)
      d3 <- run_model_SAR(data_train, topics, last_date = last_ew_start, K = K, critical_level = gamma)
      
      out <- tibble(ew_start = data_compare$ew_start, epi_week = epi_week, uf = uf, 
                    True = data_compare$sum_of_cases, ARGO = d1$ARGO_pred, 
                    GT = d2$GT_pred, SAR = d3$SAR_pred)
      out <- tail(out, compare_length)
      # out_measurements <- rbind(ARGO = get_Measurement(d1$ARGO_pred, data_compare$sum_of_cases),
      #                            GT = get_Measurement(d2$GT_pred, data_compare$sum_of_cases),
      #                            SAR = get_Measurement(d3$SAR_pred, data_compare$sum_of_cases))
      
      final_df <- rbind(final_df, out)
      }
    }

  if (save) {
    write.csv(final_df,
              sprintf("data/model_results/model_%s.csv", last_ew_start),
              row.names = F)
    }
  final_df
}



epi_week <- 202410
K <- 4
final_df <- data.frame()
epi_weeks <- seq(202410, 202417, by = 1)
for(epi_week in epi_weeks){
  # if(epi_week <= last(epi_weeks)- 2){ break }
  # Dates for training model
  last_ew_start <- get_date(week = as.numeric(substr(epi_week,5,6)), year = as.numeric(substr(epi_week,1,4)))
  first_ew_start <- last_ew_start %m-% weeks(K)
  file_last_ew_start <- last_ew_start %m+% weeks(1)
  # Dates for filtering data to compare
  file_last_ew_start_compare <- file_last_ew_start %m+% weeks(K)
  for (uf in ufs) {
    print(uf)
    out_train <- process_data(uf, file_last_ew_start, ew = epi_week)
    data_train <- out_train[[1]]; topics <- out_train[[2]]
    
    out_compare <- process_data(uf, file_last_ew_start_compare, ew = epi_week + K)
    data_compare <- out_compare[[1]] %>% filter(ew_start <=last_ew_start & ew_start > first_ew_start)
    
    if(uf == "ES") { next }
    if(uf == "RR"){ next }
    
    d1 <- run_model_ARGO(data_train, topics, last_date = last_ew_start, K = K)
    d2 <- run_model_GT(data_train, topics, last_date = last_ew_start, K = K)
    d3 <- run_model_SAR(data_train, topics, last_date = last_ew_start, K = K)
    
    out <- tibble(ew_start = data_compare$ew_start, epi_week = epi_week, uf = uf, 
                  True = data_compare$sum_of_cases, ARGO = d1$ARGO_pred, GT = d2$GT_pred, SAR = d3$SAR_pred )
    # out_measurements <- rbind(ARGO = get_Measurement(d1$ARGO_pred, data_compare$sum_of_cases),
    #                            GT = get_Measurement(d2$GT_pred, data_compare$sum_of_cases),
    #                            SAR = get_Measurement(d3$SAR_pred, data_compare$sum_of_cases))
    
    final_df <- rbind(final_df, out)
  }
}

