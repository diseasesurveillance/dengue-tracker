# Run the models_series_refined first
highlight_values_seg <- function(df, type = c("min", "max"), n = 1, segments = NULL) {
  type <- match.arg(type)
  if (!(n %in% c(1, 2))) stop("n must be either 1 or 2")
  mat <- as.matrix(df)
  nr <- nrow(mat); nc <- ncol(mat)
  
  # Define column segments
  if (is.null(segments)) {
    seg_indices <- list(1:nc)
  } else {
    # segments should be a vector of breakpoints, e.g. c(4,8)
    breaks <- sort(unique(segments))
    seg_indices <- list(
      seq_len(breaks[1]),
      if (length(breaks) > 1) seq(breaks[1]+1, breaks[2]) else integer(0),
      if (length(breaks) > 1) seq(breaks[length(breaks)]+1, nc) else seq(breaks[1]+1, nc)
    )
    # remove any empty segments
    seg_indices <- Filter(function(x) length(x) > 0, seg_indices)
  }
  
  # Process each row
  out <- t(apply(mat, 1, function(x) {
    row_out <- as.character(x)
    # For each segment, find target values and highlight
    for (seg in seg_indices) {
      vals <- x[seg]
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) next
      
      if (type == "min") {
        ranked <- sort(vals, na.last = NA)
      } else {
        ranked <- sort(vals, decreasing = TRUE, na.last = NA)
      }
      target_vals <- ranked[1:n]
      
      # apply highlighting within this segment
      for (j in seq_along(seg)) {
        col <- seg[j]
        if (!is.na(x[col])) {
          if (x[col] == target_vals[1]) {
            row_out[col] <- paste0("\\textcolor{red}{", x[col], "}")
          } else if (n == 2 && length(target_vals) >=2 && x[col] == target_vals[2]) {
            row_out[col] <- paste0("\\textcolor{blue}{", x[col], "}")
          }
        }
      }
    }
    row_out
  }))
  
  # return as data.frame with original column names
  out_df <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out_df) <- names(df)
  rownames(out_df) <- rownames(df)
  return(out_df)
}

highlight_values_seg <- function(df, type = c("min", "max", "closest"), n = 1, segments = NULL, nominal = 0.95) {
  type <- match.arg(type)
  if (!(n %in% c(1, 2))) stop("n must be either 1 or 2")
  mat <- as.matrix(df)
  nr <- nrow(mat); nc <- ncol(mat)
  
  # Define column segments
  if (is.null(segments)) {
    seg_indices <- list(1:nc)
  } else {
    breaks <- sort(unique(segments))
    seg_indices <- list(
      seq_len(breaks[1]),
      if (length(breaks) > 1) seq(breaks[1] + 1, breaks[2]) else integer(0),
      if (length(breaks) > 1) seq(breaks[length(breaks)] + 1, nc) else seq(breaks[1] + 1, nc)
    )
    seg_indices <- Filter(function(x) length(x) > 0, seg_indices)
  }
  
  # Process each row
  out <- t(apply(mat, 1, function(x) {
    row_out <- as.character(x)
    for (seg in seg_indices) {
      vals <- x[seg]
      vals_valid <- vals[!is.na(vals)]
      if (length(vals_valid) == 0) next
      
      # Identify target values
      if (type %in% c("min", "max")) {
        ranked <- sort(vals_valid, decreasing = (type == "max"), na.last = NA)
        target_vals <- ranked[1:n]
      } else if (type == "closest") {
        diffs <- abs(vals_valid - nominal)
        min_diff <- min(diffs)
        candidate_idx <- which(abs(vals - nominal) == min_diff)
        candidate_vals <- vals[candidate_idx]
        if (length(candidate_vals) > 1) {
          best_val <- max(candidate_vals, na.rm = TRUE)
          target_vals <- candidate_vals[abs(candidate_vals - nominal) == min_diff & candidate_vals == best_val]
        } else {
          target_vals <- candidate_vals
        }
      }
      
      # Apply coloring
      for (j in seq_along(seg)) {
        col <- seg[j]
        if (!is.na(x[col]) && x[col] %in% target_vals) {
          row_out[col] <- paste0("\\textcolor{red}{", x[col], "}")
        } else if (n == 2 && type %in% c("min", "max") &&
                   length(target_vals) >= 2 && !is.na(x[col]) && x[col] == target_vals[2]) {
          row_out[col] <- paste0("\\textcolor{blue}{", x[col], "}")
        }
      }
    }
    row_out
  }))
  
  out_df <- as.data.frame(out, stringsAsFactors = FALSE)
  colnames(out_df) <- colnames(df)
  rownames(out_df) <- rownames(df)
  return(out_df)
}



get_nth_last <- function(df, n = 1) {
  df %>%
    group_by(ew_pred, uf) %>%
    # rank all the ew
    arrange(desc(ew)) %>%
    mutate(rn = row_number()) %>%
    # only remains the exact last nth week
    filter(rn == n) %>%
    ungroup() %>%
    select(-rn)
}

get_metrics_by_states <- function(temp){
  # to avoid Inf
  temp$Naive  <- ifelse(temp$Naive == 0, 1, temp$Naive )
  ########### get metrics table ###########
  model_preds_metrics <- temp %>% select(True, DCGT_pred, DC_pred, GT_prediction, cases_est_id, Naive,
                                         DCGT_CoverageRate_95, DC_CoverageRate_95, GT_CoverageRate_95, 
                                         DCGT_CoverageRate_50, DC_CoverageRate_50, GT_CoverageRate_50, 
                                         DCGT_CI_WD_95, DC_CI_WD_95, GT_CI_WD_95, 
                                         DCGT_CI_WD_50, DC_CI_WD_50, GT_CI_WD_50, 
                                         uf, GT_lwr_95, GT_upr_95, GT_lwr_50, GT_upr_50,  
                                         DCGT_lwr_95, DCGT_upr_95, DCGT_lwr_50, DCGT_upr_50,
                                         DC_lwr_95, DC_upr_95, DC_lwr_50, DC_upr_50,
                                         cases_est_id_min, cases_est_id_max) %>%
    rename(Real_value = True, DCGT = DCGT_pred, DC = DC_pred, GT = GT_prediction, InfoDengue = cases_est_id, 
           InfoDengue_lwr = cases_est_id_min, InfoDengue_upr = cases_est_id_max) %>%
    as.data.frame()

  real_time_list <- list()
  
  count <- 1
  for (state in brazil_ufs) {
    print(state)
    if(state == "ES"){  
      real_time_list[[brazil_states_full[count]]] <- compare_Measurement(model_preds_metrics[which(model_preds_metrics$uf == state),], 
                                                                         relative_to_naive = F,
                                                                         if_sMIS = F, if_logScore = F)
    }else{
      real_time_list[[brazil_states_full[count]]] <- compare_Measurement(model_preds_metrics[which(model_preds_metrics$uf == state),], 
                                                                         relative_to_naive = F)
    }
    count <- count + 1
  }
  real_time_list
}

# last week
temp_last1 <- get_nth_last(df1, n = 1)

# previous week
temp_last2 <- get_nth_last(df1, n = 2)

# previous 2 weeks
temp_last3 <- get_nth_last(df1, n = 3)

# previous 3 weeks
temp_last4 <- get_nth_last(df1, n = 4)

#
m1 <- get_metrics_by_states(temp_last1)

m2 <- get_metrics_by_states(temp_last2)

m3 <- get_metrics_by_states(temp_last3)

m4 <- get_metrics_by_states(temp_last4)
# process data in the main file 
# and now we store the result
result_table_last1 <- create_latex_tables(m1, brazil_states_full, latex_code = F)
result_table_last2 <- create_latex_tables(m2, brazil_states_full, latex_code = F)
result_table_last3 <- create_latex_tables(m3, brazil_states_full, latex_code = F)
result_table_last4 <- create_latex_tables(m4, brazil_states_full, latex_code = F)

result_all <- list()
# combine
for (i in 1:length(result_table_last2)) {
  result_all[[i]] <- cbind(result_table_last2[[i]], result_table_last3[[i]], result_table_last4[[i]])
}
names(result_all) <- c("RMSE", "MAE", "RMSPE", "MAPE", 
                       "CR_95", "WD_95", "CR_50", "WD_50",
                       "sMIS", "logScore", "CR_combined")
metrics_df <- result_all
names(result_all)

highlight_values_seg(df, type = "min", n = 2, segments = c(5,10))

out <- list(); out_latex <- list()
for (i in 1:10) {
  if(i <= 4){
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "min", n = 2, segments = c(5,10))
  }else if(i == 5){
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "closest", n = 1, segments = c(3,6), nominal = 0.95)
  }else if(i == 7){
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "closest", n = 1, segments = c(3,6), nominal = 0.5)
  }else{
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "min", n = 2, segments = c(3,6))
  }
  latex_temp <- xtable(out[[names(result_all)[i]]], caption = paste(toupper(name), "Comparison"))
  out_latex[[i]] <- print(latex_temp, sanitize.text.function = identity,
                          comment = FALSE, include.rownames = TRUE)
}



###### sensitivity test ######
df_y2 <- generate_Prediction(brazil_ufs, K = 15, compare_length = 5, save = F,
                           year_window = 2)

df_y1 <- generate_Prediction(brazil_ufs, K = 15, compare_length = 5, save = F,
                            year_window = 1)

epi_weeks <- c(seq(202410, 202452, by = 1), seq(202501, 202518, by = 1))
for ( i in 1:length(epi_weeks)){
  print(epi_weeks[i])
  print(i)
}



