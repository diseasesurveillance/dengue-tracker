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

# last week
temp_last1 <- get_nth_last(df1, n = 1)

# previous week
temp_last2 <- get_nth_last(df1, n = 2)

# previous 2 weeks
temp_last3 <- get_nth_last(df1, n = 3)

# previous 3 weeks
temp_last4 <- get_nth_last(df1, n = 4)

#
temp <- temp_last4

# process data in the main file 
# and now we store the result
result_table_last2 <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)
result_table_last3 <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)
result_table_last4 <- create_latex_tables(real_time_list, brazil_states_full, latex_code = F)

result_all <- list()
# combine
for (i in 1:length(result_table_last2)) {
  result_all[[i]] <- cbind(result_table_last2[[i]], result_table_last3[[i]], result_table_last4[[i]])
}
names(result_all) <- c("RMSE", "MAE", "RMSPE", "MAPE", 
                       "CR_95", "WD_95", "CR_50", "WD_50",
                       "sMIS", "logScore", "CR_combined")
metrics_df <- result_all

highlight_values_seg(df, type = "min", n = 2, segments = c(5,10))

out <- list(); out_latex <- list()
for (i in 1:10) {
  if(i <= 4){
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "min", n = 2, segments = c(5,10))
  }else if(i == 5 | i == 7){
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "max", n = 1, segments = c(3,6))
  }else{
    out[[names(result_all)[i]]] <- highlight_values_seg(result_all[[i]], type = "min", n = 2, segments = c(3,6))
  }
  latex_temp <- xtable(out[[names(result_all)[i]]], caption = paste(toupper(name), "Comparison"))
  out_latex[[i]] <- print(latex_temp, sanitize.text.function = identity,
                          comment = FALSE, include.rownames = TRUE)
}

out_latex[[i]] <- xtable(out[[names(result_all)[i]]], caption = paste(toupper(name), "Comparison"))
latex_tables[[name]] <- print(latex_table, sanitize.text.function = identity,
                              comment = FALSE, include.rownames = TRUE)

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
