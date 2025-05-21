# Run the models_series_refined first

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


###### sensitivity test ######
df_2 <- generate_Prediction(brazil_ufs, K = 15, compare_length = 5, save = F,
                           year_window = 2)
