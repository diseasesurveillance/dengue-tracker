library(leaflet)
library(sf)
library(geobr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(leaflet)
library(apexcharter)
library(highcharter)
library(leaflet.extras)
library(leafgl)
library(shinydashboard)
library(vroom)
library(plotly)
library(viridis)
library(shinyWidgets)
library(quantreg)
library(lubridate)
library(rvest)


states_map <- geobr::read_state(showProgress = F)

brazil_ufs <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)

population_state <- read_csv2("data/population_state.csv") |>
  mutate(Uf = str_to_title(Uf))


fnSetTimeLocVariablesDataMap <- function(d, map, dmapvbles) {
  # time
  d$idtime <- d[, dmapvbles[1]]
  d$nametime <- d[, dmapvbles[2]]

  # location 1
  d$idloc <- d[, dmapvbles[3]]
  # cuando hago join map y data pone nameloc.x nameloc.y. Asi que no pongo columna con name en data, solo en map
  # d$nameloc <- d$geocode

  map$idloc <- map[, dmapvbles[4], drop = TRUE]
  map$nameloc <- map[, dmapvbles[5], drop = TRUE]

  # location 2
  map$idloc2 <- map[, dmapvbles[6], drop = TRUE]
  map$nameloc2 <- map[, dmapvbles[7], drop = TRUE]

  # To avoid the following error when doing left_join, remove attribute map
  # Warning: Error in stop_native_implementation: `vec_ptype2.character.character()` is implemented at C level
  attributes(map$idloc) <- NULL
  attributes(map$idloc2) <- NULL


  return(list(d, map))
}


download_infodengue_data_by_state <- function(brazil_ufs) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1
  data_ <- data.frame()
  for (uf in brazil_ufs) {
    infodengue_data <- denguetracker::fetch_data_from_state(uf,
      ey_start = 2018,
      ey_end = 2024
    )
    filename <- sprintf("%s_%s_infodengue.csv", uf, last_ew_start)
    file_path <- paste0("data/weekly_data/infodengue/", filename)
    data_ <- rbind(data_, infodengue_data)
    write.csv(infodengue_data, file_path, row.names = F)
    cat("\nSuccessfully saved ", filename, "\n")
  }
  filename <- sprintf("BR_%s_infodengue.csv", last_ew_start)
  file_path <- paste0("data/weekly_data/infodengue/", filename)
  result <- aggregate(sum_of_cases ~ ew_start, data = data_, FUN = sum)
  write.csv(result, file_path, row.names = F)
  cat("\nSuccessfully saved ", filename, "\n")
}


download_infodengue_data_by_city <- function(brazil_ufs) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1
  
  for (uf in brazil_ufs) {
    infodengue_data <- denguetracker::fetch_data_from_cities(uf,
                                                            ey_start = 2018,
                                                            ey_end = 2024
    )
    filename <- sprintf("%s_%s_infodengue.csv", uf, last_ew_start)
    file_path <- paste0("data/weekly_data/infodengue/city/", filename)
    write.csv(infodengue_data, file_path, row.names = F)
    cat("\nSuccessfully saved ", filename, "\n")
  }
  
}


process_data <- function(uf, last_ew_start) {
  ## delete in future
  last_ew_start <- '2024-02-25'
  ##
  gt_filename <- sprintf("data/weekly_data/gtrends/%s_trends.csv", uf)
  cases_filename <- sprintf("data/weekly_data/infodengue/%s_%s_infodengue.csv", uf, last_ew_start)

  cases <- read.csv(cases_filename, stringsAsFactors = FALSE)
  trends <- read.csv(gt_filename, stringsAsFactors = FALSE, skip = 2)

  colnames(trends) <- gsub("\\.{3}.*$", "", colnames(trends))
  colnames(trends)[colnames(trends) == "Semana"] <- "Week"

  min_week <- min(trends$Week)

  cases <- cases |>
    filter(ew_start >= min_week)

  cases$ew_start <- as.Date(cases$ew_start)
  trends$Week <- as.Date(trends$Week)
  
  ## to delete:
  trends <- trends[, !colnames(trends) %in% c("tratamento.dengue")]
  ##
  
  topics <- colnames(trends)[-1]

  merged_data <- merge(cases, trends, by.x = "ew_start", by.y = "Week")
  
  # Convert all "<1" values to 0 in all columns
  for (col in names(merged_data)) {
    # Skip the ew_start column
    if (col != "ew_start") {
      # Replace "<1" with 0, converting to numeric as necessary
      merged_data[[col]] <- ifelse(merged_data[[col]] == "<1", 0, as.numeric(merged_data[[col]]))
    }
  }

  merged_data$uf <- uf
  return(list(as_tibble(merged_data), topics))
}


run_model <- function(merged_data, topics, gamma, K = 5) {
  formula_str <- paste("sum_of_cases ~ ", paste(topics, collapse = " + "))
  best_linear_transform <- lm(
    as.formula(formula_str),
    merged_data[1:(nrow(merged_data) - K), ]
  )
  prediction <- predict(best_linear_transform, merged_data)

  best_linear_transform_lower <- rq(as.formula(formula_str),
    merged_data[1:(nrow(merged_data) - K), ],
    tau = (1 - gamma) / 2
  )

  prediction_lower <- predict(best_linear_transform_lower, merged_data)


  best_linear_transform_upper <- rq(as.formula(formula_str),
    merged_data[1:(nrow(merged_data) - K), ],
    tau = 1 - (1 - gamma) / 2
  )
  prediction_upper <- predict(best_linear_transform_upper, merged_data)

  error <-
    apply(cbind(
      prediction_lower[1:(nrow(merged_data) - K)] - merged_data$sum_of_cases[1:(nrow(merged_data) - K)],
      merged_data$sum_of_cases[1:(nrow(merged_data) - K)] - prediction_upper[1:(nrow(merged_data) - K)]
    ), 1, max)

  quantile_error <- quantile(error, probs = gamma, na.rm = T)
  merged_data$lwr <- pmax(prediction_lower - quantile_error, 0)
  merged_data$upr <- pmax(prediction_upper + quantile_error, 0)
  merged_data$prediction <- pmax(prediction, 0)

  return(merged_data)
}


generate_data <- function(ufs, gamma = 0.95) {
  
  ## TODO: optimize the way this function is called. Currently, for each
  ## state, we're generating predictions for all states and then filtering
  ## in the Rmd file. It's also overwriting the file with the model results
  ## everytime it's called. Since our model is simple and does not take
  ## much time to run, we are ok for now, but when it gets more complex,
  ## this will be a concern.

  final_df <- data.frame()
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1

  for (uf in ufs) {
    out <- process_data(uf, last_ew_start)
    data <- out[[1]]
    topics <- out[[2]]

    merged_data <- run_model(data, topics, gamma)

    final_df <- rbind(final_df, merged_data)
  }
  last_ew_start <- "2024-02-25"
  write.csv(final_df,
            sprintf("data/model_results/model_%s.csv", last_ew_start),
            row.names = F)
  final_df
}


generate_data_all_country <- function(gamma = 0.95) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1
  
  out <- process_data("BR", last_ew_start)
  final_data <- out[[1]]
  topics <- out[[2]]
  
  merged_data <- run_model(final_data, topics, gamma)
  
  last_ew_start <- "2024-02-25"
  write.csv(merged_data,
            sprintf("data/model_results/model_%s_%s.csv", last_ew_start, "BR"),
            row.names = F)
  merged_data
}
