library(sf)
library(geobr)
library(ggplot2)
library(geofacet)
library(tidyverse)
library(leaflet)
library(apexcharter)
library(highcharter)
library(leaflet.extras)
library(leafgl)
library(vroom)
library(plotly)
library(viridis)
library(quantreg)
library(lubridate)
library(rvest)
library(denguetracker)


states_map <- geobr::read_state(showProgress = F)

brazil_ufs <- c(
  "AC", "AL", "AP", "AM",
  "BA", "CE", "DF", "ES",
  "GO", "MA", "MT", "MS",
  "MG", "PA", "PB", "PR",
  "PE", "PI", "RJ", "RN",
  "RS", "RO", "RR", "SC",
  "SP", "SE", "TO"
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
      ey_end = 2025
    )

    filename <- sprintf("%s_%s_infodengue.csv", uf, last_ew_start)
    ew <- max(infodengue_data$ew)
 
    file_path <- sprintf("data/weekly_data/infodengue/%s", ew)
    if (!file.exists(file_path)) {
      dir.create(file_path, recursive=T)
    }

    data_ <- rbind(data_, infodengue_data)
    write.csv(infodengue_data, file.path(file_path, filename), row.names = F)
    cat("\nSuccessfully saved ", filename, "\n")
  }
  filename <- sprintf("BR_%s_infodengue.csv", last_ew_start)
  file_path <- sprintf("data/weekly_data/infodengue/%s/%s", ew, filename)
  result <- data_ |>
    group_by(ew_start) |>
    mutate(sum_of_cases = sum(sum_of_cases), cases_est_id = sum(cases_est_id), cases_est_id_max = sum(cases_est_id_max), cases_est_id_min = sum(cases_est_id_min)) |>
    distinct()  
  write.csv(result, file_path, row.names = F)
  cat("\nSuccessfully saved ", filename, "\n")
}


download_infodengue_data_by_city <- function(brazil_ufs) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1

  for (uf in brazil_ufs) {
    infodengue_data <- denguetracker::fetch_data_from_cities(uf,
                                                            ey_start = 2018,
                                                            ey_end = 2025
    )
    filename <- sprintf("%s_%s_infodengue.csv", uf, last_ew_start)
    ew <- max(infodengue_data$ew)
    file_path <- sprintf("data/weekly_data/infodengue/%s/city/%s", ew, filename)
    write.csv(infodengue_data, file_path, row.names = F)
    cat("\nSuccessfully saved ", filename, "\n")
  }
}

correct_all_country_data_bug <- function(brazil_ufs, ew) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1
  data_ <- data.frame()
  for (uf in brazil_ufs) {
    #if(uf == "ES") { next }
    filename <- sprintf("data/weekly_data/infodengue/%s/%s_%s_infodengue.csv", ew, uf, last_ew_start)
    infodengue_data <- read_csv(filename)
    data_ <- rbind(data_, infodengue_data)
  }
  filename <- sprintf("BR_%s_infodengue.csv", last_ew_start)
  file_path <- sprintf("data/weekly_data/infodengue/%s/%s", ew, filename)
  result <- data_ |>
    select(ew_start, ew, sum_of_cases, cases_est_id, cases_est_id_min,
           cases_est_id_max) |>
    group_by(ew_start) |>
    mutate(sum_of_cases = sum(sum_of_cases), cases_est_id = sum(cases_est_id), cases_est_id_max = sum(cases_est_id_max), cases_est_id_min = sum(cases_est_id_min)) |>
    distinct()  
  write.csv(result, file_path, row.names = F)
  cat("\nSuccessfully saved ", filename, "\n")
}

process_data <- function(uf, last_ew_start, ew = NULL) {
  dir_path <- "data/weekly_data/infodengue"
  dirs <- list.dirs(dir_path, full.names=T)
  if (is.null(ew)) ew <- max(gsub(".*/(\\d+)$", "\\1", gsub("/city", "", dirs))[-1])
  
  gt_filename <- sprintf("data/weekly_data/gtrends/%s/%s_trends.csv", ew, uf)
  cases_filename <- sprintf("%s/%s_%s_infodengue.csv", file.path(dir_path, ew), uf, last_ew_start)
  
  cases <- read.csv(cases_filename, stringsAsFactors = FALSE)
  trends <- read.csv(gt_filename, stringsAsFactors = FALSE, skip = 2)

  colnames(trends) <- gsub("\\.{3}.*$", "", colnames(trends))
  colnames(trends)[colnames(trends) == "Semana"] <- "Week"

  min_week <- min(trends$Week)

  cases <- cases |>
    filter(ew_start >= min_week)
  
  # trends <- trends |>
  #   filter(Week <= max(cases$ew_start))
  cases$ew_start <- as.Date(cases$ew_start)
  trends$Week <- as.Date(trends$Week)
  
  ##
  trends <- trends[, !colnames(trends) %in% "tratamento.dengue"]
  ##
  
  topics <- colnames(trends)[-1]

  merged_data <- merge(cases, trends, by.x = "ew_start", by.y = "Week", all = TRUE)

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

# function for best trans
run_model <- function(merged_data, topics, gamma, K = 4) {
  if (unique(merged_data$uf == "RR")) topics <- c("dengue")
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

# CQR
# before change on (July 9th)
run_model_variables <- function(merged_data, topics, gamma=0.95, K = 5) {
  formula_str <- paste("sum_of_cases ~ meantemp_mean + meanumid_mean + inc_level + transmission + receptive + ",
                       paste(topics, collapse = " + "))
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


# run_model <- function(merged_data, topics, gamma, K = 4) {
#   if (unique(merged_data$uf == "RR")) topics <- c("dengue")
#   formula_str <- paste("sum_of_cases ~ ", paste(topics, collapse = " + "))
#   best_linear_transform <- lm(
#     as.formula(formula_str),
#     merged_data[1:(nrow(merged_data) - K), ]
#   )
#   prediction <- predict(best_linear_transform, merged_data, interval = "predict")
# 
#   merged_data$lwr <- pmax(as.numeric(prediction[,2]), 0)
#   merged_data$upr <- pmax(as.numeric(prediction[,3]), 0)
#   merged_data$prediction <- pmax(as.numeric(prediction[,1]), 0)
# 
#   return(merged_data)
# }

generate_data <- function(ufs,
                          last_ew_start = Sys.Date() - wday(Sys.Date()) + 1,
                          ew = NULL,
                          index_of_queries = c(1,2),
                          gamma = 0.95, year_window = 3,
                          save = F) {
  final_df <- data.frame()
  for (uf in ufs) {
    #cat(last_ew_start, ew)
    out <- process_data(uf, last_ew_start, ew = ew)
    data <- out[[1]]
    topics <- out[[2]][index_of_queries]
    
    K <- 4
    if(uf == "ES") K <- 15
    
    # Filter the data and use last three years to train
    date_fil <- last_ew_start %m-% years(year_window)
    date_fil <- date_fil %m-% weeks(K+1)
    data <- data %>% filter(ew_start >= date_fil)
    
    merged_data <- run_model(data, topics, gamma, K = K)
    if (is.null(merged_data[nrow(merged_data), "sum_of_cases"])) {
      merged_data[nrow(merged_data), "ew"] <- max(merged_data$ew, na.rm=T) + 1
    }
    final_df <- rbind(final_df, merged_data)
  }
  
  final_df <- final_df |>
    select("ew_start", "ew", "sum_of_cases", "cases_est_id", "cases_est_id_min",
           "cases_est_id_max","dengue", "sintomas.dengue", "uf", "lwr", "upr",
           "prediction")
  if (save) {
    write.csv(final_df,
              sprintf("data/model_results/model_%s.csv", last_ew_start),
              row.names = F)
  }
  final_df
}


generate_data_all_country <- function(index_of_queries = c(1,2), 
                                      gamma = 0.95, save = T) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1
  out <- process_data("BR", last_ew_start)
  final_data <- out[[1]]
  topics <- out[[2]][index_of_queries]
  final_data <- final_data |>
    select(ew_start, ew, sum_of_cases, cases_est_id, cases_est_id_min,
           cases_est_id_max, dengue, sintomas.dengue, uf) |>
    unique()
  K <- 5
  merged_data <- run_model(final_data, topics, gamma, K = K)
  merged_data[nrow(merged_data), "ew"] <- max(merged_data$ew, na.rm=T) + 1

  if (save) {
    write.csv(merged_data,
              sprintf("data/model_results/model_%s_%s.csv", last_ew_start, "BR"),
              row.names = F)
  }
  merged_data
}


render_files <- function(folder_root_directory = rprojroot::find_rstudio_root_file()) {
  rmd_files <- list.files(path = "reports", pattern = "\\.Rmd$",
                          full.names = TRUE, recursive = TRUE)
  for (file in rmd_files) {
    filename <- tools::file_path_sans_ext(basename(file))
    rmarkdown::render(input = file, output_file = paste0(folder_root_directory, "/docs/", filename, ".html"))
  }
}


## Variables
# 
# model_preds <- generate_data(brazil_ufs, gamma = 0.95, save=F)
# model_preds_br <- generate_data_all_country(gamma = 0.95, save=F)
# 0.0385*(834.49+407.49) + 0.1151*(492.5+384.5) + 0.2308*(358+94.99+94)
