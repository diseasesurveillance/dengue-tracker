############################################################################################
# Input Files
############################################################################################
# Need to specify d$idtime, d$nametime, d$idloc, map$idloc, map$nameloc, map$idloc2, map$nameloc2

#-------------------------------------------------------
# Data. csv files with the following columns
#-------------------------------------------------------

#<idtime> written in year (yyyy), month (yyyy-mm) or day (yyyy-mm-dd) format
#<nametime> it can be the same as idtime
#<idloc> id of location (unique identifier)
#<nameloc> it can be the same as idloc
#<cases> cases for each time and location
#<population> population for each time and location

#-------------------------------------------------------
# Map. Shapefile (shp, dbf, shx and prj) with the following columns
#-------------------------------------------------------

#<idloc> id of location (unique identifier)
#<nameloc> name of location
#<idloc2> id of superarea encompassing a set of <idloc>
#<nameloc2> it can be the same as idloc2

#-------------------------------------------------------
# Notes. <idloc> in the map and the data needs to be the same
#-------------------------------------------------------






############################################################################################
# libraries
############################################################################################


library(leaflet)
library(sf)
library(geobr)
library(ggplot2)
library(plotly)
library(tidyverse) # "%>%"
library(leaflet)
library(apexcharter)
library(highcharter)
library(leaflet.extras)
library(leafgl)
library(shinydashboard)
library(vroom)
library(plotly)
library(viridis)
library(shinyWidgets) # library("shinyWidgets")
library(quantreg)
library(lubridate)
# library(INLA)
# library(spdep)


############################################################################################
# Specify d$idtime, d$nametime, d$idloc, map$idloc, map$nameloc, map$idloc2, map$nameloc2
############################################################################################

brazil_ufs <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)


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


download_infodengue_data <- function(brazil_ufs) {
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1

  for (uf in brazil_ufs) {
    infodengue_data <- denguetracker::fetch_data_from_state(uf,
      ey_start = 2018,
      ey_end = 2024
    )
    filename <- sprintf("%s_%s_infodengue.csv", uf, last_ew_start)
    file_path <- paste0("data/weekly_data/infodengue/", filename)
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

  topics <- colnames(trends)[-1]
  ## to delete:
  trends <- trends[, -ncol(trends)]
  topics <- topics[1:length(topics)-1]
  ##

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


run_model <- function(merged_data, topics, log, K = 5, gamma = 0.95) {
  if (log) {
    merged_data_log <- merged_data
    merged_data_log$sum_of_cases <- log(merged_data$sum_of_cases + 1)
    merged_data_log[topics] <- log(merged_data[topics] + 1)

    formula_str <- paste("sum_of_cases ~ ", paste(topics, collapse = " + "))
    best_linear_transform <- lm(
      as.formula(formula_str),
      merged_data_log[1:(nrow(merged_data_log) - 5), ]
    )
    prediction <- predict(best_linear_transform, merged_data_log)
    error <- abs(prediction[1:(nrow(merged_data) - 5)] - merged_data_log$sum_of_cases[1:(nrow(merged_data) - 5)])
    quantile_error <- quantile(error, probs = 0.95)
    merged_data_log$lwr <- prediction - quantile_error
    merged_data_log$upr <- prediction + quantile_error
    merged_data_log$lwr <- exp(merged_data_log$lwr) - 1
    merged_data_log$upr <- exp(merged_data_log$upr) - 1
    merged_data_log$prediction <- exp(prediction) - 1
    merged_data_log$sum_of_cases <- exp(merged_data_log$sum_of_cases) - 1
    return(merged_data_log)
  }
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

  quantile_error <- quantile(error, probs = 0.95, na.rm = T)
  merged_data$lwr <- prediction_lower - quantile_error
  merged_data$upr <- prediction_upper + quantile_error
  merged_data$prediction <- prediction

  return(merged_data)
}


generate_data <- function(brazil_ufs, log = F) {
  final_df <- data.frame()
  last_ew_start <- Sys.Date() - wday(Sys.Date()) + 1

  for (uf in brazil_ufs) {
    #### Data process
    out <- process_data(uf, last_ew_start)
    data <- out[[1]]
    topics <- out[[2]]

    merged_data <- run_model(data, topics, log)

    final_df <- rbind(final_df, merged_data)
    ##### Plots - Linear scale
    # fig <- plot_data(merged_data, uf)
    ##### Plots - Log scale
    # fig_log <- plot_data(merged_data, uf, log=T)
    # write.csv(final_df, "data/final_df.csv", row.names = F)
  }

  final_df
}
