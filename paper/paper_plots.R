library(dplyr)
library(stringr)

plot_geofacet_series <- function(merged_data, K = 5) {
  data <- merged_data |> filter(ew_start >= as.Date("2023-12-25"))
  data$day <- as.Date(format(data$ew_start,"%d/%m"), "%d/%m")
  #date_no_delay <- data[nrow(data) - K, ]$ew_start
  
  ggplot(data) +
    geom_line(data=data,
      aes(
        x = ew_start, y = sum_of_cases, group = 1,
        colour = "Suspected Cases \n (subject to delays)"
      ),
      size = 1
    ) +
    geom_line(data=data,
      aes(
        x = ew_start, y = True, group = 1,
        colour = "Baseline"
      ),
      size = 1
    ) +
    
    # geom_line(data = data |> filter(ew_start <= date_no_delay), aes(
    #   x = ew_start, y = prediction,
    #   group = 1, color = "Fitted Model"
    # ), linetype = 1, size = .5) +
    geom_line(data=data  ,aes(x = day,y = prediction, 
                                                                       group = 1, color = "Estimate via Google Trends \n (95% C.I.)"),size=1) +
    geom_line(data=data  ,aes(x = day,y = DCGT_pred, 
                                                               group = 1, color = "Estimate via DCGT"),size=1) +
    geom_line(data=data  ,aes(x = day,y = DC_pred, 
                                                               group = 1, color = "Estimate via DC"),size=1) +
    geom_ribbon(data=data  ,aes(x = day,ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
    geom_line(data=data  ,aes(x = day,y = cases_est_id, 
                                                                       group = 2, color = "Estimate via InfoDengue"),size=1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, colour = "black",
                                     vjust = 1, hjust = 1),
          axis.title = element_blank(),
          legend.key.height= unit(1.2, 'cm'),
          legend.key.width= unit(1.5, 'cm'),
          legend.text = element_text(size=15),
          legend.position = c(0.15, 0.15)) +
    scale_x_date(date_labels = "%B",date_breaks = "1 month")+
    scale_y_continuous(expand = c(0, 0), limits=c(0, NA), labels = scales::unit_format(scale = 1e-3, unit = "k")) +
    scale_colour_manual("", 
                        breaks = c("Suspected Cases \n (subject to delays)", "Fitted Model", 
                                   "Estimate via Google Trends \n (95% C.I.)","Estimate via InfoDengue",
                                   "Estimate via DCGT", "Estimate via DC", "Baseline"),
                        values = c("Suspected Cases \n (subject to delays)" = "#ffa600", 
                                   "Fitted Model"="#003f5c",
                                   "Estimate via Google Trends \n (95% C.I.)" = "#ef5675",
                                   "Estimate via InfoDengue" = "#7a5195",
                                   "Estimate via DCGT" = "#f9ff70",
                                   "Estimate via DC" = "#039fdb",
                                   "Baseline" = "#00FF00")) +
    
    
    facet_geo(~ uf, grid = "br_states_grid1", label = "name", scale = "free_y") +
    theme(strip.text = element_text(face="bold", size=11))
}


plot_trends_data <- function(merged_data, state, K = 5) {
  #if (uf == "ES") K <- 15
  date_no_delay <- merged_data[nrow(merged_data) - K, ]$ew_start
  merged_data <- merged_data |> filter((ew_start >= "2023-12-31") & (uf == state))
  
  merged_data <- merged_data |>
    mutate(desc_cases = paste("Week: ", ew_start, "\nEpidemiological Week : ", ew %% 100, "\nSuspected cases : ", round(sum_of_cases))) |>
    mutate(desc_id = paste("Week: ", ew_start, "\nEpidemiological Week : ", ew %% 100, "\nEstimated cases : ", cases_est_id,
                           "\nSource: InfoDengue")) |>
    mutate(desc_gt = paste("Week: ", ew_start, "\nEpidemiological Week : ", ew %% 100, "\nEstimated cases : ", round(prediction),
                           "\nSource: Our model"))
  
  fig <- ggplot(merged_data) +
    geom_line(
      aes(
        x = ew_start, y = sum_of_cases, group = 1,
        colour = "Suspected Cases \n (subject to delays)",
        text = desc_cases
      ),
      size = 1
    ) +
    geom_line(
      aes(
        x = ew_start, y = True, group = 1,
        colour = "True number"
      ),
      size = 1
    ) +
    geom_line(data = merged_data |> filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = prediction, group = 1, color = "Fitted Model", text = desc_gt), 
      linetype = 1, size = .5) +
    geom_line(data = merged_data |> filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = ARGO_pred, group = 1, color = "SARIMAX Fitted Model"), 
      linetype = 1, size = .5) +
    geom_line(data = merged_data |> filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = SAR_pred, group = 1, color = "SAR Fitted Model"), 
      linetype = 1, size = .5) +
    geom_ribbon(
      data = merged_data |> filter(ew_start <= date_no_delay), aes(x = ew_start, ymin = lwr, ymax = upr),
      linetype = 2, size = .5, alpha = 0.1, fill = "#003f5c",color="#003f5c"
    ) +
    geom_line(data = merged_data |> filter(ew_start >= date_no_delay), aes(
      x = ew_start, y = prediction,
      group = 1, color = "Estimate via Google Trends \n (95% C.I.)", text = desc_gt
    ), size = 1) +
    geom_ribbon(
      data = merged_data |> filter(ew_start >= date_no_delay), aes(x = ew_start, ymin = lwr, ymax = upr),
      fill = "#D81B60", linetype = 2, alpha = 0.3
    ) +
    geom_line(data=merged_data |> filter(ew_start>=date_no_delay),aes(x = ew_start,y = ARGO_pred, 
                                                               group = 1, color = "Estimate via SARIMAX"),size=1) +
    geom_line(data=merged_data |> filter(ew_start>=date_no_delay),aes(x = ew_start,y = SAR_pred, 
                                                               group = 1, color = "Estimate via SAR"),size=1) +
    geom_line(data = merged_data |> filter(ew_start >= date_no_delay), aes(
      x = ew_start, y = cases_est_id,
      group = 1, color = "Estimate via InfoDengue", text = desc_id
    ), size = 1) +
    labs(
      x = "", y = "Number of weekly suspected cases",
      caption = paste0(
        "Last updated using reported suspected cases until ",
        max(merged_data$ew_start[!is.na(merged_data$sum_of_cases)]),
        "\n and Google Trends data until ", max(merged_data$ew_start),
        "\n More information: https://diseasesurveillance.github.io/dengue-tracker"
      )
    )   +
    theme(
      axis.text.x = element_text(size = 18), legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18), plot.title = element_text(size = 12)
    ) +
    theme_bw() +
    scale_x_date(
      date_breaks = "2 month",
      date_labels = "%b/%y"
    ) +
    scale_colour_manual("",
                        breaks = c("Suspected Cases \n (subject to delays)", "Fitted Model", 
                                   "Estimate via Google Trends \n (95% C.I.)","Estimate via InfoDengue",
                                   "Estimate via SARIMAX", "Estimate via SAR", "True number"),
                        values = c("Suspected Cases \n (subject to delays)" = "#ffa600", 
                                   "Fitted Model"="#003f5c",
                                   "Estimate via Google Trends \n (95% C.I.)" = "#ef5675",
                                   "Estimate via InfoDengue" = "#7a5195",
                                   "Estimate via SARIMAX" = "#f9ff70",
                                   "Estimate via SAR" = "#039fdb",
                                   "True number" = "#00FF00")) +
    theme(
      legend.position = c(.25, .8),
      legend.key.size = unit(1.2, "line"),
      legend.key.width = unit(2, "line"),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(
        size = 12, angle = 45, colour = "black",
        vjust = 1, hjust = 1
      ),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "gray79",
        size = 0.25,
        linetype = 4
      )
    ) +
    coord_cartesian(expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(ifelse(uf == "Brazil", "Dengue in Brazil", paste("Dengue in", uf, "(BR)")))
}


plot_map_best_metric <- function(model_results, states_map, metric) {
  states <- names(model_results)

  lowest_df <- tibble(name_state = character(),
                       best_model = character())

  for(state in states) {
    if (state %in% c("Espírito Santo", "Roraima")) next;
    #lowest_MAE$State <- state
    lowest <- model_results[[state]]$Models[[which.min(model_results[[state]][[metric]])]]
    
    row <- tibble(name_state = str_to_title(state), best_model = lowest)
    
    lowest_df <- add_row(lowest_df, row)
  }
  
  merged_data <- states_map %>%
    left_join(lowest_df, by = c("name_state" = "name_state"))
  
  # Plot the merged data
  ggplot(data = merged_data) +
    geom_sf(aes(fill = best_model)) +
    theme_bw() +
    theme(
      panel.background = element_blank(),      # Remove background of the plot panel
      panel.grid.major = element_blank(),       # Remove major grid lines
      panel.grid.minor = element_blank(),       # Remove minor grid lines
      plot.background = element_blank(),        # Remove background of the entire plot
      axis.text = element_blank(),              # Remove axis text
      axis.title = element_blank(),             # Remove axis titles
      axis.ticks = element_blank(),             # Remove axis ticks
      panel.border = element_blank()
    ) +
    labs(fill = "Best Model",
         title = sprintf("Best Model by State (Based on %s)", metric))
}
