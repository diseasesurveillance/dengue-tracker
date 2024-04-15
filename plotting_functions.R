state_level_cloropleth <- function(predicted_cases, states_map) {
  cases <- predicted_cases |>
    filter(ew_start == max(ew_start)) |>
    select(uf, sum_of_cases, prediction)

  mapF <- merge(
    merge(cases, states_map, by.x = "uf", by.y = "abbrev_state"),
    population_state,
    by.x = "name_state",
    by.y = "Uf"
  ) |> mutate(prevalence = (prediction/Pop)*10^5)
  
  labelOptionss <- labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")

  pal <- colorNumeric("YlOrRd", domain = mapF$prevalence)
  labels <- sprintf("<strong> %s </strong> <br/> Prediction: %g suspected cases <br/> Prevalence per 100k hab: %g", mapF$name_state, round(mapF$prediction), round(mapF$prevalence)) |>
    lapply(htmltools::HTML)

  mapF <- st_as_sf(mapF)
  leaflet(mapF) |>
    addTiles() |>
    addPolygons(
      fillColor = ~ pal(prevalence), layerId = ~uf, group = "poly",
      weight = 1, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
      label = labels, labelOptions = labelOptionss,
      highlightOptions = highlightOptions(weight = 3, color = "gray", dashArray = "", fillOpacity = 0.7, bringToFront = FALSE)
    ) |>
    addLegend(pal = pal, values = ~prevalence, opacity = 0.7, position = "bottomright", title = "Estimated cases<br/>per 100k hab.") |>
    addSearchFeatures(targetGroups = "poly", options = searchFeaturesOptions(zoom = 7, autoCollapse = TRUE, openPopup = TRUE))
}


panel_plot_states <- function(merged_data, uf, K = 5) {
  if (uf == "ES") K <- 15
  date_no_delay <- merged_data[nrow(merged_data) - K, ]$ew_start
  merged_data$year <- format(as.Date(merged_data$ew_start, format="%d/%m/%Y"),"%Y")
  merged_data$day <- as.Date(format(merged_data$ew_start,"%d/%m"), "%d/%m")

  merged_data <- merged_data |>
    mutate(desc_cases = paste("Week: ", ew_start, "\nEpidemiological Week: ", ew %% 100, "\nSuspected cases: ", 
                              round(sum_of_cases))) |>
    mutate(desc_id = paste("Week: ", ew_start, "\nEpidemiological Week : ", ew %% 100, "\nEstimated cases : ", cases_est_id,
                           "\nSource: InfoDengue")) |>
    mutate(desc_gt = paste("Week: ", ew_start, "\nEpidemiological Week : ", ew %% 100, "\nEstimated cases : ", round(prediction),
                           "\nSource: Our model"))

  fig <- ggplot(merged_data) +
    geom_line(aes(x=day,y = sum_of_cases, group = 1, 
                  colour = "Suspected Cases \n (subject to delays)"),
              size=1) +
    geom_line(data=merged_data |> filter(ew_start<=date_no_delay),
              aes(x = day,y = prediction, group = 1, color = "Fitted Model", text = desc_gt),
              linetype=1, size = .5) +
    geom_ribbon(data=merged_data |> filter(ew_start<=date_no_delay),aes(x = day,ymin=lwr, ymax=upr),
                linetype=2, size = .5,alpha=0.1,fill = "#003f5c",color="#003f5c")+
    geom_line(data=merged_data |> filter(ew_start>=date_no_delay),
              aes(x = day,y = prediction, group = 1, color = "Estimate via Google Trends \n (95% C.I.)", text = desc_gt),
              size=1) +
    geom_ribbon(data=merged_data |> filter(ew_start>=date_no_delay),aes(x = day,ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
    geom_line(data=merged_data |> filter(ew_start>=date_no_delay %m-% weeks(3)),
              aes(x = day,y = cases_est_id, group = 2, color = "Estimate via InfoDengue", text = desc_id), 
              size=1) +
    labs(x = "",
         y = "Number of weekly suspected cases",
         caption = paste0(
           "Last updated using reported suspected cases until ",
           max(merged_data$ew_start[!is.na(merged_data$sum_of_cases)]),
           "\n and Google Trends data until ", max(merged_data$ew_start),
           "\n More information: https://diseasesurveillance.github.io/dengue-tracker"
         )
    ) +
    theme(axis.text.x = element_text(size=18), legend.text = element_text(size = 14),
          legend.title = element_text( size = 16,face="bold"),
          axis.title=element_text(size=18),plot.title = element_text(size=12))+
    theme_bw()+
    scale_x_date(date_labels = "%B",date_breaks = "3 month")+
    scale_colour_manual("", 
                        breaks = c("Suspected Cases \n (subject to delays)", "Fitted Model", "Estimate via Google Trends \n (95% C.I.)","Estimate via InfoDengue"),
                        values = c("Suspected Cases \n (subject to delays)" = "#ffa600", 
                                   "Fitted Model"="#003f5c",
                                   "Estimate via Google Trends \n (95% C.I.)" = "#ef5675",
                                   "Estimate via InfoDengue" = "#7a5195")) +
    theme(legend.position = "top",
          legend.key.size = unit(1.2,"line"),
          legend.key.width= unit(2, 'line'),
          legend.text=element_text(size=12),
          axis.text.x = element_text(size = 12, angle = 45, colour = "black",
                                     vjust = 1, hjust = 1), 
          axis.title = element_text(size = 12), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(color = "gray79",
                                            size = 0.25, 
                                            linetype = 4),
          strip.text = element_text(face = "bold"),
          strip.background = element_blank(),
          panel.spacing = unit(2, "lines"))+
    coord_cartesian(expand = FALSE) +  
    scale_y_continuous(labels = scales::comma)+
    ggtitle(ifelse(uf == "Brazil", "Panel plot - Dengue in Brazil", 
                   paste("Panel plot - Dengue in", uf, "(BR)")))+
    facet_wrap(facets = year ~ .)
  ggplotly(fig, tooltip = "text") |>
    config(displayModeBar = F)
}


plot_trends_data <- function(merged_data, uf, K = 5) {
  if (uf == "ES") K <- 15
  date_no_delay <- merged_data[nrow(merged_data) - K, ]$ew_start
  merged_data <- merged_data |> filter(ew_start >= "2021-01-01")
  
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
    geom_line(data = merged_data |> filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = prediction, group = 1, color = "Fitted Model", text = desc_gt), 
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
    geom_line(data = merged_data |> filter(ew_start >= date_no_delay %m-% weeks(3)), aes(
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
                        breaks = c("Suspected Cases \n (subject to delays)", "Fitted Model", "Estimate via Google Trends \n (95% C.I.)","Estimate via InfoDengue"),
                        values = c("Suspected Cases \n (subject to delays)" = "#ffa600", 
                                   "Fitted Model"="#003f5c",
                                   "Estimate via Google Trends \n (95% C.I.)" = "#ef5675",
                                   "Estimate via InfoDengue" = "#7a5195")) +
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
  ggplotly(fig, tooltip = "text") |>
    config(displayModeBar = F)
}


plot_geofacet <- function(merged_data, K = 5) {
  data <- merged_data |> filter(ew_start >= as.Date("2023-12-25"))
  data$day <- as.Date(format(data$ew_start,"%d/%m"), "%d/%m")
  date_no_delay <- data[nrow(data) - K, ]$ew_start
  
  ggplot(data) +
    geom_line(
      aes(
        x = ew_start, y = sum_of_cases, group = 1,
        colour = "Suspected Cases \n (subject to delays)"
      ),
      size = 1
    ) +
    geom_line(data = data |> filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = prediction,
      group = 1, color = "Fitted Model"
    ), linetype = 1, size = .5) +
    geom_line(data=data |> filter(ew_start>=date_no_delay),aes(x = day,y = prediction, 
                                                                       group = 1, color = "Estimate via Google Trends \n (95% C.I.)"),size=1) +
    geom_ribbon(data=data |> filter(ew_start>=date_no_delay),aes(x = day,ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
    geom_line(data=data |> filter(ew_start>=date_no_delay %m-% weeks(3)),aes(x = day,y = cases_est_id, 
                                                                       group = 2, color = "Estimate via InfoDengue"),size=1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, colour = "black",
                                     vjust = 1, hjust = 1),
          axis.title = element_blank(),
          legend.key.height= unit(2, 'cm'),
          legend.key.width= unit(2, 'cm'),
          legend.text = element_text(size=15),
          legend.position = c(0.15, 0.15)) +
    scale_x_date(date_labels = "%B",date_breaks = "1 month")+
    scale_y_continuous(expand = c(0, 0), limits=c(0, NA), labels = scales::unit_format(scale = 1e-3, unit = "k")) +
    scale_colour_manual("", 
                        breaks = c("Suspected Cases \n (subject to delays)", "Fitted Model", "Estimate via Google Trends \n (95% C.I.)","Estimate via InfoDengue"),
                        values = c("Suspected Cases \n (subject to delays)" = "#ffa600", 
                                   "Fitted Model"="#003f5c",
                                   "Estimate via Google Trends \n (95% C.I.)" = "#ef5675",
                                   "Estimate via InfoDengue" = "#7a5195")) +
    
    facet_geo(~ uf, grid = "br_states_grid1", label = "name", scale = "free_y") +
    theme(strip.text = element_text(face="bold", size=11))
}

plot_geofacet_2 <- function(merged_data, states_map, K = 5) {
  data <- merged_data |> filter(ew_start >= as.Date("2023-12-25"))
  data <- merge(
    merge(data, states_map, by.x = "uf", by.y = "abbrev_state"),
    population_state,
    by.x = "name_state",
    by.y = "Uf"
  ) |> mutate(prevalence = (prediction/Pop)*10^5,
              id_prev = (cases_est_id/Pop)*10^5,
              cases_prev = (sum_of_cases/Pop)*10^5)
  
  data$day <- as.Date(format(data$ew_start,"%d/%m"), "%d/%m")
  
  date_no_delay <- data[nrow(data) - K, ]$ew_start
  
  ggplot(data) +
    geom_line(
      aes(
        x = ew_start, y = cases_prev, group = 1,
        colour = "Suspected Cases \n (subject to delays)"
      ),
      size = 1
    ) +
    geom_line(data = data |> filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = prevalence,
      group = 1, color = "Fitted Model"
    ), linetype = 1, size = .5) +
    geom_line(data=data |> filter(ew_start>=date_no_delay),aes(x = day,y = prevalence, 
                                                               group = 1, color = "Estimate via Google Trends \n (95% C.I.)"),size=1) +
    # geom_ribbon(data=data |> filter(ew_start>=date_no_delay),aes(x = day,ymin=lwr, ymax=upr),
    #             fill = "#D81B60", linetype=2, alpha=0.3)+
    geom_line(data=data |> filter(ew_start>=date_no_delay),aes(x = day,y = id_prev, 
                                                               group = 2, color = "Estimate via InfoDengue"),size=1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, colour = "black",
                                     vjust = 1, hjust = 1),
          axis.title = element_blank(),
          legend.key.height= unit(2, 'cm'),
          legend.key.width= unit(2, 'cm'),
          legend.text = element_text(size=15),
          legend.position = c(0.15, 0.15)) +
    scale_x_date(date_labels = "%B",date_breaks = "1 month")+
    scale_y_continuous(expand = c(0, 0), limits=c(0, NA), labels = scales::unit_format(scale = 1e-3, unit = "k")) +
    scale_colour_manual("", 
                        breaks = c("Suspected Cases \n (subject to delays)", "Fitted Model", "Estimate via Google Trends \n (95% C.I.)","Estimate via InfoDengue"),
                        values = c("Suspected Cases \n (subject to delays)" = "#ffa600", 
                                   "Fitted Model"="#003f5c",
                                   "Estimate via Google Trends \n (95% C.I.)" = "#ef5675",
                                   "Estimate via InfoDengue" = "#7a5195")) +
    
    facet_geo(~ uf, grid = "br_states_grid1", label = "name") +
    theme(strip.text = element_text(face="bold", size=11))
}