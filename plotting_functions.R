fnMapFiltered <- function(vSelectTime, inputSelectLoc2, inputSelectVble) {
  dtime <- d[which(d$idtime == vSelectTime), ]
  map <- left_join(map, dtime, by = c("idloc" = "idloc"))
  map <- map[which(map$idloc2 == inputSelectLoc2), ]

  map$vble <- map[, inputSelectVble, drop = TRUE]
  return(map)
}


fnLeafletMap <- function(mapF) {
  labelOptionss <- labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")

  # mapF <- mapFiltered()
  pal <- colorNumeric("YlOrRd", domain = mapF$vble)
  labels <- sprintf("<strong> %s </strong> <br/> %g ", mapF$nameloc, mapF$vble) %>%
    lapply(htmltools::HTML)

  leaflet(mapF) %>%
    addTiles() %>% # setView(lng = latIni, lat = longIni, zoom = zoomIni) %>%
    addPolygons(
      fillColor = ~ pal(vble), layerId = ~idloc, group = "poly",
      weight = 1, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
      label = labels, labelOptions = labelOptionss,
      highlightOptions = highlightOptions(weight = 3, color = "gray", dashArray = "", fillOpacity = 0.7, bringToFront = FALSE)
    ) %>%
    addLegend(pal = pal, values = ~vble, opacity = 0.7, title = NULL, position = "bottomright") %>%
    addSearchFeatures(targetGroups = "poly", options = searchFeaturesOptions(zoom = 7, autoCollapse = TRUE, openPopup = TRUE))
}


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
  labels <- sprintf("<strong> %s </strong> <br/> Prediction: %g cases <br/> Prevalence per 100k hab: %g", mapF$name_state, round(mapF$prediction), round(mapF$prevalence)) |>
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
  date_no_delay <- merged_data[nrow(merged_data) - K, ]$ew_start
  merged_data$year <- format(as.Date(merged_data$ew_start, format="%d/%m/%Y"),"%Y")
  merged_data$day <- as.Date(format(merged_data$ew_start,"%d/%m"), "%d/%m")

  ggplot(merged_data) +
    geom_line(aes(x=day,y = sum_of_cases, group = 1, 
                  colour = "Suspected Cases \n (subject to delays)"),
              size=1) +
    geom_line(data=merged_data %>% filter(ew_start<=date_no_delay),aes(x = day,y = prediction, 
                                                                       group = 1, color = "Fitted Model"),linetype=1, size = .5) +
    geom_ribbon(data=merged_data %>% filter(ew_start<=date_no_delay),aes(x = day,ymin=lwr, ymax=upr),
                linetype=2, size = .5,alpha=0.1,fill = "#004D40",color="#1E88E5")+
    geom_line(data=merged_data %>% filter(ew_start>=date_no_delay),aes(x = day,y = prediction, 
                                                                       group = 1, color = "Corrected Estimate"),size=1) +
    geom_ribbon(data=merged_data %>% filter(ew_start>=date_no_delay),aes(x = day,ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
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
                        breaks = c("Suspected Cases \n (subject to delays)", "Corrected Estimate","Fitted Model"),
                        values = c("Suspected Cases \n (subject to delays)" = "#004D40", 
                                   "Corrected Estimate" = "#D81B60",
                                   "Fitted Model"="#1E88E5")) +
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
  
}


fnDFiltered <- function(inputSelectVble, mapF) {
  dFiltered <- as.data.frame(d)
  dFiltered <- dFiltered[which(dFiltered$idloc %in% mapF$idloc), ] # select idloc in mapF (in state)
  dFiltered$vble <- dFiltered[, inputSelectVble]
  return(dFiltered)
}


fnTimeplot <- function(dF) {
  dF <- dF[order(dF$idtime), ]
  apex(dF, aes(x = idtime, y = vble, group = idloc), type = "line") # %>%
}


plot_trends_data <- function(merged_data, uf, K = 5) {
  date_no_delay <- merged_data[nrow(merged_data) - K, ]$ew_start
  merged_data <- merged_data %>% filter(ew_start >= "2021-01-01")
  fig <- ggplot(merged_data) +
    geom_line(
      aes(
        x = ew_start, y = sum_of_cases, group = 1,
        colour = "Suspected Cases \n (subject to delays)"
      ),
      size = 1
    ) +
    geom_line(data = merged_data %>% filter(ew_start <= date_no_delay), aes(
      x = ew_start, y = prediction,
      group = 1, color = "Fitted Model"
    ), linetype = 1, size = .5) +
    geom_ribbon(
      data = merged_data %>% filter(ew_start <= date_no_delay), aes(x = ew_start, ymin = lwr, ymax = upr),
      linetype = 2, size = .5, alpha = 0.1, fill = "#004D40", color = "#1E88E5"
    ) +
    geom_line(data = merged_data %>% filter(ew_start >= date_no_delay), aes(
      x = ew_start, y = prediction,
      group = 1, color = "Corrected Estimate"
    ), size = 1) +
    geom_ribbon(
      data = merged_data %>% filter(ew_start >= date_no_delay), aes(x = ew_start, ymin = lwr, ymax = upr),
      fill = "#D81B60", linetype = 2, alpha = 0.3
    ) +
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
      breaks = c("Suspected Cases \n (subject to delays)", "Corrected Estimate", "Fitted Model"),
      values = c(
        "Suspected Cases \n (subject to delays)" = "#004D40",
        "Corrected Estimate" = "#D81B60",
        "Fitted Model" = "#1E88E5"
      )
    ) +
    theme(
      legend.position = c(.15, .9),
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
  return(fig)
}


render_files <- function(folder_root_directory) {
  rmd_files <- list.files(path = "reports", pattern = "\\.Rmd$", full.names = TRUE)
  for (file in rmd_files) {
    filename <- tools::file_path_sans_ext(basename(file))
    setwd(folder_root_directory)
    rmarkdown::render(input = file, output_file = paste0("../docs/", filename, ".html"))
  }
}
