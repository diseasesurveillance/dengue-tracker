
fnMapFiltered <- function(vSelectTime, inputSelectLoc2){
  dtime <- d[which(d$idtime == vSelectTime), ]
  map <- left_join(map, dtime, by = c("idloc" = "idloc"))
  map <- map[which(map$idloc2 == inputSelectLoc2), ]
  
  map$vble <- map[, inputSelectVble, drop = TRUE]
  return(map)
}




fnLeafletMap <- function(mapF){
  labelOptionss <- labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
  
  # mapF <- mapFiltered()
  pal <- colorNumeric("YlOrRd", domain = mapF$vble)
  labels <- sprintf("<strong> %s </strong> <br/> %g ", mapF$nameloc, mapF$vble) %>%
    lapply(htmltools::HTML)
  
  leaflet(mapF) %>% addTiles() %>% #setView(lng = latIni, lat = longIni, zoom = zoomIni) %>%
    addPolygons(fillColor = ~pal(vble), layerId = ~ idloc, group = "poly",
                weight = 1, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
                label = labels, labelOptions = labelOptionss,
                highlightOptions = highlightOptions(weight = 3, color = "gray", dashArray = "", fillOpacity = 0.7, bringToFront = FALSE)) %>%
    addLegend(pal = pal, values = ~vble, opacity = 0.7, title = NULL, position = "bottomright") %>%
    addSearchFeatures(targetGroups  = "poly", options = searchFeaturesOptions(zoom = 7, autoCollapse = TRUE, openPopup = TRUE))
  
}  


fnDFiltered <- function(inputSelectVble, mapF){
  dFiltered <-  as.data.frame(d)
  dFiltered <- dFiltered[which(dFiltered$idloc %in% mapF$idloc), ] # select idloc in mapF (in state)
  dFiltered$vble <- dFiltered[, inputSelectVble]
  return(dFiltered)  
}

fnTimeplot <- function(dF){
  dF <- dF[order(dF$idtime), ]
  apex(dF, aes(x = idtime, y = vble, group = idloc), type = "line")# %>%
  # ax_colors(c("#8485854D", "#FF0000")) %>% ax_stroke(width = c(3, 2))%>% 
  #  ax_fill(opacity = 1, type = "solid")
  # %>% add_event_marker(as.Date(input$selectTime), y = 0)
  
}

plot_trends_data <- function(merged_data, uf) {
  title <- sprintf("Number of cases in %s", uf)
  fig <- ggplot(merged_data, aes(x = ew_start)) +
    geom_line(aes(y = sum_of_cases, group = 1, colour = "Number of Cases \n (subject to delays)"),linewidth=1) +
    geom_line(aes(y = prediction, group = 1, colour = "Corrected estimate"),linewidth=1) +
    labs(x = "Week", y = "Total Number of Weekly Cases") +
    scale_colour_manual("", 
                        breaks = c("Number of Cases \n (subject to delays)", "Corrected estimate"),
                        values = c("Number of Cases \n (subject to delays)" = "#004D40", "Corrected estimate" = "#D81B60")) +
    theme(axis.text.x = element_text(size=18), legend.text = element_text(size = 14),legend.title = element_text( size = 16,face="bold"),axis.title=element_text(size=18),plot.title = element_text(size=12))+
    theme_bw()+
    scale_x_date(date_breaks = "3 month",
                 date_labels = "%m/%y")+
    geom_ribbon(aes(ymin=lwr, ymax=upr),
                fill = "#D81B60", linetype=2, alpha=0.3)+
    theme(legend.position = c(.2, .9),
          legend.key.size = unit(1.2,"line"),
          legend.key.width= unit(2, 'line'),
          legend.text=element_text(size=16),
          axis.text.x  = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(color = "gray79",
                                            size = 0.25, 
                                            linetype = 4))+
    coord_cartesian(expand = FALSE) +
    scale_y_continuous(labels = scales::comma)+
    ggtitle(title)
  ggsave(sprintf("figures/%s_plot.png", uf), plot = fig, width = 11, height = 7)
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