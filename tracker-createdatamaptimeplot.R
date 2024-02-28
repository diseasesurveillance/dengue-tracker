
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