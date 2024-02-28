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
#library(INLA)
#library(spdep)


############################################################################################
# Specify d$idtime, d$nametime, d$idloc, map$idloc, map$nameloc, map$idloc2, map$nameloc2
############################################################################################

fnSetTimeLocVariablesDataMap <- function(d, map, dmapvbles){
  
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



