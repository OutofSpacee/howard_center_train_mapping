library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)

prepare_train_data <- function() {
  
  alltrains <- st_read("GeoJson Files/NARN1_2024_08_01_12-13-41_PM.geojson")
  
  traindata <- st_transform(alltrains, crs = 4326)
  
  #for popups
  cleantable <- traindata %>%
    select(
      FRAARCID,
      Transports_coal,
      STATEAB,
      RROWNER1,
      Evidence 
    )
  
  return(traindata)
}

get_unique_states <- function(data) {
  return(sort(unique(data$STATEAB)))
}