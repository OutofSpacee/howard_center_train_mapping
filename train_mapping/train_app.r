library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)
library(leaflet.extras)



# Sources
source("global.R")
source("ui.R")
source("server.R")

# Run 
shinyApp(ui = ui, server = server)
