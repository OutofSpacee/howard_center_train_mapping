library(shiny)
library(leaflet)
library(shinyjs)
library(DT)

# Choices for dropdown
vars <- c("Yes", "No", "Unknown")

ui <- navbarPage(
  "Coal Trains", 
  id = "nav",
  tabPanel(
    "Map",
    div(
      class = "outer",
      tags$head(includeCSS("styles.css")),
      useShinyjs(),
      leafletOutput("map", width = "100%", height = "100%")
    ),
    tags$div(
      id = "loading-data",
      style = "position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); z-index: 1000;",
      h3("Loading map, please wait...")
    ),
    # New panel for save button
    absolutePanel(
      id = "save-panel", 
      class = "panel panel-default", 
      fixed = TRUE,
      draggable = FALSE, 
      top = 60, 
      left = "auto", 
      bottom = "auto", 
      right = 355,
      width = "auto", 
      height = "auto",
      actionButton("save_btn", "Save Session", class = "btn-success", icon = icon("save"))
    ),
    # Controls Pane
    absolutePanel(
      id = "controls", 
      class = "panel panel-default", 
      fixed = TRUE,
      draggable = FALSE, 
      top = 60, 
      left = "auto", 
      right = 20, 
      bottom = "auto",
      width = 330, 
      height = "auto",
      h2("Rail Line Controls"),
      selectInput(
        "coal_transport", 
        "Transports Coal?",
        choices = c("Yes", "No", "Unknown"),
        selectize = TRUE
      ),
      selectInput(
        "evidence", 
        "Evidence?", 
        choices = c(
          "Company Docs",
          "Government Docs",
          "Media Coverage",
          "Social Media",
          "Logical Assumptions",
          "Imagery"
        ),
        multiple = TRUE,
        selectize = TRUE
      ),
      actionButton("update_btn", "Update Selected Lines", class = "btn-primary", icon = icon("sync")),
      tags$hr(),
      selectizeInput("selected_states", "Select States / Territories :",
                     choices = NULL, 
                     multiple = TRUE,
                     options = list(placeholder = 'search',
                                    onInitialize = I('function() { this.setValue(""); }'),
                                    plugins = list('remove_button', 'drag_drop')),
      ),
      selectInput(
        "baselayer", 
        "Select Base Layer",
        choices = c("Streets", "Satellite", "Blank"),
        selectize = TRUE,
        selected = "Blank"
      )
    ),
    # Results Pane
    absolutePanel(
      id = "results", 
      class = "panel panel-default", 
      fixed = TRUE,
      draggable = FALSE, 
      top = "auto", 
      left = "auto", 
      right = 20, 
      bottom = 20,
      width = 600, 
      height = "auto",
      h3("Selected Rail Lines"),
      DTOutput("selected_table")
    ),
    absolutePanel(id = "zoom-button-panel",
                  class = "panel panel-default",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 130, left = "auto",
                  bottom = "auto", right = 355,
                  width = "auto", height = "auto",
                  uiOutput("zoom_button")
    ),
    tags$div(
      id = "saving-overlay",
      style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5); z-index: 9999;",
      tags$div(
        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); background-color: white; padding: 20px; border-radius: 5px; text-align: center;",
        tags$h3("Writing Save File . . . "),
        tags$div(class = "spinner")
      )
    )
  ),
  # Table View section
  tabPanel(
    "Full Data View",
    fluidPage(
      h2("Rail Line Data"),
      actionButton("refresh_data", "Refresh Data", class = "btn-primary", icon = icon("sync")),
      DTOutput("full_data_table")
    )
  ),
  # Add this new tags$script section at the end of the UI
  tags$script(HTML("
    Shiny.addCustomMessageHandler('showWarningModal', function(message) {
      $('#warningModal').modal('show');
    });
  "))
)