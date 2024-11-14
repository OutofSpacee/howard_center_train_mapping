library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(sf)
library(rlang)
library(leaflet.extras)
library(shinyjs)
library(zip)
library(RColorBrewer)
library(later)

colorFactorPalette <- function(n) {
  colorFactor(colorRampPalette(brewer.pal(8, "Set2"))(n), domain = NULL)
}

server <- function(input, output, session) {
  
  ###
  ## Setting App Up ##
  ###
  
  #----------------------------#
  ## MAP VIEW
  #----------------------------#
  
  train_data <- reactiveVal(NULL)
  updated_train_data <- reactiveVal(NULL)
  current_base_layer <- reactiveVal("Blank")
  overwrite_confirm <- reactiveVal(FALSE)
  update_button_clicked <- reactiveVal(FALSE)
  
  # Loading data
  observe({
    if (is.null(isolate(train_data()))) {
      data <- prepare_train_data()  
      train_data(data)
      updated_train_data(data)
    }
  })
  
  # Hiding loading after data is loaded
  observeEvent(train_data(), {
    shinyjs::hide("loading-data")
  })
  
  output$zoom_button <- renderUI({
    req(current_zoom())
    if (current_zoom() > 12) {
      actionButton("zoom_action", "Color Rail Lines", class = "btn-info", icon = icon("paint-brush"))
    }
  })
  
  selected_lines <- reactiveVal(character(0))
  
  # Set map and data
  output$map <- renderLeaflet({
    req(train_data())
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 20)) %>%
      addTiles() %>%
      fitBounds(-130, 20, -60, 50) %>%
      setMaxBounds(lng1 = -130, lat1 = 20, lng2 = -60, lat2 = 50) %>%
      addDrawToolbar(
        targetGroup = "drawnItems",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
  })
  
  observe({
    leafletProxy("map") %>%
      clearTiles()
    
    new_base_layer <- input$baselayer
    current_base_layer(new_base_layer)  
    
    if (new_base_layer == "Streets") {
      leafletProxy("map") %>%
        addProviderTiles("Esri.WorldStreetMap")
    } else if (new_base_layer == "Satellite") {
      leafletProxy("map") %>%
        addProviderTiles("Esri.WorldImagery")  
    } else if (new_base_layer == "Blank") {
      leafletProxy("map") %>%
        addProviderTiles("CartoDB.Positron")
    }
  })
  
  # To use for saving btn
  shinyjs::useShinyjs()
  
  #----------------------------#
  ## FULL DATA TABLE VIEW
  #----------------------------#
  
  # Rendering full data table for second tab
  output$full_data_table <- renderDT({
    req(train_data())
    datatable(
      train_data() %>%
        st_drop_geometry(),
      filter = 'top',
      options = list(
        pageLength = 25,
        scrollY = "600px",
        scrollX = TRUE,
        deferRender = TRUE,
        scroller = TRUE,
        searchHighlight = TRUE
      )
    )
  })
  
  ## Populating Select State / Territories
  observe({
    req(train_data())
    states <- get_unique_states(train_data())
    
    updateSelectizeInput(session, "selected_states", choices = states, server = TRUE)
  })
  
  ###
  ## As App is Running ##
  ###
  
  #----------------------------#
  ## MAP FEATURES
  #----------------------------#
  
  #----------------------------#
  ## Creating, Loading and Drawing Filtered Data
  #----------------------------#
  
  previous_selection <- reactiveVal(character(0))
  current_selected_states <- reactiveVal(character(0))
  
  filtered_data <- reactive({
    req(train_data())
    if (length(input$selected_states) == 0) {
      return(NULL)
    } else {
      train_data() %>%
        filter(STATEAB %in% input$selected_states)
    }
  })
  
  getLayerIds <- function(map, group = NULL) {
    all_layers <- map$x$calls
    if (!is.null(group)) {
      group_layers <- all_layers[sapply(all_layers, function(layer) layer$method == "addLayer" && layer$args[[1]]$group == group)]
    } else {
      group_layers <- all_layers[sapply(all_layers, function(layer) layer$method == "addLayer")]
    }
    
    layer_ids <- sapply(group_layers, function(layer) layer$args[[1]]$layerId)
    return(unique(layer_ids))
  }
  
  # The drawing of the filtered data
  display_filtered_data <- function(map, data, group = "filtered_lines") {
    map %>%
      addPolylines(data = data,
                   layerId = ~FRAARCID,
                   color = ~ifelse(Transports_coal == "Yes", "red", 
                                   ifelse(Transports_coal == "No", "green", "blue")),
                   weight = 2,
                   opacity = 0.7,
                   group = group,
                   popup = ~paste("Line ID:", FRAARCID, "<br>Transports Coal:", Transports_coal, "<br>State/Territory:", STATEAB))
  }
  
  # Observe changes in the selected states
  observeEvent(input$selected_states, {
      current_selection <- input$selected_states
      prev_selection <- previous_selection()
      
      added_states <- setdiff(current_selection, prev_selection)
      removed_states <- setdiff(prev_selection, current_selection)
      
      if (length(current_selection) == 0) {
        print("All states removed")
        leafletProxy("map") %>% 
          clearGroup("all_states")
      } else {
        if (length(added_states) > 0) {
          print(paste("Added states:", paste(added_states, collapse = ", ")))
          new_data <- filtered_data() %>% filter(STATEAB %in% added_states)
          leafletProxy("map") %>% 
            display_filtered_data(new_data, group = paste0("state_", added_states))
        }
      }
      
      if (length(removed_states) > 0) {
        print(paste("Removed states:", paste(removed_states, collapse = ", ")))
        leafletProxy("map") %>% 
          clearGroup(paste0("state_", removed_states))
      }
      previous_selection(current_selection)
      current_selected_states(current_selection)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observe({
    req(filtered_data())
    current_states <- input$selected_states
    if (length(current_states) > 0) {
      leafletProxy("map") %>% 
        clearGroup("filtered_lines")
      for (state in current_states) {
        state_data <- filtered_data() %>% filter(STATEAB == state)
        leafletProxy("map") %>% 
          display_filtered_data(state_data, group = paste0("state_", state))
      }
    } else if (!update_button_clicked()) {
      leafletProxy("map") %>% 
        clearGroup(paste0("state_", previous_selection()))
    }
  })
  
  #----------------------------#
  ## Selecting Rail Lines to Edit
  #----------------------------#
  
  # Selecting rail lines
  selected_lines_data <- reactive({
    req(train_data(), selected_lines())
    train_data() %>%
      filter(FRAARCID %in% selected_lines()) %>%
      st_drop_geometry() %>%
      select(Transports_coal, STATEAB, RROWNER1, Evidence)
  })
  
  # Handle box selection, update selected lines, and handle deselection
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$properties$feature_type == "rectangle") {
      coordinates <- feature$geometry$coordinates[[1]]
      bbox <- st_bbox(c(
        xmin = min(sapply(coordinates, `[[`, 1)), 
        ymin = min(sapply(coordinates, `[[`, 2)),
        xmax = max(sapply(coordinates, `[[`, 1)), 
        ymax = max(sapply(coordinates, `[[`, 2))
      ))
      
      selection_polygon <- st_as_sfc(bbox) %>% st_set_crs(4326)
      current_data <- filtered_data()
      if (st_crs(current_data) != 4326) {
        current_data <- st_transform(current_data, 4326)
      }
      
      selected_lines_sf <- current_data[st_intersects(current_data, selection_polygon, sparse = FALSE), ]
      new_selected_FRAARCIDs <- selected_lines_sf$FRAARCID
      current_selection <- unique(c(selected_lines(), new_selected_FRAARCIDs))
      selected_lines(current_selection)
      
      # Displaying the selected lines
      leafletProxy("map") %>%
        clearGroup("selected") %>%
        addPolylines(data = selected_lines_sf, 
                     color = "yellow", 
                     weight = 4, 
                     opacity = 1, 
                     group = "selected")
      
      # Displaying selected lines on table as well
      output$selected_table <- renderDT({
        req(selected_lines_data())
        datatable(
          selected_lines_data(),
          options = list(
            pageLength = 10,
            scrollY = "300px",
            scrollCollapse = TRUE,
            dom = 't'
          ),
          selection = 'none'
        )
      })
    }
  })
  
  # Handle deletion of drawn features
  observeEvent(input$map_draw_deleted_features, {
    selected_lines(character(0))
    leafletProxy("map") %>%
      clearGroup("selected")
    
    output$selected_table <- renderDT({
      datatable(
        data.frame(),
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          dom = 't'
        ),
        selection = 'none'
      )
    })
  })
  
  #----------------------------#
  ## BUTTONS
  #----------------------------#
  
  # COLOR RAILS BTN
  
  current_zoom <- reactiveVal(0)
  lines_colored <- reactiveVal(FALSE)
  
  remove_line_coloring <- function() {
    leafletProxy("map") %>%
      clearGroup("colored_lines") %>%
      addPolylines(data = filtered_data(),
                   layerId = ~FRAARCID,
                   color = ~ifelse(Transports_coal == "Yes", "red", 
                                   ifelse(Transports_coal == "No", "green", "blue")),
                   weight = 2,
                   opacity = 0.7,
                   group = "all_lines",
                   popup = ~paste("Line ID:", FRAARCID, "<br>Transports Coal:", Transports_coal, "<br>State/Territory:", STATEAB))
    
    lines_colored(FALSE)
    showNotification("Line coloring removed", type = "message")
  }
  
  observe({
    new_zoom <- input$map_zoom
    current_zoom(new_zoom)
    
    if (new_zoom < 12 && lines_colored()) {
      remove_line_coloring()
    }
  })
  
  observeEvent(input$zoom_action, {
    req(filtered_data())
    
    if (nrow(filtered_data()) > 0) {
      # Generate a color palette based on unique FRAARCID values
      unique_ids <- unique(filtered_data()$FRAARCID)
      color_palette <- colorFactorPalette(length(unique_ids))
      
      leafletProxy("map") %>%
        clearGroup("all_lines") %>%
        addPolylines(data = filtered_data(),
                     layerId = ~FRAARCID,
                     color = ~color_palette(FRAARCID),
                     weight = 2,
                     opacity = 0.7,
                     group = "colored_lines",
                     popup = ~paste("Line ID:", FRAARCID, "<br>Transports Coal:", Transports_coal, "<br>State/Territory:", STATEAB))
      
      lines_colored(TRUE)
      showNotification("Rail lines colored by FRAARCID", type = "message")
    } else {
      showNotification("No rail lines in current view", type = "warning")
    }
  })
  
  #
  # UPDATE BTN + OVERWRITING
  # 
  
  # Canceling and confirming overwriting data
  observeEvent(input$confirm_overwrite, {
    overwrite_confirm(TRUE)
    removeModal()
    update_selected_lines()
  })
  observeEvent(input$cancel_overwrite, {
    removeModal()
    deselect_lines()
  })
  
  observeEvent(input$update_btn, {
    req(length(selected_lines()) > 0)
    # Checking
    overwritten_lines <- train_data() %>%
      filter(FRAARCID %in% selected_lines(),
             Transports_coal %in% c("Yes", "No"),
             Transports_coal != input$coal_transport)
    if (nrow(overwritten_lines) > 0 && !overwrite_confirm()) {
      leafletProxy("map") %>%
        clearGroup("overwrite_highlight") %>%
        addPolylines(data = overwritten_lines,
                     color = "black",
                     weight = 4,
                     opacity = 1,
                     group = "overwrite_highlight")
      # Warning
      showModal(modalDialog(
        title = "Warning: Overwriting Classified Tracks",
        "You are about to change previously classified tracks. Do you want to continue?",
        footer = tagList(
          actionButton("cancel_overwrite", "Cancel", class = "btn-secondary"),
          actionButton("confirm_overwrite", "Confirm", class = "btn-danger")
        ),
        easyClose = FALSE,
        fade = TRUE
      ))
    } else {
      update_selected_lines()
    }
  })
  
  # Deselection function
  deselect_lines <- function() {
    selected_lines(character(0))
    
    leafletProxy("map") %>%
      clearGroup("selected") %>%
      clearGroup("overwrite_highlight")
    
    output$selected_table <- renderDT({
      datatable(
        data.frame(),
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          dom = 't'
        ),
        selection = 'none'
      )
    })
  }
  
  # Update function
  update_selected_lines <- function() {
    update_button_clicked(TRUE)
    
    evidence_string <- paste(input$evidence, collapse = ", ")
    
    updated_data <- train_data() %>%
      mutate(
        Transports_coal = if_else(FRAARCID %in% selected_lines(), input$coal_transport, Transports_coal),
        Evidence = if_else(FRAARCID %in% selected_lines(), evidence_string, Evidence)
      )
    train_data(updated_data)
    
    current_zoom <- input$map_zoom
    current_center <- input$map_center
    
    deselect_lines()
    
    lines_colored(FALSE)
    
    leafletProxy("map") %>%
      addProviderTiles(
        provider = switch(input$baselayer,
                          "Streets" = "Esri.WorldStreetMap",
                          "Satellite" = "Esri.WorldImagery",
                          "Blank" = "CartoDB.Positron"
        ),
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = current_center$lng, lat = current_center$lat, zoom = current_zoom) %>%
      clearGroup("all_lines") %>%
      clearGroup("colored_lines")
    
    overwrite_confirm(FALSE)
    
    isolate({
        updateSelectizeInput(session, "selected_states", selected = current_selected_states(), server = TRUE)
    })
  }
  
  #
  # REFRESH FULL DATA TABLE BTN
  #
  
  observeEvent(input$refresh_data, {
    output$full_data_table <- renderDT({
      req(train_data())
      datatable(
        train_data() %>%
          st_drop_geometry(),
        filter = 'top',
        options = list(
          pageLength = 25,
          scrollY = "600px",
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          searchHighlight = TRUE
        )
      )
    })
  })
  
  #
  # SAVING BTN
  #
  # Checking files + saving data in new file + auto zip
  
  observeEvent(input$save_btn, {
    shinyjs::show("saving-overlay")
    tryCatch({
      create_unique_file <- function() {
        base_name <- "NARN"
        extension <- ".geojson"
        timestamp <- format(Sys.time(), "%Y_%m_%d_%I-%M-%S_%p")
        
        all_files <- list.files("GeoJson Files", pattern = paste0("^", base_name, "\\d*_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_[AP]M", extension, "$"))
        
        file_numbers <- as.numeric(gsub(paste0("^", base_name, "(\\d*)_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_[AP]M", extension, "$"),
                                        "\\1",
                                        all_files))
        file_numbers <- file_numbers[!is.na(file_numbers)]
        
        if (length(file_numbers) == 0) {
          next_number <- 1
        } else {
          next_number <- max(file_numbers) + 1
        }
        
        return(paste0(base_name, next_number, "_", timestamp, extension))
      }
      
      # Create directories if they don't exist
      dir.create("GeoJson Files", showWarnings = FALSE)
      dir.create("Zipped Files", showWarnings = FALSE)
      
      new_filename <- create_unique_file()
      geojson_path <- file.path("GeoJson Files", new_filename)
      
      st_write(train_data(), geojson_path, driver = "GeoJSON")
      
      zip_filename <- gsub("\\.geojson$", ".zip", new_filename)
      zip_path <- file.path("Zipped Files", zip_filename)
      
      zip::zip(zipfile = zip_path, files = geojson_path)
      
      print(paste("DATA SAVED AS:", geojson_path))
      print(paste("DATA ZIPPED AS:", zip_path))
      showNotification(paste("DATA SAVED AND ZIPPED SUCCESSFULLY. GeoJSON:", geojson_path, "ZIP:", zip_path), type = "message")
      
      shinyjs::hide("saving-overlay")
      showNotification(paste("DATA SAVED AND ZIPPED SUCCESSFULLY. GeoJSON:", geojson_path, "ZIP:", zip_path), type = "message")
      
    }, error = function(e) {
      print(paste("Error occurred:", e$message))
      showNotification(paste("Error: ", e$message), type = "error")
    })
  })
}