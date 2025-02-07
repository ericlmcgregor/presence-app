# mod_map.R
library(leaflet)
library(leaflet.extras)
library(shinyjs)  # Add this for loading states

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),  # Enable shinyjs
    fluidRow(
      column(9,
             box(width = NULL,
                 title = "Interactive Map",
                 status = "primary",
                 solidHeader = TRUE,
                 leafletOutput(ns("map"), height = "600px")
             )
      ),
      column(3,
             # Instructions box
             box(width = NULL,
                 title = "Instructions",
                 status = "info",
                 solidHeader = TRUE,
                 tags$div(
                   tags$p("Follow these steps to use the app:"),
                   tags$ol(
                     tags$li("Click 'Toggle Draw Mode' to enable polygon drawing"),
                     tags$li("Draw a polygon on the map to select an area"),
                     tags$li("View species found in that area in the table below"),
                     tags$li("Select a different species from the dropdown to view its presence/absence"),
                     tags$li("Use 'Download GeoJSON' to export the current species data")
                   ),
                   tags$p("Green markers indicate species presence, red markers indicate absence.")
                 )
             ),
             # Controls panel
             box(width = NULL,
                 title = "Species Selection",
                 status = "primary",
                 solidHeader = TRUE,
                 div(
                   id = ns("loading_message"),
                   style = "display: none;",
                   tags$p(
                     tags$span(class = "fa fa-spinner fa-spin"),
                     "Loading species data...",
                     style = "color: #3c8dbc;"
                   )
                 ),
                 selectInput(ns("species_select"), "Choose Common Name", choices = NULL, selected = ""),
                 div(style = "margin-top: 20px;",
                     actionButton(ns("clear_map"), "Clear Selection", class = "btn-warning"),
                     actionButton(ns("toggle_draw"), "Toggle Draw Mode", class = "btn-info")
                 ),
                 div(style = "margin-top: 20px;",
                     downloadButton(ns("download"), "Download GeoJSON", class = "btn-success")
                 )
             )
      )
    ),
    # Data table in full-width row below
    fluidRow(
      column(12,
             box(width = NULL,
                 title = "Species in Selected Area",
                 status = "primary",
                 solidHeader = TRUE,
                 div(style = "height: 400px; overflow-y: auto;",
                     DT::dataTableOutput(ns("species_table"))
                 )
             )
      )
    )
  )
}

mod_map_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      polygon = NULL,
      draw_mode = FALSE,
      species_data = NULL,
      selected_species = NULL,
      current_sf_data = NULL
    )

    # Initialize map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        worldCopyJump = FALSE,
        maxBounds = list(c(-90, -180), c(90, 180)),
        continuousWorld = FALSE
      )) %>%
        addProviderTiles("CartoDB.Positron", group = "Basemap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
        addDrawToolbar(
          targetGroup = "Area of Interest",
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          polylineOptions = FALSE,
          markerOptions = FALSE,
          editOptions = list(edit = FALSE, remove = TRUE),
          singleFeature = TRUE
        ) %>%
        addLayersControl(
          baseGroups = c("Basemap", "Imagery"),
          overlayGroups = c("Area of Interest", "Presence/Absence"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # Handle drawing new polygons
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      if (!is.null(feature)) {
        coords <- feature$geometry$coordinates[[1]]
        coords_matrix <- do.call(rbind, lapply(coords, function(pt) c(pt[[1]], pt[[2]])))
        if (!identical(coords_matrix[1, ], coords_matrix[nrow(coords_matrix), ])) {
          coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
        }
        rv$polygon <- sf::st_sfc(sf::st_polygon(list(coords_matrix)), crs = 4326)
        # Show loading message
        shinyjs::show("loading_message")
        update_species_table()
      }
    })

    # Update species table when polygon changes
    update_species_table <- reactive({
      req(rv$polygon)
      poly_wkt <- sf::st_as_text(rv$polygon)
      query <- sprintf("
        WITH filtered_plots AS (
          SELECT final_cn FROM plot_locations
          WHERE ST_Intersects(geometry, ST_GeomFromText('%s'))
        )
        SELECT common_name, genus, species, COUNT(DISTINCT p.final_cn) AS frequency
        FROM tree_records t
        JOIN filtered_plots p ON t.final_cn = p.final_cn
        GROUP BY common_name, genus, species
        ORDER BY frequency DESC", poly_wkt)

      rv$species_data <- dbGetQuery(con, query)

      # Select the first species by default
      if (nrow(rv$species_data) > 0) {
        first_species <- rv$species_data$common_name[1]
        updateSelectInput(session, "species_select",
                          choices = unique(rv$species_data$common_name),
                          selected = first_species)
      }
      # Hide loading message
      shinyjs::hide("loading_message")
    })

    # Render species table
    output$species_table <- DT::renderDataTable({
      req(rv$species_data)
      DT::datatable(
        rv$species_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "350px",
          scroller = TRUE,
          dom = 'rtip'
        ),
        selection = 'single'
      )
    })

    # Update presence/absence points when species selection changes
    observe({
      req(input$species_select, rv$polygon)

      poly_wkt <- sf::st_as_text(rv$polygon)
      query <- sprintf("
        WITH filtered_plots AS (
          SELECT final_cn FROM plot_locations
          WHERE ST_Intersects(geometry, ST_GeomFromText('%s', 4326))
        )
        SELECT ST_AsText(p.geometry) AS geometry,
               CASE
                 WHEN COUNT(t.common_name) > 0 THEN 'Present'
                 ELSE 'Absent'
               END AS status
        FROM plot_locations p
        LEFT JOIN tree_records t
          ON p.final_cn = t.final_cn
          AND t.common_name = '%s'
        WHERE p.final_cn IN (SELECT final_cn FROM filtered_plots)
        GROUP BY p.final_cn",
                       poly_wkt, input$species_select)

      df <- dbGetQuery(con, query)

      if (nrow(df) > 0) {
        df_sf <- sf::st_as_sf(df, wkt = "geometry", crs = 4326)
        rv$current_sf_data <- df_sf

        leafletProxy("map") %>%
          clearGroup("Presence/Absence") %>%
          addCircleMarkers(
            data = df_sf,
            group = "Presence/Absence",
            color = ~ifelse(status == "Present", "green", "red"),
            popup = ~status
          )
      }
    })

    # Download handler
    output$download <- downloadHandler(
      filename = function() {
        species_name <- input$species_select
        if (species_name == "") species_name <- "no_species_selected"
        paste0(species_name, "_presence.geojson")
      },
      content = function(file) {
        req(rv$current_sf_data)
        sf::st_write(rv$current_sf_data, file, driver = "GeoJSON")
      }
    )

    # Clear map button handler
    observeEvent(input$clear_map, {
      rv$polygon <- NULL
      rv$species_data <- NULL
      rv$current_sf_data <- NULL
      leafletProxy("map") %>%
        clearGroup("Area of Interest") %>%
        clearGroup("Presence/Absence")
      updateSelectInput(session, "species_select", selected = NULL)
    })

    # Toggle draw mode
    observeEvent(input$toggle_draw, {
      rv$draw_mode <- !rv$draw_mode
      leafletProxy("map") %>%
        clearGroup("Area of Interest") %>%
        clearGroup("Presence/Absence")
    })
  })
}
