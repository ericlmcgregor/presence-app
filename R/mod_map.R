library(leaflet)
library(leaflet.extras)
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = "500px")
  )
}

# mod_map_server <- function(id, con) {
#   moduleServer(id, function(input, output, session) {
#     output$map <- renderLeaflet({
#       leaflet() %>%
#         addProviderTiles("CartoDB.Positron") %>%
#         addDrawToolbar(
#           targetGroup = "selected",
#           polylineOptions = FALSE,
#           markerOptions = FALSE,
#           editOptions = list(edit = FALSE, remove = TRUE)
#         )
#     })
#   })
# }

# mod_map_server <- function(id, con, polygon) {
#   moduleServer(id, function(input, output, session) {
#
#     output$map <- renderLeaflet({
#       leaflet() %>%
#         addProviderTiles("CartoDB.Positron") %>%
#         addDrawToolbar(
#           targetGroup = "selected",
#           polylineOptions = FALSE,
#           markerOptions = FALSE,
#           editOptions = list(edit = FALSE, remove = TRUE)
#         )
#     })
#
#     observeEvent(input$map_draw_new_feature, {
#       feature <- input$map_draw_new_feature
#       if (!is.null(feature)) {
#         coords <- feature$geometry$coordinates[[1]]  # Extract coordinates
#
#         # Convert list to matrix (lng, lat)
#         coords_matrix <- do.call(rbind, lapply(coords, unlist))
#
#         # Ensure it's a valid polygon (close it if necessary)
#         if (!identical(coords_matrix[1, ], coords_matrix[nrow(coords_matrix), ])) {
#           coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
#         }
#
#         # Convert to sf polygon object
#         sf_polygon <- sf::st_sfc(sf::st_polygon(list(coords_matrix)), crs = 4326)
#
#         # Update the reactive polygon
#         polygon(sf_polygon)
#       }
#     })
#   })
# }

mod_map_server <- function(id, con, polygon) {
  moduleServer(id, function(input, output, session) {

    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        worldCopyJump = FALSE,  # Prevents repeating world effect
        maxBounds = list(c(-90, -180), c(90, 180)),  # Locks map to single instance
        continuousWorld = FALSE  # Prevents wrapping horizontally
      )) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addDrawToolbar(
          targetGroup = "selected",
          polylineOptions = FALSE,
          markerOptions = FALSE,
          editOptions = list(edit = FALSE, remove = TRUE)
        )
    })

    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      if (!is.null(feature)) {
        coords <- feature$geometry$coordinates[[1]]

        print("🚨 Raw Leaflet Coordinates:")
        print(coords)

        # Convert list to matrix (lng, lat)
        coords_matrix <- do.call(rbind, lapply(coords, function(pt) c(pt[[1]], pt[[2]])))

        # Close polygon if necessary
        if (!identical(coords_matrix[1, ], coords_matrix[nrow(coords_matrix), ])) {
          coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
        }

        # Convert to sf polygon object with proper CRS
        sf_polygon <- sf::st_sfc(sf::st_polygon(list(coords_matrix)), crs = 4326)

        print("🚨 Processed Polygon (sf object):")
        print(sf_polygon)

        # Update reactive polygon
        polygon(sf_polygon)
      }
    })
  })
}


