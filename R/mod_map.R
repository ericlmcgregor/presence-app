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
      leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%  # Prevents wrapping
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
        coords <- feature$geometry$coordinates[[1]]  # Extract coordinates

        # 🚨 Debug: Print raw coordinates before transformation
        print("🚨 Raw coordinates from Leaflet (before processing):")
        print(coords)

        # Convert list to matrix (lng, lat)
        coords_matrix <- do.call(rbind, lapply(coords, function(pt) c(pt[[1]], pt[[2]])))

        # 🚨 Debug: Print matrix after conversion
        print("🚨 Converted coords_matrix:")
        print(coords_matrix)

        # Close polygon if necessary
        if (!identical(coords_matrix[1, ], coords_matrix[nrow(coords_matrix), ])) {
          coords_matrix <- rbind(coords_matrix, coords_matrix[1, ])
        }

        # Convert to sf polygon object, ensuring correct CRS
        sf_polygon <- sf::st_sfc(sf::st_polygon(list(coords_matrix)), crs = 4326)

        # 🚨 Debug: Print final polygon
        print("🚨 Final sf polygon:")
        print(sf_polygon)

        # Update the reactive polygon
        polygon(sf_polygon)
      }
    })
  })
}


