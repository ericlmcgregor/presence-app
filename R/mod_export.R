mod_export_ui <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download"), "Download Data")
}

mod_export_server <- function(id, species_data) {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() { paste0(input$species_select, "_presence.geojson") },
      content = function(file) {
        sf::st_write(species_data(), file, driver = "GeoJSON")
      }
    )
  })
}
