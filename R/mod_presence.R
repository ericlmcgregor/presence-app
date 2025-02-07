library(leaflet)

mod_presence_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("species_select"), "Choose Common Name", choices = NULL, selected = ""),
    leafletOutput(ns("presence_map"), height = "500px")
  )
}



mod_presence_server <- function(id, con, species_data, polygon) {
  moduleServer(id, function(input, output, session) {


    observe({
      req(species_data())
      # Add a placeholder and ensure no default selection
      updateSelectInput(session, "species_select",
                        choices = c("", unique(species_data()$common_name)),
                        selected = "")
    })

    output$presence_map <- renderLeaflet({
      req(input$species_select, polygon())

      print(paste("Selected common name:", input$species_select))
      print("🚨 Current polygon WKT:")
      print(sf::st_as_text(polygon()))

      poly_wkt <- sf::st_as_text(polygon())

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
      GROUP BY p.final_cn  -- Ensure one row per plot
    ", poly_wkt, input$species_select)


      df <- dbGetQuery(con, query)

      print("Query result preview:")
      print(head(df))

      if (nrow(df) == 0) {
        showNotification("No presence/absence data found in the selected area.", type = "warning")
        return(leaflet() %>% addProviderTiles("CartoDB.Positron"))
      }

      df_sf <- sf::st_as_sf(df, wkt = "geometry", crs = 4326)

      print("sf object preview:")
      print(head(df_sf))

      leaflet(df_sf) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(
          color = ~ifelse(status == "Present", "green", "red"),  # Ensure "Absent" points are red
          popup = ~status
        )
    })

  })
}


