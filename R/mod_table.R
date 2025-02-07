library(DBI)

mod_table_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("species_table"))
}

log_info <- function(msg) {
  print(paste0(Sys.time(), ": ", msg))
}

mod_table_server <- function(id, con, polygon) {
  moduleServer(id, function(input, output, session) {
    species_data <- reactive({
      log_info("Polygon input received:")
      print(polygon())

      if (is.null(polygon())) {
        log_info("Null polygon - returning NULL")
        return(NULL)
      }

      poly_wkt <- sf::st_as_text(polygon())
      log_info("WKT string created:")
      print(poly_wkt)

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


      log_info("Executing query:")
      print(query)

      result <- dbGetQuery(con, query)
      log_info("Query results:")
      print(head(result))

      result
    })

    output$species_table <- DT::renderDataTable({ species_data() })
    return(species_data)
  })
}


