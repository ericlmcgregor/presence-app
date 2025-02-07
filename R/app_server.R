#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
# app_server <- function(input, output, session) {
#   con <- connect_db()  # Connect to SQLite
#   DBI::dbExecute(con, "SELECT load_extension('mod_spatialite')")
#   polygon <- reactiveVal(NULL)  # Stores selected polygon
#
#   mod_map_server("map_module", con, polygon)
#
#   species_data <- mod_table_server("table_module", con, polygon)
#
#   mod_presence_server("presence_module", con, species_data)
#
#   mod_export_server("export_module", species_data)
# }

# app_server <- function(input, output, session) {
#   con <- connect_db()  # Connect to SQLite
#   DBI::dbExecute(con, "SELECT load_extension('mod_spatialite')")
#   polygon <- reactiveVal(NULL)  # Stores the selected polygon
#
#   mod_map_server("map_module", con, polygon)  # Pass reactive polygon
#
#   species_data <- mod_table_server("table_module", con, polygon)
#
#   mod_presence_server("presence_module", con, species_data)
#
#   mod_export_server("export_module", species_data)
# }


app_server <- function(input, output, session) {
  con <- connect_db()  # Connect to SQLite
  DBI::dbExecute(con, "SELECT load_extension('mod_spatialite')")
  polygon <- reactiveVal(NULL)  # Initialize the reactive polygon

  # Ensure polygon is passed to all relevant modules
  mod_map_server("map_module", con, polygon)
  species_data <- mod_table_server("table_module", con, polygon)
  mod_presence_server("presence_module", con, species_data, polygon)  # Pass polygon here
  mod_export_server("export_module", species_data)
}
