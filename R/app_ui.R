library(shinydashboard)
library(leaflet)
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      dashboardHeader(title = "Tree Species Presence/Absence Viewer", titleWidth = 500),
      dashboardSidebar(collapsed = TRUE),  # Collapsed sidebar for more map space
      dashboardBody(
        mod_map_ui("map_module"),
        mod_export_ui("export_module")  # Keep export functionality if needed
      )
    )
  )
}

# app_ui <- function(request) {
#   tagList(
#     # Keep this function for adding external resources
#     golem_add_external_resources(),
#
#     # Use shinydashboard layout instead of fluidPage()
#     dashboardPage(
#       dashboardHeader(title = "Tree Species Presence/Absence"),
#       dashboardSidebar(
#         sidebarMenu(
#           menuItem("Select Area", tabName = "map", icon = icon("map")),
#           menuItem("Species Table", tabName = "table", icon = icon("table")),
#           menuItem("Presence/Absence", tabName = "presence", icon = icon("leaf")),
#           menuItem("Export Data", tabName = "export", icon = icon("download"))
#         )
#       ),
#       dashboardBody(
#         tabItems(
#           tabItem(tabName = "map", mod_map_ui("map_module")),
#           tabItem(tabName = "table", mod_table_ui("table_module")),
#           tabItem(tabName = "presence", mod_presence_ui("presence_module")),
#           tabItem(tabName = "export", mod_export_ui("export_module"))
#         )
#       )
#     )
#   )
# }


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "presence"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
