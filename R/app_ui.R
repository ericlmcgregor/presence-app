library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(RSQLite)
library(DBI)
library(DT)
library(golem)
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinydashboard::dashboardPage(
      dashboardHeader(title = "Tree Species Presence/Absence Viewer", titleWidth = 500),
      dashboardSidebar(collapsed = TRUE),  # Collapsed sidebar for more map space
      dashboardBody(
        mod_map_ui("map_module"),
        mod_export_ui("export_module")  # Keep export functionality if needed
      )
    )
  )
}



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
