#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyauthr::loginUI(id = "login"),
    div(
      id = "dashboard",
        bslib::page_navbar(
          shinyjs::useShinyjs(),
          title = "Elecciones 2024 - Baja California",
          bslib::nav_spacer(),
          mod_mapa_principal_ui("mapa_principal_1"),
          mod_enc_salida_ui("enc_salida_1"),
          mod_resultados_ui("resultados_1")
        )
    ) %>%
      shinyjs::hidden()
    # Your application UI logic
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
      app_title = "tablero.bajaCalifornia.diaD.2024"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
