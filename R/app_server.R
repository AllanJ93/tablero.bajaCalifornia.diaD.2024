#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # bd_apertura
  mod_mapa_principal_server("mapa_principal_1")
  mod_resultados_server("resultados_1")
  mod_enc_salida_server("enc_salida_1")
}
