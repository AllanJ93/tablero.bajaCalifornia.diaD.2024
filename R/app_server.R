#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # bd_apertura

  credentials <-
    shinyauthr::loginServer(
      id = "login",
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      sodium_hashed = T,
      log_out = reactive(logout_init())
    )

  observe({
    req(credentials()$user_auth == T)
    shinyjs::show("dashboard")
  })

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  mod_mapa_principal_server("mapa_principal_1")
  mod_resultados_server("resultados_1")
  mod_enc_salida_server("enc_salida_1")
}
