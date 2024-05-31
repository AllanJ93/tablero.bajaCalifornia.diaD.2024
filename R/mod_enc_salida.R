#' mapa_principal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr leaflet highcharter sf gt ggplot2
#' @importFrom shiny NS tagList

mod_enc_salida_ui <- function(id){
  ns <- NS(id)
  bslib::nav_panel(
    title = "Encuesta de Salida",
    bslib::card(
      full_screen = T,
      card_header("Status de casillas"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Menú",
          open = "open",
          id = "control_enc_salida",
          width = "400px",
          selectInput(inputId = ns("equipo_input"),
                      label = "Equipo",
                      choices = c("Todos",
                                  sort(unique(bd_equipos$equipo)))
          )
        ),
        highchartOutput(ns("casillas_faltantes_equipo"))
      )
    ),
    icon = icon("square-poll-vertical")
  )
}

#' mapa_principal Server Functions
#'
#' @noRd
mod_enc_salida_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shp_casillas <-
      muestra_shp %>%
      inner_join(bd_encuesta_salida) %>%
      mutate(status = tidyr::replace_na(replace = "Sin reportar", status),
             municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
      left_join(bd_equipos, by = c("municipio" = "NOMBRE"))

    casillas_reportadas <-
      reactive({
        input$equipo_input

        if(input$equipo_input == "Todos") {
          shp_casillas |>
            as_tibble() |>
            filter(status == "Reportada") |>
            count(hora = lubridate::floor_date(Date, unit = "hours"), equipo)
        }
        else {
          shp_casillas |>
            as_tibble() |>
            filter(equipo == input$equipo_input) |>
            count(hora = lubridate::floor_date(Date, unit = "hours"), id)
        }
      })

    output$casillas_faltantes_equipo <- renderHighchart({

      if(input$equipo_input == "Todos"){

        bd_plot <-
          casillas_reportadas() |>
          tidyr::pivot_wider(id_cols = hora, names_from = equipo, values_from = n) |>
          mutate(across(.cols = !hora, .fns = ~ tidyr::replace_na(data = .x, replace = 0)))

        g <-
          highchart() |>
          hc_xAxis(categories = format(bd_plot$hora, "%I %p"),
                   labels = list(style = list(fontSize = "18px"))) |>
          hc_yAxis(labels = list(style = list(fontSize = "18px"))) |>
          hc_add_series(name = "Equipo 1",
                        data = bd_plot$`1-MEXICALI-SAN FELIPE`, type = "line",
                        color = "red", zIndex = 1) |>
          hc_add_series(name = "Equipo 2",
                        data = bd_plot$`2-ENSENADA-SAN QUINTIN`, type = "line",
                        color = "blue", zIndex = 1) |>
          hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}", style = list(fontSize = "24px"))), align = "right")
      } else {

        # browser()

        # casillas_reportadas() |>
        #   mutate(cuota = 6) |>
        #   filter(n <= cuota) |>
        #   tidyr::pivot_wider(id_cols = hora, names_from = equipo, values_from = n) |>
        #   mutate(across(.cols = !hora, .fns = ~ tidyr::replace_na(data = .x, replace = 0)))

        bd_plot <-
          casillas_reportadas() |>
          tidyr::pivot_wider(id_cols = hora, names_from = equipo, values_from = n) |>
          mutate(across(.cols = !hora, .fns = ~ tidyr::replace_na(data = .x, replace = 0)))

        g <-
          highchart() |>
          hc_xAxis(categories = format(bd_plot$hora, "%I %p"),
                   labels = list(style = list(fontSize = "18px"))) |>
          hc_yAxis(labels = list(style = list(fontSize = "18px"))) |>
          hc_add_series(name = "Equipo 1",
                        data = bd_plot$`1-MEXICALI-SAN FELIPE`, type = "line",
                        color = "red", zIndex = 1) |>
          hc_add_series(name = "Equipo 2",
                        data = bd_plot$`2-ENSENADA-SAN QUINTIN`, type = "line",
                        color = "blue", zIndex = 1) |>
          hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}", style = list(fontSize = "24px"))), align = "right")

      }



      return(g)

    })


  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
