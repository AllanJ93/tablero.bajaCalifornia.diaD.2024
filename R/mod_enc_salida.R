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
          ),
          actionButton(inputId = ns("siguiente_casillas"),
                       label = "Siguiente tabla")
        ),
        plotOutput(ns("casillas_faltantes_equipo"))
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
      inner_join(bd_encuesta_salida, by = "id") %>%
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

    indice_casillas <- reactiveVal(1)

    observeEvent(input$siguiente_casillas,{
      indice_casillas(indice_casillas() + 1)
    })

    output$casillas_faltantes_equipo <- renderPlot({

      if(input$equipo_input == "Todos"){

        bd_plot <-
          casillas_reportadas() |>
          mutate(hora = lubridate::as_datetime(hora, "America/Tijuana"))
        # tidyr::pivot_wider(id_cols = hora, names_from = equipo, values_from = n) |>
        # mutate(across(.cols = !hora, .fns = ~ tidyr::replace_na(data = .x, replace = 0)))

        g <-
          bd_plot |>
          ggplot(aes(x = hora, y = n, color = equipo)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          geom_text(aes(label = scales::comma(n)),
                    color = "black", vjust = -2, size = 8) +
          theme_minimal() +
          labs(color = "Equipo") +
          scale_y_continuous(expand = c(1,0)) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 18),
                legend.title = element_text(size = 24))

      } else {

        bd_total <-
          casillas_reportadas() |>
          mutate(hora = lubridate::as_datetime(hora, "America/Tijuana")) |>
          tidyr::complete(hora = seq(from = min(casillas_reportadas()$hora),
                                     to = max(casillas_reportadas()$hora),
                                     by = "hours"),
                          tidyr::nesting(id),
                          fill = list(n = 0))

        orden_grupos <-
          bd_total |>
          filter(hora == max(hora)) |>
          arrange(n) |>
          tibble::rownames_to_column("grupo") |>
          mutate(grupo = ((as.numeric(grupo)-1) %/% 7)+1)

        lista <-
          bd_total |>
          left_join(orden_grupos |>
                      select(id, grupo), by = "id") %>%
          split(.$grupo)

        pag <- indice_casillas() %% (length(lista)+1)

        if(pag == 0) {
          indice_casillas(indice_casillas() + 1)
          pag <- indice_casillas() %% (length(lista)+1)
        }

        bd_plot <-
          lista %>%
          purrr::pluck(pag) |>
          select(!grupo)

        orden_casillas <-
          orden_grupos %>%
          split(.$grupo) %>%
          purrr::pluck(pag) |>
          pull(id)

        g <-
          bd_plot |>
          ggplot(aes(x = hora, y = factor(id, levels = rev(orden_casillas)))) +
          geom_tile(fill = "transparent") +
          geom_text(aes(label = scales::comma(n)), size = 12) +
          labs(x = "", y = "") +
          scale_x_datetime(date_breaks = "1 hour",
                           labels = scales::date_format("%I\n%p", tz = "America/Tijuana"),
                           expand = c(0.1, 0.1),
                           position = "top") +
          theme_minimal() +
          theme(axis.text = element_text(size = 26))

      }

      return(g)

    })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
