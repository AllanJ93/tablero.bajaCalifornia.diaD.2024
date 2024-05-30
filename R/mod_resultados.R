#' resultados UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_resultados_ui <- function(id){
  ns <- NS(id)
  bslib::nav_panel(
    title = "Resultados",
    bslib::card(
      full_screen = F,
      card_header("Resultados de la encuesta de salida"),
      max_height = "600px",
      shinyWidgets::progressBar(
        title = "Presencia en casillas de la muestra",
        id = "enc_hechas",
        value = bd_encuesta_salida |>
          filter(status %in% c("Reportada")) |>
          distinct(id) |> nrow(),
        display_pct = T,
        striped = T,
        total = 125,
        status = "success"),
      shinyWidgets::progressBar(
        title = "Encuestas de acuerdo a presencia en casillas",
        id = "enc_recolectadas",
        value = nrow(bd_encuesta_salida),
        display_pct = T,
        striped = T,
        total = round((muestra_shp |>
                         as_tibble() |>
                         left_join(bd_encuesta_salida |>
                                     distinct(id, status), by = "id") |>
                         filter(status == "Reportada") |>
                         summarise(sum(lstd_nm)) |>
                         pull())/5),
        status = "success"),
      bslib::layout_columns(
        shinycssloaders::withSpinner(highchartOutput(ns("tendencia_resultados"))),
        shinycssloaders::withSpinner(highchartOutput(ns("resultados_voto_candidato")))
      ),
    ),
    bslib::card(
      full_screen = F,
      bslib::layout_columns(
        bslib::value_box(
          title = "Casillas óptimas",
          value = textOutput(outputId = ns("faltantes_totales")),
          bsicons::bs_icon(name = "clock"),
          showcase_layout = "top right",
          theme = value_box_theme(bg = "green")),
        bslib::value_box(
          title = "Casillas no óptimas",
          value = textOutput(outputId = ns("excedentes_totales")),
          bsicons::bs_icon(name = "exclamation-triangle"),
          showcase_layout = "top right",
          theme = value_box_theme(bg = "orange"))
      ),
    ),
    icon = icon("line-chart")
  )
}

#' resultados Server Functions
#'
#' @noRd
mod_resultados_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$resultados_voto_candidato <-
      renderHighchart({

        colores <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")

        bd_resultados <-
          bd_encuesta_salida |>
          count(voto_candidato_sen) |>
          mutate(pct = round((n/sum(n))*100)) |>
          rename(respuesta = voto_candidato_sen,
                 media = pct)

        g <-
          highchart() |>
          hc_xAxis(categories = bd_resultados$respuesta,
                   labels = list(style = list(fontSize = "18px"))) |>
          hc_yAxis(min = 0,
                   max = 100,
                   tickInterval = 10,
                   labels = list(format = "{value}%"),
                   style = list(fontSize = "18px")) |>
          hc_add_series(name = "Porcentaje de votos estimados",
                        data = bd_resultados$media,
                        type = "bar",
                        # color = colores,
                        zIndex = 1,
                        stacking = "normal") |>
          hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}%", style = list(fontSize = "18px"))),
                         align = "right") |>
          hc_legend(itemStyle = list(fontSize = "24px", reversed = TRUE))

        return(g)

      })

    output$tendencia_resultados <-
      renderHighchart({

        bd_tendencia <-
          tibble(fecha = seq.Date(from = lubridate::today(),
                                  to = lubridate::today() + 3,
                                  by = "day"),
                 resultado = 100*(0.5))

        g <-
          highchart() |>
          hc_xAxis(categories = bd_tendencia$fecha,
                   labels = list(style = list(fontSize = "18px"))) |>
          hc_yAxis(min = 0,
                   max = 100,
                   tickInterval = 10,
                   labels = list(format = "{value}%"),
                   style = list(fontSize = "18px")) |>
          hc_add_series(name = "Intención de voto", data = bd_tendencia$resultado, type = "line", color = "green") |>
          hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}%", style = list(fontSize = "24px"))), align = "right")

        return(g)

      })

    output$faltantes_totales <- renderText({

      res <- paste("40 ", "(", scales::percent(40/125), ")", sep = "")

      return(res)

    })

    output$excedentes_totales <- renderText({

      res <- paste("3 ", "(", scales::percent(3/125), ")", sep = "")

      return(res)

    })



  })
}

## To be copied in the UI
# mod_resultados_ui("resultados_1")

## To be copied in the server
# mod_resultados_server("resultados_1")
