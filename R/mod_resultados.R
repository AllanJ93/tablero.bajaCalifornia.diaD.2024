#' resultados UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr survey srvyr
#' @importFrom shiny NS tagList
mod_resultados_ui <- function(id){
  ns <- NS(id)
  bslib::nav_panel(
    title = "Resultados",
    bslib::card(
      full_screen = F,
      card_header("Resultados de la encuesta de salida"),
      max_height = "800px",
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
        # total = round((muestra_shp |>
        #                  as_tibble() |>
        #                  left_join(bd_encuesta_salida |>
        #                              distinct(id, status), by = "id") |>
        #                  filter(status == "Reportada") |>
        #                  summarise(sum(lstd_nm)) |>
        #                  pull())/5),
        total = 10000,
        status = "success"),
      shinycssloaders::withSpinner(highchartOutput(ns("resultados_voto_candidato"))),
    ),
    bslib::card(
      full_screen = F,
      bslib::layout_columns(
        shinycssloaders::withSpinner(highchartOutput(ns("tendencia_resultados"))),
        shinycssloaders::withSpinner(highchartOutput(ns("llegada_info")))
        # bslib::value_box(
        #   title = "Casillas no óptimas",
        #   value = textOutput(outputId = ns("excedentes_totales")),
        #   bsicons::bs_icon(name = "exclamation-triangle"),
        #   showcase_layout = "top right",
        #   theme = value_box_theme(bg = "orange"))
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
          procesar_prop_voto_sen() |>
          mutate(respuesta = voto_sen_candidato,
                 media = round(total*100))

          # count(voto_sen_candidato_O1) |>
          # mutate(pct = round((n/sum(n))*100)) |>
          # na.omit() |>
          # rename(respuesta = voto_sen_candidato_O1,
          #        media = pct)

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
          bd_encuesta_salida |>
          count(hora = lubridate::floor_date(Date, "hours"), voto_sen_candidato_O1) |>
          group_by(hora) |>
          tidyr::complete(voto_sen_candidato = unique(bd_encuesta_salida$voto_sen_candidato),
                          fill = list(n = 0)) |>
          ungroup() |>
          mutate(tot = sum(n), .by = c(hora)) |>
          mutate(n_acum = cumsum(n),
                 tot_acum = cumsum(tot), .by = c(voto_sen_candidato_O1),
                 movil = n_acum/tot_acum) |>
          filter(grepl(pattern = "Hank", x = voto_sen_candidato_O1)) |>
          mutate(movil = round(movil*100)) |>
          rename(fecha = hora,
                 resultado = movil)

        g <-
          highchart() |>
          hc_xAxis(categories = format(bd_tendencia$fecha, "%I %p"),
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

    output$llegada_info <-
      renderHighchart({

        bd_informacion <-
          bd_encuesta_salida |>
          count(hora = lubridate::floor_date(Date, "hours")) |>
          mutate(acum = cumsum(n))

        g <-
          highchart() |>
          hc_xAxis(categories = format(bd_informacion$hora, "%I %p"),
                   labels = list(style = list(fontSize = "18px"))) |>
          hc_yAxis(labels = list(style = list(fontSize = "18px"))) |>
          hc_add_series(name = "Entrevistas",
                        data = bd_informacion$acum, type = "line",
                        color = "red", zIndex = 1) |>
          hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}", style = list(fontSize = "24px"))), align = "right")

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
