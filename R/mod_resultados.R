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
      shinycssloaders::withSpinner(plotOutput(ns("resultados_voto_candidato"))),
    ),
    bslib::card(
      full_screen = F,
      bslib::layout_columns(
        shinycssloaders::withSpinner(plotOutput(ns("tendencia_resultados"))),
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
      renderPlot({

        colores <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")

        bd_resultados <-
          bd_encuesta_salida |>
          procesar_prop_voto_sen() |>
          mutate(respuesta = voto_sen_candidato,
                 media = round(total*100))|>
          mutate(color_res = case_when(
            grepl('Morena',x = voto_sen_candidato)~"#A6032F",
            grepl('PAN|PRD|PRI',x = voto_sen_candidato)~"#0339a6",
            grepl('PVEM',x = voto_sen_candidato)~"#98BF5E",
            grepl('Morena',x = voto_sen_candidato)~"#A6032F",
            grepl('PT',x = voto_sen_candidato)~"#D91136",
            grepl('MC',x = voto_sen_candidato)~"#F27405",
            grepl('No respondió',x = voto_sen_candidato)~"gray60",
            grepl('Nulo',x = voto_sen_candidato)~"black",
            grepl('Voto nulo',x = voto_sen_candidato)~"black"
          ))|>
          mutate(color_res = ifelse(choque==T,"yellow",color_res))

        colores_voto_sen_candiato<- bd_resultados|>
          select(voto_sen_candidato,color_res)

        colores_voto_sen_candiato<-
          setNames(pull(colores_voto_sen_candiato[2]),
                   pull(colores_voto_sen_candiato[1]))


        g <-
       # bd_resultados %>%
       #   ggplot(aes(x = reorder(voto_sen_candidato, total),
       #              y = total,
       #              color = voto_sen_candidato)) +
       #   geom_point(size = 4) +
       #   geom_linerange(aes(ymin = total_low, ymax = total_upp)) +
       #   coord_flip() +
       #   scale_y_continuous(labels = scales::percent) +
       #   labs(x = "", y = "") +
       #   theme_minimal() +
       #   theme(legend.position = "none",
       #         axis.text = element_text(size = 16))

        bd_resultados|>
          #mutate(total_upp = total_upp+.05)%>%
          ggplot(aes(x = reorder(voto_sen_candidato, total),
                     y = total,
                     color = voto_sen_candidato)) +
          geom_point(size = 4) +
          geom_linerange(aes(ymin = total_low, ymax = total_upp),) +
          coord_flip() +
          scale_y_continuous(labels = scales::percent) +
          labs(x = "", y = "") +
          scale_color_manual(values = colores_voto_sen_candiato)+
          geom_text(aes(label = scales::percent(x = total, accuracy = 1.)),       #agregar
                    nudge_y = 0,nudge_x = -0.3, size = 5, show.legend = F) +   #agregar
          geom_text(aes( label =  scales::percent(x = total_upp, accuracy = 1.), x = voto_sen_candidato, y = total_upp),
                    size = 3, colour = "green",nudge_x = 0.15)+
          geom_text(aes( label =  scales::percent(x = total_low, accuracy = 1.), x = voto_sen_candidato, y = total_low),
                    size = 3, colour = "red",nudge_x = 0.15)+
          theme_minimal() +
          theme(legend.position = "none",
                axis.text = element_text(size = 16))

        return(g)

      })

    output$tendencia_resultados <-
      renderPlot({

        bd_plot <-
        bd_encuesta_salida |>
          mutate(voto_sen_candidato = ifelse(grepl('Gustavo Sánchez y Guadalupe Gutiérrez', voto_sen_candidato),
                                             'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD',voto_sen_candidato),
                 peso = weights(survey::svydesign(id = ~1,
                                                  data = bd_encuesta_salida,
                                                  strata = ~estrato,
                                                  weights = ~peso_estra*peso_individuo)|>
                                  srvyr::as_survey_design()))

        g <-
        bd_plot |>
        count(hora = lubridate::floor_date(Date, "minutes"), voto_sen_candidato) |>
          group_by(hora) |>
          tidyr::complete(tidyr::nesting(voto_sen_candidato),
                   fill = list(n = 0)) |>
          ungroup() |>
          mutate(tot = sum(n), .by = c(hora)) |>
          mutate(n_acum = cumsum(n),
                 tot_acum = cumsum(tot), .by = c(voto_sen_candidato),
                 movil = n_acum/tot_acum) |>
          filter(voto_sen_candidato %in% c("Nulo")) |>
          # tail()
          ggplot(aes(x = hora, y = movil, color = voto_sen_candidato)) +
          geom_point() +
          geom_line() +
          scale_y_continuous(labels = scales::percent) +
          labs(color = "", y = "", subtitle = 'Media móvil intención de voto') +
          theme_minimal() +
          theme(axis.text = element_text(size = 18),
                legend.position = "none",
                axis.title = element_text(size = 24),
                plot.subtitle = element_text(size = 20))

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
