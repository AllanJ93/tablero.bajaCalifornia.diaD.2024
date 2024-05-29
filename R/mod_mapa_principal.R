#' mapa_principal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr leaflet highcharter sf
#' @importFrom shiny NS tagList

mod_mapa_principal_ui <- function(id){
  ns <- NS(id)
  bslib::page_navbar(
    shinyjs::useShinyjs(),
    title = "Elecciones 2024 - Baja California",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Mapa",
      bslib::card(
        full_screen = T,
        card_header("Mapa principal"),
        layout_sidebar(
          sidebar = sidebar(
            title = "Menú",
            open = "open",
            id = "control_mapa",
            # h6("Equipo"),
            selectInput(inputId = "equipo_input",
                        label = "Equipo",
                        choices = c("Todos",
                                    sort(unique(bd_equipos$equipo)))
            ),
          ),
          leafletOutput(ns("mapa_principal"))
        )
      ),
      icon = icon("map")
    ),
    bslib::nav_panel(
      title = "Resultados",
      bslib::card(
        full_screen = F,
        card_header("Resultados de la encuesta de salida"),
        max_height = "600px",
        shinyWidgets::progressBar(
          title = "Presencia en casillas de la muestra",
          id = "enc_hechas",
          value = bd_campo_simulada |>
            filter(status %in% c("Abierta")) |>
            distinct(id) |> nrow(),
          display_pct = T,
          striped = T,
          total = 125,
          status = "success"),
        shinycssloaders::withSpinner(highchartOutput(ns("resultados_voto_candidato"))),
      ),
      bslib::card(
        full_screen = F,
        card_header("Segunda tarjeta"),
        bslib::value_box(
          title = "Casillas óptimas",
          value = textOutput(outputId = "faltantes_totales"),
          bsicons::bs_icon(name = "clock"),
          showcase_layout = "top right",
          theme = value_box_theme(bg = "green")),
        bslib::value_box(
          title = "Casillas no óptimas",
          value = textOutput(outputId = "excedentes_totales"),
          bsicons::bs_icon(name = "exclamation-triangle"),
          showcase_layout = "top right",
          theme = value_box_theme(bg = "orange"))),
      icon = icon("line-chart")
    )
  )
}

#' mapa_principal Server Functions
#'
#' @noRd
mod_mapa_principal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$mapa_principal <-
      leaflet::renderLeaflet({

        pal_equipos <-
          leaflet::colorFactor(palette = topo.colors(n_distinct(bd_equipos$equipo)),
                               domain = unique(bd_equipos$equipo))

        # pal_estatus_casillas <-
        #   leaflet::colorFactor(palette = topo.colors(n_distinct(bd_campo_simulada$status)),
        #                        domain = unique(bd_campo_simulada$status))

        shp_casillas <-
          muestra_shp |>
          left_join(bd_campo_simulada) |>
          mutate(status = tidyr::replace_na(replace = "Sin reportar", status)) |>
          group_by(status) |>
          mutate(totales_status = paste(status, n())) |>
          mutate(municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
          left_join(bd_equipos, by = c("municipio" = "NOMBRE"))

        pal_estatus_casillas <-
          leaflet::colorFactor(palette = c("green", "red", "orange", "gray70"),
                               domain = unique(shp_casillas$totales_status))

        mun_shp |>
          left_join(bd_equipos, by = "NOMBRE") |>
          leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            color = ~pal_equipos(equipo),
            opacity = 1,
            weight = 1,
            fill = T,
            fillOpacity = 0.1) |>
          addLegend(title = "Equipos",
                    pal = pal_equipos,
                    values = ~equipo,
                    position = "topright") |>
          addCircleMarkers(
            data = shp_casillas,
            stroke = F,
            color = ~pal_estatus_casillas(totales_status),
            fillOpacity = 1,
            group = "Status de casillas",
            label = paste("Casilla: ", shp_casillas$id),
            popup = paste(paste0("Municipio: ", gsub(pattern = "[0-9].", replacement = "", x = shp_casillas$municip)),
                          paste("Sección: ", shp_casillas$seccion),
                          paste("Tipo casilla: ", shp_casillas$tp_csll),
                          paste("Status: ", shp_casillas$status),
                          paste("Equipo: ", shp_casillas$equipo),
                          sep = "<br>")) |>
          addLegend(title = "Status de casillas",
                    na.label = "Sin status",
                    data = shp_casillas,
                    pal = pal_estatus_casillas,
                    values = ~ totales_status,
                    position = "bottomright")

      })

    output$resultados_voto_candidato <- renderHighchart({

      # browser()

      colores <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")

      bd_resultados <-
        tibble(respuesta = c("Julieta Ramírez Padilla y Armando Ayala por MORENA",
                             "No sabe / No contesta",
                             "Ninguno",
                             "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde",
                             "Gustavo Sánchez Vázquez y Guadalupe Gutiérrez Fregoso por PAN-PRI-PRD",
                             "Jaime Bonilla y Janeth Tapia por el PT",
                             "David Saúl Guakil y Argelia Núñez por Movimiento Ciudadano",
                             "Otro candidato no registrato"),
               media = c(0.46, 0.11, 0.12, 0.1, 0.09, 0.08, 0.05, 0.03)) |>
        mutate(media = media*100)

      g <-
        highchart() |>
        hc_xAxis(categories = bd_resultados$respuesta,
                 labels = list(style = list(fontSize = "18px"))) |>
        hc_yAxis(min = 0,
                 max = 100,
                 tickInterval = 10,
                 labels = list(format = "{value}%"),
                 style = list(fontSize = "18px")) |>
        hc_add_series(name = "PCT",
                      data = bd_resultados$media,
                      type = "bar",
                      # color = colores,
                      zIndex = 1,
                      stacking = "normal") |>
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}%", style = list(fontSize = "24px"))), align = "right") |>
        hc_legend(itemStyle = list(fontSize = "24px", reversed = TRUE))

      return(g)

    })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
