#' mapa_principal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr leaflet highcharter sf gt
#' @importFrom shiny NS tagList

mod_mapa_principal_ui <- function(id){
  ns <- NS(id)
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
          width = "400px",
          selectInput(inputId = ns("equipo_input"),
                      label = "Equipo",
                      choices = c("Todos",
                                  sort(unique(bd_equipos$equipo)))
          ),
          gt_output(ns("faltantes"))
        ),
        shinyWidgets::prettyRadioButtons(inline = T,
                                         inputId = ns("cuestionario_input"),
                                         label = "Cuestionario",
                                         choices = c("Apertura", "Encuesta de salida", "Cierre", "Conteo rápido")
        ),
        leafletOutput(ns("mapa_principal"))
      )
    ),
    icon = icon("map")
  )
}

#' mapa_principal Server Functions
#'
#' @noRd
mod_mapa_principal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shp_casillas_react <-
      reactive({
        input$cuestionario_input

        muestra_shp %>%
          {
            if(input$cuestionario_input == "Apertura"){
              left_join(., bd_apertura)
            }
            else if(input$cuestionario_input == "Cierre") {
              left_join(., bd_cierre)
            }
            else if(input$cuestionario_input == "Encuesta de salida") {
              left_join(., bd_encuesta_salida)
            }
            else if(input$cuestionario_input == "Conteo rápido") {
              left_join(., bd_conteo_rapido)
            }
          } %>%
          mutate(status = tidyr::replace_na(replace = "Sin reportar", status),
                 municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
          left_join(bd_equipos, by = c("municipio" = "NOMBRE"))

      })

    casillas_abiertas <-
      reactive({
      input$equipo_input

      if(input$equipo_input == "Todos") {
        shp_casillas_react() |>
          as_tibble() |>
          filter(status != "Reportada") |>
          count(equipo, status) |>
          tidyr::pivot_wider(names_from = status, values_from = n) |>
          arrange(desc(`Sin reportar`)) |>
          rename(Equipo = equipo,
                 'Casillas sin reportar' = 'Sin reportar')
      }
      else {
        shp_casillas_react() |>
          as_tibble() |>
          filter(equipo == input$equipo_input) |>
          filter(status == "Sin reportar") |>
          select(Casilla = id,
                 Status = status,
                 Municipio = municipio)
      }
    })

    output$mapa_principal <-
      leaflet::renderLeaflet({

        pal_equipos <-
          leaflet::colorFactor(palette = topo.colors(n_distinct(bd_equipos$equipo)),
                               domain = unique(bd_equipos$equipo))

        pal_status_casillas <-
          leaflet::colorFactor(palette = c("green", "gray70"),
                               domain = unique(shp_casillas_react()$status))

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
            data = shp_casillas_react(),
            stroke = F,
            color = ~pal_status_casillas(status),
            fillOpacity = 1,
            group = "Status de casillas",
            label = paste("Casilla: ", shp_casillas_react()$id),
            popup = paste(paste0("Municipio: ", gsub(pattern = "[0-9].", replacement = "", x = shp_casillas_react()$municip)),
                          paste("Sección: ", shp_casillas_react()$seccion),
                          paste("Tipo casilla: ", shp_casillas_react()$tp_csll),
                          paste("Status: ", shp_casillas_react()$status),
                          paste("Equipo: ", shp_casillas_react()$equipo),
                          sep = "<br>")) |>
          addLegend(title = "Status de casillas",
                    na.label = "Sin reportar",
                    data = shp_casillas_react(),
                    pal = pal_status_casillas,
                    values = ~ status,
                    position = "bottomright")

      })

    proxy_mapa_principal <- leafletProxy("mapa_principal")

    output$faltantes <-
      render_gt({
        casillas_abiertas() %>%
          gt()
        # tab_header(title = md(glue::glue("**Cluster {isolate(input$cluster)}**")),
        #            subtitle = glue::glue("Cuota: {balance_cluster()$cuota}   Hecho: {balance_cluster()$hecho}   Faltan: {balance_cluster()$faltan}")) |>
        # tab_spanner(label = "Entrevistas faltanes por edad y sexo", columns = c(Edad, Hombre, Mujer))
      })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
