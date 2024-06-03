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
                                         choices = c("Apertura", "Encuesta de salida", "Cierre", "Conteo rápido"),
                                         selected = "Cierre",
        ),
        shinycssloaders::withSpinner(leafletOutput(ns("mapa_principal")))
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

        res <-
        muestra_shp %>%
          {
            if(input$cuestionario_input == "Apertura"){
              left_join(., bd_apertura |>
                          select(!seccion), by = "id") %>%
                mutate(status = tidyr::replace_na(replace = "Sin reportar", status),
                       municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
                left_join(bd_equipos, by = c("municipio" = "NOMBRE"))
            }
            else if(input$cuestionario_input == "Cierre") {
              left_join(., bd_cierre |>
                          select(!seccion), by = "id")%>%
                mutate(status = tidyr::replace_na(replace = "Sin reportar", status),
                       municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
                left_join(bd_equipos, by = c("municipio" = "NOMBRE"))
            }
            else if(input$cuestionario_input == "Encuesta de salida") {
              left_join(., bd_encuesta_salida |>
                          select(!seccion), by = "id")%>%
                mutate(status = tidyr::replace_na(replace = "Sin reportar", status),
                       municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
                left_join(bd_equipos, by = c("municipio" = "NOMBRE"))
            }
            else if(input$cuestionario_input == "Conteo rápido") {
              left_join(., bd_conteo_rapido |>
                          select(!seccion), by = "id")%>%
                mutate(status = tidyr::replace_na(replace = "Sin reportar", status),
                       municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
                left_join(bd_equipos, by = c("municipio" = "NOMBRE"))
            }
          }

            if(input$cuestionario_input == "Apertura") {

              ubicaciones_apertura <-
                bd_apertura |>
                filter(!is.na(Longitude) | !is.na(Latitude)) |>
                sf::st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326) |>
                tibble::rownames_to_column(var = "id_entrevista")

              catalogos <-
                muestra_shp |>
                tibble::rownames_to_column(var = "id_casilla") |>
                as_tibble() |>
                select(id_casilla, id)

              casillas_cercanas <-
                st_distance(ubicaciones_apertura,
                            muestra_shp) %>%
                as_tibble %>%
                rowwise() %>%
                mutate(id_casilla = which.min(c_across(everything()))) |>
                tibble::rownames_to_column(var = "id_entrevista") |>
                mutate(id_entrevista = as.character(id_entrevista),
                       id_casilla = as.character(id_casilla)) |>
                left_join(catalogos, by = c("id_casilla" = "id_casilla"))

              base_correcciones <-
                ubicaciones_apertura |>
                left_join(casillas_cercanas, by = "id_entrevista") |>
                mutate(correccion = dplyr::if_else(condition = id.x == id.y,
                                                   true = "Registro correcto",
                                                   false = "Corregida")) |>
                as_tibble() |>
                select(SbjNum, Srvyr, casilla_reportada = id.x, casilla_mas_cercana = id.y)

              ubicaciones_apertura <-
                ubicaciones_apertura |>
                left_join(base_correcciones, by = c("SbjNum", "Srvyr"))

              res <-
              res |>
                left_join(base_correcciones, by = c("SbjNum", "Srvyr")) |>
                # filter(id != casilla_mas_cercana) |>
                # filter(casilla_mas_cercana == "1605B1") |>
                # select(id, casilla_reportada,  casilla_mas_cercana, status) |>
                mutate(id = dplyr::if_else(condition = !is.na(casilla_mas_cercana),
                                           true = casilla_mas_cercana,
                                           false = id)) |>
                mutate(status = dplyr::if_else(condition = id == casilla_mas_cercana,
                                               true = "Reportada",
                                               false = "Sin reportar")) |>
                # filter(id == "1219C1")
                # print(n = Inf)
                mutate(status = tidyr::replace_na(replace = "Sin reportar", status))
            }

        return(res)

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

        # browser()

        if(input$cuestionario_input == "Apertura") {

          ubicaciones_apertura <-
            bd_apertura |>
            filter(!is.na(Longitude) | !is.na(Latitude)) |>
            sf::st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326) |>
            tibble::rownames_to_column(var = "id_entrevista")

          catalogos <-
            muestra_shp |>
            tibble::rownames_to_column(var = "id_casilla") |>
            as_tibble() |>
            select(id_casilla, id)

          casillas_cercanas <-
            st_distance(ubicaciones_apertura,
                        muestra_shp) %>%
            as_tibble %>%
            rowwise() %>%
            mutate(id_casilla = which.min(c_across(everything()))) |>
            tibble::rownames_to_column(var = "id_entrevista") |>
            mutate(id_entrevista = as.character(id_entrevista),
                   id_casilla = as.character(id_casilla)) |>
            left_join(catalogos, by = c("id_casilla" = "id_casilla"))

          base_correcciones <-
            ubicaciones_apertura |>
            left_join(casillas_cercanas, by = "id_entrevista") |>
            mutate(correccion = dplyr::if_else(condition = id.x == id.y,
                                               true = "Registro correcto",
                                               false = "Corregida")) |>
            as_tibble() |>
            select(SbjNum, Srvyr, casilla_reportada = id.x, casilla_mas_cercana = id.y)

          ubicaciones_apertura <-
            ubicaciones_apertura |>
            left_join(base_correcciones, by = c("SbjNum", "Srvyr"))

        }

        if(input$cuestionario_input == "Encuesta de salida") {

          ubicaciones_apertura <-
            bd_encuesta_salida |>
            filter(!is.na(Longitude) | !is.na(Latitude)) |>
            sf::st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326) |>
            tibble::rownames_to_column(var = "id_entrevista")

        }

        pal_status_casillas <-
          leaflet::colorFactor(palette = c("blue", "gray70"),
                               domain = unique(shp_casillas_react()$status))

        mapa_principal <-
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
                    position = "topright") %>%
          addCircleMarkers(
            radius = 7,
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

        if(input$cuestionario_input %in% c("Apertura")) {

          mapa_principal <-
            mapa_principal |>
            addCircleMarkers(
              radius = 3,
              data = ubicaciones_apertura,
              stroke = F,
              color = 'red',
              fillOpacity = 1,
              group = "Apertura",
              label = paste("Usuario: ", ubicaciones_apertura$Srvyr),
              popup = paste(paste0("Casilla reportada: ", ubicaciones_apertura$casilla_reportada),
                            paste("Casilla mas cercana: ", ubicaciones_apertura$casilla_mas_cercana),
                            sep = "<br>"))

        }

        if(input$cuestionario_input %in% c("Encuesta de salida")) {

          mapa_principal <-
            mapa_principal |>
            addCircleMarkers(
              radius = 3,
              data = ubicaciones_apertura,
              stroke = F,
              color = 'red',
              fillOpacity = 1,
              group = "Apertura",
              label = paste(paste("Usuario: ", ubicaciones_apertura$Srvyr),
                            paste("SbjNum: ", ubicaciones_apertura$SbjNum),
                            sep = "<br>")
              )

        }

        return(mapa_principal)
      })

    proxy_mapa_principal <- leafletProxy("mapa_principal")

    output$faltantes <-
      render_gt({
        casillas_abiertas() %>%
          gt()

      })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
