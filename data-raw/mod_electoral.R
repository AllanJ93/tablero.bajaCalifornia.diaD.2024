#' electoral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import leaflet dplyr ggplot2
#' @importFrom shiny NS tagList
mod_electoral_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(
      border = FALSE,
      fill = FALSE,
      bg = "#f7f1de",
      sidebar = sidebar(
        class = "bg_light",
        title = "Filtros",
        accordion(
          accordion_panel(
            "Variables",
            icon = bs_icon("sliders"),
            selectInput(ns("variables"),
                        "Variable",
                        choices = c("Ganador" = "ganador", "Índice" = "indice")
            ),
            selectInput(ns("opciones"),
                        "Elecciones",
                        choices = ""
            )
          ),
          accordion_panel(
            "Geografía",
            icon = bs_icon("geo-alt"),
            shinyWidgets::pickerInput(ns("nivel"), "Niveles", choices = bd$info$nivel[-1]),
            shinyWidgets::pickerInput(ns("unidad"), "Secciones", choices = c("No aplica" = ""))
          )
        ),
        shinyWidgets::actionBttn(ns("filtrar"), "Filtrar",
                                 color = "primary", style = "simple",
                                 icon = bs_icon("filter"))
      ),
      card(
        id = ns("vboxes"),
        layout_columns(
          fill = FALSE,
          value_box(
            title = "Ganador",
            theme_color = 'secondary',
            showcase = bs_icon("trophy-fill"),
            value = textOutput(ns("ganador"))
          ),
          value_box(
            title = "Votos obtenidos por Morena",
            theme_color = 'primary',
            showcase = bs_icon("percent"),
            value = textOutput(ns("voto_morena")),
            p("Porcentaje calculado respecto al total de votos")
          ),
          value_box(
            title = "Participación",
            theme_color = 'info',
            showcase = bs_icon("hand-thumbs-up"),
            value = textOutput(ns("participacion"))
          )
        )
      ),
      layout_column_wrap(
        width = 1/2,
        height = 600,
        card(
          card_header("Mapa"),
          leafletOutput(ns("mapa"), height = 600),
          full_screen = T
        ),
        layout_column_wrap(
          width = 1,
          navset_card_tab(
            full_screen = T,
            title = textOutput(ns("card_1")),
            nav_panel(
              shiny::icon("chart-simple"),
              plotOutput(ns("viz_1")),
            ),
            nav_panel(
              shiny::icon("chart-line"),
              plotOutput(ns("viz_2"))
            ),
            nav_panel(
              shiny::icon("circle-info"),
              "Resultados relativos son calculados respecto a lista nominal."
            )
          ),
          card(
            card_header("Distribución de la participación"),
            plotOutput(ns("viz_4"))
          )
        )
      )
    ),
    card(
      id = ns("sankey"),
      card_header("Comparativa electoral"),
      plotOutput(ns("viz_3"))
    )
  )
}

#' electoral Server Functions
#'
#' @noRd
mod_electoral_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      showModal(modalDialog(
        title = "Para empezar",
        "Seleccione sus filtros y dé click en 'Filtrar' para comenzar a visualizar los datos.",
        easyClose = T,
        fade = T,
        footer = tagList(
          modalButton("Ok"),
        )
      ))
    })

    observeEvent(input$nivel, {
      req(!is.null(input$nivel))
      ja <- bd$info$shp[[1]] |>
        as_tibble() |>
        select(contains(gsub("_22", "", input$nivel))) |>
        distinct() |>
        arrange(.data[[glue::glue('nombre_{input$nivel}')]])

      ja <- purrr::set_names(ja[[1]], ja[[2]])

      nombre <- case_when(grepl("municipio", input$nivel) ~ "Municipios",
                          grepl("distritol", input$nivel) ~ "Distritos locales",
                          grepl("distritof", input$nivel) ~ "Distritos federales")

      shinyWidgets::updatePickerInput(session, "unidad", label = nombre,  choices = c("Todos" = "", ja))
    })

    aux_elecciones <- reactiveVal(bd$nombres_elecciones)

    colores <- reactiveVal(bd$info$colores)

    bd_partido <-  eventReactive(input$filtrar, {
      if(!is.null(input$nivel)){
        ja <- shp() |>
          as_tibble()
      } else{
        ja <- data()
      }

      if(input$unidad != "" & !is.null(input$nivel)) {
        ja <- ja |>
          filter(.data[[input$nivel]] == .env$input$unidad)
      }
      return(ja)
    })

    shp_secc <- eventReactive(input$filtrar, {
      ja <- bd$info$shp[["seccion"]]
      if(input$unidad != "" & !is.null(input$nivel)){
        ja <- ja |>
          filter(.data[[input$nivel]] == .env$input$unidad)
      }
      return(ja)
    })

    shp <- eventReactive(input$filtrar, {
      req(!is.null(input$nivel))
      bd$info$shp[[input$nivel]]
    })

    ya <- reactiveValues(historico = "")

    observeEvent(input$filtrar, {
      shinyjs::toggle("vboxes", condition = input$variables == "ganador")
      shinyjs::toggle("sankey", condition = input$variables == "ganador")

      combinacion <- paste(input$nivel, input$unidad, input$opciones)

      if(input$unidad == "" & !is.null(input$nivel) & !combinacion %in% ya$historico) {
        lflt |>
          hideGroup(last(ya$historico)) |>
          clearControls() |>
          addPolygons(data = shp(),
                      weight = 1,  # Aumenta el grosor de la línea
                      stroke = TRUE,
                      color = "grey",
                      fillColor = shp()[[glue::glue("col_{isolate(input$opciones)}")]],
                      opacity = 0.3,
                      fillOpacity = 0.7,
                      group = combinacion,
                      label = ~label_mun()
          )

        ya$historico <- append(ya$historico, combinacion)
      }
      else if((input$unidad != "" | is.null(input$nivel)) & !combinacion %in% ya$historico){
        lflt |>
          clearGroup("unidad") |>
          hideGroup(last(ya$historico)) |>
          clearControls() |>
          addPolygons(data = shp_secc(),
                      weight = 1,
                      stroke = TRUE,
                      group = combinacion,
                      fillOpacity = 0.7,
                      opacity = 1,
                      color = shp_secc()[[glue::glue("col_{isolate(input$opciones)}")]],
                      popup = ~label_leaf()
                        )

        ya$historico <- append(ya$historico, combinacion)
      }
      else{
        lflt |>
          hideGroup(last(ya$historico)) |>
          clearControls() |>
          showGroup(combinacion)

        ya$historico <- append(ya$historico, combinacion)
      }

      if(length(strsplit(last(ya$historico), " ")[[1]]) > 2){
        if(input$unidad != "" | is.null(input$nivel)){
          aux <- shp_secc()
        } else{
          aux <- shp()
        }
        bbox <- aux |>
          sf::st_bbox()

        lflt |>
          flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
      }

      if(input$variables == "indice") {
        c_principal <- colores()[names(colores()) == input$opciones]
        no_principal <- last(colortools::complementary(c_principal, plot = F))

        lab1 <- if_else(input$opciones == "rezago", "Más", "Menos")
        lab2 <- if_else(input$opciones == "rezago", "Menos", "Más")

        lflt |>
          addLegend("topright",
                    colors = rev(c(no_principal,"white", c_principal)),
                    na.label = "NA",
                    opacity = 0.5,
                    labels = rev(c(lab1, rep("",1), lab2)),
                    labFormat = labelFormat(),
                    title = glue::glue("Índice {toupper(input$opciones)}"),
                    group = toupper(input$opciones)
          )
      }
    })

    opciones <- reactiveVal()

    nominal <- eventReactive(input$filtrar, {
      req(input$variables == "ganador" & nrow(data()) > 0)
      bd$info$bd |>
        select(seccion, glue::glue("ele_nominal_{isolate(input$opciones)}")) |>
        filter(seccion %in% data()$seccion) |>
        summarise_if(is.numeric, sum, na.rm = T) |>
        pull()
    })

    label <- reactiveVal("Ganador")

    observeEvent(input$variables, {
      if(input$variables == "ganador"){
        opciones <- aux_elecciones()$eleccion |>
          purrr::set_names(aux_elecciones()$Nombre)

        opciones(opciones)

        label("Ganador")
      }
      else if (input$variables == "indice"){
        opciones <- purrr::set_names(names(colores()), toupper(names(colores())))
        opciones(opciones)
        label("Índices")
      }
      updateSelectInput(session, "opciones",label = label(), choices = opciones())
    })

    nombre_1 <- eventReactive(input$variables, {
      if(input$variables == "ganador"){
        nombre <- "Resultados electorales"
      } else {
        nombre <- "Participación e índice"
      }
      return(nombre)
    })

    output$card_1 <- renderText({
      nombre_1()
    })

    data <- eventReactive(input$filtrar,{
      req(input$opciones != "")
      shp_secc() |>
        as_tibble() |>
        filter(!is.na(.data[[glue::glue("col_{isolate(input$opciones)}")]]))
    })

    absolutos <- eventReactive(input$filtrar, {
      req(input$opciones != "" & input$variables == "ganador")
      absolutos <- bd_partido() |>
        select(contains("ele") & contains(input$opciones)) |>
        summarise_if(is.numeric, sum, na.rm = T)

      return(absolutos)
    })

    abs_longer <- reactive({
      req(nrow(absolutos()) > 0)
      longer <- absolutos() |>
        tidyr::pivot_longer(cols = everything()) |>
        mutate(name = gsub(glue::glue('ele|_|{isolate(input$opciones)}'), "", name))

      return(longer)
    })

    # # Visualizaciones ---------------------------------------------------------

    output$ganador <- renderText({
      req(nrow(absolutos()) > 0)
      quitar <- c("nominal|total|validos|noreg|participacion")
      text <- abs_longer() |>
        filter(!grepl(quitar, name)) |>
        filter(value == max(value)) |>
        slice(1) |>
        pull(name) |>
        toupper()

      return(text)
    })

    output$voto_morena <- renderText({
      req(nrow(absolutos()) > 0)
      if(isolate(input$opciones == "gb_23")) {
        num <- absolutos()[[glue::glue("ele_delfina_{isolate(input$opciones)}")]]
      } else{
        num <- absolutos()[[glue::glue("ele_morena_{isolate(input$opciones)}")]]
      }

      texto <- (num/absolutos()[[glue::glue("ele_participacion_{isolate(input$opciones)}")]]) |>
        scales::percent(0.1)

      return(texto)
    })

    output$participacion <- renderText({
      req(nrow(absolutos()) > 0)
      texto <- (absolutos()[[glue::glue("ele_participacion_{isolate(input$opciones)}")]]/nominal()) |>
        scales::percent(0.1)

      return(texto)
    })

    label_leaf <- eventReactive(input$filtrar, {
      if(input$variables == "ganador") {
        label <- select(data(), all_of(glue::glue("label_{isolate(input$opciones)}"))) |>
          pull(1)
      } else if (input$variables == "indice"){
        label <- select(data(), label_ind_2) |>
          pull(1)
      }
      label <- label |>
        lapply(htmltools::HTML)

      return(label)
    })

    label_mun <- eventReactive(input$filtrar, {
      if(input$variables == "ganador"){
        label <- shp() |>
          as_tibble() |>
          transmute(g = paste0(.data[[glue::glue("nombre_{input$nivel}")]], ", ", "GANADOR: ", toupper(.data[[glue::glue("ganador_{input$opciones}")]]))) |>
          pull(g)
      } else{
        label <- shp() |>
          as_tibble() |>
          transmute(.data[[glue::glue("nombre_{input$nivel}")]]) |>
          pull()
      }
      return(label)
    })

    lflt <- leaflet::leafletProxy("mapa")

    output$mapa <- renderLeaflet({
      centro <- sf::st_centroid(bd$info$shp[[1]] %>%
                                  sf::st_make_valid() %>%
                                  sf::st_union()) %>%
        sf::st_coordinates()

      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = centro[[1]], lat = centro[[2]], zoom = 8)
    })

    output$viz_1 <- renderPlot({
      req(nrow(data()) > 0)
      if(isolate(input$variables) == "ganador"){
        bd_g <- procesar_secciones_ganadas(bd = data(),
                                           eleccion = isolate(input$opciones))

        graficar_barras(bd = bd_g,
                        x = "ganador",
                        y = "pct",
                        label = "label",
                        fill = "ganador",
                        colores = colores(),
                        eje_x = "Partidos ganadores",
                        eje_y = "Secciones ganadas")

      } else if (isolate(input$variables) == "indice"){
        ja <- procesar_pointrange(data(),
                                  indice = isolate(input$opciones),
                                  partidos = isolate(opciones()),
                                  elecciones = colores(),
                                  partido = T)


        indice_pointrange(ja,
                          isolate(input$opciones),
                          grupo = "partidos",
                          colores = colores(),
                          indice = toupper(isolate(input$opciones)),
                          texto = 10)
      }
    }, res = 96)

    output$viz_2 <- renderPlot({
      req(nrow(data()) > 0)
      if(isolate(input$variables) == "ganador"){
        g <- calcular_relativos(bd = abs_longer(),
                                partidos = bd$info$partidos,
                                nominal = nominal())

        graficar_barras(g, x = "name",
                        y = "pct",
                        label = "label",
                        fill = "name",
                        colores = colores(),
                        eje_x = "Partidos",
                        eje_y = "Votos relativos")
      } else if(isolate(input$variables) == "indice"){
        ja <- procesar_pointrange(bd_partido(),
                                  indice = isolate(input$opciones),
                                  partidos = isolate(opciones()),
                                  elecciones = aux_elecciones()$eleccion)
        indice_pointrange(ja,
                          isolate(input$opciones),
                          grupo = "eleccion",
                          colores = isolate(aux_elecciones()$color),
                          indice = isolate(toupper(input$opciones)),
                          texto = 12)
      }
    }, res = 96)

    output$viz_4 <- renderPlot({
      req(nrow(data()) > 0)
      if(isolate(input$variables) == "ganador"){
        data() |>
          graficar_violin(x = glue::glue("ganador_{isolate(input$opciones)}"),
                          y = glue::glue("pct_participacion_{isolate(input$opciones)}"),
                          fill = glue::glue("ganador_{isolate(input$opciones)}"),
                          colores = colores(),
                          eje_x = "Partidos ganadores",
                          eje_y = "Participación")
      } else if(isolate(input$variables) == "indice") {
        graficar_tiles(
          bd = data(),
          x = "quant_participacion",
          y = glue::glue("quant_{isolate(input$opciones)}"),
          low = "#118ab2",
          high = "#ef476f",
          name = "Coincidencias",
          eje_x = "Índice de participación",
          eje_y = glue::glue("Índice {isolate(input$opciones)}")
        )
      }
    }, res = 100)

    output$viz_3 <- renderPlot({
      input$filtrar
      req(isolate(input$variables) == "ganador", nrow(data()) > 0)
      procesar_sankey(data(), elecciones = opciones()) |>
        graficar_sankey(colores = colores())

    }, res = 100)

  })
}

## To be copied in the UI
# mod_electoral_ui("electoral_1")

## To be copied in the server
# mod_electoral_server("electoral_1")
