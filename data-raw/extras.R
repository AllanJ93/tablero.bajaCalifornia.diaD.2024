temporal <-
shp_casillas |>
  as_tibble() |>
  # filter(equipo == input$equipo_input) |>
  count(hora = lubridate::floor_date(Date, unit = "hours"), id) |>
  rename(casilla = id, entrevistas = n) |>
  group_by(hora) |>
  tidyr::complete(casilla = unique(muestra_shp$id),
                  fill = list(entrevistas = 0)) |>
  ungroup() |>
  left_join(muestra_shp |>
              distinct(id, municip) |>
              mutate(municip = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
              left_join(bd_equipos, by = c("municip" = "NOMBRE")), by = c("casilla" = "id")) |>
  openxlsx2::write_xlsx(file = "data-raw/entrevistas_casilla_hora.xlsx")

temporal |>
  filter(casilla == "1073B1")





muestra_shp |>
  distinct(id, municip) |>
  mutate(municip = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
  left_join(bd_equipos, by = c("municip" = "NOMBRE"))
