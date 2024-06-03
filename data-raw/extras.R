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
  group_by(casilla) |>
  summarise(n = sum(entrevistas)) |>
  arrange(n)

temporal |>
  filter(casilla == "1073B1")





muestra_shp |>
  distinct(id, municip) |>
  mutate(municip = gsub(pattern = "[0-9]. ", replacement = "", x = municip)) |>
  left_join(bd_equipos, by = c("municip" = "NOMBRE"))


bd_encuesta_salida |>
  select(id, voto_sen_candidato) |>
  mutate(voto_sen_candidato = ifelse(grepl('Gustavo Sánchez y Guadalupe Gutiérrez', voto_sen_candidato),
                                     'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD',voto_sen_candidato)) |>
  group_by(id) |>
  count(voto_sen_candidato) |>
  ungroup() |>
  tidyr::pivot_wider(id_cols = id, names_from = voto_sen_candidato, values_from = n) |>
  mutate(across(.cols =  !id, .fns = ~ tidyr::replace_na(data = ., replace = 0))) |>
  arrange(desc(`Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD`)) |>
  select(id, `Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD`) |> print(n = Inf)
  summary(n)
  # filter(grepl(pattern = "0398", x = id)) |>
  print(n = Inf)
  head(10) |>
  select(id)

