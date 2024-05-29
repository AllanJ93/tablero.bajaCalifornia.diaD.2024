

casillas_muestra <-
  readr::read_csv(file = "../conteo_rapido/BC/data/muestra.csv") |>
  as_tibble() |>
  select(municipio, seccion, tipo_casilla, domicilio) |>
  tidyr::separate(col = seccion,
                  into = c("entidad", "seccion")) |>
  mutate(municipio = gsub(pattern = "[0-9]. ", replacement = "", x = municipio)) |>
  select(!entidad)

casillas_muestra |>
  openxlsx2::write_xlsx(file = "H:/Shared drives/Morant Consultores/Clientes/Vladimir_Ramirez/ExitPoll_BC_2024/Insumos/muestra_casillas.xlsx")
