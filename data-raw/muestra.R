## code to prepare `muestra` dataset goes here

muestra_shp <-
  sf::read_sf("../conteo_rapido/BC/data/muestra.shp") |>
  tidyr::separate(col = seccion,
                  into = c("entidad", "seccion"),
                  sep = "_",
                  remove = T) |>
  mutate(id = paste0(seccion, tp_csll))

usethis::use_data(muestra_shp, overwrite = TRUE)

bd_campo_simulada <-
  muestra_shp |>
  as_tibble() |>
  mutate(id = paste0(seccion, tp_csll),
         status = sample(c("Incidente", "Abierta", "Cerrada"), n(), replace = TRUE)) |>
  sample_n(size = round(n()*0.4)) |>
  select(id, status)

usethis::use_data(bd_campo_simulada, overwrite = TRUE)
