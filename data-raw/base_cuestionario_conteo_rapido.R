## code to prepare `base_cuestionario_conteo_rapido` dataset goes here

bd_simulada_conteo_rapido <-
  muestra_shp |>
  as_tibble() |>
  distinct(seccion, tp_csll, id) |>
  sample_n(size = n()*0.7) |>
  mutate(status = "Reportada")

bd_conteo_rapido <- bd_simulada_conteo_rapido

usethis::use_data(bd_conteo_rapido, overwrite = TRUE)
