## code to prepare `base_cuestionario_cierre` dataset goes here

bd_simulada_apertura <-
  muestra_shp |>
  as_tibble() |>
  distinct(seccion, tp_csll, id) |>
  sample_n(size = n()*0.7) |>
  mutate(status = "Reportada")

bd_cierre <- bd_simulada_apertura

usethis::use_data(bd_cierre, overwrite = TRUE)
