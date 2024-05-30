## code to prepare `base_cuestionario_encuesta_salida` dataset goes here

bd_simulada_encuesta_salida <-
  muestra_shp |>
  as_tibble() |>
  distinct(seccion, tp_csll, id) |>
  sample_n(size = n()*0.7) |>
  mutate(status = "Reportada")

bd_encuesta_salida <- bd_simulada_encuesta_salida

usethis::use_data(bd_encuesta_salida, overwrite = TRUE)
