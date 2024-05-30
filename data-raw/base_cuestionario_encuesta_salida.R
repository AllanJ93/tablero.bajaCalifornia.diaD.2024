## code to prepare `base_cuestionario_encuesta_salida` dataset goes here

# Base m[inima para funcionamiento del tablero

# bd_simulada_encuesta_salida <-
#   muestra_shp |>
#   as_tibble() |>
#   distinct(seccion, tp_csll, id) |>
#   sample_n(size = n()*0.7) |>
#   mutate(status = "Reportada")
#
# bd_encuesta_salida <- bd_simulada_encuesta_salida

# Base a partir de la muestra

casillas_simuladas <-
  muestra_shp |>
  as_tibble() |>
  select(id) |>
  sample_n(size = n()*0.87) |>
  pull()

cuotas_simulacion <-
  muestra_shp |>
  as_tibble() |>
  filter(id %in% casillas_simuladas) |>
  mutate(muestra = round((lstd_nm/5)*0.74)) |>
  select(id, muestra)

bd_simulada_encuesta_salida <-
  purrr::map2_df(.x = cuotas_simulacion$id,
                 .y = round((cuotas_simulacion$muestra)*0.2),
                 .f = ~ tibble(id = rep(.x, .y),
                               voto_candidato_sen = sample(x = c("Julieta Ramírez Padilla y Armando Ayala por MORENA",
                                                                 "No sabe / No contesta",
                                                                 "Ninguno",
                                                                 "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde",
                                                                 "Gustavo Sánchez Vázquez y Guadalupe Gutiérrez Fregoso por PAN-PRI-PRD",
                                                                 "Jaime Bonilla y Janeth Tapia por el PT",
                                                                 "David Saúl Guakil y Argelia Núñez por Movimiento Ciudadano",
                                                                 "Otro candidato no registrato"),
                                                           size = .y,
                                                           replace = T))) |>
  mutate(status = "Reportada")

bd_encuesta_salida <- bd_simulada_encuesta_salida

usethis::use_data(bd_encuesta_salida, overwrite = TRUE)
