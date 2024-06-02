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

# casillas_simuladas <-
#   muestra_shp |>
#   as_tibble() |>
#   select(id) |>
#   sample_n(size = n()*0.87) |>
#   pull()
#
# cuotas_simulacion <-
#   muestra_shp |>
#   as_tibble() |>
#   filter(id %in% casillas_simuladas) |>
#   mutate(muestra = round((lstd_nm/5)*0.74)) |>
#   select(id, muestra)
#
# bd_simulada_encuesta_salida <-
#   purrr::map2_df(.x = cuotas_simulacion$id,
#                  .y = round((cuotas_simulacion$muestra)*0.2),
#                  .f = ~ tibble(id = rep(.x, .y),
#                                voto_candidato_sen = sample(x = c("Julieta Ramírez Padilla y Armando Ayala por MORENA",
#                                                                  "No sabe / No contesta",
#                                                                  "Ninguno",
#                                                                  "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde",
#                                                                  "Gustavo Sánchez Vázquez y Guadalupe Gutiérrez Fregoso por PAN-PRI-PRD",
#                                                                  "Jaime Bonilla y Janeth Tapia por el PT",
#                                                                  "David Saúl Guakil y Argelia Núñez por Movimiento Ciudadano",
#                                                                  "Otro candidato no registrato"),
#                                                            size = .y,
#                                                            replace = T))) |>
#   mutate(status = "Reportada")
#
# bd_encuesta_salida <- bd_simulada_encuesta_salida

# Base simulada con diseno muestral

bd_muestra_pesos <-
  readr::read_csv(file = "./data-raw/muestra_weigth.csv")

usethis::use_data(bd_muestra_pesos, overwrite = TRUE)

bd_datos_recibiods <-
  readr::read_csv(file = "./data-raw/sim_enc_sal_1.csv")

# Genera una secuencia de tiempos espaciados cada 10 minutos
fecha <- as.Date("2023-02-02")
inicio <- lubridate::ymd_hms(paste(fecha, "08:0:00"))
fin <- lubridate::ymd_hms(paste(fecha, "19:00:00"))
secuencia_tiempos <- seq(from = inicio, to = fin, by = "10 mins")

# bd_simulada_encuesta_salida <-
#   union_datos_x_muestra(datos_recibidos = bd_datos_recibiods,
#                         muestra_ori = bd_muestra) |>
#   mutate(status = "Reportada") |>
#   rename(geometria = geometry) |>
#   mutate(Date = sample(x = secuencia_tiempos, size = n(), replace = T))

# Estrucutra base final

bd_encuesta_salida_survey <-
  openxlsx2::read_xlsx(file = "data-raw/bd_encuesta_salida_surveytogo.xlsx", na.strings = "-1") |>
  as_tibble() |>
  mutate(id = paste0(seccion, tipo_casilla),
         status = "Reportada",
         across(.cols = starts_with("voto_sen_"), .fns = ~ as.character(.x))) |>
  filter(lubridate::as_datetime("2024-06-02 08:00:00", "America/Tijuana") < lubridate::as_datetime(Date, "America/Tijuana")) |>
  filter(!Srvyr %in% c("Katheryn Hernandez", 'test')) |>
  mutate(id = case_when(condtion = Srvyr == "PARTIDA RICARDO " ~ "2065C2",
                        T ~ id),
         seccion = case_when(condtion = Srvyr == "PARTIDA RICARDO " ~ "2065",
                        T ~ seccion),
         tipo_casilla = case_when(condtion = Srvyr == "PARTIDA RICARDO " ~ "C2",
                             T ~ tipo_casilla))

# bd_encuesta_salida_survey |>
#   filter(grepl(pattern = "2065", x = id)) |>
#   select(seccion, tipo_casilla, id, Srvyr)

# n_simualciones <- 4000
#
# dummy_base_salida <-
#   tibble(Date = sample(x = secuencia_tiempos, size = n_simualciones, replace = T),
#          id = sample(x = sample(x = muestra_shp$id, size = round(nrow(muestra_shp)*0.7) ) , size = n_simualciones, replace = T),
#          voto_sen_candidato_O1 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O2 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O3 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O4 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O5 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O6 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O7 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O8 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T),
#          voto_sen_candidato_O9 = sample(x = unique(c(bd_encuesta_salida_survey$voto_sen_candidato_O1, NA_character_, "Juan Carlos Hank Krauss y Mónica Vega por el Partido Verde")), size = n_simualciones, replace = T)) |>
#   mutate(status = "Reportada")

bd_encuesta_salida_simulada <-
  bd_encuesta_salida_survey #|>
  # bind_rows(dummy_base_salida)

bd_encuesta_salida <-
  homologacion_sen_cand(datos_recibidos = bd_encuesta_salida_simulada) |>
  union_datos_x_muestra(muestra_ori = bd_muestra_pesos)
  # head(1)

usethis::use_data(bd_encuesta_salida, overwrite = TRUE)

