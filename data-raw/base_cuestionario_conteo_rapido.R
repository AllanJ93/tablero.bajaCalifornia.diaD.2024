## code to prepare `base_cuestionario_conteo_rapido` dataset goes here

# ESTRCTURA MINIMA

# bd_simulada_conteo_rapido <-
#   muestra_shp |>
#   as_tibble() |>
#   distinct(seccion, tp_csll, id) |>
#   sample_n(size = n()*0.7) |>
#   mutate(status = "Reportada")
#
# bd_conteo_rapido <- bd_simulada_conteo_rapido

bd_conteo_rapido_survey <-
  openxlsx2::read_xlsx(file = "data-raw/bd_conteo_rapido_surveytogo.xlsx") |>
  as_tibble() |>
  mutate(id = paste0(seccion, tipo_casilla),
         status = "Reportada") |>
  filter(lubridate::as_datetime("2024-06-02 08:00:00", "America/Tijuana") < lubridate::as_datetime(Date, "America/Tijuana")) |>
  filter(!Srvyr %in% c("Katheryn Hernandez", 'test'))

bd_conteo_rapido <- bd_conteo_rapido_survey

usethis::use_data(bd_conteo_rapido, overwrite = TRUE)

