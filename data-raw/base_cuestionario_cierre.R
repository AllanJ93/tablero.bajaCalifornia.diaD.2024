## code to prepare `base_cuestionario_cierre` dataset goes here

# ESTRUCTURA MINIMA

# bd_simulada_apertura <-
#   muestra_shp |>
#   as_tibble() |>
#   distinct(seccion, tp_csll, id) |>
#   sample_n(size = n()*0.7) |>
#   mutate(status = "Reportada")
#
# bd_cierre <- bd_simulada_apertura

# ESTRCUTURA FINAL

bd_cierre_survey <-
  openxlsx2::read_xlsx(file = "data-raw/bd_cierre_surveytogo.xlsx") |>
  as_tibble() |>
  mutate(id = paste0(seccion, tipo_casilla),
         status = "Reportada") |>
  filter(lubridate::as_datetime("2024-06-02 08:00:00", "America/Tijuana") < lubridate::as_datetime(Date, "America/Tijuana")) |>
  filter(!Srvyr %in% c("Katheryn Hernandez", 'test'))

bd_cierre_survey |>
  couint

bd_cierre <- bd_cierre_survey

usethis::use_data(bd_cierre, overwrite = TRUE)
