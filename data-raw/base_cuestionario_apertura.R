## code to prepare `base_cuestionario_apertura` dataset goes here

# ESTRUCTURA MINIMA

# bd_simulada_apertura <-
#   muestra_shp |>
#   as_tibble() |>
#   distinct(seccion, tp_csll, id) |>
#   sample_n(size = n()*0.7) |>
#   mutate(status = "Reportada")
#
# bd_apertura <- bd_simulada_apertura

# ESTRUCUTRA FINAL

bd_apertura_survey <-
  openxlsx2::read_xlsx(file = "data-raw/bd_apertura_surveytogo.xlsx") |>
  as_tibble() |>
  mutate(id = paste0(seccion, tipo_casilla),
         status = "Reportada")

bd_apertura <-
  bd_apertura_survey |>
  filter(!Srvyr %in% c("Katheryn Hernandez", 'test')) |>
  filter(lubridate::as_date(lubridate::today()) == lubridate::floor_date(Date, "days")) |>
  filter(lubridate::as_datetime("2024-06-02 07:00:00", "America/Tijuana") < lubridate::as_datetime(Date, "America/Tijuana"))

usethis::use_data(bd_apertura, overwrite = TRUE)
