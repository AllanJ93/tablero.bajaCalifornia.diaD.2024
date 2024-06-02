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
  # filter(!Srvyr %in% c("Katheryn Hernandez", "test"))
  filter(!Srvyr %in% c("Katheryn Hernandez")) |>
  filter(!(Srvyr == 'test' & lubridate::as_date(Date) != lubridate::as_date("2024-06-01")))

bd_conteo_rapido <- bd_conteo_rapido_survey

usethis::use_data(bd_conteo_rapido, overwrite = TRUE)

