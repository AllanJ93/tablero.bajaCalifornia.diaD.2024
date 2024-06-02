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
  # filter(!Srvyr %in% c("Katheryn Hernandez", 'test'))
  filter(!Srvyr %in% c("Katheryn Hernandez")) |>
  filter(!(Srvyr == 'test' & lubridate::as_date(Date) != lubridate::as_date("2024-06-01")))

bd_cierre <- bd_cierre_survey

usethis::use_data(bd_cierre, overwrite = TRUE)
