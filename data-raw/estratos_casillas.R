## code to prepare `estratos_casillas` dataset goes here

estratos_casillas <-
readr::read_csv(file = "./data-raw/casilla_clasi.csv") |>
  as_tibble()

usethis::use_data(estratos_casillas, overwrite = TRUE)
