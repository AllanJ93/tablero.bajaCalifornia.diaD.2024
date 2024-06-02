## code to prepare `correcciones` dataset goes here

catalogo_correcciones <-
base_correcciones |>
  distinct(Srvyr, casilla_mas_cercana)

usethis::use_data(catalogo_correcciones, overwrite = TRUE)
