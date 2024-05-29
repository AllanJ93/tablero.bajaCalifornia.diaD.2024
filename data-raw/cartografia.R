## code to prepare `cartografia` dataset goes here

mun_shp <-
  sf::read_sf("H:/Shared drives/Morant Consultores/Insumos/INE/SHP/2023/02 BAJA CALIFORNIA/MUNICIPIO.shp")

usethis::use_data(mun_shp, overwrite = TRUE)

bd_equipos <-
  mun_shp |>
  as_tibble() |>
  distinct(NOMBRE) |>
  mutate(equipo = dplyr::case_when(NOMBRE %in% c("MEXICALI", "SAN FELIPE") ~ paste("1", "MEXICALI", "SAN FELIPE", sep = "-"),
                                   NOMBRE %in% c("ENSENADA", "SAN QUINTIN") ~ paste("2", "ENSENADA", "SAN QUINTIN", sep = "-"),
                                   NOMBRE %in% c("TECATE", "TIJUANA", "PLAYAS DE ROSARITO") ~ paste("3", "TECATE", "TIJUANA", "PLAYAS DE ROSARITO", sep = "-")))

usethis::use_data(bd_equipos, overwrite = TRUE)
