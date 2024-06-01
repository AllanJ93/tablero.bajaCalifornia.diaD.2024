## code to prepare `credenciales` dataset goes here

library(shinyauthr)
library(sodium)
library(dplyr)

user_base <-
  data.frame(
  user = c("kathy", "admin", "vladimir"),
  permissions = c("admin", "admin", "admin"),
  name = c("Katheryn", "Soporte Técnico", "Vladimir"),
  stringsAsFactors = FALSE,
  row.names = NULL,
  password = c("luna", "1", "HongKong") %>%
    purrr::map_chr(~sodium::password_store(.x))
)

usethis::use_data(user_base, overwrite = TRUE)

# shinymanager::create_db(
#   credentials_data = credentials,
#   sqlite_path = "inst/app/data/credenciales.sqlite", # will be created
#   passphrase = "morantconsultores"
# )
