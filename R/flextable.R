flextable(cwidth = 2, cheight = 1) %>%
  theme_vanilla() %>%
  color(j = c(1:4, 6:11), color = color_morena, part = "header") %>%
  bold(j = c("Diferencia\nventaja\n(puntos)"), bold = TRUE, part = "body") %>%
  bg(j = c("Diferencia\nventaja\n(puntos)"), bg = "#E2F0D9", part = "body") %>%
  bg(j = c("Diferencia\nventaja\n(puntos)"), bg = "#E2F0D9", part = "header") %>%
  bg(i = ~ `Casa Encuestadora` == "RESULTADO GPPOLLS", bg = color_morena, part = "body") %>%
  color(i = ~ `Casa Encuestadora` == "RESULTADO GPPOLLS", color = "white", part = "body") %>%
  bold(i = ~ `Casa Encuestadora` == "RESULTADO GPPOLLS", bold = T, part = "body") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  autofit()
