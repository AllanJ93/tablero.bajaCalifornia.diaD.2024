## code to prepare `entregable_encuesta_salida` dataset goes here
library(officer)

bd_result_sen<-
  bd_encuesta_salida|>
  procesar_prop_voto_sen()|>
  mutate(respuesta = voto_sen_candidato,
         media = round(total*100))|>
  mutate(color_res = case_when(
    grepl('Morena',x = voto_sen_candidato)~"#A6032F",
    grepl('PAN|PRD|PRI',x = voto_sen_candidato)~"#0339a6",
    grepl('PVEM',x = voto_sen_candidato)~"#98BF5E",
    grepl('Morena',x = voto_sen_candidato)~"#A6032F",
    grepl('PT',x = voto_sen_candidato)~"#D91136",
    grepl('MC',x = voto_sen_candidato)~"#F27405",
    grepl('No respondió',x = voto_sen_candidato)~"gray60",
    grepl('Nulo',x = voto_sen_candidato)~"black",
    grepl('Voto nulo',x = voto_sen_candidato)~"black"
  ))|>
  mutate(color_res = ifelse(choque==T,"yellow",color_res))

colores_voto_sen_candiato<- bd_result_sen|>
  select(voto_sen_candidato,color_res)

colores_voto_sen_candiato<-
  setNames(pull(colores_voto_sen_candiato['color_res']),
           pull(colores_voto_sen_candiato['voto_sen_candidato']))


bd_result_pr<-
  bd_encuesta_salida|>
  procesar_prop_voto_pr()|>
  mutate(respuesta = voto_pr_candidato,
         media = round(total*100))|>
  mutate(color_res = case_when(
    grepl('Morena',x = voto_pr_candidato)~"#A6032F",
    grepl('PAN|PRD|PRI',x = voto_pr_candidato)~"#0339a6",
    #grepl('PVEM',x = voto_pr_candidato)~"#98BF5E",
    grepl('Morena',x = voto_pr_candidato)~"#A6032F",
    #grepl('PT',x = voto_pr_candidato)~"#D91136",
    grepl('MC',x = voto_pr_candidato)~"#F27405",
    grepl('No respondió',x = voto_pr_candidato)~"gray60",
    grepl('Nulo',x = voto_pr_candidato)~"black",
    grepl('Voto nulo',x = voto_pr_candidato)~"black"
  ))|>
  mutate(color_res = ifelse(choque==T,"yellow",color_res))

colores_voto_pr_candiato<- bd_result_pr|>
  select(voto_pr_candidato,color_res)

colores_voto_pr_candiato<-
  setNames(pull(colores_voto_pr_candiato['color_res']),
           pull(colores_voto_pr_candiato['voto_pr_candidato']))


bd_result_ident_partido<-
  bd_encuesta_salida|>
  procesar_prop_ident_partido()|>
  mutate(respuesta = ident_partido,
         media = round(total*100))|>
  mutate(color_res = case_when(
    grepl('MORENA',x = ident_partido)~"#A6032F",
    grepl('PAN',x = ident_partido)~"#0339a6",
    grepl('PRI',x = ident_partido)~"#038C33",
    grepl('PRD',x = ident_partido)~"#F2B705",
    grepl('PVEM',x = ident_partido)~"#98BF5E",
    #grepl('Morena',x = ident_partido)~"#A6032F",
    grepl('PT',x = ident_partido)~"#D91136",
    grepl('MC',x = ident_partido)~"#F27405",
    grepl('No respondió',x = ident_partido)~"gray60",
    grepl('Nulo',x = ident_partido)~"black",
    grepl('Voto nulo',x = ident_partido)~"black",
    grepl('Ninguno',x = ident_partido)~"black",
    grepl('No sabe',x = ident_partido)~"gray60",
  ))|>
  mutate(choque = F)

colores_ident_partido<- bd_result_ident_partido|>
  select(ident_partido,color_res)

colores_ident_partido<-
  setNames(pull(colores_ident_partido['color_res']),
           pull(colores_ident_partido['ident_partido']))



fecha_formato <- format(lubridate::today(), format = "%Y_%B_%d")

hora <- paste0(lubridate::hour(Sys.time()), "h")



press_pptx_path <- paste0("entregable/",
                          "Avance_encuesta_salida_BC",
                          "_",
                          fecha_formato,
                          "_",
                          hora,
                          ".pptx")


pptx <- read_pptx(path = "./data-raw/plantilla_vladimir.pptx")

add_slide(pptx, layout = "portada", master = "Office Theme") |>
  ph_with(value = "Encuesta de salida para Baja California", location = ph_location_label(ph_label = "titulo"))

#######################################################################
#######################################################################
# Bloque 2: Presidencia de la República 2024 --------------------------------------------------
#######################################################################
#######################################################################
add_slide(pptx, layout = "portada", master = "Office Theme") |>
  ph_with(value = "Voto al Senado de Baja California 2024",
          location = ph_location_label(ph_label = "titulo"))


voto_sen_candidato_graf <-
  bd_result_sen|>
  ggplot(aes(x = reorder(voto_sen_candidato, total),
             y = total,
             color = voto_sen_candidato)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = total_low, ymax = total_upp),) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "") +
  scale_color_manual(values = colores_voto_sen_candiato)+
  geom_text(aes(label = scales::percent(x = total, accuracy = 1.)),       #agregar
            nudge_y = 0,nudge_x = -0.3, size = 8, show.legend = F) +   #agregar
  geom_text(aes( label =  scales::percent(x = total_upp, accuracy = 1.), x = voto_sen_candidato, y = total_upp),
            size = 5, colour = "green",nudge_x = 0.15)+
  geom_text(aes( label =  scales::percent(x = total_low, accuracy = 1.), x = voto_sen_candidato, y = total_low),
            size = 5, colour = "red",nudge_x = 0.15)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16))



add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = voto_sen_candidato_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = "Encuesta de salida para resultados al Senado de Baja California 2024",
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "portada", master = "Office Theme") |>
  ph_with(value = "Voto para la Presidencia de la República México 2024",
          location = ph_location_label(ph_label = "titulo"))

voto_pr_candidato_graf <-
  bd_result_pr|>
  ggplot(aes(x = reorder(voto_pr_candidato, total),
             y = total,
             color = voto_pr_candidato)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = total_low, ymax = total_upp),) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "") +
  scale_color_manual(values = colores_voto_pr_candiato)+
  geom_text(aes(label = scales::percent(x = total, accuracy = 1.)),       #agregar
            nudge_y = 0,nudge_x = -0.3, size = 8, show.legend = F) +   #agregar
  geom_text(aes( label =  scales::percent(x = total_upp, accuracy = 1.), x = voto_pr_candidato, y = total_upp),
            size = 5, colour = "green",nudge_x = 0.15)+
  geom_text(aes( label =  scales::percent(x = total_low, accuracy = 1.), x = voto_pr_candidato, y = total_low),
            size = 5, colour = "red",nudge_x = 0.15)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = voto_pr_candidato_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = "Encuesta de salida para resultados a la Presidencia de México 2024",
          location = ph_location_label(ph_label = "titulo"))

add_slide(pptx, layout = "portada", master = "Office Theme") |>
  ph_with(value = "Identificacion partidista",
          location = ph_location_label(ph_label = "titulo"))


ident_partido_graf <-
  bd_result_ident_partido|>
  ggplot(aes(x = reorder(ident_partido, total),
             y = total,
             color = ident_partido)) +
  geom_point(size = 4) +
  #geom_linerange(aes(ymin = total_low, ymax = total_upp),) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "") +
  scale_color_manual(values = colores_ident_partido)+
  geom_text(aes(label = scales::percent(x = total, accuracy = 1.)),       #agregar
            nudge_y = 0,nudge_x = -0.3, size = 8, show.legend = F) +   #agregar
  #  geom_text(aes( label =  scales::percent(x = total_upp, accuracy = 1.), x = ident_partido, y = total_upp),
  #            size = 5, colour = "green",nudge_x = 0.15)+
  #  geom_text(aes( label =  scales::percent(x = total_low, accuracy = 1.), x = ident_partido, y = total_low),
  #            size = 5, colour = "red",nudge_x = 0.15)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = ident_part_graf, location = ph_location_label(ph_label = "imagen_principal")) |>
  ph_with(value = "Identificacion partidista",
          location = ph_location_label(ph_label = "titulo"))

print(pptx, press_pptx_path)
