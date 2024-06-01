
bd_temporal <-
  homologacion_sen_cand(datos_recibidos = bd_encuesta_salida)


bd_temporal_2 <-
union_datos_x_muestra(datos_recibidos = bd_temporal,
                      muestra_ori = bd_muestra_pesos)

bd_temporal_2 |>
procesar_prop_voto_sen()

bd_temporal_2 |>
  procesar_tot_voto_sen() |>
  summarise(sum(total))
