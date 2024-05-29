#' procesamiento
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

union_datos_x_muestra <-function(datos_recibidos, muestra_ori){
  datos_recibidos|>
    mutate(id_casilla = paste0(sprintf('%04d',seccion),tipo_casilla))|>
    left_join(muestra_ori|>
                select(id_casilla,municipio,tipo_seccion,casilla,clasificacion,estrato,N,n,peso)|>
                rename(N_ori=N,n_ori=n,peso_ori=peso,municipio_ori = municipio), by = "id_casilla")
}
