#' procesamiento
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Homolgacion de resultados ----------------------------------------------

# Homologa los resultados de las multiples casillas de eleccion para senado
# Todo se reduce a una sola variable llamada "voto_sen_candidato"

homologacion_sen_cand<- function(datos_recibidos){

  datos_recibidos<-
    datos_recibidos|>
    as_tibble()|>
    mutate(
      bool_1 = ifelse(!is.na(voto_sen_candidato_O1),1,0),
      bool_2 = ifelse(!is.na(voto_sen_candidato_O2),1,0),
      bool_3 = ifelse(!is.na(voto_sen_candidato_O3),1,0),
      bool_4 = ifelse(!is.na(voto_sen_candidato_O4),1,0),
      bool_5 = ifelse(!is.na(voto_sen_candidato_O5),1,0),
      bool_6 = ifelse(!is.na(voto_sen_candidato_O6),1,0),
      bool_7 = ifelse(!is.na(voto_sen_candidato_O7),1,0),
      bool_8 = ifelse(!is.na(voto_sen_candidato_O8),1,0),
      bool_9 = ifelse(!is.na(voto_sen_candidato_O9),1,0),
      is_gus_1 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O1),1,0),
      is_gus_2 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O2),1,0),
      is_gus_3 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O3),1,0),
      is_gus_4 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O4),1,0),
      is_gus_5 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O5),1,0),
      is_gus_6 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O6),1,0),
      is_gus_7 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O7),1,0),
      is_gus_8 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O8),1,0),
      is_gus_9 = ifelse(grepl("Gustavo Sánchez y Guadalupe Gutiérrez",voto_sen_candidato_O9),1,0)
    )

  datos_recibidos<-
    datos_recibidos|>
    mutate(ctrol_1 = rowSums(across(starts_with("bool_"))))|>
    mutate(voto_sen_candidato = ifelse(ctrol_1>3,'Nulo',NA) )|>
    mutate(voto_sen_candidato = ifelse(ctrol_1==1,
                                       gsub('_NA|NA_|','',
                                            paste(
                                              voto_sen_candidato_O1,
                                              voto_sen_candidato_O2,
                                              voto_sen_candidato_O3,
                                              voto_sen_candidato_O4,
                                              voto_sen_candidato_O5,
                                              voto_sen_candidato_O6,
                                              voto_sen_candidato_O7,
                                              voto_sen_candidato_O8,
                                              voto_sen_candidato_O9,sep = '_')
                                       ), voto_sen_candidato))|>
    mutate( ctrol_2 =rowSums(across(starts_with("is_gus_"))))|>
    mutate(voto_sen_candidato = ifelse(((ctrol_2==2 & ctrol_1 == 2)|(ctrol_2==3 & ctrol_1 == 3) ),
                                       gsub('_NA|NA_|_Gustavo Sánchez y Guadalupe Gutiérrez del','',
                                            paste(
                                              voto_sen_candidato_O1,
                                              voto_sen_candidato_O2,
                                              voto_sen_candidato_O3,
                                              voto_sen_candidato_O4,
                                              voto_sen_candidato_O5,
                                              voto_sen_candidato_O6,
                                              voto_sen_candidato_O7,
                                              voto_sen_candidato_O8,
                                              voto_sen_candidato_O9,sep = '_')
                                       ), voto_sen_candidato))|>
    mutate(voto_sen_candidato = ifelse(is.na(voto_sen_candidato),'Nulo',voto_sen_candidato))|>
    select(-starts_with('bool'),-starts_with('is_gus'))|>
    mutate(voto_sen_candidato = case_when(
      grepl('PRD PAN',voto_sen_candidato) ~  'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRD',
      grepl('PRI PAN',voto_sen_candidato) ~  'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI',
      grepl('PRD PRI',voto_sen_candidato) ~  'Gustavo Sánchez y Guadalupe Gutiérrez del PRI PRD',
      .default = voto_sen_candidato
    ) )|>
    mutate(voto_sen_candidato=  ifelse(grepl('PRD',voto_sen_candidato)&grepl('PRI',voto_sen_candidato)&grepl('PAN',voto_sen_candidato),
                                       'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD',voto_sen_candidato))


}

# Union de respuestas con base de la muestra ----------------------------------------------

# Se anexan resultados de la encuesta y se les agregan variables de
# estratificacion incluidas en la base muestra_weight, esta se sustituye en
# muestra_ori

# La base resultante de "homologacion_sen_cand" se utiliza en datos_recibidos

union_datos_x_muestra <-function(datos_recibidos, muestra_ori){
  result<-datos_recibidos|>
    left_join(muestra_ori|>
                transmute(id_casilla,municipio,tipo_seccion,casilla,clasificacion,estrato,N,n,peso)|>
                rename(N_ori=N,n_ori=n,peso_ori=peso,municipio_ori = municipio), by = c("id" = "id_casilla"))

  result|>
    left_join(result|>
                distinct(estrato,id)|>
                count(estrato,sort = T), by='estrato')|>
    mutate(peso_estra = N_ori/n)|>
    mutate(peso_individuo = 5)
}

# Base procesada con objeto survey tomando todas las opciones de coalicion ----------------------------------------------
# Por proprciones
#' Title
#'
#' @param datos_recibidos
#'
#' @return
#' @export
#'
#' @examples
procesar_prop_voto_sen_tod_op <-function(datos_recibidos){
  design <- survey::svydesign(id = ~1,
                      data = datos_recibidos,
                      strata = ~estrato,
                      weights = ~peso_estra*peso_individuo)|>
    srvyr::as_survey_design()


  design|>
    group_by(voto_sen_candidato)|>
    summarise(total = srvyr::survey_mean(vartype = 'ci', df = Inf ,level = 0.99))|>
    mutate(total_low = ifelse(total_low<0,0.0001,total_low ) )
}


# Por totales de votos
procesar_tot_voto_sen_tod_op <-function(datos_recibidos){
  design <- survey::svydesign(id = ~1,
                      data = datos_recibidos,
                      strata = ~estrato,
                      weights = ~peso_estra*peso_individuo)|>
    srvyr::as_survey_design()


  design|>
    group_by(voto_sen_candidato)|>
    summarise(total = srvyr::survey_total(vartype = 'ci', df = Inf ,level = 0.99))|>
    mutate(total_low = ifelse(total_low<0,0.0001,total_low ) )
}


# Base procesada con objeto survey solo por candidatos----------------------------------------------
# Por proprciones
procesar_prop_voto_sen <-function(datos_recibidos){
  datos_recibidos<-datos_recibidos|>
    mutate(voto_sen_candidato = ifelse(grepl('Gustavo Sánchez y Guadalupe Gutiérrez', voto_sen_candidato),
                                       'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD',voto_sen_candidato) )


  design <- survey::svydesign(id = ~1,
                      data = datos_recibidos,
                      strata = ~estrato,
                      weights = ~peso_estra*peso_individuo)|>
    srvyr::as_survey_design()


  design|>
    group_by(voto_sen_candidato)|>
    summarise(total = srvyr::survey_mean(vartype = 'ci', df = Inf ,level = 0.99))|>
    mutate(total_low = ifelse(total_low<0,0.0001,total_low ) )
}

# Por votos totales
procesar_tot_voto_sen <-function(datos_recibidos){
  datos_recibidos<-datos_recibidos|>
    mutate(voto_sen_candidato = ifelse(grepl('Gustavo Sánchez y Guadalupe Gutiérrez', voto_sen_candidato),
                                       'Gustavo Sánchez y Guadalupe Gutiérrez del PAN PRI PRD',voto_sen_candidato) )


  design <- survey::svydesign(id = ~1,
                      data = datos_recibidos,
                      strata = ~estrato,
                      weights = ~peso_estra*peso_individuo)|>
    srvyr::as_survey_design()


  design|>
    group_by(voto_sen_candidato)|>
    summarise(total = srvyr::survey_total(vartype = 'ci', df = Inf ,level = 0.99))|>
    mutate(total_low = ifelse(total_low<0,0.0001,total_low ) )
}


# Base procesada con objeto survey para identificacion partidista ----------------------------------------------
# Por proprciones
procesar_prop_ident_partido <-function(datos_recibidos){
  design <- survey::svydesign(id = ~1,
                      data = datos_recibidos,
                      strata = ~estrato,
                      weights = ~peso_estra*peso_individuo)|>
    srvyr::as_survey_design()


  design|>
    group_by(ident_partido)|>
    summarise(total = srvyr::survey_mean(vartype = 'ci', df = Inf ,level = 0.99))|>
    mutate(total_low = ifelse(total_low<0,0.0001,total_low ) )
}

# Por cantidad de respuestas
procesar_tot_ident_partido <-function(datos_recibidos){
  design <- survey::svydesign(id = ~1,
                      data = datos_recibidos,
                      strata = ~estrato,
                      weights = ~peso_estra*peso_individuo)|>
    srvyr::as_survey_design()


  design|>
    group_by(ident_partido)|>
    summarise(total = srvyr::survey_total(vartype = 'ci', df = Inf ,level = 0.99))|>
    mutate(total_low = ifelse(total_low<0,0.0001,total_low ) )
}

