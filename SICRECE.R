rm(list = ls())

library(rio)
library(dplyr)
library(purrr)

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

setwd('C:/Users/estadisticaumc04/Desktop/SYRA_ordenado/01. Evaluación 2024/02. ECE 4P/01. BD')

marco = import('Marco 4P para ajuste NR.sav')
evaluados = import('BD_Prim_ECE4P_EstudiantesJunto.sav')
save = 'C:/Users/estadisticaumc04/Desktop/SYRA_ordenado/01. Evaluación 2024/02. ECE 4P/03. SICRECE/'

############################################################################
### 01. Caracteristica de la IE, generar archivo para la evaluación censal
caracteristica_ie = function(marco,total_est,grado,tipo){
  marco = marco %>% filter(Eliminar==1)
  if(grado == '6P'){
    marco = marco %>% filter(!is.na(FO_Condicion))
  }else{
    marco = marco
    bd = marco %>% select(cod_mod7,anexo,gestion2,area,caracteristica2,
                          codgeo,Region26,Provincia,Distrito,cen_pob,.data[[total_est]])
    colnames(bd) = c('cod_mod','anexo','Gestion','area','caracteristica','codgeo',
                      'region','Provincia','Distrito','ccpp','estudiantes_programados')
    
  }
  
  if (tipo=='ECE') {
    export(bd,paste0(save,'01 - Características_ IE_',grado,'.xlsx'))
  }
  return(marco)
}
caracteristica_ie(marco,'alum_4P_UMC','4P','ECE')

############################################################################
### 02.Resultados de estudiantes
estudiantes = function(bd,grado,mp1,mp2,nl1,nl2,tipo){
  bd_1 = bd %>% select(cod_mod7,anexo,Seccion,cor_est,dni_est,paterno,materno,
                     nombre1,nombre2,.data[[nl1]],.data[[mp1]])
  colnames(bd_1)[10] = "ID_logro"
  colnames(bd_1)[11] = "medida"
  bd_1 = bd_1 %>% filter(!is.na(medida))
  
  bd_2 = bd %>% select(cod_mod7,anexo,Seccion,cor_est,dni_est,paterno,materno,
                       nombre1,nombre2,.data[[nl2]],.data[[mp2]])
  colnames(bd_2)[10] = "ID_logro"
  colnames(bd_2)[11] = "medida"
  bd_2 = bd_2 %>% filter(!is.na(medida))
  
  bd_1$ID_competencia = 1
  bd_2$ID_competencia = 2
  
  bd_f = rbind(bd_1,bd_2)
  bd_f = bd_f %>% select(cod_mod7,anexo,Seccion,cor_est,ID_competencia,dni_est,
                         paterno,materno,nombre1,nombre2,ID_logro,medida)
  colnames(bd_f) = c('cod_mod','anexo','seccion','codigo_alumno','ID_competencia','dni_estudiante',	
                      'paterno','materno','nombre1','nombre2','ID_logro','medida')
  
  bd_f$medida = round2(bd_f$medida,0)
  bd_f = arrange(bd_f,cod_mod,anexo,seccion,codigo_alumno,ID_competencia,dni_estudiante,
                 paterno,materno,nombre1,nombre2)
  valores_sicrece = c(4, 1, 2, 3)
  bd_f$ID_logro = valores_sicrece[bd_f$ID_logro]
  
  if (tipo=='ECE'){
    a = list()
    a[[1]] = bd_f[1:531089,]
    a[[2]] = bd_f[531090:1062177,]

    export(a,paste0(save,'02 - Resultados Estudiantes_',grado,'.xlsx')) 
  }else{
    return(bd_f)
  }
}
estudiantes(evaluados,'4P','M500_ECE_4P_2024_CT','M500_ECE_4P_2024_MA',
            'grupo_ECE_4P_2024_CT','grupo_ECE_4P_2024_MA','ECE')

############################################################################
### 03.Cobertura
cobertura = function(marco,estudiante_prog,estudiante_eva,tipo){
  marco_1 = marco %>% filter(Eliminar==1)
  
  if (tipo=='ECE') {
  marco_1 = marco_1 
  }else{
  marco_1 = marco_1 %>% filter(!is.na(FO_Condicion)) 
  }

 ## Programados
  cob_1 = marco_1 %>%
          summarise(cantidad_IE_prog = n(),
                    cantitdad_estudiante_prog = sum(.data[[total_estudiante]]))
  cob_1$id_nivel_resultados = 1
  cob_1$id_institucion = '9233000000'
  cob_1 =  cob_1%>% select(id_nivel_resultados,id_institucion,cantidad_IE_prog,cantitdad_estudiante_prog)
  
  cob_2 = marco_1 %>%
          group_by(cod_DRE) %>%
          summarise(cantidad_IE_prog = n(),
                    cantitdad_estudiante_prog = sum(.data[[total_estudiante]]))
  cob_2$id_nivel_resultados = 2
  cob_2$id_institucion = paste0(cob_2$cod_DRE,'00')
  cob_2 = cob_2 %>% select(id_nivel_resultados,id_institucion,cantidad_IE_prog,cantitdad_estudiante_prog)
  
  if (tipo=='ECE') {
  cob_3 = marco_1 %>%
          group_by(cod_UGEL) %>%
          summarise(cantidad_IE_prog = n(),
                    cantitdad_estudiante_prog = sum(.data[[total_estudiante]]))
  cob_3$id_nivel_resultados = 3
  cob_3$id_institucion = cob_3$cod_UGEL
  cob_3 = cob_3 %>% select(id_nivel_resultados,id_institucion,cantidad_IE_prog,cantitdad_estudiante_prog)
    cob_f = rbind(cob_1,cob_2,cob_3)
    rm(cob_1,cob_2,cob_3)
  }else{
    cob_f = rbind(cob_1,cob_2) 
    rm(cob_1,cob_2)
  }
  
  ## Evaluados
  marco_2 = marco_1 %>% filter(FO_Condicion==1)
  
  cob_1 = marco_2 %>%
          summarise(cantidad_IE_ev = n(),
                    cantitdad_estudiante_ev = sum(.data[[estudiante_eva]]))
  cob_1$id_nivel_resultados = 1
  cob_1$id_institucion = '9233000000'
  cob_1 =  cob_1%>% select(id_nivel_resultados,id_institucion,cantidad_IE_ev,cantitdad_estudiante_ev)
  
  cob_2 = marco_2 %>%
          group_by(cod_DRE) %>%
          summarise(cantidad_IE_ev = n(),
                    cantitdad_estudiante_ev = sum(.data[[estudiante_eva]]))
  cob_2$id_nivel_resultados = 2
  cob_2$id_institucion = paste0(cob_2$cod_DRE,'00')
  cob_2 = cob_2 %>% select(id_nivel_resultados,id_institucion,cantidad_IE_ev,cantitdad_estudiante_ev)
  
  if (tipo=='ECE') {
  cob_3 = marco_2 %>%
          group_by(cod_UGEL) %>%
          summarise(cantidad_IE_ev = n(),
                    cantitdad_estudiante_ev = sum(.data[[estudiante_eva]]))
  cob_3$id_nivel_resultados = 3
  cob_3$id_institucion = cob_3$cod_UGEL
  cob_3 = cob_3 %>% select(id_nivel_resultados,id_institucion,cantidad_IE_ev,cantitdad_estudiante_ev)
    
  cob_e = rbind(cob_1,cob_2,cob_3)
  rm(cob_1,cob_2,cob_3) 
  }else{
  cob_e = rbind(cob_1,cob_2)
  rm(cob_1,cob_2) 
  }
  
  cob = merge(cob_f,cob_e,by=c('id_nivel_resultados','id_institucion'))
  cob = cob %>% select(id_nivel_resultados,id_institucion,cantidad_IE_prog,cantidad_IE_ev,
                       cantitdad_estudiante_prog,cantitdad_estudiante_ev)
  colnames(cob) = c('id_nivel_resultados','id_institucion','cant_ie_programadas','cant_ie_evaluadas',
                    'cant_estudiantes_programados','cant_estudiantes_evaluados')
  cob = cob %>%
        mutate(cobertura_ie = round2((cant_ie_evaluadas*100/cant_ie_programadas),1),
               cobertura_estudiantes = round2((cant_estudiantes_evaluados*100/cant_estudiantes_programados),1))
  
  export(cob,paste0(save,'03.1 - Cobertura_',grado,'.xlsx'))
  rm(cob,cob_e,cob_f)
}

cobertura(marco,'alum_4P_UMC','alum_eva_4P','ECE')

############################################################################
### 04.Medida Promedio
mp = function(data, tipo, grado) {
  estratos = if (tipo == "ECE") {
              c("9233000000", "cod_DRE", "cod_UGEL")
              } else {
              c("9233000000", "cod_DRE")
              }
  
  cursos = c("CT", "MA")
  mp_vars = c("M500_ECE_4P_2024_CT","M500_ECE_4P_2024_MA")
  peso_vars = c("ajuste_NR_L", "ajuste_NR_M")
  
  # Función interna para calcular medidas promedio
  res_mp = function(data, estrato, mp, peso, curso) {
    data = data %>%
           mutate('9233000000'='9233000000')
      data %>%
      filter(Entrega_resultados == 1) %>%
      group_by(.data[[estrato]]) %>%
      summarise(
        medida = weighted.mean(.data[[mp]], w = .data[[peso]], na.rm = TRUE),
        .groups = "drop") %>%
      mutate(id_nivel_resultados = case_when(estrato == "9233000000" ~ 1,
                                             estrato == "cod_DRE" ~ 2,
                                             TRUE ~ 3),
            ID_competencia = ifelse(curso == "CT", 1, 2),
            id_institucion = .data[[estrato]]) %>%
      select(id_nivel_resultados, id_institucion, ID_competencia, medida)
  }
  
  # Generar resultados para todas las combinaciones
  resultados_mp = map2_dfr(cursos, seq_along(cursos), function(curso, i) {
    map_dfr(estratos, ~ res_mp(data, .x, mp_vars[i], peso_vars[i], curso))
  })
  
  # Post-procesamiento de resultados
  resultados_mp = resultados_mp %>%
    mutate(id_institucion = ifelse(id_nivel_resultados == 2, paste0(id_institucion, "00"), id_institucion),
           medida = round2(medida, 0)) %>%
    arrange(id_nivel_resultados, id_institucion, ID_competencia)
  
  # Exportar resultados a un archivo Excel
  export(resultados_mp, paste0(save, "03.2 - Medida promedio_",grado,".xlsx"))
}

mp(evaluados,'ECE','4P')

############################################################################
### 05.Medida Promedio por estrato
mp_2 = function(data, tipo, grado) {
  estratos = if (tipo == "ECE") {
    list(c("9233000000", "gestion2"),
         c("9233000000", "area"),
         c("cod_DRE", "gestion2"),
         c("cod_DRE", "area"),
         c("cod_UGEL", "gestion2"),
         c("cod_UGEL", "area"))
  } else {
    list(c("9233000000", "gestion2"),
         c("9233000000", "area"),
         c("cod_DRE", "gestion2"),
          c("cod_DRE", "area"))
  } 
  
  cursos = c("CT", "MA")
  mp_vars = c("M500_ECE_4P_2024_CT","M500_ECE_4P_2024_MA")
  peso_vars = c("ajuste_NR_L", "ajuste_NR_M")
  
  # Función interna para calcular medidas promedio
  res_mp_2 = function(data, estrato1, estrato2, mp, peso, curso) {
    data = data %>%
           mutate('9233000000'='9233000000')
    data %>%
    filter(!is.na(.data[[mp]]), Entrega_resultados == 1) %>%
    group_by(.data[[estrato1]],.data[[estrato2]]) %>%
    summarise(medida = weighted.mean(.data[[mp]], w = .data[[peso]], na.rm = TRUE),.groups = "drop") %>%
    mutate(id_estrato = case_when(estrato2 == "gestion2" ~ 2,
                                  estrato2 == "area" ~ 3,
                                    TRUE ~ 99),
           id_nivel_resultados = case_when(estrato1 == "9233000000" ~ 1,
                                           estrato1 == "cod_DRE" ~ 2,
                                           estrato1 == "cod_UGEL" ~ 3,
                                           TRUE ~ 99),
            ID_competencia = ifelse(curso == "CT", 1, 2),
            id_institucion = .data[[estrato1]],
            id_item_estrato = .data[[estrato2]]) %>%
      select(id_nivel_resultados, id_institucion, ID_competencia,id_estrato,id_item_estrato,medida)
  }
  
  # Generar resultados para todas las combinaciones
  resultados_mp_2 = map_dfr(cursos, function(curso) {
    i = which(cursos == curso)
    map_dfr(estratos, ~ res_mp_2(data, .x[1], .x[2], mp_vars[i], peso_vars[i], curso))
  })
  
  # Post-procesamiento de resultados
  resultados_mp_2 <- resultados_mp_2 %>%
    mutate(
      id_institucion = ifelse(id_nivel_resultados == 2, paste0(id_institucion, "00"), id_institucion),
      medida = round2(medida, 0)
    ) %>%
    arrange(id_nivel_resultados, id_institucion, ID_competencia, id_estrato, id_item_estrato)
  
  # Exportar resultados a un archivo Excel
  export(resultados_mp_2, paste0(save, "03.3 - Medida promedio (Estrato)_",grado,".xlsx"))
}
mp_2(evaluados,'ECE','4P')

### 06.Niveles de logro
nl = function(data, tipo, grado) {
  estratos = if (tipo == "ECE") {
    c("9233000000", "cod_DRE", "cod_UGEL")
  } else {
    c("9233000000", "cod_DRE")
  }
  
  cursos = c("CT", "MA")
  mp_vars = c("grupo_ECE_4P_2024_CT","grupo_ECE_4P_2024_MA")
  peso_vars = c("ajuste_NR_L", "ajuste_NR_M")
  
  # Función interna para calcular medidas promedio
  res_nl = function(data, estrato, nl, peso, curso) {
    data = data %>%
      mutate('9233000000'='9233000000')
    data %>%
      filter(!is.na(.data[[nl]]), Entrega_resultados == 1) %>%
      group_by(.data[[estrato]]) %>%
      count(.data[[nl]], wt = .data[[peso]], name = "ponderado") %>%
      mutate(Porcentaje = 100 * ponderado / sum(ponderado)) %>%
      mutate(id_nivel_resultados = case_when(estrato == "9233000000" ~ 1,
                                             estrato == "cod_DRE" ~ 2,
                                             TRUE ~ 3),
             ID_competencia = ifelse(curso == "CT", 1, 2)) %>%
      rename(id_institucion = .data[[estrato]], ID_logro = .data[[nl]],Cantidad_estudiantes=ponderado)%>%
      select(id_nivel_resultados,id_institucion,ID_competencia,ID_logro,Cantidad_estudiantes,Porcentaje)
  }
  
  # Generar resultados para todas las combinaciones
  resultados_nl = map2_dfr(cursos, seq_along(cursos), function(curso, i) {
    map_dfr(estratos, ~ res_nl(data, .x, mp_vars[i], peso_vars[i], curso))
  })
  
  # Post-procesamiento de resultados
  resultados_nl = resultados_nl %>%
                  mutate(id_institucion = ifelse(id_nivel_resultados == 2, paste0(id_institucion, "00"), id_institucion),
                         Cantidad_estudiantes = round2(Cantidad_estudiantes,0),
                         Porcentaje = round2(Porcentaje,1) ) 
  valores_sicrece = c(4, 1, 2, 3)
  resultados_nl$ID_logro = valores_sicrece[resultados_nl$ID_logro]
  resultados_nl = arrange(resultados_nl,id_nivel_resultados,id_institucion,ID_competencia,ID_logro)
  
  # Exportar resultados a un archivo Excel
  export(resultados_nl, paste0(save, "03.4 - Resultados_",grado,".xlsx"))
}

nl(evaluados,'ECE','4P')

### 07.Niveles de logro por estrato
nl_2 = function(data, tipo, grado) {
  estratos <- if (tipo == "ECE") {
    list(c("9233000000", "gestion2"),
         c("9233000000", "area"),
         c("cod_DRE", "gestion2"),
         c("cod_DRE", "area"),
         c("cod_UGEL", "gestion2"),
         c("cod_UGEL", "area"))
  } else {
    list(c("9233000000", "gestion2"),
         c("9233000000", "area"),
         c("cod_DRE", "gestion2"),
         c("cod_DRE", "area"))
  } 
  cursos = c("CT", "MA")
  mp_vars = c("grupo_ECE_4P_2024_CT","grupo_ECE_4P_2024_MA")
  peso_vars = c("ajuste_NR_L", "ajuste_NR_M")
  
  # Función interna para calcular medidas promedio
  res_nl_2 = function(data, estrato1, estrato2, nl, peso, curso) {
    data = data %>%
      mutate('9233000000'='9233000000')
    data %>%
      filter(!is.na(.data[[nl]]), Entrega_resultados == 1) %>%
      group_by(.data[[estrato1]],.data[[estrato2]]) %>%
      count(.data[[nl]], wt = .data[[peso]], name = "ponderado") %>%
      mutate(Porcentaje = 100 * ponderado / sum(ponderado),
             id_estrato = case_when(estrato2 == "gestion2" ~ 2,
                                    estrato2 == "area" ~ 3,
                                    TRUE ~ 99),
             id_nivel_resultados = case_when(estrato1 == "9233000000" ~ 1,
                                             estrato1 == "cod_DRE" ~ 2,
                                             estrato1 == "cod_UGEL" ~ 3,
                                             TRUE ~ 99),
             ID_competencia = ifelse(curso == "CT", 1, 2)) %>%
      rename(id_institucion = .data[[estrato1]],
             id_item_estrato = .data[[estrato2]],
             ID_logro = .data[[nl]],
             Cantidad_estudiantes=ponderado) %>%
      select(id_nivel_resultados,id_institucion,ID_competencia,id_estrato,id_item_estrato,ID_logro,Cantidad_estudiantes,Porcentaje)
  }
  
  # Generar resultados para todas las combinaciones
  resultados_nl_2 = map_dfr(cursos, function(curso) {
    i = which(cursos == curso)
    map_dfr(estratos, ~ res_nl_2(data, .x[1], .x[2], mp_vars[i], peso_vars[i], curso))
  })
  
  # Post-procesamiento de resultados
  resultados_nl_2 <- resultados_nl_2 %>%
    mutate(
      id_institucion = ifelse(id_nivel_resultados == 2, paste0(id_institucion, "00"), id_institucion),
      Cantidad_estudiantes = round2(Cantidad_estudiantes,0),
      Porcentaje = round2(Porcentaje,1)) 
  
  valores_sicrece = c(4, 1, 2, 3)
  resultados_nl_2$ID_logro = valores_sicrece[resultados_nl_2$ID_logro]
  resultados_nl_2 = arrange(resultados_nl_2,id_nivel_resultados,id_institucion,ID_competencia,id_estrato,id_item_estrato,ID_logro)
    
  # Exportar resultados a un archivo Excel
  export(resultados_nl_2, paste0(save, "03.5 - Resultados (Estratos)_",grado,".xlsx"))
}
nl_2(evaluados,'ECE','4P')

### 08.Sexo por estratos
sexo_estrato = function(data, grado) {
  # Paso 1: Procesar estratos
  process_estratos = function(data, estrato1, estrato2, nl, peso, curso) {
    data <- data %>%
      mutate("9233000000" = "9233000000",
             sexo1 = sexo ) %>%
      filter(!is.na(.data[[nl]]))
    
    niveles = data %>%
      group_by(.data[[estrato1]], .data[[estrato2]]) %>%
      count(.data[[nl]], wt = .data[[peso]], name = "ponderado") %>%
      mutate(Valor = 100 * ponderado / sum(ponderado),
        id_nivel_resultados = 1,
        id_institucion = "9233000000",
        id_competencia = ifelse(curso == "CT", 1, 2),
        id_estrato = 1,
        id_sub_estrato = estrato2) %>%
      rename(id_item_estrato = .data[[estrato1]],
        id_item_sub_estrato = .data[[estrato2]],
        id_logro = .data[[nl]],
        cantidad_estudiantes = ponderado,
        porcentaje = Valor
      ) %>%
      select(
        id_nivel_resultados, id_institucion, id_competencia, id_estrato,
        id_item_estrato, id_sub_estrato, id_item_sub_estrato, id_logro,
        cantidad_estudiantes, porcentaje
      )
    
    niveles$id_item_sub_estrato <- as.character(niveles$id_item_sub_estrato)
    return(niveles)
  }
  
  # Estratos a procesar
  estratos <- list(
    c("sexo", "sexo1"),
    c("sexo", "gestion2"),
    c("sexo", "area")
  )
  
  # Paso 2: Procesar CT
  resultados_ct <- lapply(estratos, function(estrato) {
    process_estratos(data, estrato[1], estrato[2], "grupo_ECE_4P_2024_CT", "ajuste_NR_L", "CT")
  }) %>%
    bind_rows()
  
  # Paso 3: Procesar MA
  resultados_ma <- lapply(estratos, function(estrato) {
    process_estratos(data, estrato[1], estrato[2], "grupo_ECE_4P_2024_MA", "ajuste_NR_M", "MA")
  }) %>%
    bind_rows()
  
  # Paso 4: Unir resultados y realizar transformaciones finales
  bd_f <- bind_rows(resultados_ct, resultados_ma) %>%
    mutate(
      id_logro = c(4, 1, 2, 3)[id_logro],
      id_item_estrato = ifelse(id_item_estrato == 2, 1, 2),
      id_item_sub_estrato = ifelse(
        id_sub_estrato == "sexo1" & id_item_sub_estrato == 1, 2,
        ifelse(id_sub_estrato == "sexo1" & id_item_sub_estrato == 2, 1, id_item_sub_estrato)
      ),
      id_sub_estrato = ifelse(
        id_sub_estrato == "sexo1", 1,
        ifelse(id_sub_estrato == "gestion2", 2, 3)
      )
    ) %>%
    arrange(
      id_nivel_resultados, id_institucion, id_competencia, id_estrato,
      id_item_estrato, id_sub_estrato, id_item_sub_estrato, id_logro
    ) %>%
    mutate(
      cantidad_estudiantes = round2(cantidad_estudiantes, 0),
      porcentaje = round2(porcentaje, 1)
    )
  
  # Paso 5: Exportar resultados
  export(bd_f,paste0(save, 'resultados_estrato_sexo_',grado,'.xlsx'))

}

sexo_estrato(evaluados,'4P')

### 09. sEXO POR ESTUDIANTE
sexo_estudiante = function(data, grado) {
  # Selección de columnas iniciales
  bd = data %>% 
       select(cod_mod7, anexo, Seccion, cor_est, paterno, materno, nombre1, nombre2, sexo)
  
  # Concatenar nombres y limpiar columnas
  bd$nombre = mapply(function(v1, v2) paste0(v1, ifelse(v2 != "", paste0(" ", v2), "")), 
                      bd$nombre1, bd$nombre2)
  bd = bd %>% select(cod_mod7, anexo, Seccion, cor_est, paterno, materno, nombre, sexo)
  
  # Cambiar valores de sexo
  bd$sexo = ifelse(bd$sexo == 1, 2, 1)
  
  # Agregar columna id_estrato y reordenar columnas
  bd$id_estrato =1
  bd = bd %>% select(cod_mod7, anexo, Seccion, cor_est, paterno, materno, nombre, id_estrato, sexo)
  
  # Renombrar última columna
  colnames(bd)[9] <- "id_item_estrato"
  
  # Ordenar los datos
  bd = arrange(bd, cod_mod7, anexo, Seccion, cor_est, paterno, materno, nombre)
  
  # Renombrar columnas finales
  colnames(bd) = c('cod_mod', 'anexo', 'seccion', 'codigo_alumno', 'paterno', 'materno',
                    'nombre1', 'id_estrato', 'id_item_estrato')
  
  # Exportar resultados
  export(bd, paste0(save, 'sexo_por_estudiantes_',grado,'.xlsx'))
}

sexo_estudiante(evaluados,'4P')  
  

