# Calcula los datos y los guarda en la base de datos

# Librerias ----
library(tidyverse) # 1.3.2
library(DBI) # 1.1.3
library(data.table) # 1.14.6
library(foreign) # 0.8-84

# Ejercicio completo con tabla de mortalidad ----

# 1.- Importar ----

# Descargan los archivos de Inegi y se descomprimen en disco duro externo.
# Página web o sitio de consulta:
# https://www.inegi.org.mx/programas/mortalidad/#Microdatos
path <- "/media/mario/home/R/Mortalidad/"

# Funciones para etiquetar
## Etiqueta las variables categóricas simples
etiqueta_base <- function(defun) {
  defun <- defun |>
    mutate(TLOC_RESID = recode_factor(TLOC_RESID,
                                      "1" = "De 1 a 999",
                                      "2" = "De 1 000 a 1 999",
                                      "3" = "De 2 000 a 2 499",
                                      "4" = "De 2 500 a 4 999",
                                      "5" = "De 5 000 a 9 999",
                                      "6" = "De 10 000 a 14 999",
                                      "7" = "De 15 000 a 19 999",
                                      "8" = "De 20 000 a 29 999",
                                      "9" = "De 30 000 a 39 999",
                                      "10" = "De 40 000 a 49 999",
                                      "11" = "De 50 000 a 74 999",
                                      "12" = "De 75 000 a 99 999",
                                      "13" = "De 100 000 a 249 999",
                                      "14" = "De 250 000 a 499 999",
                                      "15" = "De 500 000 a 999 999",
                                      "16" = "De 1 000 000 a 1 499 999",
                                      "17" = "De 1 500 000 y más",
                                      "99" = "No especificado", .ordered = TRUE))
  
  defun <- defun |>
    mutate(TLOC_OCURR = recode_factor(TLOC_OCURR,
                                      "1" = "De 1 a 999",
                                      "2" = "De 1 000 a 1 999",
                                      "3" = "De 2 000 a 2 499",
                                      "4" = "De 2 500 a 4 999",
                                      "5" = "De 5 000 a 9 999",
                                      "6" = "De 10 000 a 14 999",
                                      "7" = "De 15 000 a 19 999",
                                      "8" = "De 20 000 a 29 999",
                                      "9" = "De 30 000 a 39 999",
                                      "10" = "De 40 000 a 49 999",
                                      "11" = "De 50 000 a 74 999",
                                      "12" = "De 75 000 a 99 999",
                                      "13" = "De 100 000 a 249 999",
                                      "14" = "De 250 000 a 499 999",
                                      "15" = "De 500 000 a 999 999",
                                      "16" = "De 1 000 000 a 1 499 999",
                                      "17" = "De 1 500 000 y más",
                                      "99" = "No especificado", .ordered = TRUE))
  
  defun <- defun |>
    mutate(SEXO = recode_factor(SEXO,
                                "1" = "Hombre",
                                "2" = "Mujer",
                                "9" = "No especificado"))
  
  defun <- defun |>
    mutate(MES_OCURR = recode_factor(MES_OCURR,
                                     "1" = "Enero",
                                     "2" = "Febrero",
                                     "3" = "Marzo",
                                     "4" = "Abril",
                                     "5" = "Mayo",
                                     "6" = "Junio",
                                     "7" = "Julio",
                                     "8" = "Agosto",
                                     "9" = "Septiembre",
                                     "10" = "Octubre",
                                     "11" = "Noviembre",
                                     "12" = "Diciembre",
                                     "99" = "No especificado"))
  
  defun <- defun |>
    mutate(MES_REGIS = recode_factor(MES_REGIS,
                                     "1" = "Enero",
                                     "2" = "Febrero",
                                     "3" = "Marzo",
                                     "4" = "Abril",
                                     "5" = "Mayo",
                                     "6" = "Junio",
                                     "7" = "Julio",
                                     "8" = "Agosto",
                                     "9" = "Septiembre",
                                     "10" = "Octubre",
                                     "11" = "Noviembre",
                                     "12" = "Diciembre"))
  
  defun <- defun |>
    mutate(MES_NACIM = recode_factor(MES_NACIM,
                                     "1" = "Enero",
                                     "2" = "Febrero",
                                     "3" = "Marzo",
                                     "4" = "Abril",
                                     "5" = "Mayo",
                                     "6" = "Junio",
                                     "7" = "Julio",
                                     "8" = "Agosto",
                                     "9" = "Septiembre",
                                     "10" = "Octubre",
                                     "11" = "Noviembre",
                                     "12" = "Diciembre",
                                     "99" = "No especificado"))
  
  defun <- defun |>
    mutate(OCUPACION = recode_factor(OCUPACION,
                                     "1" = "Funcionarios, directores y jefes",
                                     "2" = "Profesionistas y técnicos",
                                     "3" = "Trabajadores auxiliares en actividades administrativas",
                                     "4" = "Comerciantes, empleados en ventas y agentes de ventas",
                                     "5" = "Trabajadores en servicios personales y de vigilancia",
                                     "6" = "Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca",
                                     "7" = "Trabajadores artesanales, en la construcción y otros oficios",
                                     "8" = "Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte",
                                     "9" = "Trabajadores en actividades elementales y de apoyo",
                                     "10" = "Busca trabajo",
                                     "11" = "No trabaja",
                                     "97" = "No aplica a menores de 5 años",
                                     "98" = "Insuficientemente especificada",
                                     "99" = "No especificada"))
  
  defun <- defun |>
    mutate(ESCOLARIDA = recode_factor(ESCOLARIDA,
                                      "1" = "Sin escolaridad",
                                      "2" = "Preescolar",
                                      "3" = "Primaria incompleta",
                                      "4" = "Primaria completa",
                                      "5" = "Secundaria incompleta",
                                      "6" = "Secundaria completa",
                                      "7" = "Bachillerato o preparatoria incompleto",
                                      "8" = "Bachillerato o preparatoria completo",
                                      "9" = "Profesional",
                                      "10" = "Posgrado",
                                      "88" = "No aplica a menores de 3 años",
                                      "99" = "No especificado"))
  
  defun <- defun |>
    mutate(EDO_CIVIL = recode_factor(EDO_CIVIL,
                                     "1" = "Soltero(a)",
                                     "2" = "Divorciado(a)",
                                     "3" = "Viudo(a)",
                                     "4" = "Unión libre",
                                     "5" = "Casado(a)",
                                     "6" = "Separado(a)",
                                     "8" = "No aplica a menores de 12 años",
                                     "9" = "No especificado"))
  
  defun <- defun |>
    mutate(PRESUNTO = recode_factor(PRESUNTO,
                                    "1" = "Accidente",
                                    "2" = "Homicidio",
                                    "3" = "Suicidio",
                                    "4" = "Se ignora",
                                    "5" = "Operaciones legales y de guerra",
                                    "8" = "No aplica para muerte natural"))
  
  defun <- defun |>
    mutate(OCURR_TRAB = recode_factor(OCURR_TRAB,
                                      "1" = "Sí",
                                      "2" = "No",
                                      "8" = "No aplica para muerte natural y menores de 5 años",
                                      "9" = "No especificado"))
  
  defun <- defun |>
    mutate(LUGAR_OCUR = recode_factor(LUGAR_OCUR,
                                      "0" = "Vivienda particular",
                                      "1" = "Vivienda colectiva",
                                      "2" = "Escuela u oficina pública",
                                      "3" = "Áreas deportivas",
                                      "4" = "Calle o carretera (vía pública)",
                                      "5" = "Área comercial o de servicios",
                                      "6" = "Área industrial (taller, fabrica u obra)",
                                      "7" = "Granja (rancho o parcela)",
                                      "8" = "Otro",
                                      "9" = "Se ignora",
                                      "88" = "No aplica para muerte natural"))
  
  defun <- defun |>
    mutate(NECROPSIA = recode_factor(NECROPSIA,
                                     "1" = "Sí",
                                     "2" = "No",
                                     "9" = "No especificada"))
  
  defun <- defun |>
    mutate(ASIST_MEDI = recode_factor(ASIST_MEDI,
                                      "1" = "Con asistencia médica",
                                      "2" = "Sin asistencia médica",
                                      "9" = "No especificada"))
  
  defun <- defun |>
    mutate(SITIO_OCUR = recode_factor(SITIO_OCUR,
                                      "1" = "Secretaría de Salud",
                                      "2" = "IMSS PROSPERA",
                                      "3" = "IMSS",
                                      "4" = "ISSSTE",
                                      "5" = "PEMEX",
                                      "6" = "Secretaría de la Defensa Nacional (SEDENA)",
                                      "7" = "Secretaría de Marina (SEMAR)",
                                      "8" = "Otra unidad pública",
                                      "9" = "Unidad médica privada",
                                      "10" = "Vía pública",
                                      "11" = "Hogar",
                                      "12" = "Otro lugar",
                                      "99" = "No especificado"))
  
  defun <- defun |>
    mutate(COND_CERT = recode_factor(COND_CERT,
                                     "1" = "Médico tratante",
                                     "2" = "Médico legista",
                                     "3" = "Otro médico",
                                     "4" = "Persona autorizada por la SSA",
                                     "5" = "Autoridad civil",
                                     "8" = "Otra",
                                     "9" = "No especificada"))
  
  defun <- defun |>
    mutate(NACIONALID = recode_factor(NACIONALID,
                                      "1" = "Mexicana",
                                      "2" = "Extranjera",
                                      "9" = "No especificada"))
  
  defun <- defun |>
    mutate(DERECHOHAB = recode_factor(DERECHOHAB,
                                      "1" = "Ninguna",
                                      "2" = "IMSS",
                                      "3" = "ISSSTE",
                                      "4" = "PEMEX",
                                      "5" = "SEDENA",
                                      "6" = "SEMAR",
                                      "7" = "Seguro Popular",
                                      "8" = "Otra",
                                      "9" = "IMSS PROSPERA",
                                      "99" = "No especificada"))
  
  defun <- defun |>
    mutate(EMBARAZO = recode_factor(EMBARAZO,
                                    "1" = "El embarazo",
                                    "2" = "El parto",
                                    "3" = "El puerperio",
                                    "4" = "43 días a 11 meses después del parto o aborto",
                                    "5" = "No estuvo embarazada en los últimos once meses previos a la muerte",
                                    "8" = "No aplica",
                                    "9" = "No especificado"))
  
  defun <- defun |>
    mutate(REL_EMBA = recode_factor(REL_EMBA,
                                    "1" = "Sí tuvieron relación las causas",
                                    "2" = "No tuvieron relación las causas",
                                    "8" = "No aplica",
                                    "9" = "No especificada"))
  
  defun <- defun |>
    mutate(VIO_FAMI = recode_factor(VIO_FAMI,
                                    "1" = "Sí",
                                    "2" = "No",
                                    "8" = "No aplica cuando no es homicidio",
                                    "9" = "No especificado"))
  
  defun <- defun |>
    mutate(AREA_UR = recode_factor(AREA_UR,
                                   "1" = "Urbana",
                                   "2" = "Rural",
                                   "9" = "No especificada"))
  
  defun <- defun |>
    mutate(EDAD_AGRU = recode_factor(EDAD_AGRU,
                                     "01" = "Menores de un año",
                                     "02" = "De un año",
                                     "03" = "De 2",
                                     "04" = "De 3",
                                     "05" = "De 4",
                                     "06" = "De 5 a 9",
                                     "07" = "De 10 a 14",
                                     "08" = "De 15 a 19",
                                     "09" = "De 20 a 24",
                                     "10" = "De 25 a 29",
                                     "11" = "De 30 a 34",
                                     "12" = "De 35 a 39",
                                     "13" = "De 40 a 44",
                                     "14" = "De 45 a 49",
                                     "15" = "De 50 a 54",
                                     "16" = "De 55 a 59",
                                     "17" = "De 60 a 64",
                                     "18" = "De 65 a 69",
                                     "19" = "De 70 a 74",
                                     "20" = "De 75 a 79",
                                     "21" = "De 80 a 84",
                                     "22" = "De 85 a 89",
                                     "23" = "De 90 a 94",
                                     "24" = "De 95 a 99",
                                     "25" = "De 100 a 104",
                                     "26" = "De 105 a 109",
                                     "27" = "De 110 a 114",
                                     "28" = "De 115 a 119",
                                     "29" = "De 120",
                                     "30" = "No especificada", .ordered = TRUE))
  
  defun <- defun |>
    mutate(COMPLICARO = recode_factor(COMPLICARO,
                                      "1" = "Sí complicaron el embarazo",
                                      "2" = "No complicaron el embarazo",
                                      "8" = "No aplica",
                                      "9" = "No especificada"))
  
  defun <- defun |>
    mutate(MES_CERT = recode_factor(MES_CERT,
                                    "1" = "Enero",
                                    "2" = "Febrero",
                                    "3" = "Marzo",
                                    "4" = "Abril",
                                    "5" = "Mayo",
                                    "6" = "Junio",
                                    "7" = "Julio",
                                    "8" = "Agosto",
                                    "9" = "Septiembre",
                                    "10" = "Octubre",
                                    "11" = "Noviembre",
                                    "12" = "Diciembre",
                                    "99" = "No especificado"))
  
  defun <- defun |>
    mutate(LENGUA = recode_factor(LENGUA,
                                  "1" = "Sí",
                                  "2" = "No",
                                  "8" = "No aplica a menores de 3 años",
                                  "9" = "Se ignora"))
  
  defun <- defun |>
    mutate(COND_ACT = recode_factor(COND_ACT,
                                    "1" = "Sí",
                                    "2" = "No",
                                    "8" = "No aplica a menores de 5 años",
                                    "9" = "Se ignora"))
  
  return(defun)
}

## Etiqueta las variables categóricas simples
fechas_base <- function(defun) {
  if("DIA_OCURR" %in% names(defun))
    defun$DIA_OCURR[which(defun$DIA_OCURR == "99")] <- NA
  else
    defun$DIA_OCURR <- NA
  defun$MES_OCURR[which(defun$MES_OCURR == "99")] <- NA
  defun$ANIO_OCUR[which(defun$ANIO_OCUR == "9999")] <- NA
  if("DIA_REGIS" %in% names(defun))
    defun$DIA_REGIS[which(defun$DIA_REGIS == "99")] <- NA
  else
    defun$DIA_REGIS <- NA
  defun$MES_REGIS[which(defun$MES_REGIS == "99")] <- NA
  if("DIA_NACIM" %in% names(defun))
    defun$DIA_NACIM[which(defun$DIA_NACIM == "99")] <- NA
  else
    defun$DIA_NACIM <- NA
  defun$MES_NACIM[which(defun$MES_NACIM == "99")] <- NA
  defun$ANIO_NACIM[which(defun$ANIO_NACIM == "9999")] <- NA
  
  if(max(defun$ANIO_OCUR) < 100)
    defun$ANIO_OCUR <- 1900 + defun$ANIO_OCUR
  
  if(max(defun$ANIO_REGIS) < 100)
    defun$ANIO_REGIS <- 1900 + defun$ANIO_REGIS
  
  if(max(defun$ANIO_NACIM) < 100)
    defun$ANIO_NACIM <- 1900 + defun$ANIO_NACIM

  defun <- defun |>
    mutate(FECHA_OCUR =
             as.Date(str_c(ANIO_OCUR,
                           MES_OCURR,
                           DIA_OCURR,
                           sep = "-"),
                     "%Y-%m-%d"))
  
  defun$FECHA_OCUR[which(is.na(defun$FECHA_OCUR))] <-
    with(defun[which(is.na(defun$FECHA_OCUR)),],
         as.Date(str_c(ANIO_OCUR,
                       MES_OCURR,
                       DIA_REGIS,
                       sep = "-") ,
                 "%Y-%m-%d"))
  
  defun$FECHA_OCUR[which(is.na(defun$FECHA_OCUR))] <-
    with(defun[which(is.na(defun$FECHA_OCUR)),],
         as.Date(str_c(ANIO_OCUR,
                       MES_REGIS,
                       DIA_REGIS,
                       sep = "-") ,
                 "%Y-%m-%d"))
  
  defun$FECHA_OCUR[which(is.na(defun$FECHA_OCUR))] <-
    with(defun[which(is.na(defun$FECHA_OCUR)),],
         as.Date(str_c(ANIO_REGIS,
                       MES_REGIS,
                       DIA_REGIS,
                       sep = "-") ,
                 "%Y-%m-%d"))
  
  defun$FECHA_OCUR[which(is.na(defun$FECHA_OCUR))] <-
    with(defun[which(is.na(defun$FECHA_OCUR)),],
         as.Date(str_c(ANIO_REGIS,
                       MES_REGIS,
                       "15",
                       sep = "-") ,
                 "%Y-%m-%d"))
  
  defun <- defun |>
    mutate(FECHA_REG =
             as.Date(str_c(ANIO_REGIS,
                           MES_REGIS,
                           DIA_REGIS,
                           sep = "-"),
                     "%Y-%m-%d"))
  
  defun$FECHA_REG[which(is.na(defun$FECHA_REG))] <-
    with(defun[which(is.na(defun$FECHA_REG)),],
         as.Date(str_c(ANIO_REGIS,
                       MES_REGIS,
                       "15",
                       sep = "-") ,
                 "%Y-%m-%d"))
  
  return(defun)
}

categoriza_causas_principales <- function(defun) {
  defun <- defun |>
    mutate(causa_de_muerte = factor(
      if_else(
        substr(LISTA_MEX, 1, 2) == "01", "Enfermedades infecciosas intestinales",
        if_else(
          substr(LISTA_MEX, 1, 3) == "04A" |
            substr(LISTA_MEX, 1, 3) == "04B" |
            substr(LISTA_MEX, 1, 3) == "04C", "Sífilis",
          if_else(
            substr(LISTA_MEX, 1, 3) == "06J" |
              substr(LISTA_MEX, 1, 3) == "06K", "Hepatitis viral",
            if_else(
              substr(LISTA_MEX, 1, 3) == "06T", "COVID-19",
              if_else(
                substr(LISTA_MEX, 1, 2) == "08" |
                  substr(LISTA_MEX, 1, 2) == "09" |
                  substr(LISTA_MEX, 1, 2) == "10" |
                  substr(LISTA_MEX, 1, 2) == "11" |
                  substr(LISTA_MEX, 1, 2) == "12" |
                  substr(LISTA_MEX, 1, 2) == "13" |
                  substr(LISTA_MEX, 1, 2) == "14" |
                  substr(LISTA_MEX, 1, 2) == "15", "Tumores malignos",
                if_else(
                  substr(LISTA_MEX, 1, 3) == "19A" |
                    substr(LISTA_MEX, 1, 3) == "19B"  , "Anemias",
                  if_else(
                    substr(LISTA_MEX, 1, 3) == "20A" |
                      substr(LISTA_MEX, 1, 3) == "20B" |
                      substr(LISTA_MEX, 1, 3) == "20C", "Enfermedades de la glándula tiroides",
                    if_else(
                      substr(LISTA_MEX, 1, 3) == "20D", "Diabetes mellitus",
                      if_else(
                        substr(LISTA_MEX, 1, 2) == "21", "Desnutrición y otras deficiencias nutricionales",
                        if_else(
                          substr(LISTA_MEX, 1, 2) == "26" |
                            substr(LISTA_MEX, 1, 2) == "27" |
                            substr(LISTA_MEX, 1, 2) == "28" |
                            substr(LISTA_MEX, 1, 2) == "29", "Enfermedades del corazón",
                          if_else(
                            substr(LISTA_MEX, 1, 2) == "30", "Enfermedades cerebrovasculares",
                            if_else(
                              substr(LISTA_MEX, 1, 3) == "32A" |
                                substr(LISTA_MEX, 1, 3) == "32B" |
                                substr(LISTA_MEX, 1, 3) == "32C" |
                                substr(LISTA_MEX, 1, 3) == "32D" |
                                substr(LISTA_MEX, 1, 3) == "33A" |
                                substr(LISTA_MEX, 1, 3) == "33K", "Infecciones respiratorias agudas",
                              if_else(
                                substr(LISTA_MEX, 1, 3) == "33B" |
                                  substr(LISTA_MEX, 1, 3) == "33C"  , "Influenza y neumonía",
                                if_else(
                                  substr(LISTA_MEX, 1, 3) == "33D" |
                                    substr(LISTA_MEX, 1, 3) == "33E"  , "Bronquitis crónica y la no especificada, enfisema y asma",
                                  if_else(
                                    substr(LISTA_MEX, 1, 3) == "33G", "Enfermedades pulmonares obstructivas crónicas",
                                    
                                    if_else(
                                      substr(LISTA_MEX, 1, 3) == "35F" |
                                        substr(LISTA_MEX, 1, 3) == "35G"  , "Hernia de la cavidad abdominal",
                                      if_else(
                                        substr(LISTA_MEX, 1, 3) == "35L" |
                                          substr(LISTA_MEX, 1, 3) == "35M"  , "Enfermedades del hígado",
                                        if_else(
                                          substr(LISTA_MEX, 1, 3) == "38C" |
                                            substr(LISTA_MEX, 1, 3) == "38D" |
                                            substr(LISTA_MEX, 1, 3) == "38E", "Enfermedades renales",
                                          if_else(
                                            (substr(LISTA_MEX, 1, 2) == "43" |
                                               substr(LISTA_MEX, 1, 2) == "44" |
                                               substr(LISTA_MEX, 1, 2) == "45") &
                                              substr(LISTA_MEX, 1, 3) != "43R", "Embarazo, parto y puerperio, excepto otras causas obstétricas directas",
                                            if_else(
                                              substr(LISTA_MEX, 1, 2) == "46", "Ciertas afecciones originadas en el periodo perinatal",
                                              if_else(
                                                substr(LISTA_MEX, 1, 2) == "47", "Malformaciones congénitas, deformidades y anomalías cromosómicas",
                                                if_else(
                                                  substr(LISTA_MEX, 1, 2) == "49" |
                                                    substr(LISTA_MEX, 1, 2) == "50" |
                                                    substr(LISTA_MEX, 1, 2) == "51" |
                                                    substr(LISTA_MEX, 1, 2) == "52" |
                                                    substr(LISTA_MEX, 1, 2) == "53" |
                                                    substr(LISTA_MEX, 1, 2) == "57" |
                                                    substr(LISTA_MEX, 1, 2) == "58", "Accidentes",
                                                  if_else(
                                                    substr(LISTA_MEX, 1, 2) == "55", "Agresiones",
                                                    if_else(
                                                      substr(LISTA_MEX, 1, 2) == "54", "Suicidio",
                                                      "Otro"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          ))
                                      )
                                    )
                                  ))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ))
  return(defun)
}

## Calculo de variables para el año 2021
procesa_defun21 <- function(){
  defun <- tibble(read.dbf(paste0(path, "2021/defun21.dbf")))
  defun <- fechas_base(defun)
  defun <- defun |>
    arrange(FECHA_OCUR)
  defun <- etiqueta_base(defun)
  defun <- categoriza_causas_principales(defun)
  saveRDS(defun, file = "docs/data/defun21.rds")
  
  defun <- defun |>
    filter(FECHA_OCUR >= "2021-01-01")
  saveRDS(defun, file = "docs/data/defunO21.rds")
  rm(defun)
}

## Calculo de variables para el año 2020
procesa_defun20 {
  defun <- tibble(read.dbf(paste0(path, "2020/defun20.dbf")))
  defun <- fechas_base(defun)
  defun <- defun |>
    arrange(FECHA_OCUR)
  defun <- etiqueta_base(defun)
  defun <- categoriza_causas_principales(defun)
  saveRDS(defun, file = "docs/data/defun20.rds")
  
  defun <- defun |>
    filter(FECHA_OCUR >= "2020-01-01")
  defun21 <- readRDS("docs/data/defun21.rds") |>
    filter(FECHA_OCUR >= "2020-01-01" & FECHA_OCUR < "2021-01-01")
  
  defunB <- bind_rows(defun, defun21)
  saveRDS(defunB, file = "docs/data/defunO20.rds")
  rm(defun, defun21, defunB)
}


## Calculo de variables para el año N (de 1990 a 2019)
procesa_defun <- function(ddig, dir_alm) {
  ydig <- ddig + ifelse(ddig < 50, 2000, 1900)
  
  ruta <- paste0(path, sprintf("%s/%04d/DEFUN%02d.dbf", dir_alm, ydig, ddig))
  if (!file.exists(ruta)) {
    ruta <- paste0(path, sprintf("%s/%04d/DEFUN%02d.DBF", dir_alm, ydig, ddig))
    if (!file.exists(ruta)) {
      ruta <- paste0(path, sprintf("%s/DEFUN%02d.dbf", dir_alm, ddig))
      if (!file.exists(ruta)) {
        ruta <- paste0(path, sprintf("%s/DEFUN%02d.DBF", dir_alm, ddig))
        if (!file.exists(ruta)) {
          return(NULL)
        }
      }
    }
  }
  defun <- tibble(read.dbf(ruta))
  defun <- fechas_base(defun)
  defun <- defun |>
    arrange(FECHA_OCUR)
  try(defun <- categoriza_causas_principales(defun), silent=TRUE)  

  fecha1 <- paste0(ydig, "-01-01")
  fecha2 <- paste0(ydig + 1, "-01-01")
  
  defun21 <- readRDS("docs/data/defun21.rds") |>
    filter(FECHA_OCUR >= fecha1 & FECHA_OCUR < fecha2)
  # Elimina variables nuevas
  defun <- defun |>
    select(-colnames(defun)[!(colnames(defun) %in% colnames(defun21))])
  # Agrega variables faltantes
  defun[colnames(defun21)[!(colnames(defun21) %in% colnames(defun))]] <- NA

  saveRDS(defun, file = sprintf("docs/data/defun%02d.rds", ddig))
  
  defun <- defun |>
    filter(FECHA_OCUR >= fecha1)
  
  if(ddig < 50) {
    years <- c(20:(ddig + 1))
    sraey <- c((ddig + 1):21)
  } else  {
    if(ddig == 99) {
      years <- c(20:0)
      sraey <- c(0:21)
    } else {
      years <- c(20:0, 99:(ddig+1))
      sraey <- c((ddig+1):99, 0:21)
    }
  }
  defun_list <- lapply(years, function(year) {
    readRDS(sprintf("docs/data/defun%02d.rds", year)) %>%
      filter(FECHA_OCUR >= fecha1 & FECHA_OCUR < fecha2)
  })
  names(defun_list) <- sprintf("defun%02d", years)
  list2env(defun_list, envir = environment())
  
  df_list <-
    lapply(sraey, function(x)
      get(sprintf("defun%02d", x)))
  defunB <- rbind(defun, do.call(rbind, df_list))
  
  saveRDS(defunB, file = sprintf("docs/data/defunO%02d.rds", ddig))
}

# procesa_defun(19, "2015_2019")
# procesa_defun(18, "2015_2019")
# procesa_defun(17, "2015_2019")
# procesa_defun(16, "2015_2019")
# procesa_defun(15, "2015_2019")
# procesa_defun(14, "2010_2014")
# procesa_defun(13, "2010_2014")
# procesa_defun(12, "2010_2014")
# procesa_defun(11, "2010_2014")
# procesa_defun(10, "2010_2014")
# procesa_defun(9, "2005_2009")
# procesa_defun(8, "2005_2009")
# procesa_defun(7, "2005_2009")
# procesa_defun(6, "2005_2009")
# procesa_defun(5, "2005_2009")
# procesa_defun(4, "2000_2004")
# procesa_defun(3, "2000_2004")
# procesa_defun(2, "2000_2004")
# procesa_defun(1, "2000_2004")
# procesa_defun(0, "2000_2004")
# procesa_defun(99, "1995_1999")
# procesa_defun(98, "1995_1999")
# procesa_defun(97, "1995_1999")
# procesa_defun(96, "1995_1999")
# procesa_defun(95, "1995_1999")
# procesa_defun(94, "1990_1994")
# procesa_defun(93, "1990_1994")
# procesa_defun(92, "1990_1994")
# procesa_defun(91, "1990_1994")
# procesa_defun(90, "1990_1994")

# 2.- Entender ----

# 3.- Comunicar ----

