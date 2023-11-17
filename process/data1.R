# Calcula los datos y los guarda en la base de datos

# Librerias ----
library(tidyverse) # 2.0.0
library(data.table) # 1.14.8
library(foreign) # 0.8-85

# Ejercicio completo con tabla de mortalidad ----

# 1.- Importar ----

# Descargan los archivos de Inegi y se descomprimen en disco duro externo.
# Página web o sitio de consulta:
# https://www.inegi.org.mx/programas/mortalidad/#Microdatos
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/datos/defunciones_generales_base_datos_1990_1994_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/datos/defunciones_generales_base_datos_1995_1999_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/datos/defunciones_generales_base_datos_2000_2004_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/datos/defunciones_generales_base_datos_2005_2009_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/datos/defunciones_generales_base_datos_2010_2014_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/datos/defunciones_generales_base_datos_2015_2019_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/2020/defunciones_base_datos_2020_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/2021/defunciones_base_datos_2021_dbf.zip
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/2022/defunciones_base_datos_2022_dbf.zip
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
  if("TLOC_OCURR" %in% names(defun))
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
                                     "99" = "No especificada",
                                     .default = "No especificada")) #! Diferente
  
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
  
  if("PRESUNTO" %in% names(defun)) {
    defun <- defun |>
      rename(TIPO_DEFUN = PRESUNTO)
  }
  defun <- defun |>
    mutate(TIPO_DEFUN = recode_factor(TIPO_DEFUN,
                                    "1" = "Accidente",
                                    "2" = "Homicidio",
                                    "3" = "Suicidio",
                                    "4" = "Enfermedad",
                                    "5" = "Intervención legal",
                                    "8" = "No aplica",
                                    "9" = "Se ignora",
                                    .default = "No aplica"))

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
                                     "9" = "No especificada",
                                     .default = "No especificada"))
  
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
                                      "7" = "Equivalente al Seguro Popular",
                                      "8" = "Otra",
                                      "9" = "IMSS BIENESTAR",
                                      "10" = "Instituto de Seguridad Social para las Fuerzas Armadas Mexicanas (ISSFAM)",
                                      "99" = "No especificada",
                                      .default = "No especificada"))
  if("EMBARAZO" %in% names(defun))
    defun <- defun |>
      mutate(EMBARAZO = recode_factor(EMBARAZO,
                                      "1" = "El embarazo",
                                      "2" = "El parto",
                                      "3" = "El puerperio",
                                      "4" = "43 días a 11 meses después del parto o aborto",
                                      "5" = "No estuvo embarazada en los últimos once meses previos a la muerte",
                                      "8" = "No aplica",
                                      "9" = "No especificado",
                                      .default = "No especificada"))
  if("REL_EMBA" %in% names(defun))
    defun <- defun |>
      mutate(REL_EMBA = recode_factor(REL_EMBA,
                                      "1" = "Sí tuvieron relación las causas",
                                      "2" = "No tuvieron relación las causas",
                                      "8" = "No aplica",
                                      "9" = "No especificada"))
  if("VIO_FAMI" %in% names(defun))
    defun <- defun |>
      mutate(VIO_FAMI = recode_factor(VIO_FAMI,
                                      "1" = "Sí",
                                      "2" = "No",
                                      "8" = "No aplica cuando no es homicidio",
                                      "9" = "No especificado"))
  # @todo 2022
  # Nota: Para identificar la Condición de violencia familiar en la base de 
  # datos de la edición 2022 de las estadísticas de defunciones registradas, 
  # se deben considerar los registros que tengan en el campo Causa 
  # de defunción (CAUSA_DEF) el rango de X850-Y099, así como el Parentesco del 
  # presunto agresor (PAR_AGRE). El campo PAR_AGRE permite identificar la 
  # condición de violencia familiar en las defunciones por presunto homicidio; 
  # los códigos del 01 al 26 y del 37 al 52 corresponden a Sí hubo violencia 
  # familiar; los códigos del 27 al 36 y del 53 al 71 a No hubo violencia 
  # familiar; y los códigos 72 y 99 a condición de violencia familiar No 
  # especificada.
  
  if("AREA_UR" %in% names(defun))
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
                                     "29" = "De 120 y más",
                                     "30" = "No especificada", .ordered = TRUE))
  if("COMPLICARO" %in% names(defun))
    defun <- defun |>
      mutate(COMPLICARO = recode_factor(COMPLICARO,
                                        "1" = "Sí complicaron el embarazo",
                                        "2" = "No complicaron el embarazo",
                                        "8" = "No aplica",
                                        "9" = "No especificada"))
  if("MES_CERT" %in% names(defun))
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
  if("LENGUA" %in% names(defun))
    defun <- defun |>
      mutate(LENGUA = recode_factor(LENGUA,
                                    "1" = "Sí",
                                    "2" = "No",
                                    "8" = "No aplica a menores de 3 años",
                                    "9" = "Se ignora"))
  if("COND_ACT" %in% names(defun))
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
  defun$ANIO_REGIS[which(defun$ANIO_REGIS == "9999")] <- NA
  
  if("DIA_NACIM" %in% names(defun))
    defun$DIA_NACIM[which(defun$DIA_NACIM == "99")] <- NA
  else
    defun$DIA_NACIM <- NA
  defun$MES_NACIM[which(defun$MES_NACIM == "99")] <- NA
  defun$ANIO_NACIM[which(defun$ANIO_NACIM == "9999")] <- NA
  
  if(max(defun$ANIO_OCUR, na.rm = TRUE) < 100)
    defun$ANIO_OCUR <- 1900 + defun$ANIO_OCUR
  
  if(max(defun$ANIO_REGIS, na.rm = TRUE) < 100)
    defun$ANIO_REGIS <- 1900 + defun$ANIO_REGIS
  
  if(max(defun$ANIO_NACIM, na.rm = TRUE) < 100)
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

# El catálogo LISTA_MEX es un listado de las causas básicas de defunciones de
# acuerdo a una lista mexicana de enfermedades. Aplica para los años: 1998 a
# 2005, 2008 a 2011 y 2013 y siguientes. Para el resto de los años se aplica
# como sigue: 1990 - 1997 No presenta este catálogo (solo LISTA_BAS, incluye
# conversión) 2006 y 2007 LISTAMEX_2006_2007 2012 LISTAMEX_2012 2015
# LISTAMEX_2015 El código presume compatibilidad entre las diversas versiones de
# LISTA_MEX.
categoriza_causas_principales <- function(defun) {
  if(!("LISTA_MEX" %in% names(defun))) {
    # La tabla LISTA_BAS corresponde a una lista básica de las principales causas de
    # defunción para el periodo 1990 a 1997.
    regla <- c(
      "01" = "01",
      "010" = "01A",
      "011" = "01B",
      "012" = "01D",
      "013" = "01E",
      "014" = "01F",
      "015" = "01G",
      "016" = "01H",
      "019" = "01",
      "02" = "02",
      "020" = "02A",
      "021" = "02B",
      "022" = "02C",
      "023" = "02D",
      "024" = "02E",
      "025" = "02F",
      "029" = "02Z",
      "03" = "03",
      "030" = "03A",
      "031" = "03B",
      "032" = "03C",
      "033" = "03D",
      "034" = "03E",
      "035" = "03F",
      "036" = "03G",
      "037" = "03H",
      "038" = "03I",
      "039" = "03Z",
      "04" = "06",
      "040" = "06A",
      "041" = "06B",
      "042" = "06C",
      "043" = "06D",
      "044" = "06E",
      "045" = "06I",
      "046" = "06J",
      "047" = "06L",
      "048" = "06M",
      "049" = "06Z",
      "05" = "07",
      "050" = "07A",
      "051" = "07C",
      "052" = "07D",
      "053" = "07E",
      "054" = "07F",
      "059" = "07",
      "06" = "04",
      "060" = "04C",
      "061" = "04D",
      "069" = "04F",
      "07" = "05",
      "070" = "05A",
      "071" = "05C",
      "072" = "05D",
      "073" = "05F",
      "074" = "05I",
      "075" = "05J",
      "076" = "05K",
      "077" = "05L",
      "078" = "05M",
      "079" = "05Z",
      "08" = "08",
      "09" = "09",
      "090" = "09A",
      "091" = "09B",
      "092" = "09C",
      "093" = "09D",
      "094" = "09E",
      "095" = "09F",
      "096" = "09G",
      "099" = "09Z",
      "10" = "10",
      "100" = "10A",
      "101" = "10B",
      "109" = "10Z",
      "11" = "11",
      "110" = "11A",
      "111" = "11B",
      "112" = "11C",
      "113" = "11D",
      "119" = "11E",
      "12" = "12",
      "120" = "12A",
      "121" = "12B",
      "122" = "12C",
      "123" = "12D",
      "124" = "12F",
      "125" = "12G",
      "126" = "12H",
      "129" = "12J",
      "13" = "13",
      "130" = "13A",
      "139" = "13Z",
      "14" = "14",
      "140" = "14A",
      "141" = "14D",
      "149" = "14Z",
      "15" = "17",
      "150" = "17A",
      "151" = "17B",
      "152" = "17C",
      "153" = "17D",
      "154" = "17E",
      "155" = "17F",
      "156" = "17G",
      "159" = "17Z",
      "16" = "16A",
      "17" = "18",
      "18" = "20",
      "180" = "20A",
      "181" = "20D",
      "182" = "20E",
      "183" = "20F",
      "184" = "06H",
      "185" = "06H",
      "189" = "20Z",
      "19" = "21",
      "190" = "21A",
      "191" = "21B",
      "192" = "21C",
      "193" = "21E",
      "20" = "19",
      "200" = "19A",
      "209" = "19Z",
      "21" = "22",
      "210" = "22",
      "211" = "22E",
      "212" = "22F",
      "213" = "22",
      "214" = "22G",
      "215" = "22C",
      "216" = "22",
      "217" = "22H",
      "218" = "22I",
      "219" = "22Z",
      "22" = "23",
      "220" = "23A",
      "221" = "23C",
      "222" = "23D",
      "223" = "23E",
      "224" = "23F",
      "225" = "23G",
      "229" = "23Z",
      "23" = "24",
      "230" = "24C",
      "231" = "24D",
      "232" = "24G",
      "233" = "24H",
      "234" = "24J",
      "235" = "24K",
      "239" = "24Z",
      "24" = "25",
      "240" = "25A",
      "241" = "25C",
      "249" = "25Z",
      "25" = "26",
      "250" = "26A",
      "251" = "26B",
      "26" = "27",
      "260" = "27A",
      "269" = "27Z",
      "27" = "28",
      "270" = "28A",
      "279" = "28Z",
      "28" = "29",
      "280" = "29A",
      "281" = "29B",
      "289" = "29Z",
      "29" = "30",
      "290" = "30A",
      "291" = "30B",
      "292" = "30C",
      "293" = "30D",
      "294" = "30E",
      "299" = "30Z",
      "30" = "31",
      "300" = "31A",
      "301" = "31C",
      "302" = "31D",
      "303" = "31E",
      "304" = "31F",
      "305" = "31G",
      "309" = "31Z",
      "31" = "32",
      "310" = "32B",
      "311" = "32C",
      "312" = "32D",
      "313" = "32E",
      "314" = "32F",
      "315" = "32H",
      "319" = "32Z",
      "32" = "33",
      "320" = "33A",
      "321" = "33B",
      "322" = "33C",
      "323" = "33D",
      "324" = "33F",
      "325" = "33G",
      "326" = "33H",
      "327" = "33J",
      "329" = "33Z",
      "33" = "34",
      "330" = "34B",
      "331" = "34C",
      "339" = "34Z",
      "34" = "35",
      "340" = "35A",
      "341" = "35D",
      "342" = "35E",
      "343" = "35G",
      "344" = "35I",
      "345" = "35J",
      "346" = "35K",
      "347" = "35M",
      "348" = "35N",
      "349" = "35Z",
      "35" = "38",
      "350" = "38A",
      "351" = "38C",
      "352" = "38D",
      "353" = "38H",
      "359" = "38I",
      "36" = "39",
      "360" = "39A",
      "361" = "39C",
      "362" = "39D",
      "363" = "39E",
      "369" = "39F",
      "37" = "41",
      "370" = "40",
      "371" = "41A",
      "372" = "41C",
      "373" = "41B",
      "374" = "41F",
      "375" = "41H",
      "376" = "41J",
      "379" = "41Z",
      "38" = "43A",
      "380" = "43A",
      "381" = "43A",
      "382" = "43B",
      "389" = "43C",
      "39" = "43",
      "390" = "43F",
      "391" = "43G",
      "392" = "43H",
      "393" = "43J",
      "394" = "43M",
      "399" = "43N",
      "40" = "45",
      "41" = "44",
      "42" = "36",
      "420" = "36A",
      "429" = "36Z",
      "43" = "37",
      "430" = "37A",
      "431" = "37E",
      "432" = "37D",
      "433" = "37",
      "434" = "37G",
      "435" = "37H",
      "436" = "37I",
      "437" = "37C",
      "439" = "37Z",
      "44" = "47",
      "440" = "47C",
      "441" = "47D",
      "442" = "47E",
      "443" = "47F",
      "444" = "47H",
      "445" = "47I",
      "446" = "47K",
      "447" = "47M",
      "449" = "47N",
      "45" = "46",
      "450" = "46A",
      "451" = "46B",
      "452" = "46C",
      "453" = "46D",
      "454" = "46E",
      "455" = "46I",
      "456" = "46J",
      "459" = "46Z",
      "46" = "48",
      "460" = "48A",
      "461" = "48B",
      "462" = "38G",
      "463" = "48C",
      "464" = "48D",
      "465" = "48E",
      "466" = "48F",
      "467" = "48G",
      "469" = "48Z",
      "47" = "49",
      "470" = "49A",
      "471" = "49B",
      "472" = "49C",
      "473" = "49D",
      "474" = "49E",
      "479" = "49Z",
      "48" = "53",
      "480" = "53A",
      "481" = "53B",
      "482" = "53C",
      "49" = "58",
      "50" = "50",
      "51" = "52",
      "52" = "51",
      "520" = "51A",
      "521" = "51B",
      "522" = "51C",
      "523" = "51D",
      "524" = "51E",
      "529" = "51Z",
      "53" = "57",
      "54" = "54",
      "55" = "55",
      "56" = "56",
      "560" = "56A",
      "561" = "56B",
      "569" = "56"
    )
    # Elabora la lista requerida a partir de la información disponible
    defun <- defun |>
      mutate(LISTA_MEX = as.character(regla[as.character(LISTA_BAS)]))
  }
  
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

#Procesamiento inicial
procesa_defun_inicial <- function(ddig, dir_alm) {
  ydig <- ddig + ifelse(ddig < 50, 2000, 1900)
  
  ruta <- paste0(path, sprintf("%s/%04d/DEFUN%02d.dbf", dir_alm, ydig, ddig))
  if (!file.exists(ruta)) {
    ruta <- paste0(path, sprintf("%s/%04d/DEFUN%02d.DBF", dir_alm, ydig, ddig))
    if (!file.exists(ruta)) {
      ruta <- paste0(path, sprintf("%s/DEFUN%02d.dbf", dir_alm, ddig))
      if (!file.exists(ruta)) {
        ruta <- paste0(path, sprintf("%s/DEFUN%02d.DBF", dir_alm, ddig))
        if (!file.exists(ruta)) {
          stop("Archivo no encontrado: ", dbf_path)
        }
      }
    }
  }
  
  defun <- read.dbf(ruta) |>
    tibble() |>
    fechas_base() |>
    arrange(FECHA_OCUR) |>
    etiqueta_base() |>
    categoriza_causas_principales()
  
  if(ddig == 22) {
    saveRDS(colnames(defun), file = "docs/data/colnames_defun.rds")
  } else {
    colnames_defun <- readRDS("docs/data/colnames_defun.rds")
    # Elimina variables nuevas
    defun <- defun |>
      select(-colnames(defun)[!(colnames(defun) %in% colnames_defun)])
    # Agrega variables faltantes
    defun[colnames_defun[!(colnames_defun %in% colnames(defun))]] <- NA
  }
  
  saveRDS(defun, file = sprintf("docs/data/defun%02d.rds", ddig))
}

# procesa_defun_inicial(22, "2022")
# procesa_defun_inicial(21, "2021")
# procesa_defun_inicial(20, "2020")
# procesa_defun_inicial(19, "2015_2019")
# procesa_defun_inicial(18, "2015_2019")
# procesa_defun_inicial(17, "2015_2019")
# procesa_defun_inicial(16, "2015_2019")
# procesa_defun_inicial(15, "2015_2019")
# procesa_defun_inicial(14, "2010_2014")
# procesa_defun_inicial(13, "2010_2014")
# procesa_defun_inicial(12, "2010_2014")
# procesa_defun_inicial(11, "2010_2014")
# procesa_defun_inicial(10, "2010_2014")
# procesa_defun_inicial(9, "2005_2009")
# procesa_defun_inicial(8, "2005_2009")
# procesa_defun_inicial(7, "2005_2009")
# procesa_defun_inicial(6, "2005_2009")
# procesa_defun_inicial(5, "2005_2009")
# procesa_defun_inicial(4, "2000_2004")
# procesa_defun_inicial(3, "2000_2004")
# procesa_defun_inicial(2, "2000_2004")
# procesa_defun_inicial(1, "2000_2004")
# procesa_defun_inicial(0, "2000_2004")
# procesa_defun_inicial(99, "1995_1999")
# procesa_defun_inicial(98, "1995_1999")
# procesa_defun_inicial(97, "1995_1999")
# procesa_defun_inicial(96, "1995_1999")
# procesa_defun_inicial(95, "1995_1999")
# procesa_defun_inicial(94, "1990_1994")
# procesa_defun_inicial(93, "1990_1994")
# procesa_defun_inicial(92, "1990_1994")
# procesa_defun_inicial(91, "1990_1994")
# procesa_defun_inicial(90, "1990_1994")

# Filtrado y Combinación
procesa_defun_filtrado <- function(ddig) {
  ydig <- ddig + ifelse(ddig < 50, 2000, 1900)
  fecha1 <- paste0(ydig, "-01-01")
  fecha2 <- paste0(ydig + 1, "-01-01")

  myu <- 22
  if(ddig < 50) {
    years <- c(myu:ddig)
    sraey <- c(ddig:myu)
  } else  {
    years <- c(myu:0, 99:ddig)
    sraey <- c(ddig:99, 0:myu)
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
  defun_combinado <- do.call(rbind, df_list)
  
  saveRDS(defun_combinado, file = sprintf("docs/data/defunO%02d.rds", ddig))
}

# for(i in c(22:0, 99:90))
#   procesa_defun_filtrado(i)

#-------------------------------------------------------------------------------

# 2.- Entender ----

# Calcula el tabulado por entidad federativa
# dónde se registra el hecho vital
elabora_tabulado_REGIS <- function(anio = "21", sexo = "", causa = "") {
  # Carga la base de datos
  defun <- readRDS(paste0("docs/data/defun", anio,".rds"))

  # Filtra por una categoría
  if(sexo != "")
    defun <- defun |> filter(SEXO == sexo)
  if(causa != "")
    defun <- defun |> filter(causa_de_muerte == causa)
  
  # Estandariza claves
  defun$ENT_REGIS <- as.integer(defun$ENT_REGIS)
  defun$MUN_REGIS <- as.integer(defun$MUN_REGIS)
  
  # Agrupa
  registradas <- defun |>
    group_by(ENT_REGIS) |>
    summarize(dato = n())
  
  colnames(registradas)[1] <- "ENT"
  registradas$MUN <- 0
  
  registradas <- registradas[,c(1,3,2)]
  
  registradas <- registradas |>
    bind_rows(bind_cols(registradas |> summarise(across(dato, sum))))
  
  registradas <- registradas |>
    mutate(ENT = ifelse(is.na(ENT), 0, ENT),
           MUN = ifelse(is.na(MUN), 0, MUN))

  defun <- defun |>
    filter(ENT_REGIS == "11")
  
  registradas2 <- defun |>
    group_by(MUN_REGIS) |>
    summarize(dato = n())
  
  colnames(registradas2)[1] <- "MUN"
  registradas2$ENT <- 11
  
  registradas3 <- rbind(registradas, registradas2)
  
  return(registradas3 |> filter(ENT < 33, MUN < 47))
}

# Calcula el tabulado por entidad federativa
# dónde ocurre el hecho vital
elabora_tabulado_OCURR <- function(anio = "21", sexo = "", causa = "") {
  # Carga la base de datos
  defun <- readRDS(paste0("docs/data/defun", anio,".rds"))
  
  # Filtra por una categoría
  if(sexo != "")
    defun <- defun |> filter(SEXO == sexo)
  if(causa != "")
    defun <- defun |> filter(causa_de_muerte == causa)

  # Estandariza claves
  defun$ENT_OCURR <- as.integer(defun$ENT_OCURR)
  defun$MUN_OCURR <- as.integer(defun$MUN_OCURR)
  
  # Registradas
  registradas <- defun |>
    group_by(ENT_OCURR) |>
    summarize(dato = n())
  
  colnames(registradas)[1] <- "ENT"
  registradas$MUN <- 0
  
  registradas <- registradas[,c(1,3,2)]
  
  registradas <- registradas |>
    bind_rows(bind_cols(registradas |> summarise(across(dato, sum))))
  
  registradas <- registradas |>
    mutate(ENT = ifelse(is.na(ENT), 0, ENT),
           MUN = ifelse(is.na(MUN), 0, MUN))
  
  defun <- defun |>
    filter(ENT_OCURR == "11")
  
  registradas2 <- defun |>
    group_by(MUN_OCURR) |>
    summarize(dato = n())
  
  colnames(registradas2)[1] <- "MUN"
  registradas2$ENT <- 11

  registradas3 <- rbind(registradas, registradas2)
  
  return(registradas3 |> filter(ENT < 33, MUN < 47))
}

# Calcula el tabulado por entidad federativa 
# de residencia habitual del fallecido
elabora_tabulado_RESID <- function(anio = "21", sexo = "", causa = "") {
  # Carga la base de datos
  defun <- readRDS(paste0("docs/data/defun", anio,".rds"))
  
  # Filtra por una categoría
  if(sexo != "")
    defun <- defun |> filter(SEXO == sexo)
  if(causa != "")
    defun <- defun |> filter(causa_de_muerte == causa)

  # Estandariza claves
  defun$ENT_RESID <- as.integer(defun$ENT_RESID)
  defun$MUN_RESID <- as.integer(defun$MUN_RESID)

  # Registradas
  registradas <- defun |>
    group_by(ENT_RESID) |>
    summarize(dato = n())
  
  colnames(registradas)[1] <- "ENT"
  registradas$MUN <- 0
  
  registradas <- registradas[,c(1,3,2)]
  
  registradas <- registradas |>
    bind_rows(bind_cols(registradas |> summarise(across(dato, sum))))

  registradas <- registradas |>
    mutate(ENT = ifelse(is.na(ENT), 0, ENT),
           MUN = ifelse(is.na(MUN), 0, MUN))
  
  defun <- defun |>
    filter(ENT_RESID == "11")
  
  registradas2 <- defun |>
    group_by(MUN_RESID) |>
    summarize(dato = n())
  
  colnames(registradas2)[1] <- "MUN"
  registradas2$ENT <- 11

  registradas3 <- rbind(registradas, registradas2)
  
  return(registradas3 |> filter(ENT < 33, MUN < 47))
}

# test <- elabora_tabulado_REGIS("21")
# test <- elabora_tabulado_OCURR("21")
# test <- elabora_tabulado_RESID("21")
# 
# test1 <- elabora_tabulado_REGIS("O21")
# test2 <- elabora_tabulado_OCURR("O21")
# test3 <- elabora_tabulado_RESID("O21")

# test1 <- elabora_tabulado_REGIS("03")
# test2 <- elabora_tabulado_OCURR("03")
# test3 <- elabora_tabulado_RESID("03")
# 
# test1 <- elabora_tabulado_REGIS("O03")
# test2 <- elabora_tabulado_OCURR("O03")
# test3 <- elabora_tabulado_RESID("O03")

# test1 <- elabora_tabulado_REGIS("90", sexo = "Hombre", causa = "Diabetes mellitus")

# Analiza las bases de datos por año


#-------------------------------------------------------------------------------

# 3.- Comunicar ----

# Conecta con la base de datos
library(DBI) # 1.1.3
con <- dbConnect(odbc::odbc(), "circinus", timeout = 10)

# Calcula los indicadores y los guarda en la base de datos
crea_tabla_defun <- function(ddig = 21) {
  anio <- sprintf("%02d", ddig)
  # Crea las tablas en la base de datos
  # Si la tabla no existe, crearla antes de escribir los datos
  if (!dbExistsTable(con, paste0("tabla_defun_",anio))) {
    query <- readLines("docs/data/tabla_defun.sql")
    query <- paste(query, collapse = " ")
    query <- gsub("21", anio, query)
    dbExecute(con, query)
    
    # Agrega las entradas para entidades y municipios
    query <- paste0("INSERT INTO tabla_defun_", anio,"(ent) VALUES")
    for (i in 0:32) {
      dbExecute(con, paste0(query, " (", i, "); ") )
    }
    query <- paste0("INSERT INTO tabla_defun_", anio,"(mun) VALUES")
    for (i in 1:46) {
      dbExecute(con, paste0(query, " (", i, "); ") )
    }
  }
  
  test <- elabora_tabulado_REGIS(anio)
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET regis = ", test$dato[i],
                    " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_REGIS(anio, sexo = "Hombre")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET regis_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_REGIS(anio, sexo = "Mujer")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET regis_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_OCURR(anio)
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET ocurr = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_OCURR(anio, sexo = "Hombre")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET ocurr_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_OCURR(anio, sexo = "Mujer")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET ocurr_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio)
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET resid = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Hombre")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET resid_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Mujer")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET resid_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  # 5 principales causas (a nivel nacional en 2021)
  test <- elabora_tabulado_RESID(anio, causa = "COVID-19")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_1 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Hombre", causa = "COVID-19")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_1_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Mujer", causa = "COVID-19")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_1_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(anio, causa = "Enfermedades del corazón")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_2 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Hombre", causa = "Enfermedades del corazón")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_2_m = ", test$dato[i],
                   " WHERE ENT =", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Mujer", causa = "Enfermedades del corazón")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_2_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(anio, causa = "Diabetes mellitus")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_3 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Hombre", causa = "Diabetes mellitus")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_3_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Mujer", causa = "Diabetes mellitus")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_3_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(anio, causa = "Tumores malignos")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_4 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Hombre", causa = "Tumores malignos")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_4_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Mujer", causa = "Tumores malignos")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_4_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(anio, causa = "Influenza y neumonía")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_5 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Hombre", causa = "Influenza y neumonía")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_5_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(anio, sexo = "Mujer", causa = "Influenza y neumonía")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET causa_5_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  # Con el recuento acumulado
  test <- elabora_tabulado_RESID(paste0("O", anio))
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_resid = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Hombre")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_resid_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Mujer")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_resid_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN= ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(paste0("O", anio), causa = "COVID-19")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_1 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Hombre", causa = "COVID-19")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_1_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Mujer", causa = "COVID-19")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_1_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(paste0("O", anio), causa = "Enfermedades del corazón")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_2 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Hombre", causa = "Enfermedades del corazón")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_2_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Mujer", causa = "Enfermedades del corazón")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_2_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(paste0("O", anio), causa = "Diabetes mellitus")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_3 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Hombre", causa = "Diabetes mellitus")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_3_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Mujer", causa = "Diabetes mellitus")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_3_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(paste0("O", anio), causa = "Tumores malignos")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_4 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Hombre", causa = "Tumores malignos")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_4_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Mujer", causa = "Tumores malignos")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_4_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  
  test <- elabora_tabulado_RESID(paste0("O", anio), causa = "Influenza y neumonía")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_5 = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Hombre", causa = "Influenza y neumonía")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_5_m = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
  test <- elabora_tabulado_RESID(paste0("O", anio), sexo = "Mujer", causa = "Influenza y neumonía")
  for(i in 1:dim(test)[1]) {
    query <- paste0("UPDATE tabla_defun_", anio," SET o_causa_5_f = ", test$dato[i],
                   " WHERE ENT = ", test$ENT[i], " AND MUN = ", test$MUN[i], ";")
    dbExecute(con, query)
  }
}

#crea_tabla_defun(22)
#for(i in c(21:0, 99:90))
#  crea_tabla_defun(i)

# Completa la documentación de los indicadores en la base de datos
library(DBI)
library(data.table)

con <- dbConnect(odbc::odbc(), "circinus", timeout = 10)

test_variales_tabla  <- function(nombre_tabla) {
  query <- paste0("SELECT * FROM tabla WHERE tabla = '", nombre_tabla, "'")
  tabla <- dbGetQuery(con, query)
  
  if(length(tabla$idtabla) == 0) {
    query <- paste0("INSERT INTO tabla(tabla) VALUES ('", nombre_tabla, "')")
    dbExecute(con, query)
    return(NULL)
  }
  
  query <- paste0("SELECT * FROM indicador WHERE idtabla = ", tabla$idtabla)
  indicador <- dbGetQuery(con, query)
  
  mnemonico <- NULL
  for(i in indicador$idmnemonico) {
    query <- paste0("SELECT * FROM mnemonico WHERE idmnemonico = ", i)
    mnemonico <- rbind(mnemonico, dbGetQuery(con, query))
  }
  
  query <- paste0("SELECT * FROM ",nombre_tabla," LIMIT 10")
  tabla_ <- dbGetQuery(con, query)
  
  r <- colnames(tabla_)
  faltan <- NULL
  for(j in r[5:length(r)])
    if(!(j %in% mnemonico$mnemonico)) {
      message(paste("Variable no referida:", j))
      faltan <- c(faltan, j)
    }
  return(faltan)
}

test_variales_tabla(sprintf("tabla_defun_22"))
# for(i in c(22:0,99:90))
#   test_variales_tabla(sprintf("tabla_defun_%02d", i))

alta_variales_tabla <- function(nombre_tabla, anio = 2020) {
  u <- test_variales_tabla(nombre_tabla)
  l <- 0
  for(k in u) {
    query <- paste0("SELECT idmnemonico FROM public.mnemonico WHERE mnemonico = '", k,"'")
    mnemo_ <- dbGetQuery(con, query)
    if(length(mnemo_$idmnemonico) > 0) {
      message("Nombre de variable documentado:", k)
      l <- l + 1
    }
  }
  
  if(l > 0) {
    # Están dados de alta los nombres, solo agrega indicador
    query <- paste0("SELECT idtabla FROM tabla WHERE tabla = '", nombre_tabla, "'")
    idtabla <- dbGetQuery(con, query)
    for(k in u) {
      query <- paste0("SELECT idmnemonico FROM public.mnemonico WHERE mnemonico = '", k,"'")
      idmnemonico <- dbGetQuery(con, query)
      query <- paste0("INSERT INTO indicador(idtabla, idmnemonico, fecha) VALUES (",idtabla$idtabla,", ",idmnemonico$idmnemonico,", ", anio, ")")
      dbExecute(con, query)
    }
    return(NULL)
  }

  # Si ninguno está documentado se procede a dar de alta estos elementos
  query <- paste0("SELECT idtabla FROM tabla WHERE tabla = '", nombre_tabla, "'")
  idtabla <- dbGetQuery(con, query)
  for(k in u) {
    query <- paste0("INSERT INTO mnemonico(mnemonico) VALUES ('", k, "') RETURNING idmnemonico")
    idmnemonico <- dbGetQuery(con, query)
    query <- paste0("INSERT INTO indicador(idtabla, idmnemonico, fecha) VALUES (",idtabla$idtabla,", ",idmnemonico$idmnemonico,", ", anio, ")")
    dbExecute(con, query)
  }
}

alta_variales_tabla("tabla_defun_22", 2022)
#alta_variales_tabla("tabla_defun_21", 2021)
#alta_variales_tabla("tabla_defun_20", 2020)
# for(i in c(19:0)) {
#   alta_variales_tabla(sprintf("tabla_defun_%02d", i), 2000+i)
# }
# for(i in c(99:90)) {
#   alta_variales_tabla(sprintf("tabla_defun_%02d", i), 1900+i)
# }
