# https://github.com/r-dbi/odbc/blob/main/README.md

# Librerias ----
library(tidyverse) # 1.3.2
library(DBI) # 1.1.3

# Conexiones ----
con <- dbConnect(odbc::odbc(), "indicadores", timeout = 10) #circinus indicadores

# Datos ----

# Lee el catálogo y los indicadores disponibles
coleccion <- dbReadTable(con, "viewb0")
opciones_coleccion = unique(coleccion$idcoleccion)
names(opciones_coleccion) <- coleccion$titulo

indicadores <- NULL
opciones_indicadores <- NULL
actualiza_indicador <- function(selColeccion) {
  indicadores <<- dbGetQuery(con, paste0("SELECT idserie, MIN(indicador) AS indicador FROM viewb1 WHERE idcoleccion = ", selColeccion, " GROUP BY idserie;"))
  opciones_indicadores <<- unique(indicadores$idserie)
  names(opciones_indicadores) <<- indicadores$indicador
}

actualiza_indicador(coleccion[1, 1])

meta <- NULL
opciones_entidad <-NULL
bd <- NULL
actualiza_opciones_entidad <- function(selIndicador, selAnio = NULL) {
  meta <<- dbGetQuery(con, paste0("SELECT * FROM viewb2 WHERE idserie = ", selIndicador, "ORDER BY orden;"))
  if(is.null(selAnio)) {
    idambito <- meta$idambito[meta$fecha == max(meta$fecha)]
  } else {
    idambito <- meta$idambito[meta$fecha == selAnio]
  }
  if (length(idambito) == 1) {
    if (idambito == 2) {
      opciones_entidad <<- c(2)
      names(opciones_entidad) <<- c("Entidad federativa")
    } else
      if (idambito == 4) {
        opciones_entidad <<- c(1, 2) #, 5
        names(opciones_entidad) <<-
          c("Municipio", "Entidad federativa") #, "Localidad"
      } else
        if (idambito == 3) {
          opciones_entidad <<- c(1, 2)
          names(opciones_entidad) <<- c("Municipio", "Entidad federativa")
        } else
          if (idambito == 8) {
            opciones_entidad <<- c(1, 2, 5, 7)
            names(opciones_entidad) <<-
              c("Municipio",
                "Entidad federativa",
                "Localidad",
                "Estados de EE.UU.")
          } else
            if (idambito == 1) {
              opciones_entidad <<- c(1)
              names(opciones_entidad) <<- c("Municipio")
            } else
              if (idambito == 5) {
                opciones_entidad <<- c(5)
                names(opciones_entidad) <<- c("Localidad")
              } else
                if (idambito == 6) {
                  opciones_entidad <<- c(6)
                  names(opciones_entidad) <<- c("País")
                } else
                  if (idambito == 7) {
                    opciones_entidad <<- c(7)
                    names(opciones_entidad) <<- c("Estados de EE.UU.")
                  }
  }
}

actualiza_bd <- function(selIndicador) {
  actualiza_opciones_entidad(selIndicador)
  campo1 <-
    dbGetQuery(con, paste0(
      "SELECT * FROM view04 WHERE ",
      paste("idind = ", meta$idind, collapse = " OR ")
    ))
  
  use_sql <- paste0(
    'SELECT geografico.ambito, geografico.cve, geografico.nom, ',
    paste(campo1$tabla, campo1$mnemonico, sep = "."),
    " AS valor FROM ",
    campo1$tabla,
    " INNER JOIN geografico ON geografico.pais = ",
    campo1$tabla,
    ".pais AND geografico.ent = ",
    campo1$tabla,
    ".ent AND geografico.mun = ",
    campo1$tabla,
    ".mun AND geografico.loc = ",
    campo1$tabla,
    ".loc WHERE ambito != 5"
  )
  
  bd <<- NULL
  for (i in 1:length(use_sql)) {
    cs <-
      cbind(meta$idserie[i], cbind(meta$fecha[i], dbGetQuery(con, use_sql[i])))
    bd <<- rbind(bd, cs)
  }
  colnames(bd)[1] <<- "no"
  colnames(bd)[2] <<- "year"
  
  bd <<- bd[order(bd$year, bd$ambito, bd$cve),]
}

actualiza_bd(indicadores[1, 1])
