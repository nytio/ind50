# Consultas la base de datos

# Librerias ----
library(tidyverse) # 2.0.0
library(DBI) # 1.2.0

# Conexiones ----
db <- R6::R6Class("Conexion",
  public = list(
    initialize = function() {
      tryCatch({
        private$dbconn <- dbConnect(odbc::odbc(), dsn="indicadores", timeout = 10)
      }, error = function(e) {
        fileError<-file(paste0("/var/log/shiny-server/I", format(Sys.Date()), ".txt"), "a")
        writeLines(paste("db::initialize:", csql, e$message), fileError)
        close(fileError)
      })
    },
    abreConexion = function(csql) {
      tryCatch({
        pU <- dbGetQuery(private$dbconn, csql)
        return(pU)
      }, error = function(e) {
        fileError<-file(paste0("/var/log/shiny-server/I", format(Sys.Date()), ".txt"), "a")
        writeLines(paste("db::abreConexion:", csql, e$message), fileError)
        close(fileError)
        return(NULL)
      })
    },
    abreConexionTran = function(csql) {
      dbBegin(private$dbconn)
      tryCatch({
        pU <- dbExecute(private$dbconn, csql)
        dbCommit(private$dbconn)
        return(pU)
      }, error = function(e) {
        dbRollback(private$dbconn)
        fileError<-file(paste0("/var/log/shiny-server/I", format(Sys.Date()), ".txt"), "a")
        writeLines(paste("db::abreConexionTran:", csql, e$message), fileError)
        close(fileError)
        return(NULL)
      })
    },
    abreConexionTran2 = function(csql) {
      dbBegin(private$dbconn)
      tryCatch({
        pU <- dbGetQuery(private$dbconn, csql)
        dbCommit(private$dbconn)
        return(pU)
      }, error = function(e) {
        dbRollback(private$dbconn)
        fileError<-file(paste0("/var/log/shiny-server/I", format(Sys.Date()), ".txt"), "a")
        writeLines(paste("db::abreConexionTran2:", csql, e$message), fileError)
        close(fileError)
        return(NULL)
      })
    },
    finalize = function() {
      dbDisconnect(private$dbconn)
    }
  ),
  private = list(dbconn = NULL)
)$new()


# Datos ----

# Lee el catálogo y los indicadores disponibles
paneles <- db$abreConexion("SELECT * FROM panel")
opciones_panel <- paneles$idpanel # Actualizar con los nombres de los temas
names(opciones_panel) <- paneles$panel

coleccion <- NULL
opciones_coleccion <- NULL
actualiza_coleccion <- function(selPanel) {
  coleccion <<- db$abreConexion(paste0("SELECT idcoleccion, titulo FROM viewb0 WHERE idpanel = ", selPanel) )
  opciones_coleccion <<- unique(coleccion$idcoleccion)
  names(opciones_coleccion) <<- coleccion$titulo
}
actualiza_coleccion(opciones_panel[1])

indicadores <- NULL
opciones_indicadores <- NULL
actualiza_indicador <- function(selColeccion) {
  indicadores <<- db$abreConexion(paste0("SELECT idserie, MIN(indicador) AS indicador FROM viewb1 WHERE idcoleccion = ", selColeccion, " GROUP BY idserie;"))
  opciones_indicadores <<- unique(indicadores$idserie)
  names(opciones_indicadores) <<- gsub("Población de 5 años y más residente en otra entidad en junio de 2005", "Población de 5 años y más residente en otra entidad hace 5 años", indicadores$indicador)
  opciones_indicadores <<- sort(opciones_indicadores)
}
actualiza_indicador(coleccion[1, 1])

get_meta <- function(selIndicador) {
  db$abreConexion(paste0("SELECT * FROM viewb2 WHERE idserie = ", selIndicador, " ORDER BY orden;"))
}

meta <- NULL
opciones_entidad <-NULL
bd <- NULL
actualiza_opciones_entidad <- function(selIndicador, selAnio = NULL) {
  meta <<- get_meta(selIndicador)
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

actualiza_bde <- function(selIndicador, metaL, caso = 1) {
  caso1 <- switch (caso, metaL$idind, metaL$idmasculino, metaL$idfemenino)
  if(any(is.na(caso1)))
    return(NULL)
  campo1 <- NULL
  use_sql <-  paste0("SELECT * FROM view04 WHERE idind = ", caso1)
  for (i in 1:length(use_sql)) {
    campo1 <- rbind(campo1, db$abreConexion(use_sql[i]))
  }

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
    ".loc WHERE ambito != 5;"
  )
  
  bde <- NULL
  for (i in 1:length(use_sql)) {
    cs <-
      cbind(metaL$idserie[i], cbind(metaL$fecha[i], db$abreConexion(use_sql[i])))
    bde <- rbind(bde, cs)
  }
  colnames(bde)[1] <- "no"
  colnames(bde)[2] <- "year"
  
  return (bde[order(bde$year, bde$ambito, bde$cve),])
}

actualiza_bd <- function(selIndicador) {
  actualiza_opciones_entidad(selIndicador)
  bd <<- actualiza_bde(selIndicador, meta)
  bd_m <- actualiza_bde(selIndicador, meta, 2)
  bd_f <- actualiza_bde(selIndicador, meta, 3)
  if(!is.null(bd_f)) {
    colnames(bd_m)[6] <- "valor_m"
    colnames(bd_f)[6] <- "valor_f"
    bd <<- merge(bd, bd_m, by = colnames(bd_m)[1:5])
    bd <<- merge(bd, bd_f, by = colnames(bd_f)[1:5])
  }
}

actualiza_bd(indicadores[1, 1])

contabiliza_uso <- function(idind, campo) {
  query <- paste0("UPDATE indicador SET ", campo," = ", campo," + 1 WHERE idind = ", idind, ";")
  db$abreConexionTran(query)
}

consulta_bde <- function() {
  use_sql <- "SELECT idind, indicador, unidad, fecha, fuente, producto  FROM view03;"
  campo <- db$abreConexion(use_sql)
  return (campo)
}

# https://github.com/r-dbi/odbc/blob/main/README.md