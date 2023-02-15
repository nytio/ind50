# Genera los mapas temáticos y los guarda en la base de datos

# Librerias ----
library(tidyverse) # 1.3.1
library(DBI) # 1.1.3

# Conexiones ----
con <- dbConnect(odbc::odbc(), "circinus", timeout = 10) #circinus indicadores

frangeerror <- function(muppets, mini, maxi) {
  mean_val <- mean(muppets[mini:maxi])
  sum(abs(mean_val - muppets[mini:maxi]), na.rm = TRUE)
}

# Optimizado para reducir el tiempo de ejecución, en casos de tamaño de arreglo
# moderado, de la función de O(n^4) a O(n^2) donde n es el tamaño del arreglo,
# pero la complejidad temporal sigue siendo alta si el tamaño del arreglo es
# grande. Requiere de 2 segundos para calcular los rangos para un mapa.
#! Enero de 2023
rangos_arreglo <- function(muppets) {
  dim_arreglo <- length(muppets)
  arreglo <- muppets[order(muppets, decreasing = TRUE)]
  # Generar todas las combinaciones posibles de los índices
  idx <- expand.grid(i = 1:((dim_arreglo - 4)), j = 2:(dim_arreglo - 3), k = 3:(dim_arreglo - 2), l = 4:(dim_arreglo - 1))

  # Eliminar las combinaciones con índice inicial mayor que índice final
  idx <- idx[idx$i < idx$j,]
  idx <- idx[idx$j < idx$k,]
  idx <- idx[idx$k < idx$l,]
  
  # Aplicar frangeerror a cada combinación y calcular el TAI
  errors <- apply(idx, 1, function(x) {
    frangeerror(arreglo, 1, x[1]) +
      frangeerror(arreglo, x[1] + 1, x[2]) +
      frangeerror(arreglo, x[2] + 1, x[3]) +
      frangeerror(arreglo, x[3] + 1, x[4]) +
      frangeerror(arreglo, x[4] + 1, dim_arreglo)
  })
  # Calcula TAI y encuentra los índices con el TAI máximo
  TAI <- 1 - errors / frangeerror(arreglo, 1, dim_arreglo)
  maxi <- idx[TAI == max(TAI),]
  # Devolver los rangos correspondientes a los índices encontrados
  array(
    c(arreglo[dim_arreglo], arreglo[maxi$l], arreglo[maxi$k], arreglo[maxi$j], arreglo[maxi$i],
      arreglo[maxi$l + 1], arreglo[maxi$k + 1], arreglo[maxi$j + 1], arreglo[maxi$i + 1], arreglo[1]),
    c(5, 2)
  )
}

categoriza <- function(rangos, muppets) {
  ifelse(
    rangos[1, 1] <= muppets & muppets <= rangos[1, 2],
    1,
    ifelse(
      rangos[2, 1] <= muppets & muppets <= rangos[2, 2],
      2,
      ifelse(
        rangos[3, 1] <= muppets & muppets <= rangos[3, 2],
        3,
        ifelse(
          rangos[4, 1] <= muppets & muppets <= rangos[4, 2],
          4,
          ifelse(
            rangos[5, 1] <= muppets & muppets <= rangos[5, 2],
            5, 0)
        )
      )
    )
  )
}

colorea <- function(muppets) {
  colorea_rangos <- rangos_arreglo(muppets)
  #colorea_rangos <- classIntervals(muppets, n=5, style="jenks")
  colorea_clases <- categoriza(colorea_rangos, muppets)
  # colorea_clases <- findInterval(muppets, c(colorea_rangos[,1], Inf))
  colorea_cuenta <- c(sum(colorea_clases == 1), sum(colorea_clases == 2), sum(colorea_clases == 3), sum(colorea_clases == 4), sum(colorea_clases == 5))
  # colorea_cuenta <- as.vector(table(colorea_clases))
  list(clases = colorea_clases, rangos = colorea_rangos, cuenta = colorea_cuenta)
}

mapea_js <- function(x_null, id, geo2) {
  if (mode(x_null) == "numeric") {
    #! Localiza los valores nulos y les asigna valor
    x <- (function(y) ifelse(is.na(y), -9999999, y))(x_null);
    if (min(x) != max(x)) {
      #! Calcula los rangos y clases para colorear
      #! para crear el gráfico con las especificaciones
      pamd <- colorea(x);
      
      #! Procesa la leyenda de datos, redondea a dos dígitos
      #! Poner comas a los números (formato numérico).
      #! 16 de julio de 2009
      #@todo verificar si cuando no hay disponible, se use el valor cero,
      pamd$rangos <- round(pamd$rangos, digits = 2);
      pamd$rangos[,1] <- prettyNum(pamd$rangos[,1], big.mark = ",");
      pamd$rangos[,2] <- prettyNum(pamd$rangos[,2], big.mark = ",");
      
      #! Colocar el número de entidades en la clase entre paréntesis
      #! 22 de marzo de 2010
      #@todo cambiar el exceso de espacios y tabulador por elementos html
      leyenda <- paste("De ", pamd$rangos[,1], " a ", pamd$rangos[,2], " (", pamd$cuenta, ")", sep="");
      leyenda <- (function(y) ifelse(y[[1]][,1] == y[[1]][,2], paste(y[[1]][,1], " (", y[[3]], ")"), y[[2]]))(list(pamd$rangos,leyenda,pamd$cuenta));
      leyenda <- (function(y) ifelse(y[[1]][,1] == "-9,999,999", paste("No disponible (", y[[3]], ")"), y[[2]]))(list(pamd$rangos,leyenda,pamd$cuenta));
      
      #@todo usar una estrategia para si hay vacios se muestre en etiqueta, sino no.
      
      #! Salida en formato json unitario
      #! 11 de septiembre de 2013
      r <- paste(pamd$clases,',',sep="",collapse="");
      r <- substr(r,1,nchar(r)-1);
      l <- paste('"',leyenda,'",',sep="",collapse="");
      l <- substr(l,1,nchar(l)-1);
      s <- paste('"',geo2,'":{"l":[',l,'],"v":[',r,']}',sep="",collapse="");
      return(s);
    }
  } else if (mode(x_null) == "character") {
    # Considera este caso de etiquetado
    cr1 = c("Muy Bajo", "Bajo","Medio", "Alto", "Muy Alto");
    cr2 = c("Muy bajo", "Bajo","Medio", "Alto", "Muy alto");
    #@todo Generalizar para cualquier uso de etiquetas.
    
    #! Localiza los valores nulos y les asigna valor
    x <- (function(y) ifelse(is.na(y), 0, ifelse(y == cr1[1], 1, ifelse(y == cr1[2], 2, ifelse(y == cr1[3], 3, ifelse(y == cr1[4], 4, ifelse(y == cr1[5], 5, y)))))))(x_null);
    x <- (function(y) ifelse(is.na(y), 0, ifelse(y == cr2[1], 1, ifelse(y == cr2[2], 2, ifelse(y == cr2[3], 3, ifelse(y == cr2[4], 4, ifelse(y == cr2[5], 5, y)))))))(x);
    
    ca = c(sum(x == 1), sum(x == 2), sum(x == 3), sum(x == 4), sum(x == 5));
    ly <- paste(cr2, " (", ca, ")", sep="");
    
    r <- paste(x, ',', sep = "", collapse = "");
    r <- substr(r, 1, nchar(r)-1);
    l <- paste('"', ly, '",', sep = "", collapse = "");
    l <- substr(l, 1, nchar(l)-1);
    s <- paste('"', geo2, '":{"l":[', l, '],"v":[', r, ']}', sep="", collapse="");
    return(s);
  }
}

#!###################################################
#! Se conecta al servidor para generar los mapas
#! y los almacena en el servidor con el formato json
mapas_graficos_servidor_a_servidor <- function(rango) {
  # Consulta de indicadores válidos
  ind <- dbGetQuery(con, "SELECT * FROM view00;")
  for (numindicador in unlist(rango)) {
    #Verificar que el rango se encuentre en el listado de indicadores
    if (!is.na(numindicador) && numindicador %in% ind$idind) {
      #! Verifica que está relacionado con un indicador municipal o estatal
      query <-
        paste("SELECT idambito FROM viewaa WHERE idind =",
              numindicador,
              ";")
      rows <- dbGetQuery(con, query)
      geo <- rows$idambito

      #! Elementos informativos del indicador
      info <-
        dbGetQuery(con,
                   paste("SELECT * FROM view03 WHERE idind =", numindicador, ";"))
      m <- ''
      e <- ''
      u <- ''
      if (info$tipocuadro != 0) {
        # Si este cuadro no se despliega, no se genera mapa.
        if (info$tipocuadro == 14) {
          # Si se trata de valores textuales
          #! Procesa los mapas de municipios
          if (geo == 1 || geo == 3 || geo == 4 || geo == 8) {
            query2m <-
              paste(
                "SELECT CAST(valort AS text) AS valor FROM fun_indicador(",
                numindicador,
                ", 1) WHERE ID > 0 ORDER BY ID ASC;",
                sep = ""
              )
            
            rows2m <- dbGetQuery(con, query2m)
            
            if (!(all(is.na(rows2m$valor))))
              m <- mapea_js(rows2m$valor, numindicador, "m")
            
          }
          #! Procesa los mapas de estados
          if (geo == 2 || geo == 3 || geo == 4 || geo == 8) {
            query2e <-
              paste(
                "SELECT CAST(valort AS text) AS valor FROM fun_indicador(",
                numindicador,
                ", 2) WHERE ID > 0 ORDER BY ID ASC;",
                sep = ""
              )
            
            rows2e <- dbGetQuery(con, query2e)
            
            if (!is.na(rows2e$valor[11]))
              e <- mapea_js(rows2e$valor, numindicador, "e")
            
          }
          #! Procesa los mapas de EE.UU.
          if (geo == 7 || geo == 8) {
            query2u <-
              paste(
                "SELECT CAST(valort AS text) AS valor FROM fun_indicador(",
                numindicador,
                ", 3) WHERE ID > 0 AND ID <> 2 AND ID <> 15 ORDER BY ID ASC;",
                sep = ""
              )
            
            rows2u <- dbGetQuery(con, query2u)
            
            if (!(all(is.na(rows2u$valor))))
              u <- mapea_js(rows2u$valor, numindicador, "u")
            
          }
        } else {
          # Se trata de números
          #! Procesa los mapas de municipios
          if (geo == 1 || geo == 3 || geo == 4 || geo == 8) {
            query2m <-
              paste(
                "SELECT CAST(valort AS numeric) AS valor FROM fun_indicador(",
                numindicador,
                ", 1) WHERE ID > 0 ORDER BY ID ASC;",
                sep = ""
              )
            
            rows2m <- dbGetQuery(con, query2m)
            
            if (!(all(is.na(rows2m$valor))))
              m <-
              mapea_js(as.numeric(rows2m$valor), numindicador, "m")
            
          }
          #! Procesa los mapas de estados
          if (geo == 2 || geo == 3 || geo == 4 || geo == 8) {
            query2e <-
              paste(
                "SELECT CAST(valort AS numeric) AS valor FROM fun_indicador(",
                numindicador,
                ", 2) WHERE ID > 0 ORDER BY ID ASC;",
                sep = ""
              )
            
            rows2e <- dbGetQuery(con, query2e)
            
            if (!is.na(rows2e$valor[11]))
              e <- mapea_js(as.numeric(rows2e$valor), numindicador, "e")
          }
          #! Procesa los mapas de EE.UU.
          if (geo == 7 || geo == 8) {
            query2u <-
              paste(
                "SELECT CAST(valort AS numeric) AS valor FROM fun_indicador(",
                numindicador,
                ", 3) WHERE ID > 0 AND ID <> 2 AND ID <> 15 ORDER BY ID ASC;",
                sep = ""
              )
            
            rows2u <- dbGetQuery(con, query2u)
            
            if (!(all(is.na(rows2u$valor))))
              u <-
              mapea_js(as.numeric(rows2u$valor), numindicador, "u")
            
          }
        }
        # Si un mapa no existe, no aparece; así no deja comas de más.
        x <- c(m, e, u)
        js <- paste0("{", paste(x[nchar(x) > 0], collapse = ","), "}")
        
        dbExecute(
          con,
          paste(
            "UPDATE indicador SET jsonmap = '",
            js,
            "' WHERE idind = ",
            numindicador,
            ";",
            sep = ""
          )
        )
        message(numindicador)
      }
    }
  }
}

# Genera mapas del rango dado: n, n:m, dado como un rango consecutivo, o bien en lista c(m, p, n)
##mapas_graficos_servidor_a_servidor(c(2111,2301,2302))
##mapas_graficos_servidor_a_servidor(2650:2653)
#mapas_graficos_servidor_a_servidor(2653)

#t1 <- system.time( mapas_graficos_servidor_a_servidor(2653) )
#t1 <- system.time(mapas_graficos_servidor_a_servidor(2654:2708)) # 5.58681818 segundos promedio por mapa (estatal y municipal por indicador con valores enteros)

#mapas_graficos_servidor_a_servidor(2709:4053)

# Actualiza por tabla
##mapas_graficos_servidor_a_servidor(dbGetQuery(con, "SELECT idind FROM indicador WHERE idtabla = 25"))
##mapas_graficos_servidor_a_servidor(dbGetQuery(con, "SELECT idind FROM indicador WHERE idtabla = 35"))
##mapas_graficos_servidor_a_servidor(dbGetQuery(con, "SELECT idind FROM indicador WHERE idtabla = 36"))

#@todo Pendientes
#! Generar los mapas faltantes, consultando primero aquellos que no tienen mapa dado de alto (son 2300)
#! Pendientes o indicadores con errores:
#!  85 hay una categoría con cero municipios, se podría quitar de la escala. Divisi?n por cero (caso 30)
#! 145 error Todos los valores son NA en estatal, no es numérico;
#! 159 error Todos los valores son NA en estatal
#! 258 Todos los valores son iguales a 141.3, da mapa vacio.
#! 298 El estado de Guanajuato tiene valor NA, da mapa vacio.
#! 929 da mapa vacío.

#! 144
#! 158
#! 265... vector 201 en delante 

#! numindicador 144 (3:144), 158 (4:144) [Grado de marginación] y 321 (8:306) [Grado de rezago social]: error, no es numérico; sino textos
#! as.integer(factor(c("Alto", "Medio", "Alto", ""), levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")))
