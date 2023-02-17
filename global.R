# Genera gráficas y tablas

# Librerias ----
library(tidyverse) # 1.3.2
library(ggplot2) # 3.4.1
library(jsonlite) # 1.8.4
library(sf) # 1.0-9
library(openxlsx) # 4.2.5.2
library(rvest) # 1.0-3
source("database.R")

gen_barras <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(edo_sel) || is.null(ind_sel) || is.null(anio_sel))
    return(NULL)

  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)

  if(length(metadatos_sel$fecha) == 0)
    return(NULL)

  datos_barras <- bd %>%
    filter(no == ind_sel) %>%
    filter(year == anio_sel) %>%
    filter(ambito == edo_sel)
  
  if(length(datos_barras$ambito) == 0)
    return(NULL)
  
  if(edo_sel == "2") {
    datos_barras <-
      datos_barras %>% mutate(ToHighlight = ifelse(cve == 11, "gto", "no" ))
    datos_barras <-
      datos_barras %>% filter(cve != "MEX")
  } else {
    datos_barras <-
      datos_barras %>% mutate(ToHighlight = "no")
  }
  
  # Registra que se descargó un archivo
  dbExecute(con, paste0("UPDATE indicador SET hitsgph = hitsgph + 1 WHERE idind = ", metadatos_sel$idind))

  # Gráfico
  datos_barras %>%
    ggplot(aes(x = reorder(nom, valor),
               y = valor,
               fill = ToHighlight)) +
    geom_col() +
    scale_fill_manual(values = c("gto"="#00628C", "no" = "#1FB3E5"), guide = "none") +
    geom_text(aes(label = prettyNum(round(valor, 2), big.mark = ",")),
              size = 3,
              hjust = -0.2) +
    scale_y_continuous(expand = expansion(c(0, 0.2), 0),
                       label = scales::comma_format()) +
    coord_flip() +
    labs(
      title = str_c(metadatos_sel$indicador, ", ", metadatos_sel$fecha),
      #subtitle = str_c("Entidad seleccionada: ", datos_barras$entidad[1]),
      caption = str_c("Fuente: ", metadatos_sel$fuente),
      x = NULL,
      y = metadatos_sel$unidad
    ) +
    theme_bw() +
    theme(
      plot.title.position  = "plot",
      plot.title = element_text(hjust = 0.5, face = "bold", colour = "#333333"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", colour = "#333333")
    )
}

gen_mapa <- function(edo_sel, ind_sel, anio_sel) {
  if (is.null(edo_sel) || is.null(ind_sel) || is.null(anio_sel))
    return(NULL)
  
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)
  
  if(length(metadatos_sel$fecha) == 0)
    return(NULL)
  
  if (file.exists("www/datos/shp.rds")) {
    shp <- readRDS("www/datos/shp.rds")
  } else {
    gto <- read_sf("www/datos/gto.geojson") %>%
      mutate(geo = 1) %>%
      rename(cve = CLAVE, nom = NOM_MUN) %>%
      select(geo, cve, nom, geometry)
    gto <- st_simplify(gto, dTolerance = 100)
    
    mex <- read_sf("www/datos/mex.geojson") %>%
      mutate(geo = 2) %>%
      rename(cve = CVE_ENT, nom = NOM_ENT) %>%
      select(geo, cve, nom, geometry)
    mex <- st_simplify(mex, dTolerance = 1000)
    
    usa <- read_sf("www/datos/usa.geojson") %>%
      mutate(geo = 7) %>%
      rename(cve = FIPS, nom = NAME) %>%
      select(geo, cve, nom, geometry)
    usa <- st_simplify(usa, dTolerance = 1000)
    
    shp <- bind_rows(gto, mex, usa)
    rm(gto, mex, usa)
    saveRDS(shp, file = "www/datos/shp.rds")
  }
  
  colores <-
    c("#FEFED1", "#FDFC91", "#F9D114", "#EB8936", "#B93623")
  
  #@todo ajustar consultas para que coincidan las claves
  mapa <- shp %>%
    filter(geo == edo_sel)
  
  # Carga los datos del mapa temático
  k <- fromJSON(metadatos_sel$jsonmap)
  ke <- switch(edo_sel,
               '1' = k$m,
               '2' = k$e,
               '7' = k$u)
  if(is.null(ke))
    return(NULL)

  mapa$valorT <- factor(ke$v)
  colores_etq <- as.vector(ke$l)
  
  # Registra que se descargó un archivo
  dbExecute(con, paste0("UPDATE indicador SET hitsmap = hitsmap + 1 WHERE idind = ", metadatos_sel$idind))
  
  # Mapa
  mapa %>%
    ggplot() +
    geom_sf(aes(fill = valorT)) +
    theme_bw() +
    scale_fill_manual(values = colores, labels = colores_etq) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        colour = "#333333"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        face = "bold",
        colour = "#333333"
      )
    ) +
    labs(
      title = str_c(metadatos_sel$indicador, ", ", anio_sel),
      caption = str_c("Fuente: ", metadatos_sel$fuente),
      x = NULL,
      y = NULL,
      fill = metadatos_sel$unidad
    )
}

gen_lineas <- function(edo_sel, ind_sel) {
  if(is.null(edo_sel) || is.null(ind_sel))
    return(NULL)
  metadatos_sel <- meta
  
  datos_lineas <- bd %>%
    filter(no == ind_sel) %>%
    filter(ambito == edo_sel)
  
  if(length(datos_lineas$ambito) == 0)
    return(NULL)
  
  datos_lineas$year <- as.integer(as.character(datos_lineas$year))
  
  if(edo_sel == "2") {
    datos_lineas <-
      datos_lineas %>% mutate(ToHighlight = ifelse(cve == 11, "gto", "no" ))
    datos_lineas <-
      datos_lineas %>% filter(cve != "MEX")
  } else {
    datos_lineas <-
      datos_lineas %>% mutate(ToHighlight = "no")
  }
  
  if(length(unique(datos_lineas$year)) < 20)
    escala_x <- unique(datos_lineas$year)
  else
    escala_x <- seq(min(datos_lineas$year), max(datos_lineas$year), by = 5)

  datos_lineas %>%
    ggplot(aes(
      x = year,
      y = valor,
      group = nom,
      color = ToHighlight,
      linetype = ToHighlight
    )) +
    geom_line() +
    scale_color_manual(values = c("gto" = "#00628C", "no" = "#8d8d8d"), guide = "none") +
    scale_linetype_manual(values = c("gto" = "solid", "no" = "solid"), guide = "none") +
    scale_size_manual(values = c("gto" = 1.0, "no" = 0.5), guide = "none") +
    #geom_point(color = "#8d8d8d") +
    labs(
      title = str_c(min(metadatos_sel$indicador), ", ", min(datos_lineas$year), " - ", max(datos_lineas$year)),
      caption = str_c("Fuente: ", min(metadatos_sel$fuente)),
      x = NULL,
      y = min(metadatos_sel$unidad)
    ) +
    scale_x_continuous(breaks = escala_x) +
    scale_y_continuous(label = scales::comma_format()) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", colour = "#333333"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", colour = "#333333"),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )
}

tabulado <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(edo_sel) || is.null(ind_sel) || is.null(anio_sel))
    return(NULL)

  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)
  
  if(length(metadatos_sel$fecha) == 0)
    return(NULL)
  
  tab <- bd %>%
    filter(no == ind_sel) %>%
    filter(year == anio_sel) %>%
    filter(ambito == edo_sel)
  
  if(length(tab$ambito) == 0)
    return(NULL)
  
  rownames(tab) <- tab$cve
  if(is.na(metadatos_sel$idmasculino)) {
    tab <- tab %>%
      select(nom, valor)
    names(tab) <- c(names(opciones_entidad)[1], metadatos_sel$unidad)
  } else {
    tab <- tab %>%
      select(nom, valor, valor_m, valor_f)
    names(tab) <- c(names(opciones_entidad)[1], metadatos_sel$unidad, "Hombres", "Mujeres")
  }
  
  # Registra que se descargó un archivo
  dbExecute(con, paste0("UPDATE indicador SET hitstbl = hitstbl + 1 WHERE idind = ", metadatos_sel$idind))

  DT::datatable(tab,
                options = list(paging = FALSE, searching = FALSE),
                caption = str_c(metadatos_sel$indicador, ", ", anio_sel),
                selection = "none", #list(mode = "single", selected = 12, selectable = 12),
                style = "bootstrap4")
}

descargar <- function(mis_datos, selAnio, file) {
  # Crea un nuevo libro de trabajo
  # Establecer las propiedades del libro
  wb <- createWorkbook(
    creator = "Mario Hernández Morales",
    title = minimal_html(mis_datos$x$caption) %>% html_elements("caption") %>% html_text(),
    subject = names(mis_datos$x$data)[2],
    category = "Catálogo de indicadores")
  
  # Agrega una hoja de trabajo al libro y escribe el dataframe en ella
  addWorksheet(wb, "Hoja1")
  writeData(wb, "Hoja1", substr(mis_datos$x$caption, 10, nchar(mis_datos$x$caption)-10) )
  mis_datos <- mis_datos$x$data
  writeData(wb, "Hoja1", mis_datos, startRow = 3)
  metadatos_sel <- meta %>% 
    filter(fecha == selAnio)
  
  writeData(wb, "Hoja1", paste("Fuente:", metadatos_sel$fuente), startRow = dim(mis_datos)[1]+5)
  html <- minimal_html(metadatos_sel$producto)
  writeData(wb, "Hoja1", html %>% html_elements("a") %>% html_text(), startRow = dim(mis_datos)[1]+6)
  writeData(wb, "Hoja1", html %>% html_elements("a") %>% html_attrs(), startRow = dim(mis_datos)[1]+7)
  writeData(wb, "Hoja1", "Elaboró: Instituto de Planeación, Estadística y Geografía del Estado de Guanajuato (IPLANEG).", startRow = dim(mis_datos)[1]+8)
  # Crea un estilos para la tabla
  titleStyle <- createStyle(
    fontName = "Arial",
    fontSize = 13,
    textDecoration = "bold"
  )
  
  headerStyle <- createStyle(
    fontName = "Arial",
    fontSize = 11,
    fontColour = "white",
    fgFill = "#3465a4",
    halign = "center",
    textDecoration = "bold"
  )
  
  dataStyle <- createStyle(
    fontName = "Arial",
    fontSize = 11,
    borderStyle = "thin"
  )
  
  fuenteStyle <- createStyle(
    fontSize = 9,
  )
  
  # Aplica el estilo a las celdas de la tabla
  addStyle(wb, sheet = "Hoja1", style = titleStyle, rows = 1, cols = 1)
  addStyle(wb, sheet = "Hoja1", style = headerStyle, rows = 3, cols = 1:(dim(mis_datos)[2]))
  addStyle(wb, sheet = "Hoja1", style = dataStyle, rows = 4:(dim(mis_datos)[1]+8), cols = 1:(dim(mis_datos)[2]), gridExpand = TRUE)
  addStyle(wb, sheet = "Hoja1", style = fuenteStyle, rows = (dim(mis_datos)[1]+5):(dim(mis_datos)[1]+8), cols = 1, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet = "Hoja1", cols = 2:(dim(mis_datos)[2]+2), widths = "auto")
  
  # Registra que se descargó un archivo
  dbExecute(con, paste0("UPDATE indicador SET hitsxls = hitsxls + 1 WHERE idind = ", metadatos_sel$idind))
  
  # Guarda el libro de trabajo en un archivo xlsx
  saveWorkbook(wb, file)
}

tabulado2 <- function(ind_sel) {
  if(is.null(ind_sel))
    return(NULL)
  metadatos_sel <- meta %>% 
    mutate(id = paste(idserie, idind, sep = "-")) %>%
    select(id, indicador, descripcion, unidad, fecha,  fuente, producto)
  names(metadatos_sel) <- c("No", "Nombre del Indicador", "Definición del indicador", "Unidad de medida", "Fecha","Fuente del indicador", "Sitio de consulta del indicador")

  DT::datatable(metadatos_sel,
                options = list(paging = FALSE, searching = FALSE),
                #caption = "Ficha del indicador",
                selection = "none", #list(mode = "single", selected = 12, selectable = 12),
                style = "bootstrap4", escape = FALSE)
}

# https://youtu.be/_gzOovLEXWo?t=4027