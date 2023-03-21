# Genera gráficas y tablas

# Librerias ----
library(tidyverse) # 2.0.0
library(DT) # 0.27
library(ggplot2) # 3.4.1
library(jsonlite) # 1.8.4
library(sf) # 1.0-9
library(ggspatial) # 1.1.7
library(openxlsx) # 4.2.5.2
library(rvest) # 1.0.3
source("database.R")

systemfonts::register_font(
  name = 'typus',
  plain = "fonts/Regular.otf",
  bold = "fonts/Bold.otf",
  italic = "fonts/RegularItalic.otf",
  bolditalic = "fonts/BoldItalic.otf"
)

colores_tematico <-
  c("#FEFED1", "#FDFC91", "#F9D114", "#EB8936", "#B93623")
colores_simbolos <- c("#484A49", "#F5F7F6")
colores_texto <- "#333333"
  
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
  
  # Registra que se mostró un gráfico
  contabiliza_uso(metadatos_sel$idind, "hitsgph")

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
    theme_light(base_size = 13, base_family = "typus") +
    theme(
      plot.title.position  = "plot",
      plot.title = element_text(hjust = 0.5, face = "bold", colour = colores_texto),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", colour = colores_texto)
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
    gto <- read_sf("www/datos/gto.shp") %>%
      mutate(geo = 1) %>%
      rename(cve = CLAVE, nom = NOM_MUN) %>%
      select(geo, cve, nom, geometry)
    gto <- st_transform(gto, 3857)
    gto <- st_simplify(gto, dTolerance = 100)
    
    mex <- read_sf("www/datos/mex.shp") %>%
      mutate(geo = 2) %>%
      rename(cve = CVE_ENT, nom = NOM_ENT) %>%
      select(geo, cve, nom, geometry)
    mex <- st_transform(mex, 3857)
    mex <- st_simplify(mex, dTolerance = 1000)
    
    usa <- read_sf("www/datos/usa.shp") %>%
      mutate(geo = 7) %>%
      rename(cve = FIPS, nom = NAME) %>%
      select(geo, cve, nom, geometry)
    usa <- st_transform(usa, 3857)
    usa <- st_simplify(usa, dTolerance = 1000)
    
    shp <- dplyr::bind_rows(gto, mex, usa)
    rm(gto, mex, usa)
    saveRDS(shp, file = "www/datos/shp.rds")
  }
  
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
  
  # Registra que se mostró un mapa
  contabiliza_uso(metadatos_sel$idind, "hitsmap")
  
  # Mapa
  mapa %>%
    ggplot() +
    geom_sf(aes(fill = valorT)) +
    theme_light(base_size = 13, base_family = "typus") +
    scale_fill_manual(values = colores_tematico, labels = colores_etq) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", colour = colores_texto),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", colour = colores_texto)
    ) +
    labs(
      title = str_c(metadatos_sel$indicador, ", ", anio_sel),
      caption = str_c("Fuente: ", metadatos_sel$fuente),
      x = NULL,
      y = NULL,
      fill = metadatos_sel$unidad
    ) +
    coord_sf(crs = 3857) +
    annotation_scale(location = "bl", bar_cols = colores_simbolos) +
    annotation_north_arrow(location = "tr",
                           style = north_arrow_orienteering(
                             line_col = colores_simbolos[1],
                             fill = colores_simbolos[2:1]
                           ))
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
  
  if(length(unique(datos_lineas$year)) < 2)
    return(NULL)
  
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
    theme_light(base_size = 13, base_family = "typus") +
    theme(
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", colour = colores_texto),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", colour = colores_texto),
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
    names(tab) <- c(c("Municipio", "Entidad federativa")[as.numeric(edo_sel)], metadatos_sel$unidad)
  } else {
    tab <- tab %>%
      select(nom, valor, valor_m, valor_f)
    names(tab) <- c(c("Municipio", "Entidad federativa")[as.numeric(edo_sel)], metadatos_sel$unidad, "Hombres", "Mujeres")
  }
  
  mis_datos <- DT::datatable(tab,
                options = list(paging = FALSE, searching = FALSE),
                caption = str_c(metadatos_sel$indicador, ", ", anio_sel),
                selection = "none", #list(mode = "single", selected = 12, selectable = 12),
                style = "bootstrap4")
  
  # Da formato a los valores numéricos
  if(metadatos_sel$idtipodato == 1)
    mis_datos <- mis_datos %>% formatRound(columns = 2:dim(tab)[2], digits = 0)
  else if(metadatos_sel$idtipodato == 2)
    mis_datos <- mis_datos %>% formatRound(columns = 2:dim(tab)[2], digits = 2)
  
  # Registra que se mostró un tabulado
  contabiliza_uso(metadatos_sel$idind, "hitstbl")
  
  return(mis_datos)
}

descargar <- function(mis_datos, selAnio, file) {
  # Extraer los datos y el título de la tabla
  tabla <- mis_datos$x$data
  titulo <- minimal_html(mis_datos$x$caption) %>% html_text()
  
  # Crea un nuevo libro de trabajo
  wb <- createWorkbook(
    creator = "Mario Hernandez",
    title = titulo,
    subject = names(tabla)[2],
    category = "Catálogo de indicadores")
  
  # Agrega una hoja de trabajo al libro y escribe los datos en ella
  addWorksheet(wb, "Hoja1")
  writeData(wb, "Hoja1", titulo, startRow = 1, startCol = 1)
  writeData(wb, "Hoja1", tabla, startRow = 3)
  
  # Obtener metadatos del indicador seleccionado
  metadatos_sel <- meta %>% 
    filter(fecha == selAnio)
  
  # Escribir metadatos en la hoja de trabajo
  writeData(wb, "Hoja1", paste("Fuente:", metadatos_sel$fuente), startRow = nrow(tabla)+5)
  html <- minimal_html(metadatos_sel$producto) %>% html_elements("a")
  writeData(wb, "Hoja1", html %>% html_text(), startRow = nrow(tabla)+6)
  writeData(wb, "Hoja1", html %>% html_attrs(), startRow = nrow(tabla)+7)
  writeData(wb, "Hoja1", "Elaboró: Instituto de Planeación, Estadística y Geografía del Estado de Guanajuato (IPLANEG).", startRow = nrow(tabla)+8)
  
  # Establecer los estilos de la tabla
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
  
  bottomLineStyle <- createStyle(
    border = "Bottom",
    borderColour = "#3465a4",
    borderStyle = "thin"
  )
  
  fuenteStyle <- createStyle(fontSize = 9)
  styleNum <- createStyle(numFmt = "#,##0")
  styleDec <- createStyle(numFmt = "#,##0.00")
  
  # Aplica el estilo a las celdas de la tabla
  addStyle(wb, sheet = "Hoja1", style = titleStyle, rows = 1, cols = 1)
  addStyle(wb, sheet = "Hoja1", style = headerStyle, rows = 3, cols = 1:ncol(tabla))
  addStyle(wb, sheet = "Hoja1", style = dataStyle, rows = 4:(nrow(tabla)+8), cols = 1:ncol(tabla), gridExpand = TRUE)
  addStyle(wb, sheet = "Hoja1", style = bottomLineStyle, rows = nrow(tabla)+3, cols = 1:ncol(tabla), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "Hoja1", style = fuenteStyle, rows = (nrow(tabla)+5):(nrow(tabla)+8), cols = 1, gridExpand = TRUE, stack = TRUE)
  
  # Da formato a los valores numéricos
  if(metadatos_sel$idtipodato == 1)
    addStyle(wb, sheet = "Hoja1", style = styleNum, rows = 4:(nrow(tabla)+8), cols = 2:ncol(tabla), gridExpand = TRUE, stack = TRUE)
  else if(metadatos_sel$idtipodato == 2)
    addStyle(wb, sheet = "Hoja1", style = styleDec, rows = 4:(nrow(tabla)+8), cols = 2:ncol(tabla), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet = "Hoja1", cols = 2:(ncol(tabla)+2), widths = "auto")
  
  showGridLines(wb, sheet = "Hoja1", showGridLines = FALSE)
  
  # Registra que se descargó un archivo en excel
  contabiliza_uso(metadatos_sel$idind, "hitsxls")
  
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

tabulado3 <- function() {
  metadatos_sel <- consulta_bde()
  names(metadatos_sel) <- c("No", "Nombre del Indicador", "Unidad de medida", "Fecha","Fuente del indicador", "Sitio de consulta del indicador")
  
  DT::datatable(metadatos_sel,
                options = list(paging = TRUE, searching = TRUE),
                #caption = "Listado de indicadores",
                filter = list(position = "top", clear = TRUE, plain = FALSE),
                selection = "none", #list(mode = "single", selected = 12, selectable = 12),
                style = "bootstrap4", escape = FALSE)
}

# https://youtu.be/_gzOovLEXWo?t=4027