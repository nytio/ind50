# https://www.youtube.com/watch?v=_gzOovLEXWo 1:07:07

# Librerias ----
library(jsonlite) # 1.8.4
library(tidyverse) # 1.3.2
library(ggplot2) # 3.4.0
library(sf) # 1.0-9
source("database.R")

gen_barras <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(edo_sel) || is.null(ind_sel) || is.null(anio_sel))
    return(NULL)
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)

  datos_barras <- bd %>%
    filter(no == ind_sel) %>%
    filter(year == anio_sel) %>%
    filter(ambito == edo_sel)
  
  if(edo_sel == "2") {
    datos_barras <-
      datos_barras %>% mutate(ToHighlight = ifelse(cve == 11, "gto", "no" ))
    datos_barras <-
      datos_barras %>% filter(cve != "MEX")
  } else {
    datos_barras <-
      datos_barras %>% mutate(ToHighlight = "no")
  }

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
  
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)
  
  #todo@ ajustar consultas para que coincidan las claves
  mapa <- shp %>%
    filter(geo == edo_sel)
  
  # Carga los datos del mapa temático
  k <- fromJSON(metadatos_sel$jsonmap)
  ke <- switch(edo_sel,
               '1' = k$m,
               '2' = k$e,
               '7' = k$u)
  mapa$valorT <- factor(ke$v)
  colores_etq <- as.vector(ke$l)
  
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
  
  datos_lineas %>%
    ggplot(aes(
      x = year,
      y = valor,
      group = nom,
      color = ToHighlight
    )) +
    geom_line()+
    scale_color_manual(values = c("gto"="#00628C", "no" = "#DFDEDE"), guide = "none") +
    #geom_point(color = "#DAD9DB") +
    labs(
      title = str_c(min(metadatos_sel$indicador), ", ", min(datos_lineas$year), " - ", max(datos_lineas$year)),
      caption = str_c("Fuente: ", min(metadatos_sel$fuente)),
      x = NULL,
      y = min(metadatos_sel$unidad)
    ) +
    scale_x_continuous(breaks = unique(datos_lineas$year)) +
    scale_y_continuous(label = scales::comma_format()) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", colour = "#333333"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", colour = "#333333")
    )
}

tabulado <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(edo_sel) || is.null(ind_sel) || is.null(anio_sel))
    return(NULL)
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)
  
  tab <- bd %>%
    filter(no == ind_sel) %>%
    filter(year == anio_sel) %>%
    filter(ambito == edo_sel)
  
  rownames(tab) <- tab$cve
  tab <- tab %>%
    select(nom, valor)
  names(tab) <- c(names(opciones_entidad)[1], metadatos_sel$unidad)

  DT::datatable(tab,
                options = list(paging = FALSE, searching = FALSE),
                caption = str_c(metadatos_sel$indicador, ", ", anio_sel),
                selection = "none", #list(mode = "single", selected = 12, selectable = 12),
                style = "bootstrap4")
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