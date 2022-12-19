# https://www.youtube.com/watch?v=_gzOovLEXWo 1:07:07

# Librerias ----
library(tidyverse) # 1.3.1
library(ggplot2) # 3.4.0
source("database.R")
source("maps.R")

gen_barras <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(anio_sel))
    return(NULL)
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)

  datos_barras <- bd %>%
    #filter(entidad == edo_sel) %>%
    filter(no == ind_sel) %>%
    filter(year == anio_sel)
  
  # Gráfico
  datos_barras %>%
    ggplot(aes(x = reorder(nom, valor),
               y = valor)) +
    geom_col(fill = "olivedrab") +
    geom_text(aes(label = prettyNum(round(valor, 2), big.mark = ",")),
              size = 3,
              hjust = -0.2) +
    scale_y_continuous(expand = expansion(c(0, 0.2), 0),
                       label = scales::comma_format()) +
    coord_flip() +
    labs(
      title = str_c(metadatos_sel$indicador, ", ", metadatos_sel$fecha),
      #subtitle = str_c("Entidad seleccionada: ", datos_barras$entidad[1]),
      caption = str_c("Fuente: ", metadatos_sel$producto),
      x = NULL,
      y = metadatos_sel$unidad
    ) +
    theme_bw() +
    theme(
      plot.title.position  = "plot",
      plot.title = element_text(hjust = 0.5,
                                face = "bold"),
      plot.subtitle = element_text(hjust = 0.5,
                                   face = "bold")
    )
}

gen_mapa <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(anio_sel))
    return(NULL)
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)

  datos_barras <- bd %>%
    #filter(entidad == edo_sel) %>%
    filter(no == ind_sel) %>%
    filter(year == anio_sel)
  
  #todo@ ajustar consultas para que coincidan las claves
  mapa <- shp %>%
    filter(geo == edo_sel) %>%
    left_join(datos_barras, by = "cve")

  # Mapa
  mapa %>%
    ggplot() +
    geom_sf(aes(fill = valor)) +
    theme_bw() +
    scale_fill_gradientn(colors = c("white", "olivedrab")) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(
      title = str_c(metadatos_sel$indicador, ", ", anio_sel),
      caption = str_c("Fuente: ", metadatos_sel$producto),
      x = NULL,
      y = NULL,
      fill = metadatos_sel$unidad
    ) +
   guides(fill = guide_colorbar(
     title.position = "top",
     title.hjust = 0.5,
     barwidth = 20,
     barheight = 0.7
   ))
}

gen_lineas <- function(edo_sel, ind_sel, anio_sel) {
  if(is.null(anio_sel))
    return(NULL)
  metadatos_sel <- meta %>%
    filter(fecha == anio_sel)
  
  datos_lineas <- bd %>%
    #filter(geo == edo_sel) %>%
    filter(no == ind_sel)
  
  datos_lineas$year <- as.numeric(datos_lineas$year)
  datos_lineas %>%
    ggplot(aes(
      x = year,
      y = valor,
      group = nom
    )) +
    geom_line(color = "olivedrab") +
    geom_point(color = "olivedrab") +
    labs(
      title = str_c(metadatos_sel$indicador, ", ", min(datos_lineas$year), " - ", max(datos_lineas$year)),
      caption = str_c("Fuente: ", metadatos_sel$producto),
      x = NULL,
      y = metadatos_sel$unidad
    ) +
    scale_x_continuous(breaks = unique(datos_lineas$year)) +
    scale_y_continuous(label = scales::comma_format()) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold")
    )
}

anios_disponibles <- function(ind_sel) {
  bd %>%
    filter(no == ind_sel) %>%
    pull(year) %>%
    unique()
}