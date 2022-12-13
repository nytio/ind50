# https://www.youtube.com/watch?v=_gzOovLEXWo 1:07:07

# Librerias ----
options(scipen = 999)
library(tidyverse) # 1.3.1
library(sf) # 1.0-9
library(plotly) # 4.10.1 // https://cloud.r-project.org/src/contrib/plotly_4.10.0.tar.gz
source("database.R")

shp <- read_sf("www/datos/municipios.geojson") |> 
  rename(cve_mun = CVEGEO)

gen_barras <- function(edo_sel, ind_sel, anio_sel) {
  metadatos_sel <- meta |>
    filter(idserie == ind_sel)

  if(length(anio_sel) > 0) {
    datos_barras <- bd |>
      #filter(entidad == edo_sel) |>
      filter(no == ind_sel) |>
      filter(year == anio_sel)
  
    datos_barras |>
      ggplot(aes(x = reorder(nom, valor),
                 y = valor)) +
      geom_col(fill = "olivedrab") +
      geom_text(aes(label = prettyNum(round(valor, 2), big.mark = ",")),
                size = 3,
                hjust = -0.2) +
      scale_y_continuous(expand = expansion(c(0,0.2), 0),
                         label = scales::comma_format()) +
      coord_flip() +
      labs(title = metadatos_sel$indicador,
           #subtitle = str_c("Entidad seleccionada: ", datos_barras$entidad[1]),
           caption = str_c("Fuente: ", metadatos_sel$fuente),
           x = NULL, y = metadatos_sel$unidad) +
      theme_bw() +
      theme(plot.title.position  = "plot",
            plot.title = element_text(hjust = 0.5,
                                      face = "bold"),
            plot.subtitle = element_text(hjust = 0.5,
                                         face = "bold"))
  }
}

gen_mapa <- function(edo_sel, ind_sel, anio_sel) {
  metadatos_sel <- meta |> 
    filter(idserie == ind_sel)

  if(length(anio_sel) > 0) {
    datos_barras <- bd |> 
      #filter(entidad == edo_sel) |> 
      filter(no == ind_sel) |> 
      filter(year == anio_sel)
    
    mapa <- shp |> 
      filter(NOM_ENT == edo_sel) |> 
      left_join(datos_barras, by = "cve_mun")
    
    mapa |> 
      ggplot(aes(fill = valor)) + 
      geom_sf() + 
      scale_fill_gradientn(colors = c("white", "olivedrab")) + 
      theme_bw() + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            panel.border = element_blank(), 
            legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5, 
                                         face = "bold")) + 
      labs(title = metadatos_sel$indicador, 
           #subtitle = str_c("Entidad seleccionada: ", datos_barras$entidad[1]),
           caption = str_c("Fuente: ", metadatos_sel$fuente), 
           x = NULL, y = NULL,
           fill = metadatos_sel$unidad) + 
      guides(fill = guide_colorbar(title.position = "top", 
                                   title.hjust = 0.5, 
                                   barwidth = 20, 
                                   barheight = 0.7))
  }
}

gen_lineas <- function(edo_sel, ind_sel, anio_sel) {
  metadatos_sel <- meta |> 
    filter(idserie == ind_sel)
  
  if(length(anio_sel) > 0) {
    datos_lineas <- bd |> 
      #filter(entidad == edo_sel) |> 
      filter(no == ind_sel) 
    
    gl <- datos_lineas |> 
      ggplot(aes(x = year, y = valor, group = nom, 
                 text = str_c("<b>Municipio: </b>", nom, "<br>", 
                              "<b>Valor: </b>", prettyNum(round(valor, 2), big.mark = ","), "<br>",
                              "<b>Año: </b>", year))) + 
      geom_line(color = "olivedrab") +
      geom_point(color = "olivedrab") + 
      # labs(title = str_c(metadatos_sel$indicador, "<br>", 
      #                    "Entidad seleccionada: ", datos_lineas$entidad[1]),
      #      caption = str_c("Fuente: ", metadatos_sel$fuente), 
      #      x = NULL, y = metadatos_sel$unidad) + 
      scale_x_continuous(breaks = unique(datos_lineas$year)) +
      scale_y_continuous(label = scales::comma_format()) + 
      theme_bw() + 
      theme(panel.border = element_blank(), 
            legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5, 
                                         face = "bold"))
    # Gráfica interactiva: 
    gl |> 
      ggplotly(tooltip = "text")
  }
}

anios_disponibles <- function(ind_sel) {
  bd |>
    filter(no == ind_sel) |>
    pull(year) |> #todo@ objeto no encontrado!
    unique()
}