# Elabora un mapa dado un archivo de datos.
library(tidyverse) # 2.0.0
library(DT) # 0.28
library(ggplot2) # 3.4.2
library(jsonlite) # 1.8.4
library(sf) # 1.0-14
library(ggspatial) # 1.1.8
library(readr) # 2.1.4
library(rvest) # 1.0.3

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

mapea_js <- function(x_null) {
  if (mode(x_null) == "numeric") {
    #! Localiza los valores nulos y les asigna valor
    x <- (function(y) ifelse(is.na(y), -9999999, y))(x_null)
    if (min(x) != max(x)) {
      #! Calcula los rangos y clases para colorear
      #! para crear el gráfico con las especificaciones
      pamd <- colorea(x)
      
      #! Procesa la leyenda de datos, redondea a dos dígitos
      #! Poner comas a los números (formato numérico).
      #! 16 de julio de 2009
      #@todo verificar si cuando no hay disponible, se use el valor cero,
      pamd$rangos <- round(pamd$rangos, digits = 2)
      pamd$rangos[, 1] <- prettyNum(pamd$rangos[, 1], big.mark = ",")
      pamd$rangos[, 2] <- prettyNum(pamd$rangos[, 2], big.mark = ",")
      
      #! Colocar el número de entidades en la clase entre paréntesis
      #! 22 de marzo de 2010
      #@todo cambiar el exceso de espacios y tabulador por elementos html
      leyenda <- paste("De ", pamd$rangos[,1], " a ", pamd$rangos[,2], " (", pamd$cuenta, ")", sep = "")
      leyenda <- (function(y) ifelse(y[[1]][,1] == y[[1]][,2], paste(y[[1]][,1], " (", y[[3]], ")"), y[[2]]))(list(pamd$rangos, leyenda, pamd$cuenta))
      leyenda <- (function(y) ifelse(y[[1]][,1] == "-9,999,999", paste("No disponible (", y[[3]], ")"), y[[2]]))(list(pamd$rangos, leyenda, pamd$cuenta))
      
      #@todo usar una estrategia para si hay vacios se muestre en etiqueta, sino no.
      
      #! Salida en formato json unitario
      #! 11 de septiembre de 2013
      s <- list(
          l = leyenda,
          v = pamd$clases
      )
      return(s)
    }
  } else if (mode(x_null) == "character") {
    # Considera este caso de etiquetado
    #@todo Generalizar para cualquier uso de etiquetas
    cr1 <- c("muy bajo", "bajo", "medio", "alto", "muy alto")
    cr2 <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
    
    # Convertir todo a minúsculas y asignar los valores numéricos
    x <- as.numeric(factor(tolower(x_null), levels = cr1, ordered = TRUE))
    
    # Localiza los valores nulos y les asigna valor
    x <- ifelse(x_null == "", 0, x)
    x <- ifelse(is.na(x), 0, x)
    
    # Crea una lista para almacenar el conteo de ocurrencias
    ca <- numeric(length(cr1))
    
    # Contar las ocurrencias para cada etiqueta
    for (i in 1:length(cr1)) {
      ca[i] <- sum(x == i)
    }
    
    # Crea las etiquetas con los conteos
    ly <- paste(cr2, " (", ca, ")", sep = "")
    ly <- ly[ca > 0]
    
    #r <- paste(x, ',', sep = "", collapse = "")
    #r <- substr(r, 1, nchar(r) - 1)
    #l <- paste('"', ly, '",', sep = "", collapse = "")
    #l <- substr(l, 1, nchar(l) - 1)
    #s <- paste('"', geo2, '":{"l":[', l, '],"v":[', r, ']}', sep = "", collapse = "")
    s <- list(
        l = ly,
        v = x
    )
    #s <- toJSON(json_data, auto_unbox = TRUE)
    return(s)
  }
}

systemfonts::register_font(
  name = 'typus',
  plain = "/home/mario/R/ind50/fonts/Regular.otf",
  bold = "/home/mario/R/ind50/fonts/Bold.otf",
  italic = "/home/mario/R/ind50/fonts/RegularItalic.otf",
  bolditalic = "/home/mario/R/ind50/fonts/BoldItalic.otf"
)

colores_tematico <-
  c("#FEFED1", "#FDFC91", "#F9D114", "#EB8936", "#B93623")
colores_simbolos <- c("#484A49", "#F5F7F6")
colores_texto <- "#333333"

#Elabora el mapa
# Cuidar que esté ordenado por clave geográfica y completo!

#meta <- read_csv("/home/mario/Documentos/Boletines/discapacidad/tabla3.csv")
#metadatos_sel <- meta$`Prevalencia de discapacidad`[2:33]

#meta <- read_csv("/home/mario/Documentos/Boletines/discapacidad/tabla4.csv")
#metadatos_sel <- meta$`Prevalencia de discapacidad`[2:47]

meta <- read_csv("/home/mario/R/DNC/tabla02b.csv")
metadatos_sel <- meta$Frecuencia[1:46]

jsonmap <- mapea_js(metadatos_sel)
  
if (file.exists("/home/mario/R/ind50/www/datos/shp.rds")) {
  shp <- readRDS("/home/mario/R/ind50/www/datos/shp.rds")
} else {
  gto <- read_sf("/home/mario/R/ind50/www/datos/gto.shp") |>
    mutate(geo = 1) |>
    rename(cve = CLAVE, nom = NOM_MUN) |>
    select(geo, cve, nom, geometry)
  gto <- st_transform(gto, 3857)
  gto <- st_simplify(gto, dTolerance = 100)
  
  mex <- read_sf("/home/mario/R/ind50/www/datos/mex.shp") |>
    mutate(geo = 2) |>
    rename(cve = CVE_ENT, nom = NOM_ENT) |>
    select(geo, cve, nom, geometry)
  mex <- st_transform(mex, 3857)
  mex <- st_simplify(mex, dTolerance = 1000)
  
  usa <- read_sf("/home/mario/R/ind50/www/datos/usa.shp") |>
    mutate(geo = 7) |>
    rename(cve = FIPS, nom = NAME) |>
    select(geo, cve, nom, geometry)
  usa <- st_transform(usa, 3857)
  usa <- st_simplify(usa, dTolerance = 1000)
  
  shp <- dplyr::bind_rows(gto, mex, usa)
  rm(gto, mex, usa)
  saveRDS(shp, file = "/home/mario/R/ind50/www/datos/shp.rds")
}


edo_sel <- 1
#edo_sel <- 2

#@todo ajustar consultas para que coincidan las claves
mapa <- shp |>
  dplyr::filter(geo == edo_sel)

# Carga los datos del mapa temático
mapa$valorT <- as.factor(jsonmap$v)

# Mapa
p <- mapa |>
  ggplot() +
  geom_sf(aes(fill = valorT)) +
  theme_light(base_size = 13, base_family = "typus") +
  scale_fill_manual(values = colores_tematico,
                    labels = jsonmap$l,
                    breaks = 1:5) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      colour = colores_texto
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      face = "bold",
      colour = colores_texto
    )
  ) +
  labs(
    #title = str_c("none", ", ", "anio_sel"),
    #caption = str_c("Fuente: ", "metadatos_sel$fuente"),
    x = NULL,
    y = NULL,
    fill = "%"
  ) +
  coord_sf(crs = 3857) +
  annotation_scale(location = "bl", bar_cols = colores_simbolos) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_orienteering(line_col = colores_simbolos[1],
                                     fill = colores_simbolos[2:1])
  )

#ggsave("/home/mario/Documentos/Boletines/discapacidad/tabla4.png", plot = p, width = 800, height = 600, units = "px", dpi = 100)

ggsave("/home/mario/R/DNC/tabla2b.png", plot = p, width = 800, height = 600, units = "px", dpi = 100)
