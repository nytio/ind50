# PIB

# Librerias ----
library(tidyverse) # 2.0.0
library(data.table) # 1.14.8
library(readr) # 2.1.4
library(DBI) # 1.1.3

# Conecta con la base de datos
con <- dbConnect(odbc::odbc(), "circinus", timeout = 10)

# Descargan los archivos de Inegi y se descomprimen en disco duro externo.
# Página web o sitio de consulta:
# https://www.inegi.org.mx/programas/pibent/2018/#datos_abiertos
# https://www.inegi.org.mx/contenidos/programas/pibent/2018/datosabiertos/piber_datos_abiertos_csv.zip
# data_file <- "/media/mario/home/R/PIBe/piber.csv"
# data_file <- "/media/mario/home/R/PIBe/piber_percapita.csv"
# data_file <- "/media/mario/home/R/PIBe/piber_percapita_log.csv"
# data <- read_csv(data_file)
# dbWriteTable(con, "tabla_piber_pclog", data, overwrite = TRUE, row.names = FALSE)

# mnemonico_data <- read_csv("process/mnemonico.csv")
# dbWriteTable(con, "mnemonico", mnemonico_data, append = TRUE, row.names = FALSE)

# indicador_data <- read_csv("process/indicador.csv")
# dbWriteTable(con, "indicador", indicador_data, append = TRUE, row.names = FALSE)

serie_data <- read_csv("process/serie.csv")
dbWriteTable(con, "serie", serie_data, append = TRUE, row.names = FALSE)

# Close the connection
dbDisconnect(con)
