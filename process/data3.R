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
data <- "/media/mario/home/R/PIBe/piber.csv"

# Read the data from CSV file
piber_data <- read_csv(data)

# Use dbWriteTable to upload the data
dbWriteTable(con, "tabla_piber", piber_data, overwrite = TRUE, row.names = FALSE)

# Close the connection
dbDisconnect(con)
