# Importa los datos y los guarda en la base de datos

# Librerias ----
library(tidyverse) # 1.3.1
library(DBI) # 1.1.3
library(lubridate) # 1.9.1
library(foreign) # 0.8-84

# Conexiones ----
con <- dbConnect(odbc::odbc(), "indicadores", timeout = 10) #indicadores circinus

# Crea la tabla en la base de datos
query <- paste(
  "DROP TABLE IF EXISTS public.tabla_remesas;",
  "CREATE TABLE IF NOT EXISTS public.tabla_remesas (",
  "  pais integer NOT NULL DEFAULT 303,",
  "  ent integer NOT NULL,",
  "  mun integer NOT NULL DEFAULT 0,",
  "  loc integer NOT NULL DEFAULT 0,",
  "  remesas_2003 numeric,",
  "  remesas_2004 numeric,",
  "  remesas_2005 numeric,",
  "  remesas_2006 numeric,",
  "  remesas_2007 numeric,",
  "  remesas_2008 numeric,",
  "  remesas_2009 numeric,",
  "  remesas_2010 numeric,",
  "  remesas_2011 numeric,",
  "  remesas_2012 numeric,",
  "  remesas_2013 numeric,",
  "  remesas_2014 numeric,",
  "  remesas_2015 numeric,",
  "  remesas_2016 numeric,",
  "  remesas_2017 numeric,",
  "  remesas_2018 numeric,",
  "  remesas_2019 numeric,",
  "  remesas_2020 numeric,",
  "  remesas_2021 numeric,",
  "  remesas_2022 numeric,",
  "  remesasmxn_2003 numeric,",
  "  remesasmxn_2004 numeric,",
  "  remesasmxn_2005 numeric,",
  "  remesasmxn_2006 numeric,",
  "  remesasmxn_2007 numeric,",
  "  remesasmxn_2008 numeric,",
  "  remesasmxn_2009 numeric,",
  "  remesasmxn_2010 numeric,",
  "  remesasmxn_2011 numeric,",
  "  remesasmxn_2012 numeric,",
  "  remesasmxn_2013 numeric,",
  "  remesasmxn_2014 numeric,",
  "  remesasmxn_2015 numeric,",
  "  remesasmxn_2016 numeric,",
  "  remesasmxn_2017 numeric,",
  "  remesasmxn_2018 numeric,",
  "  remesasmxn_2019 numeric,",
  "  remesasmxn_2020 numeric,",
  "  remesasmxn_2021 numeric,",
  "  remesasmxn_2022 numeric,",
  "  remesaspibe_2003 numeric,",
  "  remesaspibe_2004 numeric,",
  "  remesaspibe_2005 numeric,",
  "  remesaspibe_2006 numeric,",
  "  remesaspibe_2007 numeric,",
  "  remesaspibe_2008 numeric,",
  "  remesaspibe_2009 numeric,",
  "  remesaspibe_2010 numeric,",
  "  remesaspibe_2011 numeric,",
  "  remesaspibe_2012 numeric,",
  "  remesaspibe_2013 numeric,",
  "  remesaspibe_2014 numeric,",
  "  remesaspibe_2015 numeric,",
  "  remesaspibe_2016 numeric,",
  "  remesaspibe_2017 numeric,",
  "  remesaspibe_2018 numeric,",
  "  remesaspibe_2019 numeric,",
  "  remesaspibe_2020 numeric,",
  "  remesaspibe_2021 numeric,",
  "  CONSTRAINT remesas_idx PRIMARY KEY (ent)",
  ")",
  collapse = "\n"
)
dbExecute(con, query)


# Actualiza una fuente ----
library(DBI)
library(data.table)

con <- dbConnect(odbc::odbc(), "indicadores", timeout = 10) #indicadores circinus

datos <- fread("docs/data/tabla_iter_20.csv")

# Comprobar si la tabla existe
tabla_existe <- dbExistsTable(con, "tabla_iter_20")

# Si la tabla no existe, crearla antes de escribir los datos
if (!tabla_existe) {
  #dbCreateTable(con, "tabla_iter_20", datos)
  # Es mejor ejecutar un query explícito
  query <- readLines("docs/data/tabla_iter_20.sql")
  query <- paste(query, collapse = " ")
  result <- dbExecute(con, query)
} else {
  query <- "DELETE FROM tabla_iter_20;"
  result <- dbExecute(con, query)
}
# En caso de eliminar toda la tabla, con su estructura, ejecutar:
# DROP TABLE nombre_tabla;

# Guarda todo los datos en un solo paso:
dbWriteTable(con, "tabla_iter_20", datos, overwrite = FALSE, append = TRUE, row.names = FALSE)
