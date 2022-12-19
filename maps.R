library(tidyverse) # 1.3.1
library(sf) # 1.0-9

gto <- read_sf("www/datos/gto.geojson") %>% 
  mutate(geo = 1) %>%
  rename(cve = CLAVE, nom = NOM_MUN) %>%
  select(geo, cve, nom, geometry)

mex <- read_sf("www/datos/mex.geojson") %>% 
  mutate(geo = 2) %>%
  rename(cve = CVE_ENT, nom = NOM_ENT) %>%
  select(geo, cve, nom, geometry)

usa <- read_sf("www/datos/usa.geojson") %>% 
  mutate(geo = 7) %>%
  rename(cve = FIPS, nom = NAME) %>%
  select(geo, cve, nom, geometry)

shp <- bind_rows(gto, mex, usa)
rm(gto, mex, usa)