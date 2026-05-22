#############################################################
# Bioestadística con R
# Proyecto: Tarimeras certificadas en Costa Rica
# Script 01: Limpieza y control de calidad de datos
# Base: BASE_DATOS_PROYECTO.xlsx
#############################################################

# 1. Cargar paquetes ----

library(readxl)
library(tidyverse)
library(janitor)
library(writexl)

# 2. Importar base de datos ----

datos_raw <- read_excel(
  "C:/Users/valeo/OneDrive/Escritorio/9no Semestro/Bioestadística_R/Proyecto/BASE_DATOS_PROYECTO.xlsx",
  sheet = "BASE TOTAL"
)

# 3.Revisar estructura inicial----
glimpse(datos_raw)
names(datos_raw)
head(datos_raw)
summary(datos_raw)
dim(datos_raw)

# 4. Eliminar fila innecesaria ----
# La primera fila contiene subtítulos repetidos

datos_raw <- datos_raw[-1, ]

# Verificar dimensiones
dim(datos_raw)

# 5. Limpiar nombres de columnas ----

datos <- datos_raw %>%
  clean_names()

# Revisar nombres nuevos
names(datos)

# 6. Renombrar variables ----

datos <- datos %>%
  rename(
    numero = n,
    tipo_de_certificacion = tipo,
    provincia = localizacion,
    canton = x7,
    localizacion = x8,
    capacidad_europea = capacidad,
    capacidad_americana = x15,
    capacidad_metros_cubicos = x16,
    coordenada_y = coordenadas,
    coordenada_x = x19
  )

# Revisar nombres finales
names(datos)

# 7. Limpiar espacios y valores inconsistentes ----

datos <- datos %>%
  mutate(
    across(where(is.character), str_trim),
    
    across(
      where(is.character),
      ~ ifelse(. %in% c("", " ", "N/A", "-"),
               NA,.)))

datos <- datos %>%
  mutate(
    estado = ifelse(
      estado == "Sin_informacion",
      NA,
      estado
    )
  )

# 8. Estandarizar variables categóricas ----

datos <- datos %>%
  mutate(
    provincia = str_to_title(provincia),
    canton = str_to_title(canton),
    estado = str_to_title(estado),
    tipo_de_certificacion = str_to_title(tipo_de_certificacion),
    tipo_de_horno = str_to_title(tipo_de_horno),
    tipo_de_calor = str_to_title(tipo_de_calor),
    fuente_de_calor = str_to_title(fuente_de_calor)
  )

# Revisar categorías principales

count(datos, tipo_de_certificacion, sort = TRUE)
count(datos, estado, sort = TRUE)
count(datos, provincia, sort = TRUE)
count(datos, tipo_de_horno, sort = TRUE)
count(datos, canton, sort = TRUE)
print(count(datos, canton, sort = TRUE), n = Inf)

# 9. Convertir variables numéricas ----

datos <- datos %>%
  mutate(
    ano = as.numeric(ano),
    cedula_juridica = as.numeric(cedula_juridica),
    cantidad_de_abanicos = as.numeric(cantidad_de_abanicos),
    capacidad_europea = as.numeric(capacidad_europea),
    capacidad_americana = as.numeric(capacidad_americana),
    capacidad_metros_cubicos = as.numeric(capacidad_metros_cubicos),
    coordenada_y = as.numeric(coordenada_y),
    coordenada_x = as.numeric(coordenada_x)
  )

# Revisar estructura
glimpse(datos)

# 10. Revisar valores imposibles ----

datos %>% filter(cantidad_de_abanicos < 0)
datos %>% filter(capacidad_europea < 0)
datos %>% filter(capacidad_americana < 0)
datos %>% filter(capacidad_metros_cubicos < 0)

# 11. Revisar registros repetidos por empresa ----

repetidos_empresa <- datos %>%
  count(empresa, sort = TRUE) %>%
  filter(n > 1)

repetidos_empresa

# 12. Resumen de valores faltantes ----

datos %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

# 13. Exportar base limpia ----

write_xlsx(
  datos,
  "C:/Users/valeo/OneDrive/Escritorio/9no Semestro/Bioestadística_R/Proyecto/Scrip/Base_generada_scrip_1/datos_limpios_tarimeras.xlsx"
)

## End ----
