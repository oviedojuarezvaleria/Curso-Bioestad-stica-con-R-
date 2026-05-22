#############################################################
# Bioestadística con R
# Proyecto: Tarimeras certificadas en Costa Rica
# Script 02: Análisis exploratorio de datos
# Base: datos_limpios_tarimeras.xlsx
#############################################################

# 1. Cargar paquetes ----

library(readxl)
library(tidyverse)
library(writexl)

# 2. Importar base de datos limpia ----

datos <- read_excel(
  "Base_generada_scrip_1/datos_limpios_tarimeras.xlsx"
)

# 3. Exploración inicial ----

glimpse(datos)
summary(datos)
dim(datos)
head(datos)

# 4. Exploración inicial de variables ----

# Frecuencia por provincia
datos %>%
  count(provincia, sort = TRUE)

# Frecuencia por tipo de certificación
datos %>%
  count(tipo_de_certificacion, sort = TRUE)

# Frecuencia por estado
datos %>%
  count(estado, sort = TRUE)

# 5. Estadísticos descriptivos de variables numéricas----

tabla_resumen <- tibble(
  Variable = c(
    "Promedio de abanicos",
    "Desviación estándar de abanicos",
    "Promedio capacidad europea",
    "Desviación estándar capacidad europea",
    "Promedio capacidad americana",
    "Desviación estándar capacidad americana",
    "Promedio capacidad metros cúbicos",
    "Desviación estándar capacidad metros cúbicos"
  ),
  
  Valor = c(
    mean(datos$cantidad_de_abanicos, na.rm = TRUE),
    sd(datos$cantidad_de_abanicos, na.rm = TRUE),
    
    mean(datos$capacidad_europea, na.rm = TRUE),
    sd(datos$capacidad_europea, na.rm = TRUE),
    
    mean(datos$capacidad_americana, na.rm = TRUE),
    sd(datos$capacidad_americana, na.rm = TRUE),
    
    mean(datos$capacidad_metros_cubicos, na.rm = TRUE),
    sd(datos$capacidad_metros_cubicos, na.rm = TRUE)
  )
)

tabla_resumen

# Exportar tabla resumen


write_xlsx(
  tabla_resumen,
  "C:/Users/valeo/OneDrive/Escritorio/9no Semestro/Bioestadística_R/Proyecto/Avance_2/Scrip_2/Documentos_generados/tabla_resumen.xlsx"
)

# 6. Generación de gráficos pertinentes al problema de estudio ----

# Gráfico 1: Distribución de tarimeras por provincia

grafico_provincias <- ggplot(
  datos,
  aes(x = reorder(provincia, -table(provincia)[provincia]))
) +
  geom_bar(fill = "forestgreen") +
  theme_minimal() +
  labs(
    title = "Distribución de tarimeras por provincia",
    x = "Provincia",
    y = "Número de tarimeras"
  )

grafico_provincias

# Gráfico 2: Tipo de certificación

grafico_certificacion <- ggplot(
  datos,
  aes(x = tipo_de_certificacion)
) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribución según tipo de certificación",
    x = "Tipo de certificación",
    y = "Número de empresas"
  )

grafico_certificacion

# Gráfico 3: Tipo de certificación por provincia

grafico_certificacion_provincia <- ggplot(
  datos,
  aes(
    x = provincia,
    fill = tipo_de_certificacion
  )
) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Tipo de certificación según provincia",
    x = "Provincia",
    y = "Número de tarimeras",
    fill = "Tipo de certificación"
  )

grafico_certificacion_provincia

# Gráfico 4: Estado operativo de las tarimeras

grafico_estado <- ggplot(
  datos,
  aes(x = "", fill = estado)
) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Estado operativo de las tarimeras",
    fill = "Estado"
  )

grafico_estado

###End###