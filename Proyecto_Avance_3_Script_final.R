#############################################################
# Bioestadística con R
# Proyecto: Tarimeras certificadas en Costa Rica
# Script Final: Limpieza, exploración y análisis
# Base: BASE_DATOS_HORNOS_COMPLETA_23_02.xlsx
#############################################################

# 1. Cargar paquetes ----

library(readxl)
library(tidyverse)
library(janitor)
library(writexl)

# 2. Importar base de datos ----

datos_raw <- read_excel(
  ("C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Base_original/BASE_DATOS_HORNOS_COMPLETA_23_02.xlsx"),
  sheet = "BASE TOTAL"
)

# 3. Revisar estructura inicial ----

glimpse(datos_raw)
names(datos_raw)
head(datos_raw)
summary(datos_raw)
dim(datos_raw)

# 4. Eliminar fila innecesaria ----

datos_raw <- datos_raw[-1, ]

dim(datos_raw)

# 5. Limpiar nombres de columnas ----

datos <- datos_raw %>%
  clean_names()

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

names(datos)

# 7. Limpiar espacios y valores inconsistentes ----

datos <- datos %>%
  mutate(
    across(where(is.character), str_trim),
    
    across(
      where(is.character),
      ~ ifelse(
        . %in% c("", " ", "N/A", "-", "Sin_informacion"),
        NA,
        .
      )
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

# 9. Revisar categorías principales ----

count(datos, tipo_de_certificacion, sort = TRUE)
count(datos, estado, sort = TRUE)
count(datos, provincia, sort = TRUE)
count(datos, tipo_de_horno, sort = TRUE)

# 10. Convertir variables numéricas ----

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

# 11. Revisar valores imposibles ----

datos %>% filter(cantidad_de_abanicos < 0)
datos %>% filter(capacidad_europea < 0)
datos %>% filter(capacidad_americana < 0)
datos %>% filter(capacidad_metros_cubicos < 0)

# 12. Revisar registros repetidos por empresa ----

repetidos_empresa <- datos %>%
  count(empresa, sort = TRUE) %>%
  filter(n > 1)

repetidos_empresa

# 13. Resumen de valores faltantes ----

datos %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

# 14. Exportar base limpia ----

write_xlsx(
  datos,
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/Base_datos_limpia/datos_limpios_tarimeras.xlsx"
)

#############################################################
# ANÁLISIS EXPLORATORIO
#############################################################

# 15. Exploración inicial ----

glimpse(datos)
summary(datos)
dim(datos)
head(datos)

# 16. Exploración inicial de variables ----

datos %>%
  count(provincia, sort = TRUE)

datos %>%
  count(tipo_de_certificacion, sort = TRUE)

datos %>%
  count(estado, sort = TRUE)

# 17. Estadísticos descriptivos ----

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

# 18. Exportar tabla resumen ----

write_xlsx(
  tabla_resumen,
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/tabla_resumen.xlsx"
)

# 19. Generación de gráficos ----

# Gráfico 1
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

ggsave(
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/grafico_provincias.png",
  plot = grafico_provincias,
  width = 8,
  height = 6,
  dpi = 300
)

# Gráfico 2
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

ggsave(
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/grafico_certificacion_provincia.png",
  plot = grafico_certificacion_provincia,
  width = 8,
  height = 6,
  dpi = 300
)

# Gráfico 3
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

ggsave(
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/grafico_estado_operativo.png",
  plot = grafico_estado,
  width = 8,
  height = 6,
  dpi = 300
)

# 20. Preparación para análisis inferencial ----
datos_inferencial <- datos %>%
  filter(
    !is.na(tipo_de_certificacion)
  ) 

# Verificar dimensiones finales
dim(datos_inferencial)

# Verificar categorías finales
count(datos_inferencial, tipo_de_certificacion)
count(datos_inferencial, estado)

# 21. Exportar base final para análisis inferencial ----
write_xlsx(
  datos_inferencial,
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/Base_datos_limpia/datos_finales_analisis.xlsx"
)

# 22. Tabla de contingencia ----
tabla_certificacion_provincia <- table(
  datos_inferencial$provincia,
  datos_inferencial$tipo_de_certificacion
)

tabla_certificacion_provincia

# Proporciones por provincia
prop.table(
  tabla_certificacion_provincia,
  margin = 1
)

# Exportar tabla de contingencia 

write_xlsx(
  as.data.frame.matrix(tabla_certificacion_provincia),
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/tabla_contingencia_certificacion_provincia.xlsx"
)

# 23. Verificación de supuestos para Chi-cuadrado ----
chi_preliminar <- chisq.test(
  tabla_certificacion_provincia
)

# Frecuencias esperadas
chi_preliminar$expected

# Frecuencia esperada mínima
min(chi_preliminar$expected)

# Porcentaje de celdas con frecuencia esperada < 5
sum(chi_preliminar$expected < 5)

sum(chi_preliminar$expected < 5) /
  length(chi_preliminar$expected) * 100

# 24. Prueba Chi-cuadrado ----
chi_provincia <- chisq.test(
  tabla_certificacion_provincia
)

chi_provincia

# 25. Prueba exacta de Fisher ----
fisher_provincia <- fisher.test(
  tabla_certificacion_provincia
)

fisher_provincia

# 26. Exploración de cantidad de abanicos ----
summary(datos_inferencial$cantidad_de_abanicos)

histograma_abanicos <- ggplot(
  datos_inferencial,
  aes(x = cantidad_de_abanicos)
) +
  geom_histogram(
    bins = 15
  ) +
  theme_minimal() +
  labs(
    title = "Distribución de la cantidad de abanicos",
    x = "Cantidad de abanicos",
    y = "Frecuencia"
  )

histograma_abanicos

resumen_abanicos_general <- summary(
  datos_inferencial$cantidad_de_abanicos
)

resumen_abanicos_general

# Guardar histograma

ggsave(
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/histograma_abanicos.png",
  plot = histograma_abanicos,
  width = 8,
  height = 6,
  dpi = 300
)
# 27. Verificación de normalidad ----
shapiro.test(
  datos_inferencial$cantidad_de_abanicos
)

# 28. Comparación de cantidad de abanicos según tipo de certificación ----
wilcox_abanicos <- wilcox.test(
  cantidad_de_abanicos ~ tipo_de_certificacion,
  data = datos_inferencial,
  exact = FALSE
)

wilcox_abanicos

# Resumen por tipo de certificación 
resumen_abanicos <- datos_inferencial %>%
  group_by(tipo_de_certificacion) %>%
  summarise(
    n = n(),
    mediana_abanicos = median(
      cantidad_de_abanicos,
      na.rm = TRUE
    ),
    Q1 = quantile(
      cantidad_de_abanicos,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      cantidad_de_abanicos,
      0.75,
      na.rm = TRUE
    )
  )

resumen_abanicos

#Guardar resumen
write_xlsx(
  resumen_abanicos,
  "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/resumen_abanicos.xlsx"
)

# 29. Boxplot de cantidad de abanicos según certificación ----
boxplot_abanicos <- ggplot(
  datos_inferencial,
  aes(
    x = tipo_de_certificacion,
    y = cantidad_de_abanicos
  )
) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Cantidad de abanicos según tipo de certificación",
    x = "Tipo de certificación",
    y = "Cantidad de abanicos"
  )

boxplot_abanicos

# Guardar boxplot 

ggsave(
  filename = "C:/Users/valeo/Desktop/9no Semestro/Bioestadística_R/Proyecto/Avance_3/Productos_generados/boxplot_abanicos_certificacion.png",
  plot = boxplot_abanicos,
  width = 8,
  height = 6,
  dpi = 300
)

###########
# END----
