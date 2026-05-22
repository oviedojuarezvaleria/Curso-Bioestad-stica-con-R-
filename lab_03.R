######################################
#Laboratorio 3
#ANOVA factorial en procesos de secado
#Versión 1 14/05/2026
######################################


# 1. Cargar paquetes----
library(tidyverse)
library(janitor)
library(readxl)
library(ggplot2)
library(performance)
library(knitr)
library(kableExtra)
library(openxlsx)

# 2. Importar datos----
datos <- read_excel("R_prueba_plataforma/Lab_3_R/secado_melina.xlsx",
                    sheet = "data")

# 3. Exploración inicial----
glimpse(datos)
head(datos)
summary(datos)
names(datos)

# 4. Limpieza y preparación----

#Limpiar nombres----
datos <- datos %>%
  clean_names()

#Limpiar espacios----
datos <- datos %>%
  mutate(across(where(is.character), str_trim))

#Revisar categorias----
count(datos, proceso_produccion)
count(datos, metodo_secado)

#Convertir factores----
datos <- datos %>%
  mutate(
    proceso_produccion = as.factor(proceso_produccion),
    metodo_secado = as.factor(metodo_secado)
  )

glimpse(datos)

# 5. Resumen descriptivo----

resumen <- datos %>%
  group_by(proceso_produccion, metodo_secado) %>%
  summarise(
    n = n(),
    
    calidad_prom = mean(calidad_pct),
    calidad_sd = sd(calidad_pct),
    calidad_se = calidad_sd / sqrt(n),
    
    curvatura_prom = mean(curvatura_mm),
    rajadura_prom = mean(rajadura_cm),
    
    .groups = "drop"
  )

resumen

#Guardar cuadro resumen en Excel

write.xlsx(
  resumen,
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/cuadro_resumen.xlsx",
  overwrite = TRUE
)

# 6. Gráfico de interacción----
grafico_interaccion <- ggplot(
  resumen,
  aes(x = metodo_secado,
      y = calidad_prom,
      color = proceso_produccion,
      group = proceso_produccion)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = calidad_prom - calidad_se,
      ymax = calidad_prom + calidad_se
    ),
    width = 0.1
  ) +
  labs(
    title = "Interacción entre proceso de producción y método de secado",
    x = "Método de secado",
    y = "Calidad (%)",
    color = "Proceso"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

grafico_interaccion

#Guardar gráfico----
ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/grafico_interaccion.png",
  grafico_interaccion,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# 7. Verificación de supuestos----
#Modelo ANOVA factorial----
modelo_aov <- aov(
  calidad_pct ~ proceso_produccion * metodo_secado,
  data = datos
)

#Resumen del modelo----
summary(modelo_aov)

#Normalidad----
check_normality(modelo_aov)

#Homogeneidad----
check_heteroscedasticity(modelo_aov)

# 8. Comparaciones múltiples (Tukey)----

#Tukey
tukey <- TukeyHSD(modelo_aov)

tukey

#Guardar resultados Tukey
tukey_proceso <- as.data.frame(tukey$proceso_produccion)
tukey_secado <- as.data.frame(tukey$metodo_secado)
tukey_interaccion <- as.data.frame(tukey$`proceso_produccion:metodo_secado`)

tukey_interaccion

#Agregar nombres de comparacion
tukey_interaccion <- tukey_interaccion %>%
  rownames_to_column("comparacion")
write.xlsx(
  tukey_interaccion,
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/cuadro_tukey_interaccion.xlsx",
  overwrite = TRUE
)

# 9. Defectos de la madera----

defectos <- datos %>%
  group_by(proceso_produccion, metodo_secado) %>%
  summarise(
    curvatura_presente = sum(presencia_curvatura),
    rajadura_presente = sum(presencia_rajadura),
    
    curvatura_pct = mean(presencia_curvatura) * 100,
    rajadura_pct = mean(presencia_rajadura) * 100,
    
    .groups = "drop"
  )

defectos

#Guardar cuadro defectos
write.xlsx(
  defectos,
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/cuadro_defectos.xlsx",
  overwrite = TRUE
)

# 10. Grafico de defectos----
#Preparar datos para gráficos
defectos_graf <- defectos %>%
  select(
    proceso_produccion,
    metodo_secado,
    curvatura_pct,
    rajadura_pct
  ) %>%
  pivot_longer(
    cols = c(curvatura_pct, rajadura_pct),
    names_to = "defecto",
    values_to = "porcentaje"
  )

#Frecuencia
grafico_dotplot <- ggplot(
  defectos_graf,
  aes(
    x = porcentaje,
    y = interaction(
      proceso_produccion,
      metodo_secado,
      sep = " - "
    ),
    color = defecto
  )
) +
  geom_point(size = 4) +
  labs(
    title = "Frecuencia de defectos por tratamiento",
    x = "Frecuencia (%)",
    y = "Tratamiento",
    color = "Defecto"
  ) +
  scale_color_discrete(
    labels = c("Curvatura", "Rajadura")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

grafico_dotplot

ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/grafico_defectos_dotplot.png",
  plot = grafico_dotplot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Porcentaje
grafico_heatmap <- ggplot(
  defectos_graf,
  aes(
    x = defecto,
    y = interaction(
      proceso_produccion,
      metodo_secado,
      sep = " - "
    ),
    fill = porcentaje
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(porcentaje, 1)),
    size = 4
  ) +
  scale_x_discrete(
    labels = c(
      curvatura_pct = "Curvatura",
      rajadura_pct = "Rajadura"
    )
  ) +
  scale_fill_gradient2(
    low = "steelblue1",
    mid = "khaki1",
    high = "firebrick3",
    midpoint = 30
  ) +
  labs(
    title = "Frecuencia de defectos por tratamiento",
    x = "Defecto",
    y = "Tratamiento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

grafico_heatmap

ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/grafico_defectos_heatmap.png",
  plot = grafico_heatmap,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Comparar
grafico_facetas <- ggplot(
  defectos_graf,
  aes(
    x = interaction(
      proceso_produccion,
      metodo_secado,
      sep = " - "
    ),
    y = porcentaje
  )
) +
  geom_point(size = 4, color = "steelblue") +
  facet_wrap(
    ~defecto,
    labeller = labeller(
      defecto = c(
        curvatura_pct = "Curvatura",
        rajadura_pct = "Rajadura"
      )
    )
  ) +
  labs(
    title = "Frecuencia de defectos por tratamiento",
    x = "Tratamiento",
    y = "Frecuencia (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

grafico_facetas

ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_3_R/grafico_defectos_facetas.png",
  plot = grafico_facetas,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#End##