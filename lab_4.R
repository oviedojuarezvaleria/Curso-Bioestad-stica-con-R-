######################################
#Laboratorio 4
#Análisis multivariado de la respiración del suelo
#Versión 1 29/05/2026
######################################

install.packages("ggdist")

# 1. Cargar paquetes----

library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(factoextra)
library(cluster)
library(ggdist)


# 2. Importar datos----

datos <- read_excel(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/04_respiracion_suelo_bosques.xlsx",
  sheet = "data"
)

# 3. Exploración inicial----

glimpse(datos)
head(datos)
summary(datos)
names(datos)

# 4. Limpieza y preparación----

#Limpiar nombres
datos <- datos %>%
  clean_names()

names(datos)

#Limpiar espacios
datos <- datos %>%
  mutate(
    across(
      where(is.character),
      str_trim
    )
  )

#Revisar categorías
count(datos, land_use_class)

count(datos, site_name)

count(datos, block)

#Estandarizar categorías
datos <- datos %>%
  mutate(
    land_use_class = case_when(
      land_use_class %in% c(
        "Area degradada",
        "DEGRADED_AREA",
        "Degraded",
        "degraded area",
        "área degradada"
      ) ~ "Degradada",
      
      land_use_class %in% c(
        "Bosque secundario",
        "SECONDARY_FOREST",
        "Secondary forest",
        "bosque secundario",
        "secondary forest"
      ) ~ "Bosque secundario",
      
      land_use_class %in% c(
        "Bosque primario",
        "PRIMARY_FOREST",
        "PRIMARY_Forest",
        "PRIMARY FOREST",
        "Primary forest",
        "primary forest",
        "primry forest"
      ) ~ "Bosque primario",
      
      TRUE ~ land_use_class
    )
  )

count(datos, land_use_class)

#Estandarizar nombres de sitio
datos <- datos %>%
  mutate(
    site_name = case_when(
      site_name %in% c(
        "CUENCA_MEDIA",
        "Cuenca media"
      ) ~ "Cuenca Media",
      
      site_name %in% c(
        "Finca norte",
        "finca_NORTE"
      ) ~ "Finca Norte",
      
      site_name %in% c(
        "Reserva Sur",
        "reserva sur"
      ) ~ "Reserva Sur",
      
      site_name %in% c(
        "Sendero Este",
        "sendero este"
      ) ~ "Sendero Este",
      
      TRUE ~ site_name
    )
  )

count(datos, site_name)

#Convertir factores
datos <- datos %>%
  mutate(
    plot_id = as.factor(plot_id),
    
    site_name = as.factor(site_name),
    
    block = as.factor(block),
    
    land_use_class = factor(
      land_use_class,
      levels = c(
        "Degradada",
        "Bosque secundario",
        "Bosque primario"
      )
    ),
    
    high_respiration = as.factor(high_respiration)
  )

#Convertir variables numéricas
datos <- datos %>%
  mutate(
    soil_respiration_umol_m_2_s_1 =
      as.numeric(soil_respiration_umol_m_2_s_1),
    
    soil_moisture_percent =
      as.numeric(soil_moisture_percent),
    
    p_h_suelo =
      as.numeric(p_h_suelo)
  )

glimpse(datos)

#Revisar valores faltantes
colSums(is.na(datos))

#Imputar valores faltantes
datos <- datos %>%
  mutate(
    soil_respiration_umol_m_2_s_1 =
      ifelse(
        is.na(soil_respiration_umol_m_2_s_1),
        mean(
          soil_respiration_umol_m_2_s_1,
          na.rm = TRUE
        ),
        soil_respiration_umol_m_2_s_1
      ),
    
    p_h_suelo =
      ifelse(
        is.na(p_h_suelo),
        mean(
          p_h_suelo,
          na.rm = TRUE
        ),
        p_h_suelo
      )
  )

colSums(is.na(datos))

# 5. Exploración descriptiva de la respiración del suelo----

#Resumen descriptivo
resumen_respiracion <- datos %>%
  group_by(land_use_class) %>%
  summarise(
    n = n(),
    
    respiracion_prom = mean(
      soil_respiration_umol_m_2_s_1
    ),
    
    respiracion_sd = sd(
      soil_respiration_umol_m_2_s_1
    ),
    
    respiracion_se = respiracion_sd / sqrt(n),
    
    .groups = "drop"
  )

resumen_respiracion

#Guardar cuadro resumen
write.xlsx(
  resumen_respiracion,
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/cuadro_resumen_respiracion.xlsx",
  overwrite = TRUE
)

#Gráfico descriptivo

#Boxplot+obser
grafico_box_jitter <- ggplot(
  datos,
  aes(
    x = land_use_class,
    y = soil_respiration_umol_m_2_s_1,
    fill = land_use_class
  )
) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(
    width = 0.15,
    alpha = 0.7,
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Degradada" = "tan3",
      "Bosque secundario" = "darkseagreen3",
      "Bosque primario" = "forestgreen"
    )
  ) +
  labs(
    title = "Respiración del suelo según cobertura forestal",
    x = "Cobertura forestal",
    y = expression(
      paste(
        "Respiración del suelo (",
        mu,
        "mol m"^-2,
        " s"^-1,
        ")"
      )
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "none"
  )

grafico_box_jitter

#Guardar grafico
ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/grafico_boxplot_observaciones.png",
  plot = grafico_box_jitter,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Violín Bloxplot
grafico_violin <- ggplot(
  datos,
  aes(
    x = land_use_class,
    y = soil_respiration_umol_m_2_s_1,
    fill = land_use_class
  )
) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(
    width = 0.15,
    fill = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Degradada" = "tan3",
      "Bosque secundario" = "darkseagreen3",
      "Bosque primario" = "forestgreen"
    )
  ) +
  labs(
    title = "Distribución de la respiración del suelo",
    x = "Cobertura forestal",
    y = expression(
      paste(
        "Respiración del suelo (",
        mu,
        "mol m"^-2,
        " s"^-1,
        ")"
      )
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "none"
  )

grafico_violin

#Guardar grafico
ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/grafico_violin_boxplot.png",
  plot = grafico_violin,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# 6. Análisis de componentes principales----

#Seleccionar variables para PCA
datos_pca <- datos %>%
  select(
    successional_age_yr,
    soil_respiration_umol_m_2_s_1,
    soil_temp_c,
    soil_moisture_percent,
    p_h_suelo,
    organic_matter_percent,
    soil_c_percent,
    soil_n_percent,
    c_n_ratio,
    bulk_density_g_cm3,
    canopy_cover_percent,
    litter_depth_cm,
    litter_mass_mg_ha_1,
    fine_root_biomass_g_m2,
    microbial_biomass_c_mg_kg,
    enzyme_activity_index,
    decomposition_rate_percent,
    basal_area_m2_ha,
    tree_density_ind_ha_1,
    species_richness,
    shannon_index,
    soil_fauna_count
  )

glimpse(datos_pca)

#Ejecutar PCA
pca <- prcomp(
  datos_pca,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

#Importancia de los componentes principales
pca_importancia <- summary(pca)$importance
pca_importancia

pca$rotation

#Cuadro de importancia de los componentes principales

cuadro_pca <- data.frame(
  Componente = colnames(pca$x),
  Desviacion_Estandar = pca_importancia[1, ],
  Proporcion_Varianza = pca_importancia[2, ],
  Varianza_Acumulada = pca_importancia[3, ]
)

head(cuadro_pca)

#Guardar cuadro
write.xlsx(
  cuadro_pca,
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/cuadro_importancia_pca.xlsx",
  rowNames = FALSE
)

#Gráfico de varianza explicada
grafico_scree <- factoextra::fviz_eig(
  pca,
  addlabels = TRUE,
  barfill = "forestgreen",
  barcolor = "white",
  linecolor = "darkgreen"
) +
  labs(
    title = "Varianza explicada por los componentes principales",
    x = "Componentes principales",
    y = "Porcentaje de varianza explicada"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

grafico_scree

#Guardar gráfico

ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/grafico_scree_plot.png",
  plot = grafico_scree,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Biplot del PCA
grafico_biplot <- factoextra::fviz_pca_biplot(
  pca,
  geom.ind = "point",
  habillage = datos$land_use_class,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  col.var = "black",
  repel = TRUE,
  palette = c(
    "tan3",
    "darkseagreen3",
    "forestgreen"
  )
) +
  labs(
    title = "Patrones multivariados de la respiración del suelo y variables ambientales",
    x = paste0(
      "PC1 (",
      round(pca_importancia[2,1] * 100, 1),
      "%)"
    ),
    y = paste0(
      "PC2 (",
      round(pca_importancia[2,2] * 100, 1),
      "%)"
    ),
    color = "Cobertura forestal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "bottom"
  )

grafico_biplot

#Guardar grafico
ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/grafico_biplot.png",
  plot = grafico_biplot,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 7. Análisis de agrupamiento----

#Matriz de distancias

distancias <- dist(
  scale(datos_pca),
  method = "euclidean"
)

distancias

#Agrupamiento jerárquico
cluster_hc <- hclust(
  distancias,
  method = "ward.D2"
)

cluster_hc

#Dendrograma
plot(
  cluster_hc,
  main = "Dendrograma del análisis de agrupamiento jerárquico",
  xlab = "Parcelas",
  ylab = "Distancia euclidiana",
  sub = ""
)

#Visualización de grupos
###Esto fue con ayuda de ChatGPT para poder ver los grupos
plot(
  cluster_hc,
  main = "Dendrograma del análisis de agrupamiento jerárquico",
  xlab = "Parcelas",
  ylab = "Distancia euclidiana",
  sub = ""
)

rect.hclust(
  cluster_hc,
  k = 3,
  border = c(
    "tan3",
    "darkseagreen3",
    "forestgreen"
  )
)

#Asignación de grupos
grupos <- cutree(
  cluster_hc,
  k = 3
)

table(grupos)

#Comparación entre grupos y cobertura forestal
table(
  Grupo = grupos,
  Cobertura = datos$land_use_class
)

#Tabla de contingencia entre grupos y cobertura
tabla_grupos <- table(
  Grupo = grupos,
  Cobertura = datos$land_use_class
)

tabla_grupos

#Guardar tabla de grupos
write.xlsx(
  as.data.frame.matrix(tabla_grupos),
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/tabla_grupos_cobertura.xlsx",
  rowNames = TRUE
)

#Visualización de grupos

grafico_cluster <- factoextra::fviz_cluster(
  list(
    data = scale(datos_pca),
    cluster = grupos
  ),
  geom = "point",
  ellipse.type = "convex",
  palette = c(
    "tan3",
    "darkseagreen3",
    "forestgreen"
  ),
  ggtheme = theme_minimal()
) +
  labs(
    title = "Agrupamiento de parcelas según variables edáficas, biológicas y de vegetación",
    x = "Dimensión 1",
    y = "Dimensión 2",
    color = "Grupo"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.title = element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    )
  )

grafico_cluster

#Guardar gráfico
ggsave(
  "C:/Users/valeo/OneDrive/Escritorio/R/R_prueba_plataforma/Lab_4_R/grafico_cluster.png",
  plot = grafico_cluster,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

###End###