######################################
# Laboratorio 6
# Visualización de gráficos con ggplot2
# Versión 1 31/05/2026
######################################

# 1. Cargar paquetes----

library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(openxlsx)

# 2. Importar datos----

datos <- read_excel(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_6_R/04_respiracion_suelo_bosques.xlsx",
  sheet = "data"
)

# 3. Limpieza y preparación----

# Limpiar nombres
datos <- datos %>%
  clean_names()

# Limpiar espacios
datos <- datos %>%
  mutate(
    across(
      where(is.character),
      str_trim
    )
  )

# Estandarizar categorías
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

# Convertir factores
datos <- datos %>%
  mutate(
    land_use_class = factor(
      land_use_class,
      levels = c(
        "Degradada",
        "Bosque secundario",
        "Bosque primario"
      )
    )
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

#Corregir valores negativos
datos <- datos %>%
  mutate(
    soil_moisture_percent =
      ifelse(
        soil_moisture_percent < 0,
        NA,
        soil_moisture_percent
      )
  )

# 4. Figura 1 original----
#Selección de variables candidatas para el modelado
datos_exploracion <- datos %>%
  select(
    soil_respiration_umol_m_2_s_1,
    land_use_class,
    soil_temp_c,
    soil_moisture_percent,
    soil_c_percent,
    organic_matter_percent,
    fine_root_biomass_g_m2,
    microbial_biomass_c_mg_kg,
    canopy_cover_percent
  )

#Conversión a formato largo
datos_largos <- datos_exploracion %>%
  pivot_longer(
    cols = c(
      soil_temp_c,
      soil_moisture_percent,
      soil_c_percent,
      organic_matter_percent,
      fine_root_biomass_g_m2,
      microbial_biomass_c_mg_kg,
      canopy_cover_percent
    ),
    names_to = "predictor",
    values_to = "valor"
  )

#Gráfico original
grafico_exploracion_original <- ggplot(
  datos_largos,
  aes(
    x = valor,
    y = soil_respiration_umol_m_2_s_1,
    color = land_use_class
  )
) +
  geom_point(
    alpha = 0.8,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  facet_wrap(
    ~ predictor,
    scales = "free_x",
    ncol = 3,
    labeller = labeller(
      predictor = c(
        soil_temp_c = "Temperatura del suelo (°C)",
        soil_moisture_percent = "Humedad del suelo (%)",
        soil_c_percent = "Carbono del suelo (%)",
        organic_matter_percent = "Materia orgánica (%)",
        fine_root_biomass_g_m2 = "Biomasa de raíces finas (g m⁻²)",
        microbial_biomass_c_mg_kg = "Biomasa microbiana (mg kg⁻¹)",
        canopy_cover_percent = "Cobertura de dosel (%)"
      )
    )
  ) +
  scale_color_manual(
    values = c(
      "Degradada" = "firebrick3",
      "Bosque secundario" = "goldenrod3",
      "Bosque primario" = "forestgreen"
    )
  ) +
  labs(
    title = "Relación entre la respiración del suelo y variables predictoras",
    x = NULL,
    y = expression(
      paste(
        "Respiración del suelo (",
        mu,
        "mol m"^-2,
        " s"^-1,
        ")"
      )
    ),
    color = "Cobertura forestal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "bottom",
    strip.text = element_text(
      face = "bold"
    )
  )

grafico_exploracion_original

#Guardar figura 1 original
ggsave(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_6_R/Figuras/original/figura_1_original.png",
  plot = grafico_exploracion_original,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 5. Figura 1 mejorada----
#Gráfico mejorado
grafico_exploracion_mejorado <- ggplot(
  datos_largos,
  aes(
    x = valor,
    y = soil_respiration_umol_m_2_s_1
  )
) +
  
  #Tendencia general
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "gray25",
    linewidth = 1.3
  ) +
  
  #Puntos
  geom_point(
    aes(
      fill = land_use_class
    ),
    shape = 21,
    color = "black",
    size = 3.2,
    alpha = 0.85,
    stroke = 0.4
  ) +
  
  #Tendencias por cobertura
  geom_smooth(
    aes(
      color = land_use_class
    ),
    method = "lm",
    se = FALSE,
    linewidth = 1.1
  ) +
  
  facet_wrap(
    ~ predictor,
    scales = "free_x",
    ncol = 3,
    labeller = labeller(
      predictor = c(
        soil_temp_c = "Temperatura del suelo (°C)",
        soil_moisture_percent = "Humedad del suelo (%)",
        soil_c_percent = "Carbono del suelo (%)",
        organic_matter_percent = "Materia orgánica (%)",
        fine_root_biomass_g_m2 = "Biomasa de raíces finas (g m⁻²)",
        microbial_biomass_c_mg_kg = "Biomasa microbiana (mg kg⁻¹)",
        canopy_cover_percent = "Cobertura de dosel (%)"
      )
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Degradada" = "firebrick2",
      "Bosque secundario" = "goldenrod2",
      "Bosque primario" = "forestgreen"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Degradada" = "firebrick4",
      "Bosque secundario" = "goldenrod4",
      "Bosque primario" = "darkgreen"
    )
  ) +
  
  labs(
    title = "Relación entre la respiración del suelo y variables ambientales",
    subtitle = "Comparación entre diferentes coberturas forestales",
    x = NULL,
    y = expression(
      paste(
        "Respiración del suelo (",
        mu,
        "mol m"^-2,
        " s"^-1,
        ")"
      )
    ),
    fill = "Cobertura forestal",
    color = "Cobertura forestal"
  ) +
  
  theme_bw() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    axis.title = element_text(
      size = 11
    )
  )

grafico_exploracion_mejorado

#Guardar figura 1 mejorada
ggsave(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_6_R/Figuras/mejorada/figura_1_mejorada.png",
  plot = grafico_exploracion_mejorado,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 6. Figura 2 original----
#Selección de variables para correlación
datos_cor <- datos %>%
  select(
    soil_respiration_umol_m_2_s_1,
    soil_temp_c,
    soil_moisture_percent,
    soil_c_percent,
    organic_matter_percent,
    fine_root_biomass_g_m2,
    microbial_biomass_c_mg_kg,
    canopy_cover_percent
  )

#Matriz de correlaciones
matriz_cor <- cor(
  datos_cor,
  method = "pearson",
  use = "complete.obs"
)

#Preparar matriz para gráfico
cor_long <- as.data.frame(
  as.table(matriz_cor)
)

#Mapa de calor original
grafico_correlacion_original <- ggplot(
  cor_long,
  aes(
    x = Var1,
    y = Var2,
    fill = Freq
  )
) +
  geom_tile(
    color = "white"
  ) +
  geom_text(
    aes(
      label = round(Freq, 2)
    ),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "firebrick3",
    mid = "white",
    high = "forestgreen",
    midpoint = 0,
    limits = c(-1, 1),
    name = "r de Pearson"
  ) +
  scale_x_discrete(
    labels = c(
      soil_respiration_umol_m_2_s_1 = "Respiración",
      soil_temp_c = "Temperatura",
      soil_moisture_percent = "Humedad",
      soil_c_percent = "Carbono",
      organic_matter_percent = "M. orgánica",
      fine_root_biomass_g_m2 = "Raíces finas",
      microbial_biomass_c_mg_kg = "Biomasa microbiana",
      canopy_cover_percent = "Dosel"
    )
  ) +
  scale_y_discrete(
    labels = c(
      soil_respiration_umol_m_2_s_1 = "Respiración",
      soil_temp_c = "Temperatura",
      soil_moisture_percent = "Humedad",
      soil_c_percent = "Carbono",
      organic_matter_percent = "M. orgánica",
      fine_root_biomass_g_m2 = "Raíces finas",
      microbial_biomass_c_mg_kg = "Biomasa microbiana",
      canopy_cover_percent = "Dosel"
    )
  ) +
  labs(
    title = "Correlaciones de Pearson entre la respiración del suelo y variables predictoras",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

grafico_correlacion_original

#Guardar figura 2 original----
ggsave(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_6_R/Figuras/original/figura_2_original.png",
  plot = grafico_correlacion_original,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 7. Figura 2 mejorada----
#Mapa de calor mejorado
grafico_correlacion_mejorado <- ggplot(
  cor_long,
  aes(
    x = Var1,
    y = Var2,
    fill = Freq
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.8
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.2f", Freq),
      color = abs(Freq) > 0.70
    ),
    size = 4,
    fontface = "bold"
  ) +
  
  scale_color_manual(
    values = c(
      "TRUE" = "white",
      "FALSE" = "black"
    ),
    guide = "none"
  ) +
  
  scale_fill_gradient2(
    low = "sienna4",
    mid = "white",
    high = "darkgreen",
    midpoint = 0,
    limits = c(-1, 1),
    name = "r de Pearson"
  ) +
  
  scale_x_discrete(
    labels = c(
      soil_respiration_umol_m_2_s_1 = "Respiración",
      soil_temp_c = "Temperatura",
      soil_moisture_percent = "Humedad",
      soil_c_percent = "Carbono",
      organic_matter_percent = "M. orgánica",
      fine_root_biomass_g_m2 = "Raíces finas",
      microbial_biomass_c_mg_kg = "Biomasa microbiana",
      canopy_cover_percent = "Dosel"
    )
  ) +
  
  scale_y_discrete(
    labels = c(
      soil_respiration_umol_m_2_s_1 = "Respiración",
      soil_temp_c = "Temperatura",
      soil_moisture_percent = "Humedad",
      soil_c_percent = "Carbono",
      organic_matter_percent = "M. orgánica",
      fine_root_biomass_g_m2 = "Raíces finas",
      microbial_biomass_c_mg_kg = "Biomasa microbiana",
      canopy_cover_percent = "Dosel"
    )
  ) +
  
  labs(
    title = "Matriz de correlaciones de Pearson",
    subtitle = "Respiración del suelo y variables predictoras",
    x = NULL,
    y = NULL
  ) +
  
  theme_bw() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11
    ),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    )
  )

grafico_correlacion_mejorado

#Guardar figura 2 mejorada
ggsave(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_6_R/Figuras/mejorada/figura_2_mejorada.png",
  plot = grafico_correlacion_mejorado,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

###End###