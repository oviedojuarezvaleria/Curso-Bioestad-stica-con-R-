######################################
#Laboratorio 5
#Regresión múltiple y selección de modelos
#Versión 1 30/05/2026
######################################

# 1. Cargar paquetes----
library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(factoextra)
library(cluster)
library(ggdist)
library(GGally)
library(car)

# 2. Importar datos----

datos <- read_excel(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/04_respiracion_suelo_bosques.xlsx",
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
      ),
    
    soil_moisture_percent = ifelse(
      is.na(soil_moisture_percent),
      mean(
        soil_moisture_percent,
        na.rm = TRUE
      ),
      soil_moisture_percent
    )
  )

colSums(is.na(datos))

#Corregir valores imposibles

datos <- datos %>%
  mutate(
    soil_moisture_percent =
      ifelse(
        soil_moisture_percent < 0,
        NA,
        soil_moisture_percent
      )
  )

# 5. Exploración gráfica de predictores----
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

#Gráficos exploratorios
grafico_exploracion <- ggplot(
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
grafico_exploracion

#Guardar gráfico
ggsave(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Figuras/grafico_exploracion_predictores.png",
  plot = grafico_exploracion,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 6. Correaciones----
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

#Matriz de correlaciones de Pearson

matriz_cor <- cor(
  datos_cor,
  method = "pearson"
)

matriz_cor

#Cuadro 1
cuadro_correlaciones <- round(
  matriz_cor,
  3
)

#Cambiar nombres para presentación

nombres_bonitos <- c(
  "Respiración del suelo",
  "Temperatura del suelo",
  "Humedad del suelo",
  "Carbono del suelo",
  "Materia orgánica",
  "Biomasa de raíces finas",
  "Biomasa microbiana",
  "Cobertura de dosel"
)

rownames(cuadro_correlaciones) <- nombres_bonitos
colnames(cuadro_correlaciones) <- nombres_bonitos

cuadro_correlaciones

#Guardar cuadro
write.xlsx(
  as.data.frame(cuadro_correlaciones),
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Tablas/cuadro_1_matriz_correlaciones.xlsx",
  rowNames = TRUE
)

#Cuadro 2. Correlaciones de los predictores con la respiración

cuadro_cor_respiracion <- data.frame(
  Variable = c(
    "Temperatura del suelo (°C)",
    "Humedad del suelo (%)",
    "Carbono del suelo (%)",
    "Materia orgánica (%)",
    "Biomasa de raíces finas (g m⁻²)",
    "Biomasa microbiana (mg kg⁻¹)",
    "Cobertura de dosel (%)"
  ),
  Correlacion = round(
    matriz_cor[
      "soil_respiration_umol_m_2_s_1",
      c(
        "soil_temp_c",
        "soil_moisture_percent",
        "soil_c_percent",
        "organic_matter_percent",
        "fine_root_biomass_g_m2",
        "microbial_biomass_c_mg_kg",
        "canopy_cover_percent"
      )
    ],
    3
  )
)

cuadro_cor_respiracion <- cuadro_cor_respiracion %>%
  arrange(
    desc(abs(Correlacion))
  )

cuadro_cor_respiracion

#Guardar cuadro 2
write.xlsx(
  cuadro_cor_respiracion,
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Tablas/cuadro_2_correlaciones_respiracion.xlsx",
  rowNames = FALSE
)

#Preparar matriz para gráfico
cor_long <- as.data.frame(as.table(matriz_cor))

#Mapa de calor de correlaciones
grafico_correlacion <- ggplot(
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

grafico_correlacion

#Guardar figura 2
ggsave(
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Figuras/figura_2_matriz_correlaciones.png",
  plot = grafico_correlacion,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 7. Evaluación de colinealidad----
#Selección de variables predictoras
datos_vif <- datos %>%
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

#Modelo auxiliar para VIF
modelo_vif <- lm(
  soil_respiration_umol_m_2_s_1 ~
    soil_temp_c +
    soil_moisture_percent +
    soil_c_percent +
    organic_matter_percent +
    fine_root_biomass_g_m2 +
    microbial_biomass_c_mg_kg +
    canopy_cover_percent,
  data = datos_vif
)

#Cálculo del factor de inflación de la varianza
vif(modelo_vif)
#Existe una fuerte colinealidad entre el carbono del suelo y la materia orgánica, por lo que se deberá evaluar cuidadosamente su inclusión conjunta en los modelos de regresión.

#Cuadro 3. Factores de inflación de la varianza (VIF)
cuadro_vif <- data.frame(
  Variable = c(
    "Temperatura del suelo (°C)",
    "Humedad del suelo (%)",
    "Carbono del suelo (%)",
    "Materia orgánica (%)",
    "Biomasa de raíces finas (g m⁻²)",
    "Biomasa microbiana (mg kg⁻¹)",
    "Cobertura de dosel (%)"
  ),
  VIF = round(
    vif(modelo_vif),
    2
  )
)

cuadro_vif <- cuadro_vif %>%
  mutate(
    Interpretacion = case_when(
      VIF < 5 ~ "Baja",
      VIF < 10 ~ "Moderada",
      TRUE ~ "Alta"
    )
  ) %>%
  arrange(
    desc(VIF)
  )

cuadro_vif

#Guardar cuadro 3

write.xlsx(
  cuadro_vif,
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Tablas/cuadro_3_vif.xlsx",
  rowNames = FALSE
)

# 8. Modelos candidatos----
#Modelo 1. Microclima
modelo_1 <- lm(
  soil_respiration_umol_m_2_s_1 ~
    soil_temp_c +
    soil_moisture_percent,
  data = datos
)

summary(modelo_1)

#Modelo 2. Carbono y materia orgánica
modelo_2 <- lm(
  soil_respiration_umol_m_2_s_1 ~
    soil_c_percent +
    organic_matter_percent,
  data = datos
)

summary(modelo_2)

#Modelo 3. Actividad biológica
modelo_3 <- lm(
  soil_respiration_umol_m_2_s_1 ~
    fine_root_biomass_g_m2 +
    microbial_biomass_c_mg_kg,
  data = datos
)

summary(modelo_3)

#Modelo 4. Modelo integrado
modelo_4 <- lm(
  soil_respiration_umol_m_2_s_1 ~
    soil_temp_c +
    soil_moisture_percent +
    organic_matter_percent +
    fine_root_biomass_g_m2 +
    microbial_biomass_c_mg_kg +
    canopy_cover_percent,
  data = datos
)

summary(modelo_4)

#Cuadro 4. Resumen de modelos candidatos
cuadro_modelos <- data.frame(
  Modelo = c(
    "Microclima",
    "Carbono y materia orgánica",
    "Actividad biológica",
    "Integrado"
  ),
  Variables = c(
    "Temperatura + Humedad",
    "Carbono + Materia orgánica",
    "Raíces finas + Biomasa microbiana",
    "Temperatura + Humedad + Materia orgánica + Raíces finas + Biomasa microbiana + Dosel"
  ),
  R2_Ajustado = c(
    round(summary(modelo_1)$adj.r.squared, 3),
    round(summary(modelo_2)$adj.r.squared, 3),
    round(summary(modelo_3)$adj.r.squared, 3),
    round(summary(modelo_4)$adj.r.squared, 3)
  ),
  Error_Estandar_Residual = c(
    round(summary(modelo_1)$sigma, 3),
    round(summary(modelo_2)$sigma, 3),
    round(summary(modelo_3)$sigma, 3),
    round(summary(modelo_4)$sigma, 3)
  )
)

cuadro_modelos

#Guardar cuadro 4
write.xlsx(
  cuadro_modelos,
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Tablas/cuadro_4_resumen_modelos.xlsx",
  rowNames = FALSE
)

# 9. Comparación de modelos----
#Comparación mediante AIC y BIC

aic_modelos <- AIC(
  modelo_1,
  modelo_2,
  modelo_3,
  modelo_4
)

bic_modelos <- BIC(
  modelo_1,
  modelo_2,
  modelo_3,
  modelo_4
)

aic_modelos
bic_modelos

#Cuadro 5. Comparación de modelos candidatos
cuadro_comparacion <- data.frame(
  Modelo = c(
    "Microclima",
    "Carbono y materia orgánica",
    "Actividad biológica",
    "Integrado"
  ),
  AIC = round(aic_modelos$AIC, 2),
  BIC = round(bic_modelos$BIC, 2),
  R2_Ajustado = c(
    round(summary(modelo_1)$adj.r.squared, 3),
    round(summary(modelo_2)$adj.r.squared, 3),
    round(summary(modelo_3)$adj.r.squared, 3),
    round(summary(modelo_4)$adj.r.squared, 3)
  )
)

cuadro_comparacion <- cuadro_comparacion %>%
  arrange(AIC)

cuadro_comparacion

#Guardar cuadro 5
write.xlsx(
  cuadro_comparacion,
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Tablas/cuadro_5_comparacion_modelos.xlsx",
  rowNames = FALSE
)

# 10. Selección del modelo final----
#Modelo seleccionado
modelo_final <- modelo_4
summary(modelo_final)

#Cuadro 6. Coeficientes del modelo final
cuadro_coeficientes <- data.frame(
  Variable = c(
    "Intercepto",
    "Temperatura del suelo (°C)",
    "Humedad del suelo (%)",
    "Materia orgánica (%)",
    "Biomasa de raíces finas (g m⁻²)",
    "Biomasa microbiana (mg kg⁻¹)",
    "Cobertura de dosel (%)"
  ),
  Coeficiente = round(
    coef(modelo_final),
    4
  ),
  Valor_p = round(
    summary(modelo_final)$coefficients[,4],
    4
  )
)

cuadro_coeficientes <- cuadro_coeficientes %>%
  mutate(
    Significancia = case_when(
      Valor_p < 0.05 ~ "Significativo",
      TRUE ~ "No significativo"
    )
  )

cuadro_coeficientes

#Guardar cuadro 6
write.xlsx(
  cuadro_coeficientes,
  "C:/Users/valeo/Desktop/R/R_prueba_plataforma/Lab_5_R/Tablas/cuadro_6_coeficientes_modelo_final.xlsx",
  rowNames = FALSE
)

#Resumen del modelo final
summary(modelo_final)
r2_ajustado <- round(
  summary(modelo_final)$adj.r.squared,
  3
)
r2_ajustado

###End###