######################################
#Laboratorio 2
#Crecimiento semanal en vivero 
######################################

#instalar paquete(evitar errores del lab pasado)----
install.packages("kableExtra")
install.packages("openxlsx")

#Cargar paquetes----
library(tidyverse)
library(janitor)
library(readxl)
library(naniar)
library(VIM)
library(mice)
library(knitr)
library(kableExtra)
library(openxlsx)

#Reiniciar dispositivo grafico----
try(graphics.off(), silent = TRUE)
##Chat me recomendo hacer esto porque no me corrian los gráficos

#Carpeta de salida----
ruta_salida <- "R_prueba_plataforma/Lab_2_R"

if(!dir.exists(ruta_salida)){
  dir.create(ruta_salida)
}

#Importar datos----
datos <- read_excel("R_prueba_plataforma/Lab_2_R/datos_vivero.xlsx", 
                    sheet = "Data")
glimpse(datos)
names(datos)

#Exploración inicial----
head(datos)
summary(datos)
dim(datos)

#Limpiar nombres de columnas----
datos <- datos%>%
  clean_names()
names(datos)

#Limpiar espacios en variables de texto----
datos <- datos%>%
  mutate(across(where(is.character),str_trim))
count(datos, tratamiento, sort=TRUE)

#Renombre de categorias----
datos <- datos%>%
  mutate(
    tratamiento=case_when(
      tratamiento %in% c("control")~"Control",
      TRUE~tratamiento)
  )
count(datos, tratamiento, sort = TRUE)

#Revisar valores imposibles----
datos <- datos%>%
  mutate(
    altura_cm=if_else(altura_cm<0,NA_real_,altura_cm),
    diametro_base_mm=if_else(diametro_base_mm<=0,NA_real_,diametro_base_mm)
  )
View(datos)

###Valores faltantes----
#Ver los valores faltantes
conteo_na <- sapply(datos, function(x)sum(is.na(x)))
porc_na <- round(100*sapply(datos, function(x)mean(is.na(x))),2)

resumen_na <- data.frame(
  variable=names(conteo_na),
  n_faltantes=conteo_na,
  porcentaje=porc_na
)
print(resumen_na)

#Tabla academica
kable(resumen_na, digits = 2) %>%
  kable_styling(full_width = FALSE)

#Grafico de valores faltantes
na_plot <- datos %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_faltantes") %>%
  filter(n_faltantes > 0)

grafico_na <- ggplot(na_plot, aes(x = reorder(variable, -n_faltantes),
                                  y = n_faltantes)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_faltantes),
            vjust = -0.5, size = 4) +
  labs(
    title = "Número de valores faltantes por variable",
    x = "Variable",
    y = "Número de valores faltantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grafico_na

ggsave(
  filename = file.path(ruta_salida, "grafico_valores_faltantes.png"),
  plot = grafico_na,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Patron
md.pattern(datos)

###Imputación de datos----
#Seleccionar variables para imputar
vars_imputar <- datos%>%
  select(altura_cm,diametro_base_mm)

summary(vars_imputar)

#Media
datos_media <- vars_imputar
for(i in seq_along(datos_media)){
  if(is.numeric(datos_media[[i]])){
    datos_media[[i]][is.na(datos_media[[i]])] <- 
      mean(datos_media[[i]],na.rm=TRUE)}
}
summary(datos_media)

#Imputación con PMM
metodos <- rep("pmm",ncol(vars_imputar))
names(metodos) <- names(vars_imputar)

pred <- make.predictorMatrix(vars_imputar)
diag(pred) <- 0

set.seed(123)

imp <- mice(
  vars_imputar,
  m=5,
  method = metodos,
  predictorMatrix=pred,
  maxit=10
)

#Extraer base imputada
datos_pmm <- complete(imp,1)

#Reintegrar a la base
datos_final <- bind_cols(
  datos%>%select(tratamiento,semana,planta),
  datos_pmm
)

#Verificar faltantes finales
faltantes_finales <- sapply(datos_final,function(x)sum(is.na(x)))
print(faltantes_finales)

###Inicio de resultados requeridos
#Cuadro resumen----
resumen <- datos_final%>%
  group_by(tratamiento, semana)%>%
  summarise(
    n=n(),
    
    prom_altura=mean(altura_cm),
    sd_altura=sd(altura_cm),
    se_altura=sd_altura/sqrt(n),
    
    prom_diametro=mean(diametro_base_mm),
    sd_diametro=sd(diametro_base_mm),
    se_diametro=sd_diametro/sqrt(n),
    
    .groups = "drop"
    )

#Tabla academica
kable(resumen, digits = 2) %>%
  kable_styling(full_width = FALSE)

#Grafico de altura----
grafico_altura <- ggplot(resumen, aes(x = semana, y = prom_altura, color = tratamiento)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(
    ymin = prom_altura - se_altura,
    ymax = prom_altura + se_altura
  ), width = 0.2) +
  scale_color_manual(values = c(
    "Control" = "darkgreen",
    "Nitrogeno" = "orange",
    "Nitrogeno_Potasio" = "steelblue"
  )) +
  labs(
    title = "Trayectoria promedio en altura",
    x = "Semana",
    y = "Altura (cm)"
  ) +
  theme_minimal()
theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
)

#Mostrar
grafico_altura

#Guardar
ggsave(
  filename = file.path(ruta_salida, "grafico_altura.png"),
  plot = grafico_altura,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Grafico de diametro----
grafico_diametro <- ggplot(resumen, aes(x = semana, y = prom_diametro, color = tratamiento)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(
    ymin = prom_diametro - se_diametro,
    ymax = prom_diametro + se_diametro
  ), width = 0.2) +
  scale_color_manual(values = c(
    "Control" = "darkgreen",
    "Nitrogeno" = "orange",
    "Nitrogeno_Potasio" = "steelblue"
  )) +
  labs(
    title = "Trayectoria promedio en diametro basal",
    x = "Semana",
    y = "Diametro (mm)"
  ) +
  theme_minimal()
theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
)

#Mostrar
grafico_diametro

#Guardar
ggsave(
  filename = file.path(ruta_salida, "grafico_diametro.png"),
  plot = grafico_diametro,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

#Comparación antes vs despues----
comparacion <- data.frame(
  variable = c("altura_cm", "diametro_base_mm"),
  
  na_antes = c(
    sum(is.na(datos$altura_cm)),
    sum(is.na(datos$diametro_base_mm))
  ),
  
  na_despues = c(
    sum(is.na(datos_final$altura_cm)),
    sum(is.na(datos_final$diametro_base_mm))
  ),
  
  prom_antes = c(
    mean(datos$altura_cm, na.rm = TRUE),
    mean(datos$diametro_base_mm, na.rm = TRUE)
  ),
  
  prom_despues = c(
    mean(datos_final$altura_cm),
    mean(datos_final$diametro_base_mm)
  )
)

#Redondear resultados----
comparacion <- comparacion %>%
  mutate(
    na_antes = round(na_antes, 2),
    na_despues = round(na_despues, 2),
    prom_antes = round(prom_antes, 2),
    prom_despues = round(prom_despues, 2)
  )
comparacion

#Tabla academica
kable(comparacion, digits = 2) %>%
  kable_styling(full_width = FALSE)

#Para no perder el archivo----
message("Graficos guardados correctamente en: ", ruta_salida)

#Guardar cuadros generados----
##Revisar porque chat me dijo como generar esto***
wb <- createWorkbook()

addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", resumen)

addWorksheet(wb, "Comparacion")
writeData(wb, "Comparacion", comparacion)

addWorksheet(wb, "NA")
writeData(wb, "NA", resumen_na)

saveWorkbook(wb, file.path(ruta_salida, "resultados_lab2.xlsx"), overwrite = TRUE)
