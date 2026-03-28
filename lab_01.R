###################################################
#Laboratorio 1
#Versión 1-24/03/2026
###################################################

#Cargar paquete----
library(tidyverse)
library(janitor)
library(readxl)

#Importar datos----
datos_texto <- read.delim("bosque_seco_med1.txt",
                          header=TRUE,
                          sep="\t",
                          fileEncoding = "UTF-16")
datos_excel <- read_excel("bosque_seco_med2.xlsx")

glimpse(datos_texto)
glimpse(datos_excel)
names(datos_texto)
names(datos_excel)

#Exporación inicial----
head(datos_texto)
head(datos_excel)
summary(datos_texto)
summary(datos_excel)
dim(datos_texto)
dim(datos_excel)

#Limpiar nombres de columnas----
datos_ano_1 <- datos_texto%>%
  clean_names()
names(datos_ano_1)
datos_ano_2 <- datos_excel%>%
  clean_names()
names(datos_ano_2)


#Limpiar espacios en variables de texto----
datos_ano_1 <- datos_ano_1%>%
  mutate(across(where(is.character),str_trim))

count(datos_ano_1,parcela,sort=TRUE)
count(datos_ano_1,especie,sort = TRUE)

datos_ano_2 <- datos_ano_2%>%
  mutate(across(where(is.character),str_trim))

count(datos_ano_2,parcela,sort=TRUE)
count(datos_ano_2,species,sort = TRUE)

#Estandarizar----
datos_ano_1 <- datos_ano_1 %>%
  mutate(
    especie=str_to_lower(especie),
  )
datos_ano_2<- datos_ano_2 %>%
  mutate(
    species=str_to_lower(species),
  )
view(datos_ano_1)
view(datos_ano_2)

#Recodificar categorias----
datos_ano_1 <- datos_ano_1%>%
  mutate(
    especie=case_when(
      especie%in% c("Bursera simaruba","bursera simaruba")~"Bursera_simaruba",
      especie%in% c("Cordia alliodora","Cordia.alliodora","cordia alliodora","cordia.alliodora")~"Cordia_alliadora",
      especie%in% c("Guazuma ulmifolia","guazuma ulmifolia")~"Guazuma_ulmifolia",
      TRUE~especie
    )
  )
datos_ano_2 <- datos_ano_2%>%
  mutate(
    species=case_when(
      species%in% c("bursera simaruba","Bursera simaruba","B. simaruba","b. simaruba")~"Bursera_simaruba",
      species%in% c("Cordia alliodora","C. alliodora","cordia alliodora","c. alliodora")~"Cordia_alliadora",
      species%in% c("guazuma ulmifolia","G. ulmifolia","Guazuma ulmifolia","g. ulmifolia")~"Guazuma_ulmifolia",
      TRUE~species
    ),
    parcela=case_when(
      parcela%in% c("P1","parcela_1")~"P1",
      TRUE~parcela
    )
  )
count(datos_ano_1,especie,sort=TRUE)
count(datos_ano_2,species,sort = TRUE)
count(datos_ano_2,parcela,sort = TRUE)

#Separar los datos de arboles
datos_ano_2 <- datos_ano_2 %>%
  separate(arbol, into = c("parcela_temp", "arbol_num"), sep = "-")

names(datos_ano_2)
count(datos_ano_2, parcela, parcela_temp)

#Eliminar columna repetida----
datos_ano_2 <- datos_ano_2 %>%
  select(-parcela_temp)
view(datos_ano_2)

#Estandarizar datos de arbol----
datos_ano_2 <- datos_ano_2 %>%
  mutate(arbol_num = as.numeric(arbol_num))

#Renombrar las columnas de año 2----
datos_ano_2 <- datos_ano_2 %>%
  rename(
    arbol = arbol_num,
    especie = species,
    diametro_cm = dbh_cm
  )
view(datos_ano_2)

#Agregar columna de año----
datos_ano_1 <- datos_ano_1 %>%
  mutate(ano = 1)

datos_ano_2 <- datos_ano_2 %>%
  mutate(ano = 2)

glimpse(datos_ano_1)
glimpse(datos_ano_2)

#Revisar valores imposibles----
datos_ano_1 <- datos_ano_1%>%
  mutate(
    altura_m=if_else(altura_m<0,NA_real_,altura_m),
    diametro_cm=if_else(diametro_cm<=0, NA_real_,diametro_cm)
  )
datos_ano_2 <- datos_ano_2%>%
  mutate(
    altura_m=if_else(altura_m<0,NA_real_,altura_m),
    diametro_cm=if_else(diametro_cm<=0, NA_real_,diametro_cm)
  )
view(datos_ano_1)
view(datos_ano_2)

#Crear variables de mortalidad----
  #0=muerto; 1= vivo
datos_ano_1 <- datos_ano_1 %>%
  mutate(mortalidad = if_else(!is.na(diametro_cm), 1, 0))

datos_ano_2 <- datos_ano_2 %>%
  mutate(mortalidad = if_else(!is.na(diametro_cm), 1, 0))
count(datos_ano_1,mortalidad)
count(datos_ano_2,mortalidad)

view(datos_ano_1)
view(datos_ano_2)

#Unir las tablas----
datos_total <- bind_rows(datos_ano_1, datos_ano_2)

glimpse(datos_total)
names(datos_total)
count(datos_total)
count(datos_total,especie,sort=TRUE)

#####Inicia trabajo con tabla conjunta----

###Promedio y error estandar----
resumen <- datos_total%>%
  group_by(ano,especie)%>%
  summarise(
    n=n(),
    
    prom_diametro=mean(diametro_cm, na.rm=TRUE),
    se_diametro=sd(diametro_cm,na.rm = TRUE)/sqrt(n),
    prom_altura=mean(altura_m, na.rm=TRUE),
    se_altura=sd(altura_m,na.rm = TRUE)/sqrt(n),
    .groups = "drop"
  )

###Grafico de incremento promedio en diametro entre años----
#Incremento por árbol
incremento <- datos_total %>%
  select(ano, especie, parcela, arbol, diametro_cm) %>%
  pivot_wider(names_from = ano, values_from = diametro_cm) %>%
  mutate(incremento = `2` - `1`)%>%
  filter(!is.na(incremento))

#Prom de incremento por especie
incremento_resumen <- incremento %>%
  group_by(especie) %>%
  summarise(
    prom_incremento = mean(incremento),
    se_incremento = sd(incremento) / sqrt(n()),
    .groups = "drop"
  )
incremento_resumen

#Grafico
ggplot(incremento_resumen, aes(x = especie, y = prom_incremento, fill = especie)) +
  geom_col() +
  geom_errorbar(aes(
    ymin = prom_incremento - se_incremento,
    ymax = prom_incremento + se_incremento
  ), width = 0.2) +
  scale_fill_manual(values = c("forestgreen", "magenta", "steelblue")) +
  labs(
    title = "Incremento promedio en diámetro por especie",
    x = "Especie",
    y = "Incremento promedio (cm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###Grafico de cantidad de árboles vivos/ha----
#Contar árboles 
vivos <- datos_total %>%
  filter(mortalidad == 1) %>%
  group_by(ano, especie) %>%
  summarise(
    n_vivos = n(),
    .groups = "drop"
)

#Conventir a ha
vivos <- vivos%>%
  mutate(arboles_ha=n_vivos*20
)

#Grafico
ggplot(vivos, aes(x = especie, y = arboles_ha, fill = factor(ano))) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("darkgreen", "orange"),
    name = "Año",
    labels = c("Año 1", "Año 2")
  ) +
  labs(
    title = "Cantidad de árboles vivos por hectárea",
    x = "Especie",
    y = "Árboles vivos por hectárea"
  ) +
  theme_minimal()

##Grafico de área basal por especie por hectarea----
#Calcular area basal por arbol
datos_total <- datos_total%>%
  mutate(
    area_basal=(pi*(diametro_cm^2))/40000
  )

#Suma por especie
ab_resumen <- datos_total%>%
  filter(mortalidad==1)%>%
  group_by(ano,especie)%>%
  summarise(
    ab_total=sum(area_basal,na.rm=TRUE),
    .groups = "drop"
  )

#Convertir a ha
ab_resumen <- ab_resumen%>%
  mutate(
    ab_ha=ab_total*20
  )

#Grafico
ggplot(ab_resumen, aes(x = especie, y = ab_ha, fill = factor(ano))) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("darkgreen", "orange"),
    name = "Año",
    labels = c("Año 1", "Año 2")
  ) +
  labs(
    title = "Área basal por especie (m²/ha)",
    x = "Especie",
    y = "Área basal (m²/ha)"
  ) +
  theme_minimal()
