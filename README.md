README

Proyecto de curso:
Análisis de la distribución geográfica y factores asociados a la certificación de tarimeras en Costa Rica

Descripción:
Este proyecto corresponde al curso de Bioestadística con R y tiene como objetivo analizar la distribución geográfica de las tarimeras certificadas en Costa Rica, así como evaluar posibles asociaciones entre variables geográficas y operativas relacionadas con el tipo de certificación.

Estructura del proyecto:
- Script_01_limpieza_control_calidad.R
  Incluye importación, limpieza, transformación y control de calidad de la base de datos.
- Script_02_analisis_exploratorio.R
  Incluye análisis descriptivos, exploración inicial y generación de gráficos.
- Script_Final.R
 Incluye importación de datos, limpieza, control de calidad, análisis exploratorio, análisis inferencial y generación de productos finales.
- datos_limpios_tarimeras.xlsx
  Base de datos procesada generada a partir del Script 01.
- tabla_resumen.xlsx
  Tabla de estadísticos descriptivos generada en el Script 02.
- BASE_DATOS_HORNOS_COMPLETA_23_02.xlsx
 Base de datos original utilizada en el proyecto.
- datos_limpios_tarimeras.xlsx
 Base de datos procesada obtenida después de la limpieza y control de calidad.
- datos_finales_analisis.xlsx
 Base de datos final utilizada para los análisis inferenciales.
- tabla_resumen.xlsx
 Tabla de estadísticos descriptivos de las principales variables numéricas.
- tabla_contingencia_certificacion_provincia.xlsx
 Tabla de contingencia utilizada para evaluar la asociación entre provincia y tipo de certificación.
- resumen_abanicos.xlsx
Resumen descriptivo de la cantidad de abanicos según tipo de certificación.

Productos gráficos generados:
- grafico_provincias.png
Distribución de tarimeras certificadas por provincia.
- grafico_certificacion_provincia.png
Distribución del tipo de certificación según provincia.
- grafico_estado_operativo.png
Distribución del estado operativo de las tarimeras.
- histograma_abanicos.png
Distribución de la cantidad de abanicos.
- boxplot_abanicos_certificacion.png
Comparación de la cantidad de abanicos según tipo de certificación.

Análisis realizados:
1. Limpieza y control de calidad de datos.
2. Exploración descriptiva de variables.
3. Estadísticos descriptivos.
4. Análisis de asociación entre provincia y tipo de certificación mediante prueba Chi-cuadrado.
5. Verificación de supuestos mediante frecuencias esperadas.
6. Prueba exacta de Fisher como análisis complementario.
7. Evaluación de normalidad mediante prueba de Shapiro-Wilk.
8. Comparación de la cantidad de abanicos según tipo de certificación mediante prueba de Wilcoxon.

Paquetes utilizados:
- tidyverse
- readxl
- janitor
- writexl
- ggplot2

Flujo de trabajo:
1. Ejecutar Script_01_limpieza_control_calidad.R
2. Generar base limpia
3. Ejecutar Script_02_analisis_exploratorio.R
4. Generar tablas y gráficos
5. Ejecutar Script_final
   - Importar la base de datos original.
   - Realizar limpieza y control de calidad.
   - Generar la base de datos procesada.
   - Realizar análisis exploratorio y descriptivo.
   - Generar tablas y gráficos.
   - Construir la base final para análisis inferencial.
   - Ejecutar pruebas estadísticas.
   - Exportar resultados y productos finales.

Autora:
Valeria Oviedo Juárez

Curso:
Bioestadística con R
