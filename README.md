# Economía Urbana - Taller 2: Aglomeración de Restaurantes en Milán

## Descripción

Este proyecto replica y extiende el análisis de **Leonardi y Moretti (2023)** sobre la aglomeración de restaurantes en Milán, Italia, examinando los efectos de la liberalización de la entrada al sector de restaurantes entre 2004 y 2012. El trabajo aplica técnicas de economía urbana espacial, estimación no paramétrica y tests de localización para estudiar las economías de aglomeración en servicios locales.

## Autores

- **Luis Alejandro Rubiano Guerrero** - 202013482 - [la.rubiano@uniandes.edu.co](mailto:la.rubiano@uniandes.edu.co)
- **Andrés Felipe Rosas Castillo** - 202013471 - [a.rosas@uniandes.edu.co](mailto:a.rosas@uniandes.edu.co)
- **Carlos Andrés Castillo Cabrera** - 202116837 - [ca.castilloc1@uniandes.edu.co](mailto:ca.castilloc1@uniandes.edu.co)

**Universidad de los Andes** - Curso de Economía Urbana (2025)

## Estructura del Repositorio

```
├── README.md                    # Este archivo
├── main (1).tex                 # Documento principal en LaTeX con resultados y análisis
├── Punto 1.R                    # Script de R con todos los análisis
└── outputs/                     # (generados al ejecutar)
    ├── fig1_top_milan_restaurants.png
    ├── map_growth_pc_restaurants.png
    ├── kde_precios_gauss_epan_2004_20121.png
    ├── Epanechnikov_3_anchos.png
    ├── test_DO_top5_2004_2012_Epan.png
    └── test_DO_top5_2004_2012_gauss.png
```

## Requisitos

### Software necesario
- **R** (versión ≥ 4.0)
- **LaTeX** (para compilar el documento)

### Paquetes de R
```r
library(pacman)
p_load(sf, tidyverse, tmap, dplyr, ggplot2)
```

### Datos
El archivo `Taller2_Ejercicio1.Rdata` debe contener:
- `barrios`: objeto `sf` con geometrías de 180 zonas censales de Milán
- `poblacion`: data.frame con población nocturna y diurna por zona
- `restaurants`: data.frame con información de restaurantes (2004 y 2012)

## Instrucciones de Uso

### 1. Preparar el entorno
```r
# Establecer directorio de trabajo
setwd("ruta/a/tu/directorio")

# Cargar datos
load("Taller2_Ejercicio1.Rdata")
```

### 2. Ejecutar el análisis
El script `Punto 1.R` está dividido en tres partes principales:

#### **PARTE 1**: Replicación de mapas espaciales
- Calcula restaurantes per cápita por barrio (2004 y 2012)
- Genera mapas coropléticos con desviación respecto a la media
- Produce mapa de crecimiento porcentual

#### **PARTE 2**: Distribución de precios (Kernel Density Estimation)
- **2.A**: Compara kernels Gaussiano vs Epanechnikov
- **2.B**: Analiza sensibilidad al ancho de banda (h, h/2, 2h)
- Usa regla de Silverman (rule-of-thumb) para bandwidth

#### **PARTE 3**: Test de Duranton & Overman (2005)
- Identifica top 5 barrios por crecimiento
- Implementa test de localización espacial
- Compara con distribución nula mediante 500 simulaciones

### 3. Compilar el documento
```bash
pdflatex main\ (1).tex
bibtex main\ (1)
pdflatex main\ (1).tex
pdflatex main\ (1).tex
```

## Principales Resultados

### Evolución Espacial (2004-2012)
- **2004**: Distribución relativamente uniforme (efecto de regulación de distancias mínimas)
- **2012**: Fuerte polarización espacial post-liberalización
- **Conclusión**: Evidencia de economías de aglomeración auto-reforzadas

### Distribución de Precios
- **Forma**: Unimodal con moda en 30-40 euros
- **Evolución**: Desplazamiento hacia la derecha y cola más pesada en 2012
- **Interpretación**: Concentración alrededor de precio de referencia, consistente con competencia en precios con costos de búsqueda

### Test de Localización
- **Distancias cortas (0-0.36 km)**: Aglomeración significativa
- **Distancias medias (>0.40 km)**: Dispersión relativa
- **Robustez**: Resultados consistentes con kernels Gaussiano y Epanechnikov

## Referencias Principales

- **Leonardi, M., & Moretti, E. (2023)**. The Agglomeration of Urban Amenities: Evidence from Milan Restaurants. *American Economic Review: Insights*, 5(2), 141-157.

- **Duranton, G., & Overman, H. G. (2005)**. Testing for localization using micro-geographic data. *The Review of Economic Studies*, 72(4), 1077-1106.

## Metodología Técnica

### Supuestos clave
- **Población de referencia**: Diurna (`day_pop`), ya que restaurantes atienden principalmente durante el día
- **Restaurante activo**: Si tiene coordenadas O está marcado como ethnic/Michelin/sit-down
- **Bandwidth KDE**: Regla robusta de Silverman: `h = 1.06 * min(σ, IQR/1.34) * n^(-1/5)`

### Transformaciones espaciales
- Sistema de coordenadas: **UTM 32N** (EPSG:32632) para mediciones en metros
- Distancias en kilómetros, rango [0, 1] km para test D&O

## Interpretación Económica

El trabajo documenta cómo la **liberalización regulatoria** genera:
1. **Divergencia espacial**: "Ganadores y perdedores" en dotación de amenidades
2. **Externalidades de demanda**: Restaurantes se benefician de proximidad a otros
3. **Equilibrio con diferenciación**: Precios concentrados pero no uniformes

Este patrón contrasta con economías de aglomeración en sector transable (productividad) y resalta el rol de **externalidades de demanda** en servicios locales.

## Notas Adicionales

- El código incluye **semillas aleatorias** (`set.seed`) para reproducibilidad de simulaciones
- Los mapas usan paletas **OrRd** (niveles) y **Blues** (crecimiento) siguiendo el paper original
- Todos los gráficos se guardan automáticamente en alta resolución (300 dpi)

## Licencia

Este trabajo es material académico de la Universidad de los Andes. Todos los derechos reservados a los autores y la institución.

---

**Última actualización**: 2025  
**Contacto**: Para preguntas sobre el código o metodología, contactar a cualquiera de los autores.
