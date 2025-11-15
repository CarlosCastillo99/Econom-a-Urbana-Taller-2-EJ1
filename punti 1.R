setwd("C:\\Users\\pc\\Downloads\\taller 2 urbana")
load("Taller2_Ejercicio1.Rdata")
ls()                        # para ver qué objetos trae
suppressMessages({
  library(sf)
  library(tidyverse)
})
cat("========== BARRIOS ==========\n")
cat("clase:\n"); print(class(barrios))
cat("dimensiones (filas, columnas):\n"); print(dim(barrios))
cat("nombres de variables:\n"); print(names(barrios))
cat("\n--- head(barrios) ---\n")
print(head(barrios, 5))
# Si es un objeto sf, miramos geometría y CRS
if ("sf" %in% class(barrios)) {
  cat("\n--- tipo de geometría (primeras filas) ---\n")
  print(st_geometry_type(barrios[1:5, ]))
  cat("\n--- CRS ---\n")
  print(st_crs(barrios))
}
cat("\n========== POBLACION ==========\n")
cat("clase:\n"); print(class(poblacion))
cat("dimensiones (filas, columnas):\n"); print(dim(poblacion))
cat("nombres de variables:\n"); print(names(poblacion))
cat("\n--- glimpse(poblacion) ---\n")
print(dplyr::glimpse(poblacion))
cat("\n--- resumen de las variables numéricas de poblacion ---\n")
print(summary(dplyr::select_if(poblacion, is.numeric)))
cat("\n========== RESTAURANTS ==========\n")
cat("clase:\n"); print(class(restaurants))
cat("dimensiones (filas, columnas):\n"); print(dim(restaurants))
cat("nombres de variables:\n"); print(names(restaurants))
cat("\n--- glimpse(restaurants) ---\n")
print(dplyr::glimpse(restaurants))
cat("\n--- resumen de las variables numéricas de restaurants ---\n")
print(summary(dplyr::select_if(restaurants, is.numeric)))




rm(list = ls())

# Paquetes mínimos necesarios
suppressMessages({
  library(pacman)
  p_load(sf, tidyverse, tmap)
})


# ruta
setwd("C:/Users/pc/Downloads/taller 2 urbana")

load("Taller2_Ejercicio1.Rdata")
# Objetos que vienen del .Rdata son barrios, poblacion, restaurants


library(dplyr)
library(sf)
library(tmap)

## 1. Preparar bases ----------------------------------------

# barrios: sf con geometría de las 180 zonas
# poblacion: data.frame con zona180, nite_pop, day_pop
# restaurants: data.frame con info de restaurantes

# llave común
barrios_sf <- barrios |> 
  rename(zona180 = ZONA180)

## 2. Contar restaurantes por barrio y año -------------------

# Un restaurante existe en un año si:
# - tiene coordenadas ese año, o
# - está marcado como ethnic / michelin / sitdown ese año

rest_por_barrio <- restaurants |>
  mutate(
    activo_2004 = (!is.na(lat2004) & !is.na(long2004)) |
      ethnic2004 == 1 | michelin2004 == 1 | sitdown2004 == 1,
    activo_2012 = (!is.na(lat2012) & !is.na(long2012)) |
      ethnic2012 == 1 | michelin2012 == 1 | sitdown2012 == 1,
    rest_2004 = as.integer(activo_2004),
    rest_2012 = as.integer(activo_2012)
  ) |>
  group_by(zona180) |>
  summarise(
    n_rest_2004 = sum(rest_2004, na.rm = TRUE),
    n_rest_2012 = sum(rest_2012, na.rm = TRUE),
    .groups = "drop"
  )

## 3. Unir con población y calcular tasas per cápita ---------

# usamos población diurna (day_pop), como en el paper
milan <- barrios_sf |>
  left_join(poblacion, by = "zona180") |>
  left_join(rest_por_barrio, by = "zona180")

# restaurantes por 1.000 habitantes diurnos
escala <- 1000

milan <- milan |>
  mutate(
    pc_rest_2004 = (n_rest_2004 / day_pop) * escala,
    pc_rest_2012 = (n_rest_2012 / day_pop) * escala
  )

# crecimiento porcentual de restaurantes per cápita
milan <- milan |>
  mutate(
    growth_pc = (pc_rest_2012 - pc_rest_2004) / pc_rest_2004
  )

## 4. Cortes de leyenda para que se parezcan al paper --------

# Per capita (por 1.000 hab.) – mismo rango para 2004 y 2012
breaks_pc  <- c(0, 2, 4, 6, 8, 10, 12, 14)
labels_pc  <- c("0–2", "2–4", "4–6", "6–8",
                "8–10", "10–12", "12–14")

# Crecimiento porcentual
breaks_g   <- c(-0.6, -0.3, -0.2, 0, 0.2, 0.3, 0.6)
labels_g   <- c("−0.6–−0.3", "−0.3–−0.2", "−0.2–0",
                "0–0.2", "0.2–0.3", "0.3–0.6")

tmap_mode("plot")

## 5. Mapas per cápita 2004 y 2012 ---------------------------

map_pc_2004 <- tm_shape(milan) +
  tm_polygons(
    col      = "pc_rest_2004",
    palette  = "OrRd",
    breaks   = breaks_pc,
    labels   = labels_pc,
    title    = "",
    border.col = "grey40",
    lwd      = 0.5,
    colorNA  = "grey90",
    showNA   = FALSE
  ) +
  tm_layout(
    main.title   = "Per capita number of restaurants in 2004",
    main.title.position = "center",
    legend.show = FALSE,
    frame       = FALSE
  )

print (map_pc_2004)


map_pc_2012 <- tm_shape(milan) +
  tm_polygons(
    col      = "pc_rest_2012",
    palette  = "OrRd",
    breaks   = breaks_pc,
    labels   = labels_pc,
    title    = "",
    border.col = "grey40",
    lwd      = 0.5,
    colorNA  = "grey90",
    showNA   = FALSE
  ) +
  tm_layout(
    main.title   = "Per capita number of restaurants in 2012",
    main.title.position = "center",
    legend.show = FALSE,
    frame       = FALSE
  )
print (map_pc_2012)
# leyenda común para los dos mapas de arriba
leg_pc <- tm_shape(milan) +
  tm_polygons(
    col      = "pc_rest_2012",
    palette  = "OrRd",
    breaks   = breaks_pc,
    labels   = labels_pc,
    title    = "",
    colorNA  = "grey90",
    showNA   = FALSE
  ) +
  tm_layout(
    legend.only    = TRUE,
    legend.text.size = 0.8
  )

## 6. Mapa de crecimiento porcentual -------------------------

map_growth <- tm_shape(milan) +
  tm_polygons(
    col      = "growth_pc",
    palette  = "Blues",
    breaks   = breaks_g,
    labels   = labels_g,
    title    = "",
    border.col = "grey40",
    lwd      = 0.5,
    colorNA  = "grey90",
    showNA   = FALSE
  ) +
  tm_layout(
    main.title   = "Percent growth in the number of per capita restaurants",
    main.title.position = "center",
    legend.outside      = TRUE,
    legend.text.size    = 0.8,
    frame              = FALSE
  )
print(map_growth)
## 7. Mostrar la figura “tipo paper” -------------------------

# fila de arriba: 2004 – 2012 – leyenda
tmap_arrange(
  map_pc_2004,
  map_pc_2012,
  leg_pc,
  ncol   = 3,
  widths = c(1, 1, 0.4)
)

# fila de abajo: crecimiento porcentual
map_growth



