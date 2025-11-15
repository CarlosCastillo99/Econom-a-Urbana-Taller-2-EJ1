rm(list = ls())

# Paquetes mínimos necesarios
suppressMessages({
  library(pacman)
  p_load(sf, tidyverse, tmap)
})

# ruta
setwd("C:/Users/pc/Downloads/taller 2 urbana")

load("Taller2_Ejercicio1.Rdata")

library(dplyr)
library(sf)
library(tmap)
#Luego de revisar cada variable, comenzamos a limpiar y dejar todo listo para después plotear.
# 1. Preparar bases 
# barrios: sf con geometría de las 180 zonas
# poblacion: data.frame con zona180, nite_pop, day_pop
# restaurants: data.frame con info de restaurantes

# llave común
barrios_sf <- barrios |> 
  rename(zona180 = ZONA180)

# 2. Contar restaurantes por barrio y año 

# Un restaurante existe en un año si:
# Tiene coordenadas ese año, o
# está marcado como ethnic / michelin / sitdown ese año

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

#3. Unir con población y calcular tasas per cápita

# usamos población diurna (day_pop), como en el paper
milan <- barrios_sf |>
  left_join(poblacion, by = "zona180") |>
  left_join(rest_por_barrio, by = "zona180")

#revisemos si todo está bien revisando las salidas en consola:

cat("clase milan:\n"); print(class(milan))

cat("\n dim(milan):\n"); print(dim(milan))

cat("\n nombres de variables:\n"); print(names(milan))

cat("\n--- primeras filas (sin geometría) ---\n")
print(head(sf::st_drop_geometry(milan), 10))

cat("\n ¿NA en day_pop?: ", any(is.na(milan$day_pop)), "\n")
cat(" ¿NA en n_rest_2004?: ", any(is.na(milan$n_rest_2004)), "\n")
cat(" ¿NA en n_rest_2012?: ", any(is.na(milan$n_rest_2012)), "\n")

#Ahora,
escala <- 1000  # restaurantes por 1.000 habitantes diurnos

milan <- milan |>
  mutate(
    pc_rest_2004 = n_rest_2004 / day_pop * escala,
    pc_rest_2012 = n_rest_2012 / day_pop * escala
  )

# promedio de la ciudad en cada año
media_2004 <- mean(milan$pc_rest_2004, na.rm = TRUE)
media_2012 <- mean(milan$pc_rest_2012, na.rm = TRUE)

milan <- milan |>
  mutate(
    # desviación respecto al promedio de Milán (esto es lo que permite tener valores negativos)
    diff_city_2004 = pc_rest_2004 - media_2004,
    diff_city_2012 = pc_rest_2012 - media_2012,
    # crecimiento porcentual de restaurantes per cápita
    growth_pc      = (pc_rest_2012 - pc_rest_2004) / pc_rest_2004
  )
#revisemos si todo está bien:
cat("\n--- summary diff_city_2004 ---\n")
print(summary(milan$diff_city_2004))

cat("\n--- summary diff_city_2012 ---\n")
print(summary(milan$diff_city_2012))

cat("\n--- summary growth_pc ---\n")
print(summary(milan$growth_pc))


# 4. Definir cortes y etiquetas de leyenda

# Diferencia vs promedio de la ciudad (restaurantes por 1.000 hab. diurnos)
brks_diff  <- seq(-4, 14, by = 2)
lbls_diff  <- c("-4–-2", "-2–0", "0–2", "2–4", "4–6",
                "6–8", "8–10", "10–12", "12–14")

# Para el crecimiento porcentual (recortamos a [-0.6, 0.6])
brks_grow  <- c(-0.6, -0.3, -0.2, 0, 0.2, 0.3, 0.6)
lbls_grow  <- c("-0.6–-0.3", "-0.3–-0.2", "-0.2–0",
                "0–0.2", "0.2–0.3", "0.3–0.6")

milan <- milan |>
  mutate(
    growth_clip = pmax(pmin(growth_pc, 0.6), -0.6) # solo para el mapa
  )

tmap_mode("plot")


# 6. Mapas de niveles per cápita (2004 y 2012)

map_2004 <- tm_shape(milan) +
  tm_fill(
    col    = "diff_city_2004",
    style  = "fixed",
    breaks = brks_diff,
    labels = lbls_diff,
    palette   = "OrRd",
    colorNA   = "grey90",
    title     = ""
  ) +
  tm_borders(col = "grey40", lwd = 0.5) +
  tm_layout(
    main.title   = "Per capita number of restaurants in 2004",
    legend.show  = FALSE,
    frame        = FALSE
  )
print(map_2004)
#guardemos ese plot
tmap_save(
  tm = map_2004,
  filename = "map_2004_pc_restaurants.png",
  width = 6, height = 6, units = "in", dpi = 300
)

#plot de 2012
map_2012 <- tm_shape(milan) +
  tm_fill(
    col    = "diff_city_2012",
    style  = "fixed",
    breaks = brks_diff,
    labels = lbls_diff,
    palette   = "OrRd",
    colorNA   = "grey90",
    title     = ""
  ) +
  tm_borders(col = "grey40", lwd = 0.5) +
  tm_layout(
    main.title   = "Per capita number of restaurants in 2012",
    legend.show  = FALSE,
    frame        = FALSE
  )
print(map_2012)
#guardemos
tmap_save(
  tm = map_2012,
  filename = "map_2012_pc_restaurants.png",
  width = 6, height = 6, units = "in", dpi = 300
)
# Leyenda común para los dos mapas de arriba
legend_diff <- tm_shape(milan) +
  tm_fill(
    col    = "diff_city_2012",
    style  = "fixed",
    breaks = brks_diff,
    labels = lbls_diff,
    palette = "OrRd",
    colorNA = "grey90",
    title   = ""
  ) +
  tm_layout(
    legend.only     = TRUE,
    legend.text.size = 0.8
  )

# Para hacerlo similar a la figura del paper (dos mapas + leyenda)
fig1_top <- tmap_arrange(
  map_2004,
  map_2012,
  legend_diff,
  ncol   = 3,
  widths = c(1, 1, 0.45)
)
print(fig1_top)
#guardemos
tmap_save(
  tm = fig1_top,
  filename = "fig1_top_milan_restaurants.png",
  width = 12, height = 6, units = "in", dpi = 300
)
#7. Mapa de crecimiento porcentual

brks_growth <- c(-0.6, -0.3, -0.2, 0, 0.2, 0.3, 0.6)
lbls_growth <- c("-0.6–-0.3", "-0.3–-0.2", "-0.2–0",
                 "0–0.2", "0.2–0.3", "0.3–0.6")

map_growth <- tm_shape(milan) +
  tm_fill(
    col    = "growth_pc",
    style  = "fixed",
    breaks = brks_growth,
    labels = lbls_growth,
    palette = "Blues",      
    colorNA = "grey90",
    title   = ""
  ) +
  tm_borders(col = "grey25", lwd = 0.7) +
  tm_layout(
    main.title       = "Percent growth in the number of per capita restaurants",
    main.title.size  = 1.2,
    legend.outside   = TRUE,
    legend.text.size = 0.8,
    frame            = FALSE
  )

map_growth
#guardemos
tmap_save(
  tm = map_growth,
  filename = "map_growth_pc_restaurants.png",
  width = 8, height = 6, units = "in", dpi = 300
)



