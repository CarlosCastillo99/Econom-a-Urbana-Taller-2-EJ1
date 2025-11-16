############################################################################
# Universidad de los Andes
# Taller 2
# Nombres: Carlos Castillo, Luis Rubiano y Felipe Rosas 
###########################################################################

rm(list = ls())

# Paquetes
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
#------------------------------PARTE 1------------------------------------
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
#------------------------------PARTE 2-----------------------------------------
#Distribución no paramétrica de precios (2004 y 2012)
#Kernels: Gaussiano y Epanechnikov
##Bandwidth: rule-of-thumb de Silverman
#Limpiemos el enviorment para que podamos trabajar mejor sin los resultados del ejercicio 1
rm(list = ls())
# volvamos a cargar la ruta
setwd("C:/Users/pc/Downloads/taller 2 urbana")
load("Taller2_Ejercicio1.Rdata")
# --------------------------------2.A--------------------------------------
# 1. Extraer precios y eliminar NA

prezzo_2004 <- restaurants |>
  dplyr::filter(!is.na(prezzo2004)) |>
  dplyr::pull(prezzo2004)

prezzo_2012 <- restaurants |>
  dplyr::filter(!is.na(prezzo2012)) |>
  dplyr::pull(prezzo2012)

# 2. rule-of-thumb de Silverman
#    h = 1.06 * min(sd, IQR/1.34) * n^(-1/5)

bw_rot <- function(x) {
  s   <- stats::sd(x)
  a   <- min(s, IQR(x) / 1.34)
  1.06 * a * length(x)^(-1/5)
}

bw_2004 <- bw_rot(prezzo_2004)
bw_2012 <- bw_rot(prezzo_2012)
#revisemos qué h usar
cat("Bandwidth 2004 (rule-of-thumb):", bw_2004, "\n")
cat("Bandwidth 2012 (rule-of-thumb):", bw_2012, "\n")


## 3. Estimar densidades para cada kernel y año

# Usamos el mismo rango de precios para ambos años
x_min <- min(prezzo_2004, prezzo_2012) - 5
x_max <- max(prezzo_2004, prezzo_2012) + 5

dens_2004_gauss <- density(
  prezzo_2004,
  bw     = bw_2004,
  kernel = "gaussian",
  from   = x_min,
  to     = x_max,
  n      = 1024
)

dens_2004_epan <- density(
  prezzo_2004,
  bw     = bw_2004,
  kernel = "epanechnikov",
  from   = x_min,
  to     = x_max,
  n      = 1024
)

dens_2012_gauss <- density(
  prezzo_2012,
  bw     = bw_2012,
  kernel = "gaussian",
  from   = x_min,
  to     = x_max,
  n      = 1024
)

dens_2012_epan <- density(
  prezzo_2012,
  bw     = bw_2012,
  kernel = "epanechnikov",
  from   = x_min,
  to     = x_max,
  n      = 1024
)


# 4. Gráfico comparativo por año

par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)

# layout vertical
par(
  mfrow = c(2, 1),          # 2 filas, 1 columna
  mar   = c(3, 4, 2, 1)     # márgenes más pequeños para que nos quepa en el doc
)

# 2004
plot(
  dens_2004_gauss,
  lwd  = 2,
  col  = "steelblue",
  main = "Densidad no paramétrica de precios (2004)",
  xlab = "Precio",
  ylab = "Densidad estimada"
)
lines(
  dens_2004_epan,
  lwd = 2,
  col = "firebrick",
  lty = 2
)
legend(
  "topright",
  legend = c("Gaussiano", "Epanechnikov"),
  col    = c("steelblue", "firebrick"),
  lty    = c(1, 2),
  lwd    = 2,
  bty    = "n"
)

# 2012
plot(
  dens_2012_gauss,
  lwd  = 2,
  col  = "steelblue",
  main = "Densidad no paramétrica de precios (2012)",
  xlab = "Precio",
  ylab = "Densidad estimada"
)
lines(
  dens_2012_epan,
  lwd = 2,
  col = "firebrick",
  lty = 2
)
legend(
  "topright",
  legend = c("Gaussiano", "Epanechnikov"),
  col    = c("steelblue", "firebrick"),
  lty    = c(1, 2),
  lwd    = 2,
  bty    = "n"
)

# --------------------------------2.B--------------------------------------
# (b) KDE Epanechnikov con tres anchos de banda

# 1. Tres anchos de banda por año: h, h/2 y 2h
h_2004    <- bw_2004
h_2004_m  <- bw_2004 / 2      # mitad del rule-of-thumb
h_2004_2  <- 2 * bw_2004      # doble del rule-of-thumb

h_2012    <- bw_2012
h_2012_m  <- bw_2012 / 2
h_2012_2  <- 2 * bw_2012

cat("h_2004   =", h_2004,  "\n",
    "h_2004/2 =", h_2004_m, "\n",
    "2*h_2004 =", h_2004_2, "\n\n")

cat("h_2012   =", h_2012,  "\n",
    "h_2012/2 =", h_2012_m, "\n",
    "2*h_2012 =", h_2012_2, "\n")

# 2. Rango común de precios para todas las densidades
x_min <- min(c(prezzo_2004, prezzo_2012), na.rm = TRUE) - 5
x_max <- max(c(prezzo_2004, prezzo_2012), na.rm = TRUE) + 5


# 3. Densidades Epanechnikov 2004
dens_2004_epan_h   <- density(prezzo_2004, kernel = "epanechnikov",
                              bw = h_2004,   from = x_min, to = x_max, n = 1024)
dens_2004_epan_hm  <- density(prezzo_2004, kernel = "epanechnikov",
                              bw = h_2004_m, from = x_min, to = x_max, n = 1024)
dens_2004_epan_h2  <- density(prezzo_2004, kernel = "epanechnikov",
                              bw = h_2004_2, from = x_min, to = x_max, n = 1024)

# 4. Densidades Epanechnikov 2012
dens_2012_epan_h   <- density(prezzo_2012, kernel = "epanechnikov",
                              bw = h_2012,   from = x_min, to = x_max, n = 1024)
dens_2012_epan_hm  <- density(prezzo_2012, kernel = "epanechnikov",
                              bw = h_2012_m, from = x_min, to = x_max, n = 1024)
dens_2012_epan_h2  <- density(prezzo_2012, kernel = "epanechnikov",
                              bw = h_2012_2, from = x_min, to = x_max, n = 1024)

# rango comun eje Y
ylim_range <- range(
  dens_2004_epan_h$y,  dens_2004_epan_hm$y,  dens_2004_epan_h2$y,
  dens_2012_epan_h$y,  dens_2012_epan_hm$y,  dens_2012_epan_h2$y
)

# 5. Gráfico comparativo: 2004 y 2012 (tres bandas por año)
opar <- par(mfrow = c(2, 1),
            mar   = c(4, 4, 3, 1) + 0.1)

## Panel superior: 2004
plot(dens_2004_epan_h,
     lwd  = 2,
     col  = "black",
     main = "Precios 2004 (kernel Epanechnikov)",
     xlab = "Precio",
     ylab = "Densidad estimada",
     ylim = ylim_range)   # <- aquí usas el rango común
lines(dens_2004_epan_hm, col = "steelblue",  lwd = 2, lty = 2)
lines(dens_2004_epan_h2, col = "firebrick",  lwd = 2, lty = 3)
legend("topright",
       inset  = c(0.02,0.02),
       legend = c("h (rule-of-thumb)", "h/2 (sub-suavizado)", "2h (sobre-suavizado)"),
       col    = c("black", "steelblue", "firebrick"),
       lty    = c(1, 2, 3),
       lwd    = 2,
       bty    = "n",
       cex    = 0.8)

## Panel inferior: 2012
plot(dens_2012_epan_h,
     lwd  = 2,
     col  = "black",
     main = "Precios 2012 (kernel Epanechnikov)",
     xlab = "Precio",
     ylab = "Densidad estimada",
     ylim = ylim_range)   # <- mismo rango en el segundo panel
lines(dens_2012_epan_hm, col = "steelblue",  lwd = 2, lty = 2)
lines(dens_2012_epan_h2, col = "firebrick",  lwd = 2, lty = 3)
legend("topright",
       inset  = c(0.02,0.02),
       legend = c("h (rule-of-thumb)", "h/2 (sub-suavizado)", "2h (sobre-suavizado)"),
       col    = c("black", "steelblue", "firebrick"),
       lty    = c(1, 2, 3),
       lwd    = 2,
       bty    = "n",
       cex    = 0.8)

par(opar)

# -------------------------- PARTE 3: DURANTON–OVERMAN --------------------------

library(sf)
library(dplyr)
library(ggplot2)

## 1. Top-5 barrios por crecimiento per cápita, usamos las variables que creamos en punto 1

top5_milan <- milan |>
  st_drop_geometry() |>
  arrange(desc(growth_pc)) |>
  slice_head(n = 5)

top5_ids <- top5_milan$zona180
top5_ids

# Polígonos de esos 5 barrios en CRS métrico (UTM 32N, Milán)
top5_poly <- milan |>
  filter(zona180 %in% top5_ids) |>
  st_transform(32632)
top5_poly
# 2. Parámetros del test de Duranton y Overman:

from_val <- 0        # distancia mínima (km)
to_val   <- 1        # distancia máxima (km)
n_points <- 60       # puntos de la malla para la KDE
n_sim    <- 500      # número de simulaciones bajo H0

# 3. Función: test DO para top-5 barrios en un año 

do_top5_year <- function(year) {
  
  # columnas de coordenadas según el año
  lat_var  <- paste0("lat",  year)
  long_var <- paste0("long", year)
  
  # Restaurantes en esos 5 barrios con coordenadas para ese año
  rest_top5 <- restaurants |>
    filter(zona180 %in% top5_ids,
           !is.na(.data[[lat_var]]),
           !is.na(.data[[long_var]]))
  rest_top5
  message("Año ", year, ": restaurantes georreferenciados en top-5 = ", nrow(rest_top5))

  # Pasar a sf y convertir a CRS métrico
  pts <- st_as_sf(
    rest_top5,
    coords = c(long_var, lat_var),
    crs    = 4326
  ) |>
    st_transform(32632)
  
  # Distancias observadas 0–1 km
  dmat <- st_distance(pts)
  dmat[lower.tri(dmat, diag = TRUE)] <- NA   # sólo triángulo superior
  
  dvec <- as.numeric(dmat)
  dvec <- dvec[!is.na(dvec)] / 1000          # pasar de metros a km
  dvec <- dvec[dvec > from_val & dvec <= to_val]
  
  # KDE observada con kernel Epanechnikov
  dens_obs <- density(
    dvec,
    bw     = "nrd",           # rule-of-thumb
    kernel = "epanechnikov",
    from   = from_val,
    to     = to_val,
    n      = n_points
  )
#-------------------------------------------  
  #  Simulaciones bajo H0
  set.seed(123 + year)        # semilla distinta por año
  n_pts <- nrow(pts)
  
  # matriz: filas = puntos de la malla, columnas = simulaciones
  sim_y <- matrix(NA_real_, nrow = n_points, ncol = n_sim)
  
  for (b in seq_len(n_sim)) {
    # muestreamos puntos uniformemente en la unión de los 5 barrios
    sim_pts <- st_sample(top5_poly, size = n_pts, type = "random")
    
    dmat_s <- st_distance(sim_pts)
    dmat_s[lower.tri(dmat_s, diag = TRUE)] <- NA
    
    dvec_s <- as.numeric(dmat_s)
    dvec_s <- dvec_s[!is.na(dvec_s)] / 1000
    dvec_s <- dvec_s[dvec_s > from_val & dvec_s <= to_val]
    
    dens_s <- density(
      dvec_s,
      bw     = "nrd", # ese "nrd" usa la regla de Silverman (bw.nrd): ancho de banda automático ~ 0.9 * min(sd, IQR/1.34) * n^(-1/5)
      kernel = "epanechnikov",
      from   = from_val,
      to     = to_val,
      n      = n_points
    )
    sim_y[, b] <- dens_s$y
  }
  
  # Bandas 5% y 95% en cada punto de la malla
  q05 <- apply(sim_y, 1, quantile, probs = 0.05, na.rm = TRUE)
  q95 <- apply(sim_y, 1, quantile, probs = 0.95, na.rm = TRUE)
  
  tibble(
    year     = year,
    distance = dens_obs$x,
    f_obs    = dens_obs$y,
    f_q05    = q05,
    f_q95    = q95
  )
}

# 4. Ejecutar el test para 2004 y 2012

do_2004 <- do_top5_year(2004)
do_2012 <- do_top5_year(2012)

do_all <- bind_rows(do_2004, do_2012)

head(do_all)

#Gráfico
library(ggplot2)

plot_do <- ggplot(do_all, aes(x = distance)) +
  # Banda gris (entre q05 y q95)
  geom_ribbon(aes(ymin = f_q05, ymax = f_q95),
              fill = "grey80", alpha = 0.6) +
  # Bordes punteados de la banda
  geom_line(aes(y = f_q05), linetype = "dashed", colour = "grey40") +
  geom_line(aes(y = f_q95), linetype = "dashed", colour = "grey40") +
  # Curva observada
  geom_line(aes(y = f_obs), size = 1) +
  facet_wrap(~ year, ncol = 1) +
  theme_bw() +
  labs(
    x = "Distancia entre restaurantes (km)",
    y = "Densidad de distancias",
    title = "Test de Duranton & Overman\nTop 5 barrios por crecimiento per cápita (Kernel Epanechnikov)"
  ) +
  theme(
    plot.title  = element_text(hjust = 0.5),
    strip.text  = element_text(face = "bold")
  )

plot_do

ggsave("test_DO_top5_2004_2012_Epan.png",
       plot   = plot_do,
       width  = 7,
       height = 8,
       dpi    = 300)

#Probemos con un Gaussiana a ver si es muy distinto, copiamos y pegamos pero arreglamos el kernel:
# -------------------------- PARTE 3: DURANTON–OVERMAN --------------------------

library(sf)
library(dplyr)
library(ggplot2)

## 1. Top-5 barrios por crecimiento per cápita, usamos las variables que creamos en punto 1

top5_milan <- milan |>
  st_drop_geometry() |>
  arrange(desc(growth_pc)) |>
  slice_head(n = 5)

top5_ids <- top5_milan$zona180
top5_ids

# Polígonos de esos 5 barrios en CRS métrico (UTM 32N, Milán)
top5_poly <- milan |>
  filter(zona180 %in% top5_ids) |>
  st_transform(32632)
top5_poly
# 2. Parámetros del test de Duranton y Overman:

from_val <- 0        # distancia mínima (km)
to_val   <- 1        # distancia máxima (km)
n_points <- 60       # puntos de la malla para la KDE
n_sim    <- 500      # número de simulaciones bajo H0

# 3. Función: test DO para top-5 barrios en un año 

do_top5_year <- function(year) {
  
  # columnas de coordenadas según el año
  lat_var  <- paste0("lat",  year)
  long_var <- paste0("long", year)
  
  # Restaurantes en esos 5 barrios con coordenadas para ese año
  rest_top5 <- restaurants |>
    filter(zona180 %in% top5_ids,
           !is.na(.data[[lat_var]]),
           !is.na(.data[[long_var]]))
  rest_top5
  message("Año ", year, ": restaurantes georreferenciados en top-5 = ", nrow(rest_top5))
  
  # Pasar a sf y convertir a CRS métrico
  pts <- st_as_sf(
    rest_top5,
    coords = c(long_var, lat_var),
    crs    = 4326
  ) |>
    st_transform(32632)
  
  # Distancias observadas 0–1 km
  dmat <- st_distance(pts)
  dmat[lower.tri(dmat, diag = TRUE)] <- NA   # sólo triángulo superior
  
  dvec <- as.numeric(dmat)
  dvec <- dvec[!is.na(dvec)] / 1000          # pasar de metros a km
  dvec <- dvec[dvec > from_val & dvec <= to_val]
  
  # KDE observada con kernel Gaussiano
  dens_obs <- density(
    dvec,
    bw     = "nrd",           # rule-of-thumb de Silverman
    kernel = "gaussian",      # <- aquí cambias
    from   = from_val,
    to     = to_val,
    n      = n_points
  )
  
  #-------------------------------------------  
  #  Simulaciones bajo H0
  set.seed(123 + year)        # semilla distinta por año
  n_pts <- nrow(pts)
  
  # matriz: filas = puntos de la malla, columnas = simulaciones
  sim_y <- matrix(NA_real_, nrow = n_points, ncol = n_sim)
  
  for (b in seq_len(n_sim)) {
    # muestreamos puntos uniformemente en la unión de los 5 barrios
    sim_pts <- st_sample(top5_poly, size = n_pts, type = "random")
    
    dmat_s <- st_distance(sim_pts)
    dmat_s[lower.tri(dmat_s, diag = TRUE)] <- NA
    
    dvec_s <- as.numeric(dmat_s)
    dvec_s <- dvec_s[!is.na(dvec_s)] / 1000
    dvec_s <- dvec_s[dvec_s > from_val & dvec_s <= to_val]
    
    dens_s <- density(
      dvec_s,
      bw     = "nrd",           # mismo bw rule-of-thumb
      kernel = "gaussian",      # <- aquí también
      from   = from_val,
      to     = to_val,
      n      = n_points
    )
    
    sim_y[, b] <- dens_s$y
  }
  
  # Bandas 5% y 95% en cada punto de la malla
  q05 <- apply(sim_y, 1, quantile, probs = 0.05, na.rm = TRUE)
  q95 <- apply(sim_y, 1, quantile, probs = 0.95, na.rm = TRUE)
  
  tibble(
    year     = year,
    distance = dens_obs$x,
    f_obs    = dens_obs$y,
    f_q05    = q05,
    f_q95    = q95
  )
}

# 4. Ejecutar el test para 2004 y 2012

do_2004 <- do_top5_year(2004)
do_2012 <- do_top5_year(2012)

do_all <- bind_rows(do_2004, do_2012)

head(do_all)

#Gráfico
library(ggplot2)

plot_do <- ggplot(do_all, aes(x = distance)) +
  # Banda gris (entre q05 y q95)
  geom_ribbon(aes(ymin = f_q05, ymax = f_q95),
              fill = "grey80", alpha = 0.6) +
  # Bordes punteados de la banda
  geom_line(aes(y = f_q05), linetype = "dashed", colour = "grey40") +
  geom_line(aes(y = f_q95), linetype = "dashed", colour = "grey40") +
  # Curva observada
  geom_line(aes(y = f_obs), size = 1) +
  facet_wrap(~ year, ncol = 1) +
  theme_bw() +
  labs(
    x = "Distancia entre restaurantes (km)",
    y = "Densidad de distancias",
    title = "Test de Duranton & Overman\nTop 5 barrios por crecimiento per cápita (Kernel Gaussiano)"
  ) +
  theme(
    plot.title  = element_text(hjust = 0.5),
    strip.text  = element_text(face = "bold")
  )

plot_do

ggsave("test_DO_top5_2004_2012_gauss.png",
       plot   = plot_do,
       width  = 7,
       height = 8,
       dpi    = 300)








