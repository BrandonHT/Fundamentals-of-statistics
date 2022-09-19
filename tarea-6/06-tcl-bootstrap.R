# Envíen un reporte por correo electrónico con 
# las respuestas (con título fundamentos-tarea06).

library(tidyverse)

#### Ejercicios: teorema central del límite ####

#### Ejemplo 1 ####
# Consideramos la distribución gamma con parámetro de forma
# a = 5, tasa lambda = 0.1. Su media teórica es 50 = 5/0.1
# cuya densidad teórica es

x <- seq(0, 150, 0.01)
tibble(x = x) %>% 
  mutate(densidad = dgamma(x, 5, 0.1)) %>% 
  ggplot(aes(x = x, y = densidad)) + geom_line()

# tomamos una muestra:
set.seed(232)
n <- 200
muestra <- rgamma(n, 5, 0.1)

## La distribución de los datos se ve como sigue 
# (haz un histograma de la muestra)

# histograma

# ¿Parece tener distribución normal?

# Ahora consideramos la distribución de muestreo de 
# la media de esta distribución, con tamaño de muestra
# fijo n
medias <- map_dbl(1:5000,  ~ mean(rgamma(n, 5, 0.1)))
medias_gamma <- tibble(medias = medias)

## checa un histograma, ¿ se ve normal?


####### Ejemplo: mezcla de distribuciones
# Este ejemplo es más complicado. Imaginemos
# que nuestro modelo teórico es una mezcla
# de dos poblaciones, una gamma y una normal
muestrear_pob <- function(n){
  u <- runif(n) # número aleatorio
  map_dbl(u, ~ ifelse(.x < 1/2, rgamma(1, 5, 0.1), rnorm(1, 100, 5)))
}

# El modelo teórico se puede graficar, pero también podemos
# obetener una aproximación buena haciendo una cantidad grande
# de simulaciones
muestra_aprox <- muestrear_pob(10000)
qplot(muestra_aprox, binwidth= 2)


## Ahora consideramos estimar la media de esta
## distribución con un muestra de tamaño 100
## ¿Cómo se ve la distribución de muestreo de la media?
medias <- map_dbl(1:2000,  ~ mean(#rellena))
  
## grafica un histograma y una gráfica cuantil-cuantil normal
  
#### Ejemplo discreto ####
# Tomaremos muestra de unos y ceros
set.seed(1212)
n_volados <- 200
muestra <- rbinom(n_volados, 1, prob = 0.7)
head(muestra)
  
# la media es la proporción de unos en la muestra,
# o la proporción de "soles":
mean(muestra)

## ¿Cuál es la distribución de muestreo para la proporción
# de soles en la muestra?
prop_soles <- map_dbl(1:5000,  ~ mean(#rellena))
prop_soles_tbl <- tibble(prop_soles = prop_soles)
    
## checa un histograma, ¿se ve normal? También ve 
## una gráfica qq
    
ggplot(prop_soles_tbl, aes(x = prop_soles)) + #rellena
      
ggplot(prop_soles_tbl, aes(sample = prop_soles)) + #rellena
      
#### Bootstrap
    
## Ejemplo 1: error estándar de una media
# Retomaremos el ejemplo de la prueba ENLACE de la tarea anterior
# Para cada tamaño de muestra n = 10, 100, 1000
# i) Selecciona una única muestra y utilizala para estimar la media de las
#   calificaciones de español 3o de primaria
# ii) Utiliza bootstrap para calcular el error estándar de tu estimador
# iii) Grafica la distribución bootstrap

# Retoma la muestra de tamaño 100, y calcula la correlación entre las
# calificaciones de español 3o y 6o de primaria
# Utiliza bootstrap para calcular el error estandar 
