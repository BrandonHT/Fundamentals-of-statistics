library(tidyverse)

n_volados <- 50
# posible valores del parámetro desconocido
theta = c(.4, 0.6)
# probabilidades iniciales
probs_inicial <- tibble(moneda = c(1, 2),
                        theta = theta,
                        prob_inicial = c(0.9, 0.1))
probs_inicial

# verosimilitud
crear_verosim <- function(no_soles){
  verosim <- function(theta){
    # prob de observar no_soles en 2 volados con probabilidad de sol theta
    dbinom(no_soles, n_volados, theta)
  }
  verosim
}

# evaluar verosimilitud
verosim <- crear_verosim(35)

# ahora usamos regla de bayes para hacer tabla de probabilidades
tabla_inferencia <- probs_inicial %>%
  mutate(verosimilitud = map_dbl(theta, verosim)) %>%
  mutate(inicial_x_verosim = prob_inicial * verosimilitud) %>%
  # normalizar
  mutate(prob_posterior = inicial_x_verosim / sum(inicial_x_verosim))

tabla_inferencia %>%
  mutate(moneda_obs = moneda) %>%
  select(moneda_obs, theta, prob_inicial, verosimilitud, prob_posterior)


