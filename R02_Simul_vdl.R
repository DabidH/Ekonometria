#####################################################################################################
##################           Motivación: datos truncados y censurados              ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2021                                         ##################
#####################################################################################################

rm(list=ls())

library(ggplot2)
library(AER)
library(MASS)
library(car)
library(dplyr) 
library(gridExtra) 
library(GGally)

#####################################################################################################
# Simulación de datos truncados y censurados
#####################################################################################################

simulador <- function(x){x + 20} 
observaciones <- simulador(seq(0,50,length.out = 200)) 

# Para introducir la varianza propia de los datos observacionales se añade # ruido 
# aleatorio y normal a cada observación 

set.seed(123) 
observaciones <- observaciones + rnorm(n = 200, mean = 0, sd = 12) 
datos <- data.frame(x = seq(0,50,length.out = 200), y = observaciones) 

# Regresión MCO con los datos no censurados

ggplot(data = datos, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + lims(y = c(0,100)) + labs(title = "Regresión OLS con los datos no censurados")

summary(lrm <- lm(y ~ x, data=datos))
                  
# Supongamos que el analista no observa valores de la variable Y inferiores a 30, 
# a pesar de que sí que son capaces de detectarlos y por lo tanto saben que existen. 
# La muestra observada pasaría a ser la siguiente:

ggplot(data = datos, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_point(data = filter(datos, y < 30), size = 2, color = "grey") + 
  geom_hline(yintercept = 30, linetype = "dashed") + theme_bw() + 
  lims(y = c(0,100)) + labs(title = "Observaciones por encima y debajo del límite de detección")

# Una primera opción que tiene el analista es deshacerse de las observaciones que no observa
# y por lo tanto, truncar la muestra

datos_t <- datos 
datos_t$y[datos$y < 30] <- NA

# Al truncar la muestra exclusivamente por el valor de la variable y, el estimador MCO es sesgado.
# Veamos el comportamiento del estimador MCO en la muestra entera y la truncada
ggplot(data = datos, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_point(data = filter(datos, y < 30), size = 2, color = "grey") + 
  geom_hline(yintercept = 30, linetype = "dashed") + theme_bw() + 
  geom_smooth(data = datos_t, method = "lm", se = FALSE, color = "firebrick") + 
  geom_smooth(data = datos, method = "lm", se = FALSE, color = "blue") + 
  lims(y = c(0,100)) + labs(title = "Estimador MCO con muestra truncada")

# Ante este tipo de situaciones, es convieniente utilizar el modelo de regresión
# para datos truncados

# La segunda opción es considerar la muestra censurada.
# En este contexto, el analista tendrá dos opciones: o considera "0" 
# todos los valores por debajo del límite de detección o los 
# considera "30". Podemos ver cómo afecta una opción y otra al ajuste
# MCO.

datos_0 <- datos 
datos_0$y[datos_0$y < 30] <- 0 
p1 <- ggplot(data = datos_0, aes(x = x, y = y)) + 
  geom_point(size = 2) + geom_point(data = filter(datos_0, y == 0), size = 2, color = "firebrick") +
  geom_smooth(data = datos,method = "lm", se = FALSE) + 
  geom_smooth(data = datos_0, method = "lm", se = FALSE, color = "firebrick") + 
  geom_hline(yintercept = 30, linetype = "dashed") + 
  lims(y = c(0,100)) + theme_bw() + 
  labs(title = "Observaciones censuradas ajustadas a 0")

datos_30 <- datos 
datos_30$y[datos_30$y < 30] <- 30 
p2 <- ggplot(data = datos_30, aes(x = x, y = y)) + 
  geom_point(size = 2) + geom_point(data = filter(datos_30, y == 30), size = 2, color = "firebrick") +
  geom_smooth(data = datos,method = "lm", se = FALSE) + 
  geom_smooth(data = datos_30, method = "lm", se = FALSE, color = "firebrick") + 
  geom_hline(yintercept = 30, linetype = "dashed") + theme_bw() + 
  lims(y = c(0,100)) + labs(title = "Observaciones censuradas ajustadas a 30") 
grid.arrange(p1, p2, ncol = 1)

# Ante este tipo de situaciones, es convieniente utilizar el modelo de regresión
# para datos censurados o modelo Tobit
