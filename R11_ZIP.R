#####################################################################################################
##################           Modelos de conteo con inflación de ceros (ZIP)        ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2021                                         ##################
#####################################################################################################

####################################################################################################
# Introducción
#####################################################################################################

# La regresión con inflación de ceros se utiliza para modelizar datos de conteo que tienen un exceso 
# de conteos cero. Además, la teoría sugiere que los ceros en exceso se generan por un proceso separado 
# de los valores de conteo y que los ceros en exceso se pueden modelizar de forma independiente.
# Por lo tanto, el modelo zip/zinb tiene dos partes, un modelo de conteo de Poisson o NegBin y 
# un modelo logit para predecir el exceso de ceros.

# Ejemplo 1. Los administradores escolares estudian el comportamiento de asistencia de los jóvenes
# de secundaria en dos escuelas. Las variables explicativas del número de días de ausencia incluyen 
# el género del estudiante y las puntuaciones de las pruebas estandarizadas en matemáticas y artes
# del lenguaje.

# Ejemplo 2. Los biólogos de vida silvestre quieren modelizar cuántos peces están siendo capturados
# por los pescadores en un parque natural. Se pregunta a los visitantes cuánto tiempo se quedaron,
# cuántas personas había en el grupo, si había niños en el grupo y cuántos peces fueron capturados.
# Algunos visitantes no pescan, pero no hay datos sobre si una persona pescó o no. Algunos visitantes
# que pescaron no pescaron ningún pescado, por lo que hay un exceso de ceros en los datos debido a las 
# personas que no pescaron.

#####################################################################################################
# Descripción de los datos
#####################################################################################################

# Vamos a ver un ejemplo con datos a partir del Ejemplo 2 anterior. 

# Tenemos datos sobre 250 grupos que fueron a un parque natural. Se preguntó a cada grupo sobre 
# cuántos peces capturaron (pesca), cuántos niños había en el grupo (menores), cuántas personas había 
# en el grupo (personas) y si trajeron o no una caravana al parque (acampada) .

# Además de predecir el número de peces capturados, existe interés en predecir la existencia de
# ceros en exceso, es decir, la probabilidad de que un grupo pesque cero peces. Utilizaremos las 
# variables menores, personas y acampada en nuestro modelo. 

# Empezaremos por cargar algunas librerías y leer los datos.

rm(list=ls())

library(ggplot2)
library(pscl)
library(boot)

dat <-  read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/zidata.csv")

str(dat)
summary(dat)

attach(dat)

ggplot(dat, aes(x=pesca)) + 
  geom_histogram()

# histograma con el eje x en escala log10
ggplot(dat, aes(pesca, fill = acampada)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(acampada ~ ., margins=TRUE, scales="free_y")

#####################################################################################################
# Posibles modelos para analizar estos datos
#####################################################################################################

# A continuación se muestra una lista de algunos métodos de análisis que podrías considerar a la hora 
# de analizar estos datos. Algunos de los métodos enumerados son bastante razonables, mientras que 
# otros pueden tener diversas limitaciones o han caído en desuso.

# (1) Regresión de Poisson con inflación de ceros: el enfoque de este apartado.
# (2) Regresión binomial negativa con inflación de ceros: la regresión binomial negativa funciona mejor 
# con datos demasiado dispersos, es decir, una varianza mucho mayor que la media.
# (3) Modelos de conteo ordinario: los modelos de Poisson o binomiales negativos pueden ser más 
# apropiados si no hay ceros en exceso.
# (4) Regresión MCO: puede intentar analizar estos datos utilizando la regresión OLS. Sin embargo, 
# los datos de conteo son altamente no normales y no están bien estimados por la regresión MCO.

#####################################################################################################
# El modelo de regresión de Poisson con inflación de ceros (ZIP)
#####################################################################################################

# Para analizar los datos utilizando el modelo de Poisson con inflación de ceros utilizaremos la 
# función "zeroinfl" del paquete "pscl":

summary(zip.model <- zeroinfl(pesca ~ menores + acampada | personas, data = dat))

summary(nbh.model <- hurdle(pesca ~ menores + acampada + personas | menores, data = dat, 
                            dist = "negbin", zero.dist = "binomial"))

(exp.par <- exp(coef(nbh.model)))


# El modelo estimado guarda cierta similitud con los modelos en dos etapas. En primer lugar, nos 
# encontramos un bloque de salida que contiene coeficientes de regresión de Poisson para cada una de 
# las variables junto con errores estándar, estadístico z y valores p para los coeficientes. A 
# continuación, un segundo bloque que corresponde al modelo de inflación de ceros. Esto incluye 
# coeficientes logit para predecir el exceso de ceros junto con sus errores estándar, estadístico z
# y valores p.

# Todas las variables explicativas tanto del conteo como de la inflación de ceros son estadísticamente 
# significativos. Sin embargo, nada indica si el modelo con inflación de ceros mejora sobre una
# regresión estándar de Poisson. Podemos determinar esto utilizondo el contraste de Vuong de los
# dos modelos.

summary(poisson.model <- glm(pesca ~ menores + acampada, family = poisson, data = dat))

vuong(zip.model, poisson.model)

# El contraste de Vuong indica claramente que el modelo zip es superior al Poisson.

# Nota: Hay algunas críticas y debates en la literatura sobre el uso y mal uso del contraste de 
# Vuong en modelos no anidados para contrastar la inflación de ceros de datos de conteo:
# Desmarais Bruce A., Harden Jeffrey J., 2013, “Testing for Zero Inflation in Count Models:
# Bias Correction for the Vuong Test” Stata Journal, 13, 4, 810-835
# Wilson P., 2015, “The misuse of the Vuong test for non-nested models to test for zero-inflation”
# Economics Letters, 127, 51-53
# Wilson (2015) shows that a zero-inflated model and its non-zero inflated counterpart do not 
# satisfy Vuong's criteria for non-nested models, and hence such use of the test is incorrect.

# Ver He et al. (2019) "A test of inflated zeros for Poisson regression models"

#####################################################################################################
# Interpretación de coeficientes
#####################################################################################################

# Ecuación de participación (modelo logit)
# Por cada persona adicional en el grupo (personas) se reduce la probabilidad ser "no pescador" en un
# factor de 0.57 (exp(-0.56)), lo cual es estadísticamente significativo (p<0,001)

# Ecuación de intensidad (modelo de conteo). La interpretación de los coeficientes de la ecuación
# de intensidad es equivalente a la del modelo de Poisson
# Entre los "pescadores", por cada menor adicional que acompaña al grupo, la pesca esperada 
# disminuye en un factor de 0.35 (exp(-1.04)), o lo que es lo mismo, en un 65% (exp(-1.04)-1), 
# manteniendo constante el resto de factores (p<0,001)
# Entre los "pescadores", aquellos que van de acampada (acampada) aumentan su pesca esperada en
# un 130% (exp(0.83)-1), manteniendo constante el resto de factores (p<0,001)

#####################################################################################################
# Intervalos de confianza
#####################################################################################################

# Podemos obtener intervalos de confianza para los parámetros y los parámetros exponenciados mediante
# bootstrapping. Para el modelo de Poisson, estas serían ratios de riesgo de incidencia, para el
# modelo de inflación cero, ratios de probabilidades. Usamos el paquete "bootstrap". Primero, 
# obtenemos los coeficientes de nuestro modelo original para usarlos como valores iniciales 
# para que el modelo acelere el tiempo que lleva estimar. Luego, escribimos una función corta que
# toma datos e índices como entrada y devuelve los parámetros que nos interesan. Finalmente, 
# pasamos eso a la función de arranque y hacemos 1200 repeticiones, usando cuatro núcleos (ten en cuenta
# que debes ajustar el número de núcleos de tu ordenador). Además, para obtener resultados finales,
# es posible que quieras aumentar el número de repeticiones para ayudar a garantizar resultados estables.

dput(coef(zip.model, "count"))
dput(coef(zip.model, "zero"))

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(pesca ~ menores + acampada | personas, data = data[i, ],
                start = list(count = c(1.598, -1.0428, 0.834), zero = c(1.297, -0.564)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res <- boot(dat, f, R = 1200, parallel = "snow", ncpus = 4))

# Los resultados son estimaciones de parámetros repetidas y errores estándar. Es decir, la primera 
# fila tiene el primer cálculo estimado de nuestro modelo. El segundo tiene el error estándar
# para el primer parámetro. La tercera columna contiene los errores estándar de arranque,
# que son considerablemente más grandes que los estimados por zeroinfl.

# Ahora podemos obtener los intervalos de confianza para todos los parámetros. Comenzamos en la
# escala original con IC ajustados por percentil y sesgo. También comparamos estos resultados con
# los intervalos de confianza regulares basados en los errores estándar.

parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

row.names(parms) <- names(coef(zip.model))
parms

## compara con la aproximación basada en la distribución normal
confint(zip.model)

# Los intervalos de confianza por bootstrap son considerablemente más anchos que la aproximación
# normal.

# Ahora podemos estimar el ratio de riesgo de incidencia para el modelo de Poisson y el ratio de 
# probabilidades para el modelo logit (inflación de ceros). El código es prácticamente igual al anterior:

expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

row.names(expparms) <- names(coef(zip.model))
expparms
  
# Para comprender mejor nuestro modelo, podemos calcular el número esperado de peces capturados para 
# diferentes combinaciones de las variables explicativas. De hecho, dado que estamos trabajando con
# variables explicativas esencialmente categóricos, podemos calcular los valores esperados para todas
# las combinaciones usando la función expand.grid para crear todas las combinaciones y luego la 
# función de predicción para hacerlo. También eliminamos las filas donde el número de hijos excede 
# el número de personas, lo que no tiene sentido lógicamente, utilizando la función de subconjunto.
# Finalmente creamos un gráfico.


newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("menores", "acampada", "personas")
newdata1 <- subset(newdata1, subset=(menores<=personas))
newdata1$phat <- predict(zip.model, newdata1)

ggplot(newdata1, aes(x = menores, y = phat, colour = factor(personas))) +
  geom_point() +
  geom_line() +
  facet_wrap(~acampada) +
  labs(x = "Número de menores", y = "Pesca predicha")

#####################################################################################################
# Consideraciones finales
#####################################################################################################

# A continuación, se presentan una serie de problemas que puedes considerar en el análisis de
# datos.

# (1) Dado que el modelo zip tiene tanto un modelo de conteo como un modelo logit, cada uno de los dos 
# modelos debe tener una buena especificación. Los dos modelos no necesariamente necesitan usar las 
# mismas variables explicativas.

# (2) La parte del modelo logit puede tener problemas de predicción perfecta, separación o separación 
# parcial.

# (3) Los datos de conteo a menudo usan variables de exposición para indicar la cantidad de veces que
# el evento pudo haber sucedido. Se puede incorporar el log de la variable de exposición en el modelo
# utilizando la opción offset ().

# (4) No se recomienda que los modelos con inflación de ceros sean aplicados en muestras pequeñas, si
# bien no está claro en la literatura qué constituye una muestra pequeña.

# (5) Los valores de pseudo-R2 difieren del R2 del modelo MCO. 

#####################################################################################################
# Referencias
#####################################################################################################


# Long, J. S. 1997. Regression Models for Categorical and Limited Dependent Variables. 
# Thousand Oaks, CA: Sage Publications. Everitt, B. S. and Hothorn, T. A Handbook of Statistical 
# Analyses Using R

  