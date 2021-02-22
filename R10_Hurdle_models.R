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
# Algunos visitantes no pescan, pero no hay datos sobre si una persona pescó o no. 

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

# (1) Regresión de modelo valla: el enfoque de este apartado. Partimos de la hipótesis de que el 
# exceso de ceros se debe a que hay no-pescadores, pero los pescadores siempre pescan.
# (2) Regresión con inflación de ceros: este enfoque sería apropiado si pensamos que entre los 
# pescadores existe la posibilidad de que no haya habido ninguna captura.
# (3) Modelos de conteo ordinario: los modelos de Poisson o binomiales negativos pueden ser más 
# apropiados si no hay ceros en exceso.
# (4) Regresión MCO: puede intentar analizar estos datos utilizando la regresión OLS. Sin embargo, 
# los datos de conteo son altamente no normales y no están bien estimados por la regresión MCO.

#####################################################################################################
# El modelo de regresión hurdle para datos de conteo: Poisson Hurdle (PH) y NB Hurdle (NBH)
#####################################################################################################

# Partimos del modelo de Poisson

summary(poisson.model <- glm(pesca ~ menores + acampada + personas, family="poisson", data=dat))
(exp.par.poisson <- exp(coef(poisson.model)))

# Veamos ahora el número de ceros que predice este modelo en comparación con el número de ceros
# que hemos observado. 

lambdai <- predict(poisson.model, type = "response") # predicción del conteo medio esperado para cada observación
mean(lambdai)
(exp <- sum(dpois(x = 0, lambda = lambdai)))  # suma de las probabilidades de 0 visitas para cada media
round(exp)               # 0's predichos
sum(dat$pesca == 0)     # 0's observados

# Como vemos, el número de ceros que predice el modelo es muy inferior al real. En estos casos,
# surge los modelos hurdle. Son modelos en dos etapas, como ocurría con los modelos con inflación
# de ceros. En la primera etapa se especifica el proceso de generación de los ceros y en la segunda
# el proceso de generación de los conteos positivos. La idea es que una vez superada la primera valla
# hay un número de visitas mayor que cero.

# Como hemos visto, se trata de un modelo en dos etapas claramente diferenciadas. La primera parte
# del modelo acostumbra a ser un modelo binario logit, que modeliza si una observación toma un valor
# positivo o no. La segunda parte del modelo es una distribución para datos de conteo truncada (bien
# sea una Poisson o NB truncada). La interpretación de este modelo en nuestro caso es que un proceso
# explica la decisión de acudir o no al médico, y otro proceso explica el número de visitas que se 
# realizan.

summary(ph.model <- hurdle(pesca ~ menores + acampada + personas | menores, data = dat, 
                            dist = "poisson", zero.dist = "binomial"))

# La salida de los modelos ofrece primero el modelo para los conteos positivos y, en segundo lugar, el
# modelo para el proceso de conteos cero (la valla). La interpretación de este modelo es similar
# a la de los modelos con inflación de ceros.

# Cuántos ceros predice el modelo hurdle? Para calcularlo, utilizamos la función predict con 
# type="prob". Con ello obtenemos para cada observación la probabilidad estimada para todos los 
# conteos posibles de la muestra. En este caso es una matriz 4406x90. La primera columna de esta
# matriz recoge la probabilidad de 0 conteos. Las sumamos para obtener el número esperado de 0 
# visitas al médico.

sum(predict(ph.model, type = "prob")[,1])
sum(dat$pesca == 0) 

# Por construcción, el número de visitas estimado coincide con el número de visitas observado.

#####################################################################################################
# E(y) en el modelo de regresión hurdle 
#####################################################################################################

# También podemos estimar la esperanza de y dado x utilizando los dos componentes del modelo 
# valla. La expresión matemática es:
#   
#   E(y|x) = (1-f1(0|x)) / (1-f2(0|x)) * mu_2(x)
# 
# Es decir, la esperanza del proceso de conteo dadas las variables explicativas, x, es el 
# producto de dos elementos: un ratio y una media. El ratio es la probabilidad de un no-cero
# en el primer proceso (distribución para la participación) dividido por la probabilidad de un 
# no-cero en el segundo proceso censurado (distribución para la intensidad). En nuestro caso, 
# serían logística y Poisson respectivamente. La media (mu_2) es la media de la distribución 
# de la intensidad.

# Podemos utilizar la función "predict" para obtener los conteos esperados medios para cada
# observación, cuya media sería la estimación de la esperanza incondicional de y 
# Tomando por ejemplo, el modelo PH:

mean(predict(ph.model, type = "response"))

# Tomando por ejemplo las primeras cinco observaciones:
predict(ph.model, type = "response")[1:5]

# Siguiendo con esta expresión, podemos también obtener el ratio para las probabilidades no-cero
# y la media del proceso sin truncamiento
predict(ph.model, type = "zero")[1:5]
predict(ph.model, type = "count")[1:5]

# Y por supuesto, el producto de ambos equivale al valor del conteo esperado medio:
predict(ph.model, type = "zero")[1:5] * predict(ph.model, type = "count")[1:5]

# El modelo PH parece haber acomodado el exceso de ceros pero ¿y la sobredispersión?

summary(nbh.model <- hurdle(pesca ~ menores + acampada + personas | menores, data = dat, 
                            dist = "negbin", zero.dist = "binomial"))

# Al igual que antes, por construcción, el número de visitas estimado coincide 
# con el número de visitas observado.

sum(predict(nbh.model, type = "prob")[,1])
sum(dat$pesca == 0) 


# Comparamos el ajuste de ambos modelos mediante el AIC:

AIC(poisson.model, ph.model, nbh.model)

# El modelo NBH tiene menor AIC y por lo tanto, ajusta mejor los datos.

#####################################################################################################
# Interpretación de coeficientes
#####################################################################################################

# Para interpretar los parámetros de manera sencilla calculamos el exp(beta):

(exp.par.ph <- exp(coef(ph.model)))
(exp.par.nbh <- exp(coef(nbh.model)))

(comp.modelos <- cbind(exp.par.ph, exp.par.nbh))

# Vamos a tomar como el ejemplo el modelo PH (el NBH tendría una interpretación similar)

# Ecuación de participación (modelo logit)
# Por cada menor adicional en el grupo (menores) se reduce la probabilidad ser "no pescador" en un
# factor de 0.33 (exp(-1.11)), lo cual es estadísticamente significativo (p<0,001)

# Ecuación de intensidad (modelo de conteo). La interpretación de los coeficientes de la ecuación
# de intensidad es equivalente a la del modelo de Poisson
# Entre los "pescadores", por cada menor adicional que acompaña al grupo, la pesca esperada 
# disminuye en un factor de 0.32 (exp(-1.14)), o lo que es lo mismo, en un 68% (exp(-1.14)-1), 
# manteniendo constante el resto de factores (p<0,001)
# Entre los "pescadores", aquellos que van de acampada (acampada) aumentan su pesca esperada en
# un 108% (exp(0.73)-1), manteniendo constante el resto de factores (p<0,001)
# Entre los "pescadores", una persona adicional en el grupo aumentan su pesca esperada en
# un 130% (exp(0.73)-1), manteniendo constante el resto de factores (p<0,001)

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

dput(coef(ph.model, "count"))
dput(coef(ph.model, "zero"))

f <- function(data, i) {
  require(pscl)
  m <- hurdle(pesca ~ menores + acampada + personas | menores,  data = data[i, ], 
              dist = "poisson", zero.dist = "binomial", 
              start = list(count = c(-0.826, -1.139, 0.73356, 0.83481), zero = c(0.3843, -1.111)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}
set.seed(10)
(res <- boot(dat, f, R = 1200, parallel = "snow", ncpus = 4))

# Los resultados son estimaciones de parámetros repetidas y errores estándar. Es decir, la primera 
# fila tiene el primer cálculo estimado de nuestro modelo. El segundo tiene el error estándar
# para el primer parámetro. La tercera columna contiene los errores estándar de arranque,
# que son considerablemente más grandes que los estimados por hurdle.

# Ahora podemos obtener los intervalos de confianza para todos los parámetros. Comenzamos en la
# escala original con IC ajustados por percentil y sesgo. También comparamos estos resultados con
# los intervalos de confianza regulares basados en los errores estándar.

parms <- t(sapply(c(1, 3, 5, 7, 9, 11), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

row.names(parms) <- names(coef(ph.model))
parms

# Intervalos de confianza
confint(ph.model)


#####################################################################################################
# Referencias
#####################################################################################################

# Cameron AC, Trivedi PK (2013). Regression Analysis of Count Data. Cambridge University Press, 
# Cambridge.
# Kleiber C, Zeileis A (2008). Applied Econometrics with R. Springer-Verlag, New York. 
# ISBN 978-0-387-77316-2.
# Kleiber C, Zeileis A (2016). “Visualizing Count Data Regressions Using Rootograms”. 
# The American Statistician, DOI: 10.1080/00031305.2016.1173590
# Zeileis A, Kleiber C, Jackman S (2008). “Regression Models for Count Data in R”. Journal
# of Statistical Software, 27(8). URL https://www.jstatsoft.org/article/view/v027i08.

