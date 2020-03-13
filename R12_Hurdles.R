#####################################################################################################
##################           Modelos valla (hurdle) para datos de conteo           ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

####################################################################################################
# Introducción
#####################################################################################################

# Los modelos valla (hurdle) son una clase de modelos para datos de conteo que ayudan a manejar el 
# exceso de ceros y la sobredispersión. Para motivar su uso, veamos algunos datos de R.

#####################################################################################################
# Descripción de los datos
#####################################################################################################

# Los siguientes datos provienen del paquete AER. Es una muestra de 4,406 individuos, de 66 años o
# más, que estaban cubiertos por Medicare en 1988. Una de las variables que proporcionan los datos
# es la cantidad de visitas al consultorio del médico. Nos interesa explicar el número de visitas
# (un recuento) con algunas variables explicativas del conjunto de datos.

# Empezaremos por cargar algunas librerías y leer los datos.

rm(list=ls())

library(ggplot2)
library(pscl)
library(AER)
library(countreg)
library(MASS)

data("NMES1988") 
nmes <- NMES1988[, c(1, 6:8, 13, 15, 18)] 

str(nmes)
summary(nmes)

attach(nmes)

ggplot(nmes, aes(x=visits)) + 
  geom_histogram()

ggplot(nmes, aes(x=visits)) + 
  geom_histogram() +
  facet_wrap(gender)

ggplot(nmes, aes(x=visits, fill=gender)) + 
  geom_histogram()

# Vemos cómo cerca de 700 personas registran 0 visitas y pocas más de 50:
sum(nmes$visits < 1)
sum(nmes$visits > 50)

#####################################################################################################
# Modelo de regresión de Poisson
#####################################################################################################

# Un enfoque común para analizar estos datos es la regresión de Poisson. Cuando hacemos ésto, 
# asumimos que los datos de conteo siguen una distribución de Poisson con una media condicionada en
# las variables explicativas. 

summary(mod1 <- glm(visits ~ ., data = nmes, family = "poisson"))

# Veamos ahora el número de ceros que predice este modelo en comparación con el número de ceros
# que hemos observado. 

lambdai <- predict(mod1, type = "response") # predicción del conteo medio esperado para cada observación
mean(lambdai)
(exp <- sum(dpois(x = 0, lambda = lambdai)))  # suma de las probabilidades de 0 visitas para cada media
round(exp)               # predicted number of 0's
sum(nmes$visits < 1)     # observed number of 0's

# Como vemos, el número de ceros que predice el modelo es muy inferior al real. En estos casos,
# surge los modelos hurdle. Son modelos en dos etapas, como ocurría con los modelos con inflación
# de ceros. En la primera etapa se especifica el proceso de generación de los ceros y en la segunda
# el proceso de generación de los conteos positivos. La idea es que una vez superada la primera valla
# hay un número de visitas mayor que cero.

#####################################################################################################
# El modelo de regresión hurdle para datos de conteo
#####################################################################################################

# Como hemos visto, se trata de un modelo en dos etapas claramente diferenciadas. La primera parte
# del modelo acostumbra a ser un modelo binario logit, que modeliza si una observación toma un valor
# positivo o no. La segunda parte del modelo es una distribución para datos de conteo truncada (bien
# sea una Poisson o NB truncada). La interpretación de este modelo en nuestro caso es que un proceso
# explica la decisión de acudir o no al médico, y otro proceso explica el número de visitas que se 
# realizan.

# El paquete "pscl" incluye la función "hurdle" para ajustar estos modelos. 

summary(mod.hurdle <- hurdle(visits ~ ., data = nmes, dist = "poisson", zero.dist = "binomial"))

# La salida del modelo ofrece primero el modelo para los conteos positivos y, en segundo lugar, el
# modelo para el proceso de conteos cero (la valla). La interpretación de este modelo es similar
# a la que hemos visto en los modelos con inflación de ceros.

# Cuántos ceros predice el modelo hurdle? Para calcularlo, utilizamos la función predict con 
# type="prob". Con ello obtenemos para cada observación la probabilidad estimada para todos los 
# conteos posibles de la muestra. En este caso es una matriz 4406x90. La primera columna de esta
# matriz recoge la probabilidad de 0 conteos. Las sumamos para obtener el número esperado de 0 
# visitas al médico.

sum(predict(mod.hurdle, type = "prob")[,1])
sum(nmes$visits < 1)

# podemos extraer todas las probabilidades
hurdle.prob <- colSums(predict(mod.hurdle, type = "prob"))

# Por construcción, el número de visitas estimado coincide con el número de visitas observado.

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
# observación, cuya media sería la estimación de la esperanza incondicional de y en el modelo PH:
mean(predict(mod.hurdle, type = "response"))

# Tomando por ejemplo las primeras cinco observaciones:

predict(mod.hurdle, type = "response")[1:5]

# Siguiendo con esta expresión, podemos también obtener el ratio para las probabilidades no-cero
# y la media del proceso sin truncamiento
predict(mod.hurdle, type = "zero")[1:5]
predict(mod.hurdle, type = "count")[1:5]

# Y por supuesto, el producto de ambos equivale al valor del conteo esperado medio:
predict(mod.hurdle, type = "zero")[1:5] * predict(mod.hurdle, type = "count")[1:5]

# El modelo PH parece haber acomodado el exceso de ceros pero ¿y la sobredispersión?

summary(mod.hurdle.nb <- hurdle(visits ~ ., data = nmes, dist = "negbin"))

# Comparamos el ajuste de ambos modelos mediante el AIC:

AIC(mod.hurdle)
AIC(mod.hurdle.nb) 

# El modelo NBH tiene menor AIC y por lo tanto, ajusta mejor los datos.

# También podemos definir diferentes regresores para las ecuaciones de participación
# e intensidad. Por ejemplo:

summary(mod.hurdle.nb2 <- hurdle(visits ~ . | gender + insurance, data = nmes, dist = "negbin"))

  
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
  
  
  
  
  

  
